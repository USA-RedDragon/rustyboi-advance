use crate::cpu::arm7tdmi::ARM7TDMI;
use crate::cpu::registers::{Flag, Registers};
use crate::memory;
use rustyboi_advance_debugger_lib::disassembler::{
    Condition, DataProcessingOp, Instruction, Operand2, Register, ShiftType,
};

// Common bit masks
const UPPER_24_BITS_MASK: u32 = 0xFFFFFF00;
const UPPER_16_BITS_MASK: u32 = 0xFFFF0000;
const UPPER_8_BITS_MASK: u32 = 0xFF000000;
const SIGN_BIT_MASK: u32 = 0x8000_0000;

// ARM/Thumb PC offsets
const ARM_PC_OFFSET: u32 = 8;
const ARM_PC_OFFSET_WITH_SHIFT: u32 = 12;
const THUMB_PC_OFFSET: u32 = 4;
const THUMB_PC_OFFSET_WITH_SHIFT: u32 = 6;
const ARM_INSTRUCTION_SIZE: u32 = 4;

// MSR field masks
const MSR_FLAGS_MASK: u32 = 0xFF000000;
const MSR_STATUS_MASK: u32 = 0x00FF0000;
const MSR_EXTENSION_MASK: u32 = 0x0000FF00;
const MSR_CONTROL_MASK: u32 = 0x000000FF;

// Mode change masks
const MODE_MASK: u32 = 0x1F;

#[inline]
pub fn check_condition(condition: &Condition, cpsr: u32) -> bool {
    let n = (cpsr & Flag::Negative as u32) != 0;
    let z = (cpsr & Flag::Zero as u32) != 0;
    let c = (cpsr & Flag::Carry as u32) != 0;
    let v = (cpsr & Flag::Overflow as u32) != 0;

    match condition {
        Condition::EQ => z,              // Equal
        Condition::NE => !z,             // Not equal
        Condition::CS => c,              // Carry set / unsigned higher or same
        Condition::CC => !c,             // Carry clear / unsigned lower
        Condition::MI => n,              // Minus / negative
        Condition::PL => !n,             // Plus / positive or zero
        Condition::VS => v,              // Overflow
        Condition::VC => !v,             // No overflow
        Condition::HI => c && !z,        // Unsigned higher
        Condition::LS => !c || z,        // Unsigned lower or same
        Condition::GE => n == v,         // Signed greater than or equal
        Condition::LT => n != v,         // Signed less than
        Condition::GT => !z && (n == v), // Signed greater than
        Condition::LE => z || (n != v),  // Signed less than or equal
        Condition::AL => true,           // Always
        Condition::NV => false,          // Never (deprecated)
    }
}

#[inline]
pub fn barrel_shift(
    value: u32,
    shift_type: &ShiftType,
    shift_amount: u32,
    carry_in: bool,
    is_immediate: bool,
) -> (u32, bool) {
    match shift_type {
        ShiftType::Lsl => {
            if shift_amount == 0 {
                return (value, carry_in);
            }
            // Logical shift left
            if shift_amount >= 32 {
                (
                    0,
                    if shift_amount == 32 {
                        value & 1 == 1
                    } else {
                        false
                    },
                )
            } else {
                let carry = (value >> (32 - shift_amount)) & 1 == 1;
                (value << shift_amount, carry)
            }
        }
        ShiftType::Lsr => {
            // Logical shift right
            // For immediate shifts, LSR #0 means LSR #32
            // For register shifts, LSR Rs where Rs=0 means no shift
            if shift_amount == 0 {
                if is_immediate {
                    // LSR #0 (immediate encoding) actually means LSR #32
                    // Result is 0, carry is bit 31 of value
                    return (0, (value >> 31) & 1 == 1);
                } else {
                    // LSR Rs where Rs=0 (register): no shift, preserve carry
                    return (value, carry_in);
                }
            }
            
            if shift_amount >= 32 {
                (
                    0,
                    if shift_amount == 32 {
                        value >> 31 == 1
                    } else {
                        false
                    },
                )
            } else {
                let carry = (value >> (shift_amount - 1)) & 1 == 1;
                (value >> shift_amount, carry)
            }
        }
        ShiftType::Asr => {
            // Arithmetic shift right
            // For immediate shifts, ASR #0 means ASR #32
            // For register shifts, ASR Rs where Rs=0 means no shift
            if shift_amount == 0 {
                if is_immediate {
                    // ASR #0 (immediate encoding) actually means ASR #32
                    // Result is 0xFFFFFFFF (if negative) or 0 (if positive)
                    // Carry is bit 31 of value
                    let carry = value >> 31 == 1;
                    return (if carry { 0xFFFFFFFF } else { 0 }, carry);
                } else {
                    // ASR Rs where Rs=0 (register): no shift, preserve carry
                    return (value, carry_in);
                }
            }
            
            if shift_amount >= 32 {
                let carry = value >> 31 == 1;
                (if carry { 0xFFFFFFFF } else { 0 }, carry)
            } else {
                let carry = (value >> (shift_amount - 1)) & 1 == 1;
                ((value as i32 >> shift_amount) as u32, carry)
            }
        }
        ShiftType::Ror => {
            // Rotate right
            if shift_amount == 0 {
                if is_immediate {
                    // ROR #0 (immediate) is actually RRX (Rotate Right Extended)
                    // Rotate right by 1 through carry flag
                    let new_carry = value & 1 == 1;
                    let result = (value >> 1) | (if carry_in { 1 << 31 } else { 0 });
                    (result, new_carry)
                } else {
                    // ROR Rs where Rs=0 (register) is a no-op
                    (value, carry_in)
                }
            } else {
                // For ROR, shift_amount is taken modulo 32
                let shift_amount = shift_amount % 32;
                if shift_amount == 0 {
                    // Shift by exact multiple of 32: value unchanged, carry = bit 31
                    (value, (value >> 31) & 1 == 1)
                } else {
                    let carry = (value >> (shift_amount - 1)) & 1 == 1;
                    (value.rotate_right(shift_amount), carry)
                }
            }
        }
    }
}

#[inline]
pub fn evaluate_operand2(
    operand2: &Operand2,
    registers: &Registers,
    carry_in: bool,
) -> (u32, bool) {
    match operand2 {
        Operand2::Immediate { value, rotation } => {
            // If rotation is non-zero, carry is set to bit 31 of the rotated value
            let carry = if *rotation > 0 {
                (*value & 0x80000000) != 0
            } else {
                carry_in
            };
            (*value, carry)
        }
        Operand2::Register(reg) => (registers.read_register(*reg), carry_in),
        Operand2::RegisterShifted {
            reg,
            shift_type,
            shift_amount,
        } => {
            let value = registers.read_register(*reg);
            barrel_shift(value, shift_type, *shift_amount, carry_in, true)
        }
        Operand2::RegisterShiftedByRegister {
            reg,
            shift_type,
            shift_reg,
        } => {
            let value = registers.read_register(*reg);
            let shift_amount = registers.read_register(*shift_reg) & 0xFF; // Only bottom byte used
            barrel_shift(value, shift_type, shift_amount, carry_in, false)
        }
    }
}

#[inline]
pub fn set_nz_flags(registers: &mut Registers, result: u32) {
    registers.set_flag(Flag::Negative, (result as i32) < 0);
    registers.set_flag(Flag::Zero, result == 0);
}

#[inline]
pub fn set_flags_add(registers: &mut Registers, op1: u32, op2: u32, result: u32) {
    set_nz_flags(registers, result);
    // Carry: unsigned overflow
    registers.set_flag(Flag::Carry, result < op1);
    let overflow = ((op1 ^ result) & (op2 ^ result) & SIGN_BIT_MASK) != 0;
    registers.set_flag(Flag::Overflow, overflow);
}

#[inline]
pub fn set_flags_sub(registers: &mut Registers, op1: u32, op2: u32, result: u32) {
    set_nz_flags(registers, result);
    // Carry: NOT borrow (set if op1 >= op2)
    registers.set_flag(Flag::Carry, op1 >= op2);
    let overflow = ((op1 ^ op2) & (op1 ^ result) & SIGN_BIT_MASK) != 0;
    registers.set_flag(Flag::Overflow, overflow);
}

#[inline]
pub fn set_flags_logical(registers: &mut Registers, result: u32, carry: bool) {
    set_nz_flags(registers, result);
    registers.set_flag(Flag::Carry, carry);
    // Overflow flag is unaffected by logical operations
}

#[inline]
/// Calculate the number of internal cycles for multiply operations
/// based on the multiplier value (Rs)
fn calculate_multiply_cycles(multiplier: u32) -> u32 {
    let upper_24 = multiplier & UPPER_24_BITS_MASK;
    if upper_24 == 0 || upper_24 == UPPER_24_BITS_MASK {
        return 1;
    }

    let upper_16 = multiplier & UPPER_16_BITS_MASK;
    if upper_16 == 0 || upper_16 == UPPER_16_BITS_MASK {
        return 2;
    }

    let upper_8 = multiplier & UPPER_8_BITS_MASK;
    if upper_8 == 0 || upper_8 == UPPER_8_BITS_MASK {
        return 3;
    }

    4
}

/// Enum for different load value types
#[derive(Debug, Clone, Copy)]
enum LoadType {
    Word,
    Byte,
    Halfword,
    SignedByte,
    SignedHalfword,
}

/// Enum for different store value types
#[derive(Debug, Clone, Copy)]
enum StoreType {
    Word,
    Byte,
    Halfword,
}

impl ARM7TDMI {
    /// Generic load instruction handler
    /// Handles LDR, LDRB, LDRH, LDRSB, LDRSH with unified logic
    #[inline]
    fn execute_load_generic(
        &mut self,
        rd: Register,
        rn: Register,
        addressing: &rustyboi_advance_debugger_lib::disassembler::AddressingMode,
        load_type: LoadType,
        instruction_pc: u32,
        mmio: &mut memory::mmio::Mmio,
    ) -> u32 {
        use crate::memory::Addressable;

        let (addr, writeback) = self.calculate_address(rn, addressing, instruction_pc);

        // Load value based on type
        let value = match load_type {
            LoadType::Word => {
                // Word access - rotate if unaligned
                let word = (mmio as &dyn Addressable).read32(addr & !0x3);
                let rotation = (addr & 0x3) * 8;
                word.rotate_right(rotation)
            }
            LoadType::Byte => (mmio as &dyn Addressable).read(addr) as u32,
            LoadType::Halfword => {
                // Halfword - rotate if unaligned
                let halfword = (mmio as &dyn Addressable).read16(addr & !0x1);
                let rotation = (addr & 0x1) * 8;
                (halfword as u32).rotate_right(rotation)
            }
            LoadType::SignedByte => {
                let byte = (mmio as &dyn Addressable).read(addr) as i8;
                byte as i32 as u32 // Sign-extend
            }
            LoadType::SignedHalfword => {
                let halfword = (mmio as &dyn Addressable).read16(addr & !0x1);
                
                // If address is odd, we need to rotate and sign-extend the upper byte
                if (addr & 0x1) != 0 {
                    // Misaligned: extract upper byte and sign-extend it
                    let byte = (halfword >> 8) as u8 as i8;
                    byte as i32 as u32
                } else {
                    // Aligned: sign-extend the full halfword
                    halfword as i16 as i32 as u32
                }
            }
        };

        // Writeback if needed
        // When Rd == Rn, the loaded value should take precedence
        if writeback {
            let new_base = self.calculate_writeback_value(rn, addressing, instruction_pc);
            self.registers.write_register(rn, new_base);
        }

        self.registers.write_register(rd, value);

        // If loading to PC, flush pipeline
        if rd == Register::PC {
            let is_thumb = self.registers.get_flag(Flag::ThumbState);
            let aligned_value = if is_thumb { value & !0x1 } else { value & !0x3 };
            self.branch_to(aligned_value);
            return 0; // Cycles counted during pipeline flush/refill
        }

        // Load instructions take 1S + 1N + 1I cycles
        self.add_internal_cycles(1);
        2
    }

    /// Generic store instruction handler
    /// Handles STR, STRB, STRH with unified logic
    #[inline]
    fn execute_store_generic(
        &mut self,
        rd: Register,
        rn: Register,
        addressing: &rustyboi_advance_debugger_lib::disassembler::AddressingMode,
        store_type: StoreType,
        mmio: &mut memory::mmio::Mmio,
        instruction_pc: u32,
    ) -> u32 {
        use crate::memory::Addressable;

        let mut value = self.registers.read_register(rd);

        if rd == Register::PC {
            value = value.wrapping_add(4) // Always PC + 4 for STR, regardless of mode
        }

        let (addr, writeback) = self.calculate_address(rn, addressing, instruction_pc);

        // Store value based on type
        match store_type {
            StoreType::Word => {
                mmio.write32(addr & !0x3, value);
            }
            StoreType::Byte => {
                mmio.write(addr, value as u8);
            }
            StoreType::Halfword => {
                mmio.write16(addr & !0x1, value as u16);
            }
        }

        // Writeback if needed
        if writeback {
            let new_base = self.calculate_writeback_value(rn, addressing, instruction_pc);
            self.registers.write_register(rn, new_base);
        }

        // Store instructions take 2N cycles
        2
    }

    #[inline]
    pub fn execute_data_processing(
        &mut self,
        op: &DataProcessingOp,
        set_flags: bool,
        rn: Option<Register>,
        rd: Option<Register>,
        operand2: &Operand2,
        instruction_pc: u32,
    ) -> u32 {
        let carry_in = self.registers.get_flag(Flag::Carry);
        
        // Calculate PC value for use as operand
        // When Operand2 uses a register shift, ALL PC reads (whether Rn or Rm) 
        // see PC as instruction_pc + 12 (ARM) or + 6 (Thumb) due to the extra internal cycle
        // Otherwise, PC reads as instruction_pc + 8 (ARM) or + 4 (Thumb)
        let is_thumb = self.registers.get_flag(Flag::ThumbState);
        let uses_register_shift = matches!(
            operand2,
            Operand2::RegisterShiftedByRegister { .. }
        );
        
        let pc_as_operand = if uses_register_shift {
            if is_thumb {
                instruction_pc.wrapping_add(THUMB_PC_OFFSET_WITH_SHIFT)
            } else {
                instruction_pc.wrapping_add(ARM_PC_OFFSET_WITH_SHIFT)
            }
        } else if is_thumb {
            instruction_pc.wrapping_add(THUMB_PC_OFFSET)
        } else {
            instruction_pc.wrapping_add(ARM_PC_OFFSET)
        };
        
        // Temporarily set PC to the correct value for operand reads
        let original_pc = self.registers.pc;
        self.registers.pc = pc_as_operand;
        
        let (operand2_value, shifter_carry) =
            evaluate_operand2(operand2, &self.registers, carry_in);

        // Get Rn value if needed
        let rn_value = rn.map(|r| self.registers.read_register(r)).unwrap_or(0);
        
        // Restore original PC
        self.registers.pc = original_pc;

        // Perform the operation
        let result = match op {
            DataProcessingOp::And => {
                let result = rn_value & operand2_value;
                if set_flags {
                    set_flags_logical(&mut self.registers, result, shifter_carry);
                }
                result
            }
            DataProcessingOp::Eor => {
                let result = rn_value ^ operand2_value;
                if set_flags {
                    set_flags_logical(&mut self.registers, result, shifter_carry);
                }
                result
            }
            DataProcessingOp::Sub => {
                let result = rn_value.wrapping_sub(operand2_value);
                if set_flags {
                    set_flags_sub(&mut self.registers, rn_value, operand2_value, result);
                }
                result
            }
            DataProcessingOp::Rsb => {
                let result = operand2_value.wrapping_sub(rn_value);
                if set_flags {
                    set_flags_sub(&mut self.registers, operand2_value, rn_value, result);
                }
                result
            }
            DataProcessingOp::Add => {
                let result = rn_value.wrapping_add(operand2_value);
                if set_flags {
                    set_flags_add(&mut self.registers, rn_value, operand2_value, result);
                }
                result
            }
            DataProcessingOp::Adc => {
                let carry = if carry_in { 1 } else { 0 };
                let result = rn_value.wrapping_add(operand2_value).wrapping_add(carry);
                if set_flags {
                    // For ADC, we need to detect carry in two stages:
                    // 1. Did rn + operand2 produce a carry?
                    // 2. Did (rn + operand2) + carry produce a carry?
                    let (temp, carry1) = rn_value.overflowing_add(operand2_value);
                    let (_, carry2) = temp.overflowing_add(carry);
                    let carry_flag = carry1 || carry2;

                    set_nz_flags(&mut self.registers, result);
                    self.registers.set_flag(Flag::Carry, carry_flag);
                    // Overflow for ADC
                    let overflow =
                        ((rn_value ^ result) & (operand2_value ^ result) & 0x8000_0000) != 0;
                    self.registers.set_flag(Flag::Overflow, overflow);
                }
                result
            }
            DataProcessingOp::Sbc => {
                let carry = if carry_in { 0 } else { 1 }; // Inverted for subtraction
                let result = rn_value.wrapping_sub(operand2_value).wrapping_sub(carry);
                if set_flags {
                    let borrow =
                        operand2_value > rn_value || (operand2_value == rn_value && carry != 0);
                    set_nz_flags(&mut self.registers, result);
                    self.registers.set_flag(Flag::Carry, !borrow);
                    // Overflow for SBC
                    let overflow =
                        ((rn_value ^ operand2_value) & (rn_value ^ result) & 0x8000_0000) != 0;
                    self.registers.set_flag(Flag::Overflow, overflow);
                }
                result
            }
            DataProcessingOp::Rsc => {
                let carry = if carry_in { 0 } else { 1 }; // Inverted for subtraction
                let result = operand2_value.wrapping_sub(rn_value).wrapping_sub(carry);
                if set_flags {
                    let borrow =
                        rn_value > operand2_value || (rn_value == operand2_value && carry != 0);
                    set_nz_flags(&mut self.registers, result);
                    self.registers.set_flag(Flag::Carry, !borrow);
                    // Overflow for RSC
                    let overflow =
                        ((operand2_value ^ rn_value) & (operand2_value ^ result) & 0x8000_0000)
                            != 0;
                    self.registers.set_flag(Flag::Overflow, overflow);
                }
                result
            }
            DataProcessingOp::Tst => {
                let result = rn_value & operand2_value;
                set_flags_logical(&mut self.registers, result, shifter_carry);
                result // Not written to register
            }
            DataProcessingOp::Teq => {
                let result = rn_value ^ operand2_value;
                set_flags_logical(&mut self.registers, result, shifter_carry);
                result // Not written to register
            }
            DataProcessingOp::Cmp => {
                let result = rn_value.wrapping_sub(operand2_value);
                set_flags_sub(&mut self.registers, rn_value, operand2_value, result);
                result // Not written to register
            }
            DataProcessingOp::Cmn => {
                let result = rn_value.wrapping_add(operand2_value);
                set_flags_add(&mut self.registers, rn_value, operand2_value, result);
                result // Not written to register
            }
            DataProcessingOp::Orr => {
                let result = rn_value | operand2_value;
                if set_flags {
                    set_flags_logical(&mut self.registers, result, shifter_carry);
                }
                result
            }
            DataProcessingOp::Mov => {
                if set_flags {
                    set_flags_logical(&mut self.registers, operand2_value, shifter_carry);
                }
                operand2_value
            }
            DataProcessingOp::Bic => {
                let result = rn_value & !operand2_value;
                if set_flags {
                    set_flags_logical(&mut self.registers, result, shifter_carry);
                }
                result
            }
            DataProcessingOp::Mvn => {
                let result = !operand2_value;
                if set_flags {
                    set_flags_logical(&mut self.registers, result, shifter_carry);
                }
                result
            }
        };

        // Special case: For comparison instructions (TST, TEQ, CMP, CMN) with Rd=PC and S=1,
        // restore CPSR from SPSR without branching (technically unpredictable behavior)
        let is_comparison = matches!(
            op,
            DataProcessingOp::Tst | DataProcessingOp::Teq | DataProcessingOp::Cmp | DataProcessingOp::Cmn
        );
        
        if is_comparison && rd == Some(Register::PC) && set_flags {
            // Restore CPSR from SPSR but don't write to PC or branch
            let spsr = self.registers.get_spsr();
            self.registers.cpsr = spsr;
            let new_thumb_state = (spsr & Flag::ThumbState as u32) != 0;
            self.registers.set_flag(Flag::ThumbState, new_thumb_state);
            
            // Don't write result to any register, don't branch
            // Continue normal execution (pipeline not flushed)
            return 1; // 1S cycle for the instruction
        }

        // Write result to destination register (if not a test/compare instruction)
        if let Some(rd) = rd {
            self.registers.write_register(rd, result);

            // If writing to PC, flush the pipeline
            if rd == Register::PC {
                // Special case: If S bit is set and Rd is PC, restore CPSR from SPSR
                // This is used for exception returns (e.g., SUBS PC, LR, #4)
                if set_flags {
                    let spsr = self.registers.get_spsr();
                    self.registers.cpsr = spsr;
                    let new_thumb_state = (spsr & Flag::ThumbState as u32) != 0;
                    self.registers.set_flag(Flag::ThumbState, new_thumb_state);
                }
                
                // Align based on current mode (after potentially restoring CPSR)
                // ARM mode: word aligned (clear bits [1:0])
                // Thumb mode: halfword aligned (clear bit 0)
                let is_thumb = self.registers.get_flag(Flag::ThumbState);
                let aligned_result = if is_thumb {
                    result & !0x1
                } else {
                    result & !0x3
                };
                self.branch_to(aligned_result);
                return 0; // Cycles counted during pipeline flush/refill
            }
        }

        // Data processing instructions take 1S cycle (or 1S + 1N if shift by register)
        if let Operand2::RegisterShiftedByRegister { .. } = operand2 {
            // Shift by register adds 1 internal cycle
            self.add_internal_cycles(1);
        }

        1
    }

    #[inline]
    pub fn execute_branch(&mut self, target: u32) -> u32 {
        self.branch_to(target);
        0
    }

    #[inline]
    pub fn execute_branch_link(&mut self, target: u32, instruction_pc: u32) -> u32 {
        let is_thumb = self.registers.get_flag(Flag::ThumbState);
        let return_addr = if is_thumb {
            // Thumb BL is a 32-bit instruction (4 bytes), not a regular 16-bit instruction
            // In Thumb mode, set bit 0 of LR to indicate return to Thumb mode
            instruction_pc.wrapping_add(4) | 1
        } else {
            instruction_pc.wrapping_add(ARM_INSTRUCTION_SIZE)
        };
        self.registers.set_lr(return_addr);
        self.branch_to(target);
        0
    }

    #[inline]
    pub fn execute_branch_exchange(&mut self, rm: Register) -> u32 {
        let target = self.registers.read_register(rm);

        // Bit 0 determines Thumb state
        self.registers.set_flag(Flag::ThumbState, (target & 1) == 1);

        // Branch to target with proper alignment
        // Thumb: halfword aligned (clear bit 0)
        // ARM: word aligned (clear bits [1:0])
        let aligned_target = if (target & 1) == 1 {
            target & !0x1 // Thumb mode - halfword align
        } else {
            target & !0x3 // ARM mode - word align
        };
        self.branch_to(aligned_target);
        0
    }

    #[inline]
    /// Helper to execute instruction if condition passes
    fn execute_if_condition<F>(&mut self, condition: &Condition, execute_fn: F) -> u32
    where
        F: FnOnce(&mut Self) -> u32,
    {
        if check_condition(condition, self.registers.cpsr) {
            execute_fn(self)
        } else {
            1 // Failed condition still takes 1S cycle
        }
    }

    /// Calculate the effective address for LDR/STR based on addressing mode
    /// Returns (address, writeback_needed)
    #[inline]
    fn calculate_address(
        &mut self,
        rn: Register,
        addressing: &rustyboi_advance_debugger_lib::disassembler::AddressingMode,
        instruction_pc: u32,
    ) -> (u32, bool) {
        use rustyboi_advance_debugger_lib::disassembler::AddressingMode;

        let base = if rn == Register::PC {
            let is_thumb = self.registers.get_flag(Flag::ThumbState);
            if is_thumb {
                instruction_pc.wrapping_add(THUMB_PC_OFFSET)
            } else {
                instruction_pc.wrapping_add(ARM_PC_OFFSET)
            }
        } else {
            self.registers.read_register(rn)
        };

        match addressing {
            AddressingMode::Offset { offset, writeback } => {
                let addr = base.wrapping_add(*offset as u32);
                (addr, *writeback)
            }
            AddressingMode::RegisterOffset {
                reg,
                shift,
                add,
                writeback,
            } => {
                let offset_value = self.registers.read_register(*reg);
                let offset = if let Some((shift_type, shift_amount)) = shift {
                    let carry = self.registers.get_flag(Flag::Carry);
                    // Shifts in addressing modes are always immediate
                    barrel_shift(offset_value, shift_type, *shift_amount, carry, true).0
                } else {
                    offset_value
                };

                let addr = if *add {
                    base.wrapping_add(offset)
                } else {
                    base.wrapping_sub(offset)
                };
                (addr, *writeback)
            }
            AddressingMode::PreIndexed { offset } => {
                let addr = base.wrapping_add(*offset as u32);
                (addr, true) // Pre-indexed always writes back
            }
            AddressingMode::PostIndexed { .. } => {
                // Post-indexed: use base address, writeback will be handled by caller
                (base, true)
            }
        }
    }

    /// Calculate the new base value for writeback
    /// This is separate from calculate_address to allow proper ordering in load/store
    #[inline]
    fn calculate_writeback_value(
        &mut self,
        rn: Register,
        addressing: &rustyboi_advance_debugger_lib::disassembler::AddressingMode,
        instruction_pc: u32,
    ) -> u32 {
        use rustyboi_advance_debugger_lib::disassembler::AddressingMode;

        let base = if rn == Register::PC {
            let is_thumb = self.registers.get_flag(Flag::ThumbState);
            if is_thumb {
                instruction_pc.wrapping_add(THUMB_PC_OFFSET)
            } else {
                instruction_pc.wrapping_add(ARM_PC_OFFSET)
            }
        } else {
            self.registers.read_register(rn)
        };

        match addressing {
            AddressingMode::Offset { offset, .. } => base.wrapping_add(*offset as u32),
            AddressingMode::RegisterOffset {
                reg,
                shift,
                add,
                ..
            } => {
                let offset_value = self.registers.read_register(*reg);
                let offset = if let Some((shift_type, shift_amount)) = shift {
                    let carry = self.registers.get_flag(Flag::Carry);
                    barrel_shift(offset_value, shift_type, *shift_amount, carry, true).0
                } else {
                    offset_value
                };

                if *add {
                    base.wrapping_add(offset)
                } else {
                    base.wrapping_sub(offset)
                }
            }
            AddressingMode::PreIndexed { offset } => base.wrapping_add(*offset as u32),
            AddressingMode::PostIndexed { offset } => base.wrapping_add(*offset as u32),
        }
    }

    #[inline]
    pub fn execute_ldr(
        &mut self,
        rd: Register,
        rn: Register,
        addressing: &rustyboi_advance_debugger_lib::disassembler::AddressingMode,
        byte_access: bool,
        mmio: &mut memory::mmio::Mmio,
        instruction_pc: u32,
    ) -> u32 {
        let load_type = if byte_access {
            LoadType::Byte
        } else {
            LoadType::Word
        };
        self.execute_load_generic(rd, rn, addressing, load_type, instruction_pc, mmio)
    }

    #[inline]
    pub fn execute_str(
        &mut self,
        rd: Register,
        rn: Register,
        addressing: &rustyboi_advance_debugger_lib::disassembler::AddressingMode,
        byte_access: bool,
        mmio: &mut memory::mmio::Mmio,
        instruction_pc: u32,
    ) -> u32 {
        let store_type = if byte_access {
            StoreType::Byte
        } else {
            StoreType::Word
        };
        self.execute_store_generic(rd, rn, addressing, store_type, mmio, instruction_pc)
    }

    #[inline]
    pub fn execute_ldrh(
        &mut self,
        rd: Register,
        rn: Register,
        addressing: &rustyboi_advance_debugger_lib::disassembler::AddressingMode,
        mmio: &mut memory::mmio::Mmio,
        instruction_pc: u32,
    ) -> u32 {
        self.execute_load_generic(rd, rn, addressing, LoadType::Halfword, instruction_pc, mmio)
    }

    #[inline]
    pub fn execute_strh(
        &mut self,
        rd: Register,
        rn: Register,
        addressing: &rustyboi_advance_debugger_lib::disassembler::AddressingMode,
        mmio: &mut memory::mmio::Mmio,
        instruction_pc: u32,
    ) -> u32 {
        self.execute_store_generic(rd, rn, addressing, StoreType::Halfword, mmio, instruction_pc)
    }

    #[inline]
    pub fn execute_ldrsb(
        &mut self,
        rd: Register,
        rn: Register,
        addressing: &rustyboi_advance_debugger_lib::disassembler::AddressingMode,
        mmio: &mut memory::mmio::Mmio,
        instruction_pc: u32,
    ) -> u32 {
        self.execute_load_generic(rd, rn, addressing, LoadType::SignedByte, instruction_pc, mmio)
    }

    #[inline]
    pub fn execute_ldrsh(
        &mut self,
        rd: Register,
        rn: Register,
        addressing: &rustyboi_advance_debugger_lib::disassembler::AddressingMode,
        mmio: &mut memory::mmio::Mmio,
        instruction_pc: u32,
    ) -> u32 {
        self.execute_load_generic(rd, rn, addressing, LoadType::SignedHalfword, instruction_pc, mmio)
    }

    /// Execute SWP (Swap) instruction
    /// Atomically swaps a word or byte between a register and memory
    /// Format: SWP{B} Rd, Rm, [Rn]
    /// 1. temp = memory[Rn]
    /// 2. memory[Rn] = Rm
    /// 3. Rd = temp
    #[inline]
    pub fn execute_swp(
        &mut self,
        rd: Register,
        rm: Register,
        rn: Register,
        byte: bool,
        mmio: &mut memory::mmio::Mmio,
    ) -> u32 {
        use crate::memory::Addressable;

        let addr = self.registers.read_register(rn);
        let rm_value = self.registers.read_register(rm);

        if byte {
            // SWPB - swap byte
            let temp = (mmio as &dyn Addressable).read(addr);
            (mmio as &mut dyn Addressable).write(addr, rm_value as u8);
            self.registers.write_register(rd, temp as u32);
        } else {
            // SWP - swap word
            // Word access uses aligned address
            let aligned_addr = addr & !0x3;
            let temp = (mmio as &dyn Addressable).read32(aligned_addr);
            
            // If address is misaligned, rotate the loaded value like LDR does
            let result = if addr & 0x3 != 0 {
                let rotation = (addr & 0x3) * 8;
                temp.rotate_right(rotation)
            } else {
                temp
            };
            
            // Store to aligned address (no rotation on store)
            (mmio as &mut dyn Addressable).write32(aligned_addr, rm_value);
            self.registers.write_register(rd, result);
        }

        // SWP takes 1S + 2N + 1I cycles (1 sequential, 2 non-sequential, 1 internal)
        self.add_internal_cycles(1);
        3
    }

    #[inline]
    pub fn execute_mrs(&mut self, rd: Register, spsr: bool) -> u32 {
        let value = if spsr {
            // Read SPSR (Saved Program Status Register)
            self.registers.get_spsr()
        } else {
            // Read CPSR (Current Program Status Register)
            self.registers.cpsr
        };

        self.registers.write_register(rd, value);

        // MRS takes 1S cycle
        1
    }

    /// Execute MSR (Move to PSR) instruction
    #[inline]
    pub fn execute_msr(
        &mut self,
        fields: &rustyboi_advance_debugger_lib::disassembler::MsrFields,
        operand: &rustyboi_advance_debugger_lib::disassembler::MsrOperand,
        spsr: bool,
    ) -> u32 {
        use rustyboi_advance_debugger_lib::disassembler::MsrOperand;

        // Get the value to write
        let value = match operand {
            MsrOperand::Immediate(imm) => *imm,
            MsrOperand::Register(reg) => self.registers.read_register(*reg),
        };

        let mut mask = 0u32;
        if fields.f {
            mask |= MSR_FLAGS_MASK;
        }
        if fields.s {
            mask |= MSR_STATUS_MASK;
        }
        if fields.x {
            mask |= MSR_EXTENSION_MASK;
        }
        if fields.c {
            let current_mode = self.registers.get_current_mode();
            if current_mode != crate::cpu::registers::Mode::User {
                mask |= MSR_CONTROL_MASK;
            }
        }

        if spsr {
            // Write to SPSR
            let current_spsr = self.registers.get_spsr();
            let new_spsr = (current_spsr & !mask) | (value & mask);
            self.registers.set_spsr(new_spsr);
        } else {
            // Write to CPSR
            let old_cpsr = self.registers.cpsr;
            let new_cpsr = (old_cpsr & !mask) | (value & mask);

            // Check if mode bits changed
            let old_mode = old_cpsr & MODE_MASK;
            let new_mode = new_cpsr & MODE_MASK;

            // Validate the new mode is legal
            if new_mode != old_mode && mask & 0x1F != 0 {
                // Verify the mode is valid
                let _mode = crate::cpu::registers::Mode::from(new_mode);
                // If from() doesn't panic, the mode is valid
            }

            self.registers.cpsr = new_cpsr;
            // Note: Register banking is automatic based on cpsr value
        }

        // MSR takes 1S cycle
        1
    }

    /// Execute ADR (Address) pseudo-instruction
    /// ADR loads a PC-relative address into a register
    #[inline]
    pub fn execute_adr(&mut self, rd: Register, target: u32) -> u32 {
        // ADR is typically implemented as ADD/SUB with PC as the base
        // The target address is already calculated by the decoder
        self.registers.write_register(rd, target);

        // ADR takes 1S cycle (same as MOV immediate)
        1
    }

    /// Execute MUL (Multiply) instruction
    #[inline]
    pub fn execute_mul(
        &mut self,
        rd: Register,
        rm: Register,
        rs: Register,
        set_flags: bool,
    ) -> u32 {
        let rm_val = self.registers.read_register(rm);
        let rs_val = self.registers.read_register(rs);

        let result = rm_val.wrapping_mul(rs_val);
        self.registers.write_register(rd, result);

        if set_flags {
            set_nz_flags(&mut self.registers, result);
            // Carry flag is destroyed (unpredictable), Overflow is unaffected
        }

        // MUL timing: 1S + mI cycles where m depends on multiplier value
        let m = calculate_multiply_cycles(rs_val);
        self.add_internal_cycles(m);
        1 + m
    }

    /// Execute MLA (Multiply-Accumulate) instruction
    #[inline]
    pub fn execute_mla(
        &mut self,
        rd: Register,
        rm: Register,
        rs: Register,
        rn: Register,
        set_flags: bool,
    ) -> u32 {
        let rm_val = self.registers.read_register(rm);
        let rs_val = self.registers.read_register(rs);
        let rn_val = self.registers.read_register(rn);

        let result = rm_val.wrapping_mul(rs_val).wrapping_add(rn_val);
        self.registers.write_register(rd, result);

        if set_flags {
            set_nz_flags(&mut self.registers, result);
            // Carry flag is destroyed (unpredictable), Overflow is unaffected
        }

        // MLA timing: 1S + (m+1)I cycles
        let m = calculate_multiply_cycles(rs_val);
        self.add_internal_cycles(m + 1);
        1 + m + 1
    }

    /// Helper to set flags for 64-bit multiply results
    #[inline]
    fn set_flags_for_long_multiply(&mut self, result: u64) {
        let negative = (result >> 63) == 1;
        let zero = result == 0;
        self.registers.set_flag(Flag::Negative, negative);
        self.registers.set_flag(Flag::Zero, zero);
        // Carry and Overflow are destroyed (unpredictable)
    }

    /// Execute UMULL (Unsigned Multiply Long) instruction
    #[inline]
    pub fn execute_umull(
        &mut self,
        rd_lo: Register,
        rd_hi: Register,
        rm: Register,
        rs: Register,
        set_flags: bool,
    ) -> u32 {
        let rm_val = self.registers.read_register(rm) as u64;
        let rs_val = self.registers.read_register(rs) as u64;

        let result = rm_val.wrapping_mul(rs_val);
        self.registers.write_register(rd_lo, result as u32);
        self.registers.write_register(rd_hi, (result >> 32) as u32);

        if set_flags {
            self.set_flags_for_long_multiply(result);
        }

        // UMULL timing: 1S + (m+1)I cycles
        let m = calculate_multiply_cycles(self.registers.read_register(rs));
        self.add_internal_cycles(m + 1);
        1 + m + 1
    }

    /// Execute UMLAL (Unsigned Multiply-Accumulate Long) instruction
    #[inline]
    pub fn execute_umlal(
        &mut self,
        rd_lo: Register,
        rd_hi: Register,
        rm: Register,
        rs: Register,
        set_flags: bool,
    ) -> u32 {
        let rm_val = self.registers.read_register(rm) as u64;
        let rs_val = self.registers.read_register(rs) as u64;
        let rd_lo_val = self.registers.read_register(rd_lo) as u64;
        let rd_hi_val = self.registers.read_register(rd_hi) as u64;

        let accumulator = (rd_hi_val << 32) | rd_lo_val;
        let result = rm_val.wrapping_mul(rs_val).wrapping_add(accumulator);

        self.registers.write_register(rd_lo, result as u32);
        self.registers.write_register(rd_hi, (result >> 32) as u32);

        if set_flags {
            self.set_flags_for_long_multiply(result);
        }

        // UMLAL timing: 1S + (m+2)I cycles
        let m = calculate_multiply_cycles(self.registers.read_register(rs));
        self.add_internal_cycles(m + 2);
        1 + m + 2
    }

    /// Execute SMULL (Signed Multiply Long) instruction
    #[inline]
    pub fn execute_smull(
        &mut self,
        rd_lo: Register,
        rd_hi: Register,
        rm: Register,
        rs: Register,
        set_flags: bool,
    ) -> u32 {
        let rm_val = self.registers.read_register(rm) as i32 as i64;
        let rs_val = self.registers.read_register(rs) as i32 as i64;

        let result = rm_val.wrapping_mul(rs_val) as u64;
        self.registers.write_register(rd_lo, result as u32);
        self.registers.write_register(rd_hi, (result >> 32) as u32);

        if set_flags {
            self.set_flags_for_long_multiply(result);
        }

        // SMULL timing: 1S + (m+1)I cycles
        let m = calculate_multiply_cycles(self.registers.read_register(rs));
        self.add_internal_cycles(m + 1);
        1 + m + 1
    }

    /// Execute SMLAL (Signed Multiply-Accumulate Long) instruction
    #[inline]
    pub fn execute_smlal(
        &mut self,
        rd_lo: Register,
        rd_hi: Register,
        rm: Register,
        rs: Register,
        set_flags: bool,
    ) -> u32 {
        let rm_val = self.registers.read_register(rm) as i32 as i64;
        let rs_val = self.registers.read_register(rs) as i32 as i64;
        let rd_lo_val = self.registers.read_register(rd_lo) as u64;
        let rd_hi_val = self.registers.read_register(rd_hi) as u64;

        let accumulator = ((rd_hi_val << 32) | rd_lo_val) as i64;
        let result = rm_val.wrapping_mul(rs_val).wrapping_add(accumulator) as u64;

        self.registers.write_register(rd_lo, result as u32);
        self.registers.write_register(rd_hi, (result >> 32) as u32);

        if set_flags {
            self.set_flags_for_long_multiply(result);
        }

        // SMLAL timing: 1S + (m+2)I cycles
        let m = calculate_multiply_cycles(self.registers.read_register(rs));
        self.add_internal_cycles(m + 2);
        1 + m + 2
    }

    /// Execute LDM (Load Multiple) instruction
    #[inline]
    pub fn execute_ldm(
        &mut self,
        rn: Register,
        register_list: &rustyboi_advance_debugger_lib::disassembler::RegisterList,
        addressing_mode: rustyboi_advance_debugger_lib::disassembler::AddressingModeType,
        writeback: bool,
        user_mode: bool,
        mmio: &mut memory::mmio::Mmio,
    ) -> u32 {
        use crate::memory::Addressable;
        use rustyboi_advance_debugger_lib::disassembler::AddressingModeType;

        let mut addr = self.registers.read_register(rn);
        let register_count = register_list.mask.count_ones();

        // Special case: Empty register list
        // LDM with empty rlist loads PC and adds 0x40 to base register
        if register_count == 0 {
            let value = (mmio as &dyn Addressable).read32(addr);
            self.registers.write_register(Register::PC, value);
            
            if writeback {
                self.registers.write_register(rn, addr.wrapping_add(0x40));
            }
            
            // Flush pipeline since we loaded PC
            let is_thumb = self.registers.get_flag(Flag::ThumbState);
            let aligned_value = if is_thumb { value & !0x1 } else { value & !0x3 };
            self.branch_to(aligned_value);
            return 0; // Cycles counted during pipeline flush/refill
        }

        // Calculate start address and direction based on addressing mode
        let (mut start_addr, increment) = match addressing_mode {
            AddressingModeType::IA => {
                // Increment After: start at address, increment after each transfer
                (addr, true)
            }
            AddressingModeType::IB => {
                // Increment Before: increment before each transfer
                addr = addr.wrapping_add(4);
                (addr, true)
            }
            AddressingModeType::DA => {
                // Decrement After: start at address, decrement after each transfer
                (addr, false)
            }
            AddressingModeType::DB => {
                // Decrement Before: decrement before each transfer
                addr = addr.wrapping_sub(4 * register_count);
                (addr, false)
            }
        };

        // Adjust for DA mode
        if addressing_mode == AddressingModeType::DA {
            start_addr = addr.wrapping_sub(4 * (register_count - 1));
        }

        // Load registers in order (R0-R15)
        let mut current_addr = start_addr;
        let mut loaded_pc = false;

        for reg_num in 0..16 {
            if register_list.contains(reg_num) {
                let value = (mmio as &dyn Addressable).read32(current_addr);
                let reg = Register::from_u32(reg_num);

                if user_mode && reg_num != 15 {
                    // User mode transfer (S bit set, not loading PC)
                    // Load into user mode registers instead of current mode
                    self.registers.write_user_register(reg, value);
                } else {
                    self.registers.write_register(reg, value);
                }

                if reg_num == 15 {
                    loaded_pc = true;
                }

                current_addr = current_addr.wrapping_add(4);
            }
        }

        // Writeback
        // Note: If Rn is in the register list, writeback behavior is unpredictable
        // Common implementation: don't writeback if Rn is in the list
        if writeback && !register_list.contains(rn as u32) {
            let final_addr = if increment {
                self.registers
                    .read_register(rn)
                    .wrapping_add(4 * register_count)
            } else {
                self.registers
                    .read_register(rn)
                    .wrapping_sub(4 * register_count)
            };
            self.registers.write_register(rn, final_addr);
        }

        // If PC was loaded, flush pipeline
        if loaded_pc {
            let pc_value = self.registers.pc;
            let is_thumb = self.registers.get_flag(Flag::ThumbState);
            let aligned_pc = if is_thumb {
                pc_value & !0x1
            } else {
                pc_value & !0x3
            };
            self.branch_to(aligned_pc);
            return 0; // Cycles counted during pipeline flush/refill
        }

        // LDM takes nS + 1N + 1I cycles (n sequential accesses + 1 non-sequential + 1 internal)
        self.add_internal_cycles(1);
        register_count + 1
    }

    /// Execute STM (Store Multiple) instruction
    #[inline]
    pub fn execute_stm(
        &mut self,
        rn: Register,
        register_list: &rustyboi_advance_debugger_lib::disassembler::RegisterList,
        addressing_mode: rustyboi_advance_debugger_lib::disassembler::AddressingModeType,
        writeback: bool,
        user_mode: bool,
        mmio: &mut memory::mmio::Mmio,
    ) -> u32 {
        use crate::memory::Addressable;
        use rustyboi_advance_debugger_lib::disassembler::AddressingModeType;

        let mut addr = self.registers.read_register(rn);
        let register_count = register_list.mask.count_ones();

        // Special case: Empty register list
        // STM with empty rlist stores PC and adjusts base register by 0x40
        // It simulates storing 16 registers (0x40 bytes)
        if register_count == 0 {
            let mut pc_value = self.registers.read_register(Register::PC);
            pc_value = if !self.registers.get_flag(Flag::ThumbState) {
                pc_value.wrapping_add(4)
            } else {
                pc_value.wrapping_add(2)
            };
            
            // Determine store address based on addressing mode
            // Empty list behaves as if storing 16 registers
            let store_addr = match addressing_mode {
                AddressingModeType::IA => {
                    // Increment After: store at current address
                    addr
                }
                AddressingModeType::IB => {
                    // Increment Before: increment first by 4, store there
                    addr.wrapping_add(4)
                }
                AddressingModeType::DA => {
                    // Decrement After: store at addr-60 (15 words back)
                    // This is the position of the first register in a 16-register block
                    addr.wrapping_sub(0x3C)
                }
                AddressingModeType::DB => {
                    // Decrement Before: decrement by 0x40 first, store at lowest address
                    addr.wrapping_sub(0x40)
                }
            };
            
            (mmio as &mut dyn Addressable).write32(store_addr, pc_value);
            
            if writeback {
                let new_base = match addressing_mode {
                    AddressingModeType::IA | AddressingModeType::IB => {
                        addr.wrapping_add(0x40)
                    }
                    AddressingModeType::DA | AddressingModeType::DB => {
                        addr.wrapping_sub(0x40)
                    }
                };
                self.registers.write_register(rn, new_base);
            }
            
            // STM takes 2N cycles
            return 2;
        }

        // Calculate start address and direction based on addressing mode
        let (mut start_addr, increment) = match addressing_mode {
            AddressingModeType::IA => {
                // Increment After: start at address, increment after each transfer
                (addr, true)
            }
            AddressingModeType::IB => {
                // Increment Before: increment before each transfer
                addr = addr.wrapping_add(4);
                (addr, true)
            }
            AddressingModeType::DA => {
                // Decrement After: start at address, decrement after each transfer
                (addr, false)
            }
            AddressingModeType::DB => {
                // Decrement Before: decrement before each transfer
                addr = addr.wrapping_sub(4 * register_count);
                (addr, false)
            }
        };

        // Adjust for DA mode
        if addressing_mode == AddressingModeType::DA {
            start_addr = addr.wrapping_sub(4 * (register_count - 1));
        }

        // Save the original base register value before writeback
        // This is needed when Rn is in the register list
        let original_base = self.registers.read_register(rn);

        // Writeback (done before storing for STM)
        if writeback {
            let final_addr = if increment {
                self.registers
                    .read_register(rn)
                    .wrapping_add(4 * register_count)
            } else {
                self.registers
                    .read_register(rn)
                    .wrapping_sub(4 * register_count)
            };
            self.registers.write_register(rn, final_addr);
        }

        // Check if base register is the first (lowest numbered) register in the list
        let base_is_first = register_list.contains(rn as u32) && {
            let mut is_first = true;
            for reg_num in 0..(rn as u32) {
                if register_list.contains(reg_num) {
                    is_first = false;
                    break;
                }
            }
            is_first
        };

        // Store registers in order (R0-R15)
        let mut current_addr = start_addr;

        for reg_num in 0..16 {
            if register_list.contains(reg_num) {
                let reg = Register::from_u32(reg_num);
                
                // Special case: if this is the base register and it's the first in the list,
                // store the original (pre-writeback) value
                let mut value = if reg == rn && base_is_first {
                    original_base
                } else if user_mode && reg_num != 15 {
                    // User mode transfer (S bit set, not storing PC)
                    // Store user mode registers instead of current mode
                    self.registers.read_user_register(reg)
                } else {
                    self.registers.read_register(reg)
                };

                if reg_num == 15 {
                    value = value.wrapping_add(4);
                }

                (mmio as &mut dyn Addressable).write32(current_addr, value);

                current_addr = current_addr.wrapping_add(4);
            }
        }

        // STM takes (n-1)S + 2N cycles (n-1 sequential + 2 non-sequential)
        register_count.max(1)
    }

    #[inline]
    /// Dispatch and execute an instruction
    pub fn dispatch_instruction(
        &mut self,
        instruction: &Instruction,
        instruction_pc: u32,
        mmio: &mut memory::mmio::Mmio,
    ) -> u32 {
        match instruction {
            Instruction::DataProcessing {
                op,
                condition,
                set_flags,
                rn,
                rd,
                operand2,
            } => self.execute_if_condition(condition, |cpu| {
                cpu.execute_data_processing(op, *set_flags, *rn, *rd, operand2, instruction_pc)
            }),

            Instruction::B { condition, target } => {
                self.execute_if_condition(condition, |cpu| cpu.execute_branch(*target))
            }

            Instruction::Bl { condition, target } => {
                self.execute_if_condition(condition, |cpu| cpu.execute_branch_link(*target, instruction_pc))
            }

            Instruction::Bx { condition, rm } => {
                self.execute_if_condition(condition, |cpu| cpu.execute_branch_exchange(*rm))
            }

            Instruction::Ldr {
                condition,
                rd,
                rn,
                addressing,
                byte_access,
                ..
            } => {
                if check_condition(condition, self.registers.cpsr) {
                    self.execute_ldr(*rd, *rn, addressing, *byte_access, mmio, instruction_pc)
                } else {
                    1 // Failed condition still takes 1S cycle
                }
            }

            Instruction::Str {
                condition,
                rd,
                rn,
                addressing,
                byte_access,
                ..
            } => {
                if check_condition(condition, self.registers.cpsr) {
                    self.execute_str(*rd, *rn, addressing, *byte_access, mmio, instruction_pc)
                } else {
                    1 // Failed condition still takes 1S cycle
                }
            }

            Instruction::Mrs {
                condition,
                rd,
                spsr,
            } => self.execute_if_condition(condition, |cpu| cpu.execute_mrs(*rd, *spsr)),

            Instruction::Msr {
                condition,
                fields,
                operand,
                spsr,
            } => {
                self.execute_if_condition(condition, |cpu| cpu.execute_msr(fields, operand, *spsr))
            }

            Instruction::Adr {
                condition,
                rd,
                target,
            } => self.execute_if_condition(condition, |cpu| cpu.execute_adr(*rd, *target)),

            Instruction::Ldm {
                condition,
                rn,
                registers,
                addressing_mode,
                writeback,
                user_mode,
            } => {
                if check_condition(condition, self.registers.cpsr) {
                    self.execute_ldm(
                        *rn,
                        registers,
                        *addressing_mode,
                        *writeback,
                        *user_mode,
                        mmio,
                    )
                } else {
                    1 // Failed condition still takes 1S cycle
                }
            }

            Instruction::Stm {
                condition,
                rn,
                registers,
                addressing_mode,
                writeback,
                user_mode,
            } => {
                if check_condition(condition, self.registers.cpsr) {
                    self.execute_stm(
                        *rn,
                        registers,
                        *addressing_mode,
                        *writeback,
                        *user_mode,
                        mmio,
                    )
                } else {
                    1 // Failed condition still takes 1S cycle
                }
            }

            Instruction::LdrH {
                condition,
                rd,
                rn,
                addressing,
            } => {
                if check_condition(condition, self.registers.cpsr) {
                    self.execute_ldrh(*rd, *rn, addressing, mmio, instruction_pc)
                } else {
                    1 // Failed condition still takes 1S cycle
                }
            }

            Instruction::StrH {
                condition,
                rd,
                rn,
                addressing,
            } => {
                if check_condition(condition, self.registers.cpsr) {
                    self.execute_strh(*rd, *rn, addressing, mmio, instruction_pc)
                } else {
                    1 // Failed condition still takes 1S cycle
                }
            }

            Instruction::LdrSB {
                condition,
                rd,
                rn,
                addressing,
            } => {
                if check_condition(condition, self.registers.cpsr) {
                    self.execute_ldrsb(*rd, *rn, addressing, mmio, instruction_pc)
                } else {
                    1 // Failed condition still takes 1S cycle
                }
            }

            Instruction::LdrSH {
                condition,
                rd,
                rn,
                addressing,
            } => {
                if check_condition(condition, self.registers.cpsr) {
                    self.execute_ldrsh(*rd, *rn, addressing, mmio, instruction_pc)
                } else {
                    1 // Failed condition still takes 1S cycle
                }
            }

            Instruction::Mul {
                condition,
                set_flags,
                rd,
                rm,
                rs,
            } => self
                .execute_if_condition(condition, |cpu| cpu.execute_mul(*rd, *rm, *rs, *set_flags)),

            Instruction::Mla {
                condition,
                set_flags,
                rd,
                rm,
                rs,
                rn,
            } => self.execute_if_condition(condition, |cpu| {
                cpu.execute_mla(*rd, *rm, *rs, *rn, *set_flags)
            }),

            Instruction::Umull {
                condition,
                set_flags,
                rdlo,
                rdhi,
                rm,
                rs,
            } => self.execute_if_condition(condition, |cpu| {
                cpu.execute_umull(*rdlo, *rdhi, *rm, *rs, *set_flags)
            }),

            Instruction::Umlal {
                condition,
                set_flags,
                rdlo,
                rdhi,
                rm,
                rs,
            } => self.execute_if_condition(condition, |cpu| {
                cpu.execute_umlal(*rdlo, *rdhi, *rm, *rs, *set_flags)
            }),

            Instruction::Smull {
                condition,
                set_flags,
                rdlo,
                rdhi,
                rm,
                rs,
            } => self.execute_if_condition(condition, |cpu| {
                cpu.execute_smull(*rdlo, *rdhi, *rm, *rs, *set_flags)
            }),

            Instruction::Smlal {
                condition,
                set_flags,
                rdlo,
                rdhi,
                rm,
                rs,
            } => self.execute_if_condition(condition, |cpu| {
                cpu.execute_smlal(*rdlo, *rdhi, *rm, *rs, *set_flags)
            }),

            Instruction::Swp {
                condition,
                rd,
                rm,
                rn,
                byte,
            } => {
                if check_condition(condition, self.registers.cpsr) {
                    self.execute_swp(*rd, *rm, *rn, *byte, mmio)
                } else {
                    1 // Failed condition still takes 1S cycle
                }
            }

            Instruction::Swi { condition, .. } => {
                if check_condition(condition, self.registers.cpsr) {
                    self.software_interrupt();
                    0 // Cycles counted during exception handling
                } else {
                    1 // Failed condition still takes 1S cycle
                }
            }

            _ => {
                // Other instructions not yet implemented
                unimplemented!("Unimplemented instruction: {}", instruction.to_string());
            }
        }
    }
}
