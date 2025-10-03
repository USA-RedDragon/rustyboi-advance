use crate::cpu::arm7tdmi::ARM7TDMI;
use crate::cpu::registers::{Flag, Registers};
use crate::memory;
use rustyboi_advance_debugger_lib::disassembler::{
    Condition, DataProcessingOp, Instruction, Operand2, Register, ShiftType,
};

#[inline]
/// Check if a condition passes based on the current CPSR flags
pub fn check_condition(condition: &Condition, cpsr: u32) -> bool {
    let n = (cpsr >> 31) & 1 == 1; // Negative
    let z = (cpsr >> 30) & 1 == 1; // Zero
    let c = (cpsr >> 29) & 1 == 1; // Carry
    let v = (cpsr >> 28) & 1 == 1; // Overflow

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
/// Perform a barrel shifter operation and return (result, carry_out)
pub fn barrel_shift(
    value: u32,
    shift_type: &ShiftType,
    shift_amount: u32,
    carry_in: bool,
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
            if shift_amount == 0 {
                return (value, carry_in);
            }
            // Logical shift right
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
            if shift_amount == 0 {
                return (value, carry_in);
            }
            // Arithmetic shift right
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
            let shift_amount = shift_amount % 32;
            if shift_amount == 0 {
                (value, carry_in)
            } else {
                let carry = (value >> (shift_amount - 1)) & 1 == 1;
                (value.rotate_right(shift_amount), carry)
            }
        }
    }
}

#[inline]
/// Evaluate Operand2 and return (value, shifter_carry_out)
pub fn evaluate_operand2(
    operand2: &Operand2,
    registers: &Registers,
    carry_in: bool,
) -> (u32, bool) {
    match operand2 {
        Operand2::Immediate(imm) => (*imm, carry_in),
        Operand2::Register(reg) => (registers.read_register(*reg), carry_in),
        Operand2::RegisterShifted {
            reg,
            shift_type,
            shift_amount,
        } => {
            let value = registers.read_register(*reg);
            barrel_shift(value, shift_type, *shift_amount, carry_in)
        }
        Operand2::RegisterShiftedByRegister {
            reg,
            shift_type,
            shift_reg,
        } => {
            let value = registers.read_register(*reg);
            let shift_amount = registers.read_register(*shift_reg) & 0xFF; // Only bottom byte used
            barrel_shift(value, shift_type, shift_amount, carry_in)
        }
    }
}

#[inline]
/// Set CPSR flags based on the result of an operation
pub fn set_nz_flags(registers: &mut Registers, result: u32) {
    registers.set_flag(Flag::Negative, (result as i32) < 0);
    registers.set_flag(Flag::Zero, result == 0);
}

#[inline]
/// Set CPSR flags for addition operations
pub fn set_flags_add(registers: &mut Registers, op1: u32, op2: u32, result: u32) {
    set_nz_flags(registers, result);
    // Carry: unsigned overflow
    registers.set_flag(Flag::Carry, result < op1);
    // Overflow: signed overflow (both operands have same sign, result has different sign)
    let overflow = ((op1 ^ result) & (op2 ^ result) & 0x8000_0000) != 0;
    registers.set_flag(Flag::Overflow, overflow);
}

#[inline]
/// Set CPSR flags for subtraction operations
pub fn set_flags_sub(registers: &mut Registers, op1: u32, op2: u32, result: u32) {
    set_nz_flags(registers, result);
    // Carry: NOT borrow (set if op1 >= op2)
    registers.set_flag(Flag::Carry, op1 >= op2);
    // Overflow: signed overflow
    let overflow = ((op1 ^ op2) & (op1 ^ result) & 0x8000_0000) != 0;
    registers.set_flag(Flag::Overflow, overflow);
}

#[inline]
/// Set CPSR flags for logical operations (including shifter carry)
pub fn set_flags_logical(registers: &mut Registers, result: u32, carry: bool) {
    set_nz_flags(registers, result);
    registers.set_flag(Flag::Carry, carry);
    // Overflow flag is unaffected by logical operations
}

#[inline]
/// Calculate the number of internal cycles for multiply operations
/// based on the multiplier value (Rs)
fn calculate_multiply_cycles(multiplier: u32) -> u32 {
    // Check how many of the upper bits are the same (all 0s or all 1s)
    // m = 1 if bits [8:31] are all 0 or all 1
    // m = 2 if bits [16:31] are all 0 or all 1
    // m = 3 if bits [24:31] are all 0 or all 1
    // m = 4 otherwise

    let upper_24 = multiplier & 0xFFFFFF00;
    if upper_24 == 0 || upper_24 == 0xFFFFFF00 {
        return 1;
    }

    let upper_16 = multiplier & 0xFFFF0000;
    if upper_16 == 0 || upper_16 == 0xFFFF0000 {
        return 2;
    }

    let upper_8 = multiplier & 0xFF000000;
    if upper_8 == 0 || upper_8 == 0xFF000000 {
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
        mmio: &mut memory::mmio::Mmio,
    ) -> u32 {
        use crate::memory::Addressable;

        let (addr, writeback) = self.calculate_address(rn, addressing);

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
                let halfword = (mmio as &dyn Addressable).read16(addr & !0x1) as i16;
                halfword as i32 as u32 // Sign-extend
            }
        };

        // Write to destination register
        self.registers.write_register(rd, value);

        // Writeback if needed
        if writeback {
            let (new_base, _) = self.calculate_address(rn, addressing);
            self.registers.write_register(rn, new_base);
        }

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
    ) -> u32 {
        use crate::memory::Addressable;

        let (addr, writeback) = self.calculate_address(rn, addressing);
        let value = self.registers.read_register(rd);

        // Store value based on type
        match store_type {
            StoreType::Word => {
                (mmio as &mut dyn Addressable).write32(addr & !0x3, value);
            }
            StoreType::Byte => {
                (mmio as &mut dyn Addressable).write(addr, value as u8);
            }
            StoreType::Halfword => {
                (mmio as &mut dyn Addressable).write16(addr & !0x1, value as u16);
            }
        }

        // Writeback if needed
        if writeback {
            let (new_base, _) = self.calculate_address(rn, addressing);
            self.registers.write_register(rn, new_base);
        }

        // Store instructions take 2N cycles
        2
    }

    #[inline]
    /// Execute a data processing instruction
    pub fn execute_data_processing(
        &mut self,
        op: &DataProcessingOp,
        set_flags: bool,
        rn: Option<Register>,
        rd: Option<Register>,
        operand2: &Operand2,
    ) -> u32 {
        let carry_in = self.registers.get_flag(Flag::Carry);
        let (operand2_value, shifter_carry) =
            evaluate_operand2(operand2, &self.registers, carry_in);

        // Get Rn value if needed
        let rn_value = rn.map(|r| self.registers.read_register(r)).unwrap_or(0);

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

        // Write result to destination register (if not a test/compare instruction)
        if let Some(rd) = rd {
            self.registers.write_register(rd, result);

            // If writing to PC, flush the pipeline
            if rd == Register::PC {
                // Align based on current mode
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
        match operand2 {
            Operand2::RegisterShiftedByRegister { .. } => {
                // Shift by register adds 1 internal cycle
                self.add_internal_cycles(1);
            }
            _ => {}
        }

        1 // Base cycle count
    }

    #[inline]
    /// Execute a branch instruction (B)
    pub fn execute_branch(&mut self, target: u32) -> u32 {
        self.branch_to(target);
        0 // Cycles will be counted during pipeline flush/refill
    }

    #[inline]
    /// Execute a branch with link instruction (BL)
    pub fn execute_branch_link(&mut self, target: u32) -> u32 {
        // Save return address in LR
        // PC is already 8 bytes ahead in ARM mode (2 instructions) or 4 bytes ahead in Thumb mode
        // Return address should point to the instruction after BL
        let is_thumb = self.registers.get_flag(Flag::ThumbState);
        let return_addr = if is_thumb {
            // Thumb: PC is 4 ahead, want next instruction (BL + 2), so PC - 2
            self.registers.pc.wrapping_sub(2)
        } else {
            // ARM: PC is 8 ahead, want next instruction (BL + 4), so PC - 4
            self.registers.pc.wrapping_sub(4)
        };
        self.registers.set_lr(return_addr);
        self.branch_to(target);
        0 // Cycles will be counted during pipeline flush/refill
    }

    #[inline]
    /// Execute a branch and exchange instruction (BX)
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
        0 // Cycles will be counted during pipeline flush/refill
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
    #[inline]
    fn calculate_address(
        &mut self,
        rn: Register,
        addressing: &rustyboi_advance_debugger_lib::disassembler::AddressingMode,
    ) -> (u32, bool) {
        use rustyboi_advance_debugger_lib::disassembler::AddressingMode;

        let base = self.registers.read_register(rn);

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
                    barrel_shift(offset_value, shift_type, *shift_amount, carry).0
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
            AddressingMode::PostIndexed { offset } => {
                // Post-indexed: use base address, then add offset to base
                let addr = base;
                let new_base = base.wrapping_add(*offset as u32);
                self.registers.write_register(rn, new_base);
                (addr, false) // Writeback already done
            }
        }
    }

    /// Execute a load register instruction (LDR/LDRB)
    #[inline]
    pub fn execute_ldr(
        &mut self,
        rd: Register,
        rn: Register,
        addressing: &rustyboi_advance_debugger_lib::disassembler::AddressingMode,
        byte_access: bool,
        mmio: &mut memory::mmio::Mmio,
    ) -> u32 {
        let load_type = if byte_access {
            LoadType::Byte
        } else {
            LoadType::Word
        };
        self.execute_load_generic(rd, rn, addressing, load_type, mmio)
    }

    /// Execute a store register instruction (STR/STRB)
    #[inline]
    pub fn execute_str(
        &mut self,
        rd: Register,
        rn: Register,
        addressing: &rustyboi_advance_debugger_lib::disassembler::AddressingMode,
        byte_access: bool,
        mmio: &mut memory::mmio::Mmio,
    ) -> u32 {
        let store_type = if byte_access {
            StoreType::Byte
        } else {
            StoreType::Word
        };
        self.execute_store_generic(rd, rn, addressing, store_type, mmio)
    }

    /// Execute a load register halfword instruction (LDRH)
    #[inline]
    pub fn execute_ldrh(
        &mut self,
        rd: Register,
        rn: Register,
        addressing: &rustyboi_advance_debugger_lib::disassembler::AddressingMode,
        mmio: &mut memory::mmio::Mmio,
    ) -> u32 {
        self.execute_load_generic(rd, rn, addressing, LoadType::Halfword, mmio)
    }

    /// Execute a store register halfword instruction (STRH)
    #[inline]
    pub fn execute_strh(
        &mut self,
        rd: Register,
        rn: Register,
        addressing: &rustyboi_advance_debugger_lib::disassembler::AddressingMode,
        mmio: &mut memory::mmio::Mmio,
    ) -> u32 {
        self.execute_store_generic(rd, rn, addressing, StoreType::Halfword, mmio)
    }

    /// Execute a load register signed byte instruction (LDRSB)
    #[inline]
    pub fn execute_ldrsb(
        &mut self,
        rd: Register,
        rn: Register,
        addressing: &rustyboi_advance_debugger_lib::disassembler::AddressingMode,
        mmio: &mut memory::mmio::Mmio,
    ) -> u32 {
        self.execute_load_generic(rd, rn, addressing, LoadType::SignedByte, mmio)
    }

    /// Execute a load register signed halfword instruction (LDRSH)
    #[inline]
    pub fn execute_ldrsh(
        &mut self,
        rd: Register,
        rn: Register,
        addressing: &rustyboi_advance_debugger_lib::disassembler::AddressingMode,
        mmio: &mut memory::mmio::Mmio,
    ) -> u32 {
        self.execute_load_generic(rd, rn, addressing, LoadType::SignedHalfword, mmio)
    }

    /// Execute MRS (Move PSR to Register) instruction
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

        // Create a mask based on which fields are being written
        let mut mask = 0u32;
        if fields.f {
            // Flags field (bits 24-31)
            mask |= 0xFF00_0000;
        }
        if fields.s {
            // Status field (bits 16-23)
            mask |= 0x00FF_0000;
        }
        if fields.x {
            // Extension field (bits 8-15)
            mask |= 0x0000_FF00;
        }
        if fields.c {
            // Control field (bits 0-7)
            // Only allow mode changes in privileged modes
            let current_mode = self.registers.get_current_mode();
            if current_mode != crate::cpu::registers::Mode::User {
                mask |= 0x0000_00FF;
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
            let old_mode = old_cpsr & 0x1F;
            let new_mode = new_cpsr & 0x1F;

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

                if user_mode && reg_num != 15 {
                    // User mode transfer (S bit set, not loading PC)
                    // Load user mode registers instead of current mode
                    // This is used for exception handlers
                    // For now, just load normally
                    self.registers
                        .write_register(Register::from_u32(reg_num), value);
                } else {
                    self.registers
                        .write_register(Register::from_u32(reg_num), value);
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

        // Store registers in order (R0-R15)
        let mut current_addr = start_addr;

        for reg_num in 0..16 {
            if register_list.contains(reg_num) {
                let value = if user_mode && reg_num != 15 {
                    // User mode transfer (S bit set)
                    // Store user mode registers instead of current mode
                    // For now, just store normally
                    self.registers.read_register(Register::from_u32(reg_num))
                } else {
                    self.registers.read_register(Register::from_u32(reg_num))
                };

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
                cpu.execute_data_processing(op, *set_flags, *rn, *rd, operand2)
            }),

            Instruction::B { condition, target } => {
                self.execute_if_condition(condition, |cpu| cpu.execute_branch(*target))
            }

            Instruction::Bl { condition, target } => {
                self.execute_if_condition(condition, |cpu| cpu.execute_branch_link(*target))
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
                    self.execute_ldr(*rd, *rn, addressing, *byte_access, mmio)
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
                    self.execute_str(*rd, *rn, addressing, *byte_access, mmio)
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
                    self.execute_ldrh(*rd, *rn, addressing, mmio)
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
                    self.execute_strh(*rd, *rn, addressing, mmio)
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
                    self.execute_ldrsb(*rd, *rn, addressing, mmio)
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
                    self.execute_ldrsh(*rd, *rn, addressing, mmio)
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
