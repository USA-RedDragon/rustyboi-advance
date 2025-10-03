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
    if shift_amount == 0 {
        return (value, carry_in);
    }

    match shift_type {
        ShiftType::Lsl => {
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

impl ARM7TDMI {
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
                    // For ADC, we need to handle carry properly
                    let temp = rn_value.wrapping_add(operand2_value);
                    let carry_flag = temp < rn_value || result < temp;
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
                self.branch_to(result & !0x3); // Align to word boundary
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
        // Save return address in LR (PC - 4 since PC is already 8 ahead in ARM mode)
        let return_addr = self.registers.pc.wrapping_sub(4);
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

        // Branch to target (clear bit 0)
        self.branch_to(target & !0x1);
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
        use crate::memory::Addressable;
        
        let (addr, writeback) = self.calculate_address(rn, addressing);
        
        // Perform the load
        let value = if byte_access {
            (mmio as &dyn Addressable).read(addr) as u32
        } else {
            // Word access - rotate if unaligned
            let word = (mmio as &dyn Addressable).read32(addr & !0x3);
            let rotation = (addr & 0x3) * 8;
            word.rotate_right(rotation)
        };
        
        // Write to destination register
        self.registers.write_register(rd, value);
        
        // Writeback if needed (and not post-indexed which already did it)
        if writeback {
            let (new_base, _) = self.calculate_address(rn, addressing);
            self.registers.write_register(rn, new_base);
        }
        
        // If loading to PC, flush pipeline
        if rd == Register::PC {
            self.branch_to(value & !0x3);
            return 0; // Cycles counted during pipeline flush/refill
        }
        
        // LDR takes 1S + 1N + 1I cycles (memory access + internal)
        // Add internal cycles for data transfer
        self.add_internal_cycles(1);
        
        2 // Base cycle count for LDR
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
        use crate::memory::Addressable;
        
        let (addr, writeback) = self.calculate_address(rn, addressing);
        
        // Get value to store
        let value = self.registers.read_register(rd);
        
        // Perform the store
        if byte_access {
            (mmio as &mut dyn Addressable).write(addr, value as u8);
        } else {
            // Word access - must be aligned
            (mmio as &mut dyn Addressable).write32(addr & !0x3, value);
        };
        
        // Writeback if needed (and not post-indexed which already did it)
        if writeback {
            let (new_base, _) = self.calculate_address(rn, addressing);
            self.registers.write_register(rn, new_base);
        }
        
        // STR takes 2N cycles (non-sequential memory access)
        2
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
            let new_cpsr = (self.registers.cpsr & !mask) | (value & mask);
            self.registers.cpsr = new_cpsr;
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
            } => self.execute_if_condition(condition, |cpu| cpu.execute_msr(fields, operand, *spsr)),

            Instruction::Adr {
                condition,
                rd,
                target,
            } => self.execute_if_condition(condition, |cpu| cpu.execute_adr(*rd, *target)),

            _ => {
                // Other instructions not yet implemented
                unimplemented!("Unimplemented instruction: {}", instruction.to_string());
            }
        }
    }
}
