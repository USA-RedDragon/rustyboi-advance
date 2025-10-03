#[cfg(test)]
mod tests {
    use crate::disassembler::{Condition, Disassembler, Instruction, Operand2, Register};
    use crate::test_data::{COMPLEX_INSTRUCTIONS, PERF_TEST_OPCODES, SAMPLE_INSTRUCTIONS};

    /// Test that all sample instructions decode without panicking
    #[test]
    fn test_decode_all_samples() {
        for test_instr in SAMPLE_INSTRUCTIONS.iter() {
            let pc = test_instr.pc.unwrap_or(0x08000008);
            let result = Disassembler::decode_opcode(
                test_instr.opcode,
                pc,
                |_offset| 0x12345678, // Dummy memory read
            );

            // Test should not panic and should return a valid instruction
            assert!(
                !matches!(result, Instruction::Unknown { .. }),
                "Failed to decode instruction: {} (0x{:08X}) - got unknown instruction",
                test_instr.description,
                test_instr.opcode
            );
        }
    }

    /// Test that instruction string formatting matches expected output
    #[test]
    fn test_instruction_string_formatting() {
        for test_instr in SAMPLE_INSTRUCTIONS.iter() {
            let pc = test_instr.pc.unwrap_or(0x08000008);
            let instruction =
                Disassembler::decode_opcode(test_instr.opcode, pc, |_offset| 0x12345678);

            let formatted = instruction.to_string();

            assert_eq!(
                formatted, test_instr.expected_asm,
                "Instruction formatting mismatch for {} (0x{:08X}):\n  Expected: '{}'\n  Got:      '{}'",
                test_instr.description, test_instr.opcode, test_instr.expected_asm, formatted
            );
        }
    }

    /// Test complex instructions that stress the decoder
    #[test]
    fn test_complex_instructions() {
        for test_instr in COMPLEX_INSTRUCTIONS.iter() {
            let pc = test_instr.pc.unwrap_or(0x08000008);
            let instruction =
                Disassembler::decode_opcode(test_instr.opcode, pc, |_offset| 0x12345678);

            let formatted = instruction.to_string();

            assert_eq!(
                formatted, test_instr.expected_asm,
                "Complex instruction formatting mismatch for {} (0x{:08X}):\n  Expected: '{}'\n  Got:      '{}'",
                test_instr.description, test_instr.opcode, test_instr.expected_asm, formatted
            );
        }
    }

    /// Test condition code handling
    #[test]
    fn test_condition_codes() {
        // Test all condition codes with a simple MOV instruction
        let base_opcode = 0x03A01000; // MOV R1, #0 with condition field cleared

        let conditions = [
            (0x0, "EQ"),
            (0x1, "NE"),
            (0x2, "CS"),
            (0x3, "CC"),
            (0x4, "MI"),
            (0x5, "PL"),
            (0x6, "VS"),
            (0x7, "VC"),
            (0x8, "HI"),
            (0x9, "LS"),
            (0xA, "GE"),
            (0xB, "LT"),
            (0xC, "GT"),
            (0xD, "LE"),
            (0xE, ""),
            (0xF, "NV"),
        ];

        for (cond_bits, cond_str) in conditions.iter() {
            let opcode = base_opcode | (cond_bits << 28);
            let instruction = Disassembler::decode_opcode(opcode, 0x08000000, |_| 0);
            let formatted = instruction.to_string();

            let expected = if cond_str.is_empty() {
                "MOV R1, #0x0".to_string()
            } else {
                format!("MOV{} R1, #0x0", cond_str)
            };

            assert_eq!(
                formatted, expected,
                "Condition code test failed for 0x{:X}",
                cond_bits
            );
        }
    }

    /// Test register creation optimizations
    #[test]
    fn test_register_creation() {
        // Test that both safe and optimized methods produce the same results
        for i in 0..16 {
            let safe_reg = Register::from_u32_safe(i);
            let optimized_reg = Register::from_u32(i);
            assert_eq!(
                safe_reg, optimized_reg,
                "Register creation mismatch for R{}",
                i
            );

            // Test string representation
            assert_eq!(safe_reg.to_string(), optimized_reg.to_string());
        }
    }

    /// Test register representation size optimization
    #[test]
    fn test_register_size() {
        // Verify that Register enum is properly packed
        assert_eq!(
            std::mem::size_of::<Register>(),
            1,
            "Register should be 1 byte with #[repr(u8)]"
        );

        // Test that Option<Register> is efficiently packed
        assert_eq!(
            std::mem::size_of::<Option<Register>>(),
            std::mem::size_of::<Register>(),
            "Option<Register> should have same size as Register due to niche optimization"
        );
    }

    /// Test that unknown opcodes are handled gracefully
    #[test]
    fn test_unknown_opcodes() {
        let unknown_opcodes = [
            0xF0000000, // Coprocessor space (group 111, not SWI)
            0xF1000000, // Coprocessor space (group 111, not SWI)
            0xF2000000, // Coprocessor space (group 111, not SWI)
        ];

        for &opcode in unknown_opcodes.iter() {
            let instruction = Disassembler::decode_opcode(opcode, 0x08000000, |_| 0);
            assert!(
                matches!(instruction, Instruction::Unknown { .. }),
                "Opcode 0x{:08X} should decode as Unknown, got: {}",
                opcode,
                instruction
            );

            // Test that unknown instructions can still be formatted
            let formatted = instruction.to_string();
            assert!(
                formatted.starts_with("UNK"),
                "Unknown instruction should start with 'UNK', got: {}",
                formatted
            );
        }
    }

    /// Test PC-relative address calculation accuracy
    #[test]
    fn test_pc_relative_addressing() {
        // LDR R1, [PC, #4]
        let opcode = 0xe59f1004;
        let pc = 0x08001000;

        let instruction = Disassembler::decode_opcode(opcode, pc, |_| 0);
        let formatted = instruction.to_string();

        // PC+8+4 = 0x08001000 + 8 + 4 = 0x0800100C
        assert!(
            formatted.contains("0x800100C"),
            "PC-relative address calculation incorrect: {}",
            formatted
        );
    }

    /// Test shift operations in operands
    #[test]
    fn test_shift_operations() {
        let shift_tests = [
            (0xe1a01081, "MOV R1, R1, LSL #1"), // LSL immediate
            (0xe1a010a1, "MOV R1, R1, LSR #1"), // LSR immediate
            (0xe1a010c1, "MOV R1, R1, ASR #1"), // ASR immediate
            (0xe1a010e1, "MOV R1, R1, ROR #1"), // ROR immediate
            (0xe1a01112, "MOV R1, R2, LSL R1"), // LSL register
            (0xe1a01132, "MOV R1, R2, LSR R1"), // LSR register
            (0xe1a01152, "MOV R1, R2, ASR R1"), // ASR register
            (0xe1a01172, "MOV R1, R2, ROR R1"), // ROR register
        ];

        for &(opcode, expected) in shift_tests.iter() {
            let instruction = Disassembler::decode_opcode(opcode, 0x08000000, |_| 0);
            let formatted = instruction.to_string();
            assert_eq!(
                formatted, expected,
                "Shift operation test failed for 0x{:08X}",
                opcode
            );
        }
    }

    /// Test load/store addressing modes
    #[test]
    fn test_addressing_modes() {
        let addressing_tests = [
            (0xe5901000, "LDR R1, [R0]"),        // Immediate, offset 0
            (0xe5901004, "LDR R1, [R0, #0x4]"),  // Immediate, positive offset
            (0xe5101004, "LDR R1, [R0, #-0x4]"), // Immediate, negative offset
            (0xe5b01004, "LDR R1, [R0, #0x4]!"), // Pre-indexed
            (0xe4901004, "LDR R1, [R0], #0x4"),  // Post-indexed
            (0xe7901002, "LDR R1, [R0, R2]"),    // Register offset
            (0xe7101002, "LDR R1, [R0, -R2]"),   // Register offset, subtract
        ];

        for &(opcode, expected) in addressing_tests.iter() {
            let instruction = Disassembler::decode_opcode(opcode, 0x08000000, |_| 0);
            let formatted = instruction.to_string();
            assert_eq!(
                formatted, expected,
                "Addressing mode test failed for 0x{:08X}",
                opcode
            );
        }
    }

    /// Test multi-register operations
    #[test]
    fn test_multi_register_operations() {
        // Test LDM/STM with various register combinations
        let multi_reg_tests = [
            (0xe8900001, "LDMIA R0, {R0}"),             // Single register (bit 0)
            (0xe8900003, "LDMIA R0, {R0, R1}"),         // Two registers (bits 0,1)
            (0xe890000f, "LDMIA R0, {R0, R1, R2, R3}"), // Four consecutive (bits 0-3)
            (0xe8904001, "LDMIA R0, {R0, LR}"),         // R0 and LR (bits 0,14)
            (0xe8908000, "LDMIA R0, {PC}"),             // PC only (bit 15)
            (0xe92d4000, "STMDB SP!, {LR}"),            // PUSH LR (bit 14)
            (0xe8bd8000, "LDMIA SP!, {PC}"),            // POP PC (bit 15)
        ];

        for &(opcode, expected) in multi_reg_tests.iter() {
            let instruction = Disassembler::decode_opcode(opcode, 0x08000000, |_| 0);
            let formatted = instruction.to_string();
            assert_eq!(
                formatted, expected,
                "Multi-register test failed for 0x{:08X}",
                opcode
            );
        }
    }

    /// Test system instructions (MSR/MRS/SWI)
    #[test]
    fn test_system_instructions() {
        let system_tests = [
            (0xe10f1000, "MRS R1, CPSR"),     // MRS CPSR
            (0xe14f1000, "MRS R1, SPSR"),     // MRS SPSR
            (0xe129f001, "MSR CPSR_CF, R1"),  // MSR register
            (0xe328f001, "MSR CPSR_F, #0x1"), // MSR immediate
            (0xef000000, "SWI 0x000000"),     // SWI
            (0xef123456, "SWI 0x123456"),     // SWI with comment
        ];

        for &(opcode, expected) in system_tests.iter() {
            let instruction = Disassembler::decode_opcode(opcode, 0x08000000, |_| 0);
            let formatted = instruction.to_string();
            assert_eq!(
                formatted, expected,
                "System instruction test failed for 0x{:08X}",
                opcode
            );
        }
    }

    /// Test performance-critical path doesn't allocate unnecessarily
    #[test]
    fn test_no_unnecessary_allocations() {
        // This test ensures the decoder doesn't allocate for common instructions
        // We'll run a representative sample and verify it completes quickly

        let start = std::time::Instant::now();

        for _ in 0..1000 {
            for &(opcode, _) in PERF_TEST_OPCODES.iter() {
                let _instruction = Disassembler::decode_opcode(opcode, 0x08000000, |_| 0);
            }
        }

        let elapsed = start.elapsed();

        // Should complete very quickly if no major allocations occur
        assert!(
            elapsed.as_millis() < 100,
            "Decoding took too long: {:?}ms, possible allocation issues",
            elapsed.as_millis()
        );
    }

    /// Test instruction struct sizes for memory efficiency
    #[test]
    fn test_instruction_sizes() {
        // Verify instruction structs aren't too large
        let instruction_size = std::mem::size_of::<Instruction>();

        // ARM instructions are complex, but we want to keep memory usage reasonable
        // This is more of a canary test - adjust threshold if legitimate changes increase size
        assert!(
            instruction_size <= 128,
            "Instruction size is {} bytes, which might be too large",
            instruction_size
        );

        println!("Instruction size: {} bytes", instruction_size);
        println!("Condition size: {} bytes", std::mem::size_of::<Condition>());
        println!("Register size: {} bytes", std::mem::size_of::<Register>());
        println!("Operand2 size: {} bytes", std::mem::size_of::<Operand2>());
    }

    /// Regression test for specific bug fixes
    #[test]
    fn test_regression_fixes() {
        // Test that specific problematic opcodes that were previously broken now work

        // Regression: MSR with empty fields should not panic
        let msr_empty = 0xe120f000; // MSR CPSR (no fields specified)
        let instruction = Disassembler::decode_opcode(msr_empty, 0x08000000, |_| 0);
        assert!(!matches!(instruction, Instruction::Unknown { .. }));

        // Regression: LDM/STM with register list 0 should not crash
        let ldm_empty = 0xe8900000; // LDMIA R0, {}
        let instruction = Disassembler::decode_opcode(ldm_empty, 0x08000000, |_| 0);
        let formatted = instruction.to_string();
        assert!(
            formatted.contains("{}"),
            "Empty register list should format correctly"
        );

        // Regression: Branch with maximum negative offset
        let branch_max_neg = 0xea800000; // B with -8MB offset
        let instruction = Disassembler::decode_opcode(branch_max_neg, 0x08800000, |_| 0);
        assert!(matches!(instruction, Instruction::B { .. }));
    }
}
