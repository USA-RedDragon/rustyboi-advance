/// Test data shared between benchmarks and unit tests
/// Contains sample ARM opcodes with their expected disassembly strings

#[derive(Debug, Clone)]
pub struct TestInstruction {
    pub opcode: u32,
    pub expected_asm: &'static str,
    pub description: &'static str,
    pub pc: Option<u32>, // Some tests need specific PC values
}

/// Sample ARM opcodes representing different instruction types for testing
pub const SAMPLE_INSTRUCTIONS: &[TestInstruction] = &[
    // Data processing instructions
    TestInstruction {
        opcode: 0xe3a01000,
        expected_asm: "MOV R1, #0x0",
        description: "MOV immediate",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe3a00004,
        expected_asm: "MOV R0, #0x4",
        description: "MOV immediate small value",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe3a01301,
        expected_asm: "MOV R1, #0x4000000",
        description: "MOV immediate with rotation",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe0801002,
        expected_asm: "ADD R1, R0, R2",
        description: "ADD register",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe1a01002,
        expected_asm: "MOV R1, R2",
        description: "MOV register",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe2801001,
        expected_asm: "ADD R1, R0, #0x1",
        description: "ADD immediate",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe2b00001,
        expected_asm: "ADCS R0, R0, #0x1",
        description: "ADCS immediate",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe1510002,
        expected_asm: "CMP R1, R2",
        description: "CMP register",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe3510001,
        expected_asm: "CMP R1, #0x1",
        description: "CMP immediate",
        pc: None,
    },
    // Data processing with conditions
    TestInstruction {
        opcode: 0x13a01000,
        expected_asm: "MOVNE R1, #0x0",
        description: "MOV immediate with condition",
        pc: None,
    },
    TestInstruction {
        opcode: 0x00801002,
        expected_asm: "ADDEQ R1, R0, R2",
        description: "ADD register with condition",
        pc: None,
    },
    // Load/Store instructions
    TestInstruction {
        opcode: 0xe5901000,
        expected_asm: "LDR R1, [R0]",
        description: "LDR immediate offset 0",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe5901004,
        expected_asm: "LDR R1, [R0, #0x4]",
        description: "LDR immediate offset positive",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe5101004,
        expected_asm: "LDR R1, [R0, #-0x4]",
        description: "LDR immediate offset negative",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe7901002,
        expected_asm: "LDR R1, [R0, R2]",
        description: "LDR register offset",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe5801000,
        expected_asm: "STR R1, [R0]",
        description: "STR immediate",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe7801002,
        expected_asm: "STR R1, [R0, R2]",
        description: "STR register",
        pc: None,
    },
    // PC-relative loads (these need specific PC values for target calculation)
    TestInstruction {
        opcode: 0xe59f1000,
        expected_asm: "LDR R1, [PC] ; 0x8000010",
        description: "LDR PC-relative",
        pc: Some(0x08000008),
    },
    TestInstruction {
        opcode: 0xe59f1004,
        expected_asm: "LDR R1, [PC, #0x4] ; 0x8000014",
        description: "LDR PC-relative with offset",
        pc: Some(0x08000008),
    },
    // Load/Store byte
    TestInstruction {
        opcode: 0xe5d01000,
        expected_asm: "LDRB R1, [R0]",
        description: "LDRB immediate",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe5c01000,
        expected_asm: "STRB R1, [R0]",
        description: "STRB immediate",
        pc: None,
    },
    // Load/Store Multiple
    TestInstruction {
        opcode: 0xe8900003,
        expected_asm: "LDMIA R0, {R0, R1}",
        description: "LDM two registers",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe8800003,
        expected_asm: "STMIA R0, {R0, R1}",
        description: "STM two registers",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe8b00003,
        expected_asm: "LDMIA R0!, {R0, R1}",
        description: "LDM with writeback",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe92d4000,
        expected_asm: "STMDB SP!, {LR}",
        description: "PUSH LR (STM)",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe8bd8000,
        expected_asm: "LDMIA SP!, {PC}",
        description: "POP PC (LDM)",
        pc: None,
    },
    // Branch instructions
    TestInstruction {
        opcode: 0xea000000,
        expected_asm: "B 0x8000010",
        description: "Branch forward",
        pc: Some(0x08000008),
    },
    TestInstruction {
        opcode: 0xeb000000,
        expected_asm: "BL 0x8000010",
        description: "Branch with link forward",
        pc: Some(0x08000008),
    },
    TestInstruction {
        opcode: 0xeafffffe,
        expected_asm: "B 0x8000008",
        description: "Branch backward (infinite loop)",
        pc: Some(0x08000008),
    },
    TestInstruction {
        opcode: 0x1a000000,
        expected_asm: "BNE 0x8000010",
        description: "Conditional branch",
        pc: Some(0x08000008),
    },
    TestInstruction {
        opcode: 0xe12fff10,
        expected_asm: "BX R0",
        description: "Branch exchange",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe12fff1e,
        expected_asm: "BX LR",
        description: "BX LR (return)",
        pc: None,
    },
    // Multiply instructions
    TestInstruction {
        opcode: 0xe0000291,
        expected_asm: "MUL R0, R1, R2",
        description: "MUL",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe0200391,
        expected_asm: "MLA R0, R1, R3, R0",
        description: "MLA",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe0100291,
        expected_asm: "MULS R0, R1, R2",
        description: "MUL with flags",
        pc: None,
    },
    // Long multiply
    TestInstruction {
        opcode: 0xe0800291,
        expected_asm: "UMULL R0, R0, R1, R2",
        description: "UMULL",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe0a00291,
        expected_asm: "UMLAL R0, R0, R1, R2",
        description: "UMLAL",
        pc: None,
    },
    // Halfword/Signed transfers
    TestInstruction {
        opcode: 0xe1d010b0,
        expected_asm: "LDRH R1, [R0]",
        description: "LDRH",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe1c010b0,
        expected_asm: "STRH R1, [R0]",
        description: "STRH",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe1d010d0,
        expected_asm: "LDRSB R1, [R0]",
        description: "LDRSB",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe1d010f0,
        expected_asm: "LDRSH R1, [R0]",
        description: "LDRSH",
        pc: None,
    },
    // Data processing with shifts
    TestInstruction {
        opcode: 0xe1a01081,
        expected_asm: "MOV R1, R1, LSL #0x1",
        description: "MOV with LSL shift",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe1a010a1,
        expected_asm: "MOV R1, R1, LSR #0x1",
        description: "MOV with LSR shift",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe1a010c1,
        expected_asm: "MOV R1, R1, ASR #0x1",
        description: "MOV with ASR shift",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe1a010e1,
        expected_asm: "MOV R1, R1, ROR #0x1",
        description: "MOV with ROR shift",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe1b00060,
        expected_asm: "MOVS R0, R0, RRX",
        description: "MOVS with RRX (ROR #0)",
        pc: None,
    },
    // System instructions
    TestInstruction {
        opcode: 0xe10f1000,
        expected_asm: "MRS R1, CPSR",
        description: "MRS CPSR",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe14f1000,
        expected_asm: "MRS R1, SPSR",
        description: "MRS SPSR",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe129f001,
        expected_asm: "MSR CPSR_CF, R1",
        description: "MSR register",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe328f000,
        expected_asm: "MSR CPSR_F, #0x0",
        description: "MSR immediate",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe328f101,
        expected_asm: "MSR CPSR_F, #0x40000000",
        description: "MSR immediate with rotation",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe1000090,
        expected_asm: "SWP R0, R0, [R0]",
        description: "SWP",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe1400090,
        expected_asm: "SWPB R0, R0, [R0]",
        description: "SWPB",
        pc: None,
    },
    TestInstruction {
        opcode: 0xef000000,
        expected_asm: "SWI 0x0000",
        description: "SWI",
        pc: None,
    },
    TestInstruction {
        opcode: 0xef123456,
        expected_asm: "SWI 0x123456",
        description: "SWI with comment",
        pc: None,
    },
];

/// Simple opcodes for performance testing (reused from benchmark)
pub const PERF_TEST_OPCODES: &[(u32, &str)] = &[
    (0xe3a01000, "MOV R1, #0"),         // MOV immediate
    (0xe0801002, "ADD R1, R0, R2"),     // ADD register
    (0xe1a01002, "MOV R1, R2"),         // MOV register
    (0xe2801001, "ADD R1, R0, #1"),     // ADD immediate
    (0xe1510002, "CMP R1, R2"),         // CMP register
    (0xe3510001, "CMP R1, #1"),         // CMP immediate
    (0xe5901000, "LDR R1, [R0]"),       // LDR immediate
    (0xe7901002, "LDR R1, [R0, R2]"),   // LDR register
    (0xe5801000, "STR R1, [R0]"),       // STR immediate
    (0xe7801002, "STR R1, [R0, R2]"),   // STR register
    (0xe59f1000, "LDR R1, [PC]"),       // LDR PC-relative
    (0xe8901002, "LDM R0, {R1, R12}"),  // LDM
    (0xe8801002, "STM R0, {R1, R12}"),  // STM
    (0xe89007ff, "LDM R0, {R0-R10}"),   // LDM multiple registers
    (0xea000000, "B 0x8"),              // Branch
    (0xeb000000, "BL 0x8"),             // Branch with link
    (0xe12fff10, "BX R0"),              // Branch exchange
    (0xe0000291, "MUL R0, R1, R2"),     // MUL
    (0xe0200391, "MLA R0, R1, R3, R2"), // MLA
    (0xe1d010b0, "LDRH R1, [R0]"),      // LDRH
    (0xe1c010b0, "STRH R1, [R0]"),      // STRH
    (0xe1d010d0, "LDRSB R1, [R0]"),     // LDRSB
    (0xe1d010f0, "LDRSH R1, [R0]"),     // LDRSH
    (0xe10f1000, "MRS R1, CPSR"),       // MRS
    (0xe129f001, "MSR CPSR_fc, R1"),    // MSR
    (0xe1000090, "SWP R0, R0, [R0]"),   // SWP
    (0xef000000, "SWI 0x0"),            // SWI
];

/// Complex instructions for stress testing
pub const COMPLEX_INSTRUCTIONS: &[TestInstruction] = &[
    TestInstruction {
        opcode: 0xe92d4fff,
        expected_asm: "STMDB SP!, {R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, LR}",
        description: "STM many registers with writeback",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe8bd8fff,
        expected_asm: "LDMIA SP!, {R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, PC}",
        description: "LDM many registers with writeback",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe59f1234,
        expected_asm: "LDR R1, [PC, #0x234] ; 0x8001244",
        description: "LDR PC-relative with large offset",
        pc: Some(0x08001008),
    },
    TestInstruction {
        opcode: 0xe129f00f,
        expected_asm: "MSR CPSR_CF, PC",
        description: "MSR with all fields",
        pc: None,
    },
    TestInstruction {
        opcode: 0xe1a01312,
        expected_asm: "MOV R1, R2, LSL R3",
        description: "MOV with register shift",
        pc: None,
    },
];
