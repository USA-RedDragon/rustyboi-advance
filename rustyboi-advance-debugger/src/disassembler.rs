#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
pub enum Register {
    R0 = 0,
    R1 = 1,
    R2 = 2,
    R3 = 3,
    R4 = 4,
    R5 = 5,
    R6 = 6,
    R7 = 7,
    R8 = 8,
    R9 = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,
    SP = 13, // R13
    LR = 14, // R14
    PC = 15, // R15
}

impl Register {
    #[inline]
    pub fn from_u32(reg: u32) -> Register {
        match reg {
            0 => Register::R0,
            1 => Register::R1,
            2 => Register::R2,
            3 => Register::R3,
            4 => Register::R4,
            5 => Register::R5,
            6 => Register::R6,
            7 => Register::R7,
            8 => Register::R8,
            9 => Register::R9,
            10 => Register::R10,
            11 => Register::R11,
            12 => Register::R12,
            13 => Register::SP,
            14 => Register::LR,
            15 => Register::PC,
            _ => panic!("Invalid register number: {}", reg),
        }
    }
}

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Register::R0 => "R0",
            Register::R1 => "R1",
            Register::R2 => "R2",
            Register::R3 => "R3",
            Register::R4 => "R4",
            Register::R5 => "R5",
            Register::R6 => "R6",
            Register::R7 => "R7",
            Register::R8 => "R8",
            Register::R9 => "R9",
            Register::R10 => "R10",
            Register::R11 => "R11",
            Register::R12 => "R12",
            Register::SP => "SP",
            Register::LR => "LR",
            Register::PC => "PC",
        };
        write!(f, "{}", name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ShiftType {
    Lsl,
    Lsr,
    Asr,
    Ror,
}

impl std::fmt::Display for ShiftType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            ShiftType::Lsl => "LSL",
            ShiftType::Lsr => "LSR",
            ShiftType::Asr => "ASR",
            ShiftType::Ror => "ROR",
        };
        write!(f, "{}", name)
    }
}

impl ShiftType {
    #[inline]
    fn from_bits(bits: u32) -> ShiftType {
        match bits & 0b11 {
            0 => ShiftType::Lsl,
            1 => ShiftType::Lsr,
            2 => ShiftType::Asr,
            3 => ShiftType::Ror,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operand2 {
    Immediate { value: u32, rotation: u32 }, // rotation is the actual rotate amount (0-30, in steps of 2)
    Register(Register),
    RegisterShifted {
        reg: Register,
        shift_type: ShiftType,
        shift_amount: u32,
    },
    RegisterShiftedByRegister {
        reg: Register,
        shift_type: ShiftType,
        shift_reg: Register,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Condition {
    EQ,
    NE,
    CS,
    CC,
    MI,
    PL,
    VS,
    VC,
    HI,
    LS,
    GE,
    LT,
    GT,
    LE,
    AL,
    NV,
}

impl Condition {
    #[inline]
    pub fn from_bits(cond: u32) -> Condition {
        match cond {
            0b0000 => Condition::EQ,
            0b0001 => Condition::NE,
            0b0010 => Condition::CS,
            0b0011 => Condition::CC,
            0b0100 => Condition::MI,
            0b0101 => Condition::PL,
            0b0110 => Condition::VS,
            0b0111 => Condition::VC,
            0b1000 => Condition::HI,
            0b1001 => Condition::LS,
            0b1010 => Condition::GE,
            0b1011 => Condition::LT,
            0b1100 => Condition::GT,
            0b1101 => Condition::LE,
            0b1110 => Condition::AL,
            0b1111 => Condition::NV,
            _ => unreachable!(),
        }
    }

    pub fn to_suffix(&self) -> &'static str {
        match self {
            Condition::EQ => "EQ",
            Condition::NE => "NE",
            Condition::CS => "CS",
            Condition::CC => "CC",
            Condition::MI => "MI",
            Condition::PL => "PL",
            Condition::VS => "VS",
            Condition::VC => "VC",
            Condition::HI => "HI",
            Condition::LS => "LS",
            Condition::GE => "GE",
            Condition::LT => "LT",
            Condition::GT => "GT",
            Condition::LE => "LE",
            Condition::AL => "",   // AL (always) - no suffix
            Condition::NV => "NV", // NV (never) - should not be used
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AddressingModeType {
    IA = 0, // Increment After
    IB = 1, // Increment Before
    DA = 2, // Decrement After
    DB = 3, // Decrement Before
}

impl AddressingModeType {
    #[inline]
    pub fn from_bits(p_bit: u32, u_bit: u32) -> Self {
        match (p_bit, u_bit) {
            (0, 1) => AddressingModeType::IA,
            (1, 1) => AddressingModeType::IB,
            (0, 0) => AddressingModeType::DA,
            (1, 0) => AddressingModeType::DB,
            _ => AddressingModeType::IA, // Default fallback
        }
    }

    #[inline]
    pub fn to_str(self) -> &'static str {
        match self {
            AddressingModeType::IA => "IA",
            AddressingModeType::IB => "IB",
            AddressingModeType::DA => "DA",
            AddressingModeType::DB => "DB",
        }
    }
}

impl std::fmt::Display for AddressingModeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AddressingMode {
    Offset {
        offset: i32,
        writeback: bool,
    },
    RegisterOffset {
        reg: Register,
        shift: Option<(ShiftType, u32)>,
        add: bool,
        writeback: bool,
    },
    PostIndexed {
        offset: i32,
    },
    PreIndexed {
        offset: i32,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum DataProcessingOp {
    And,
    Eor,
    Sub,
    Rsb,
    Add,
    Adc,
    Sbc,
    Rsc,
    Tst,
    Teq,
    Cmp,
    Cmn,
    Orr,
    Mov,
    Bic,
    Mvn,
}

impl std::fmt::Display for DataProcessingOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            DataProcessingOp::And => "AND",
            DataProcessingOp::Eor => "EOR",
            DataProcessingOp::Sub => "SUB",
            DataProcessingOp::Rsb => "RSB",
            DataProcessingOp::Add => "ADD",
            DataProcessingOp::Adc => "ADC",
            DataProcessingOp::Sbc => "SBC",
            DataProcessingOp::Rsc => "RSC",
            DataProcessingOp::Tst => "TST",
            DataProcessingOp::Teq => "TEQ",
            DataProcessingOp::Cmp => "CMP",
            DataProcessingOp::Cmn => "CMN",
            DataProcessingOp::Orr => "ORR",
            DataProcessingOp::Mov => "MOV",
            DataProcessingOp::Bic => "BIC",
            DataProcessingOp::Mvn => "MVN",
        };
        write!(f, "{}", name)
    }
}

impl DataProcessingOp {
    #[inline]
    fn from_opcode(opcode_type: u32) -> DataProcessingOp {
        match opcode_type {
            0x0 => DataProcessingOp::And,
            0x1 => DataProcessingOp::Eor,
            0x2 => DataProcessingOp::Sub,
            0x3 => DataProcessingOp::Rsb,
            0x4 => DataProcessingOp::Add,
            0x5 => DataProcessingOp::Adc,
            0x6 => DataProcessingOp::Sbc,
            0x7 => DataProcessingOp::Rsc,
            0x8 => DataProcessingOp::Tst,
            0x9 => DataProcessingOp::Teq,
            0xA => DataProcessingOp::Cmp,
            0xB => DataProcessingOp::Cmn,
            0xC => DataProcessingOp::Orr,
            0xD => DataProcessingOp::Mov,
            0xE => DataProcessingOp::Bic,
            0xF => DataProcessingOp::Mvn,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    // Data processing
    DataProcessing {
        op: DataProcessingOp,
        condition: Condition,
        set_flags: bool,
        rn: Option<Register>, // None for MOV/MVN
        rd: Option<Register>, // None for TST/TEQ/CMP/CMN
        operand2: Operand2,
    },

    // Memory operations
    Ldr {
        condition: Condition,
        rd: Register,
        rn: Register,
        addressing: AddressingMode,
        byte_access: bool,
        target_addr: Option<u32>, // For PC-relative addressing
    },
    Str {
        condition: Condition,
        rd: Register,
        rn: Register,
        addressing: AddressingMode,
        byte_access: bool,
        target_addr: Option<u32>, // For PC-relative addressing
    },

    // Load/Store Multiple
    Ldm {
        condition: Condition,
        rn: Register,
        registers: RegisterList,
        addressing_mode: AddressingModeType,
        writeback: bool,
        user_mode: bool,
    },
    Stm {
        condition: Condition,
        rn: Register,
        registers: RegisterList,
        addressing_mode: AddressingModeType,
        writeback: bool,
        user_mode: bool,
    },

    // Load/Store Halfword/Signed
    LdrH {
        condition: Condition,
        rd: Register,
        rn: Register,
        addressing: AddressingMode,
    },
    StrH {
        condition: Condition,
        rd: Register,
        rn: Register,
        addressing: AddressingMode,
    },
    LdrSB {
        condition: Condition,
        rd: Register,
        rn: Register,
        addressing: AddressingMode,
    },
    LdrSH {
        condition: Condition,
        rd: Register,
        rn: Register,
        addressing: AddressingMode,
    },

    // Branch
    B {
        condition: Condition,
        target: u32,
    },
    Bl {
        condition: Condition,
        target: u32,
    },
    Bx {
        condition: Condition,
        rm: Register,
    },

    // Multiply
    Mul {
        condition: Condition,
        rd: Register,
        rm: Register,
        rs: Register,
        set_flags: bool,
    },
    Mla {
        condition: Condition,
        rd: Register,
        rm: Register,
        rs: Register,
        rn: Register,
        set_flags: bool,
    },
    Umull {
        condition: Condition,
        rdlo: Register,
        rdhi: Register,
        rm: Register,
        rs: Register,
        set_flags: bool,
    },
    Umlal {
        condition: Condition,
        rdlo: Register,
        rdhi: Register,
        rm: Register,
        rs: Register,
        set_flags: bool,
    },
    Smull {
        condition: Condition,
        rdlo: Register,
        rdhi: Register,
        rm: Register,
        rs: Register,
        set_flags: bool,
    },
    Smlal {
        condition: Condition,
        rdlo: Register,
        rdhi: Register,
        rm: Register,
        rs: Register,
        set_flags: bool,
    },

    // Status register operations
    Mrs {
        condition: Condition,
        rd: Register,
        spsr: bool,
    },
    Msr {
        condition: Condition,
        fields: MsrFields,
        operand: MsrOperand,
        spsr: bool,
    },

    // Swap
    Swp {
        condition: Condition,
        rd: Register,
        rm: Register,
        rn: Register,
        byte: bool,
    },

    // Software interrupt
    Swi {
        condition: Condition,
        comment: u32,
    },

    // Special cases
    Adr {
        condition: Condition,
        rd: Register,
        target: u32,
    },

    // Unknown/Undefined
    Unknown {
        condition: Condition,
        opcode: u32,
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct MsrFields {
    pub c: bool, // Control field
    pub x: bool, // Extension field
    pub s: bool, // Status field
    pub f: bool, // Flags field
}

impl MsrFields {
    #[inline]
    pub fn from_mask(field_mask: u32) -> Self {
        Self {
            c: (field_mask & 0b0001) != 0,
            x: (field_mask & 0b0010) != 0,
            s: (field_mask & 0b0100) != 0,
            f: (field_mask & 0b1000) != 0,
        }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        !self.c && !self.x && !self.s && !self.f
    }
}

impl std::fmt::Display for MsrFields {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut fields = String::new();
        if self.c {
            fields.push('C');
        }
        if self.x {
            fields.push('X');
        }
        if self.s {
            fields.push('S');
        }
        if self.f {
            fields.push('F');
        }
        write!(f, "{}", fields)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct RegisterList {
    pub mask: u16, // Bit mask representing which registers are in the list
}

impl RegisterList {
    #[inline]
    pub fn from_mask(mask: u16) -> Self {
        Self { mask }
    }

    #[inline]
    pub fn contains(&self, reg_num: u32) -> bool {
        (self.mask & (1 << reg_num)) != 0
    }

    pub fn to_vec(&self) -> Vec<Register> {
        let mut registers = Vec::new();
        for i in 0..16 {
            if self.contains(i) {
                registers.push(Register::from_u32(i));
            }
        }
        registers
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum MsrOperand {
    Immediate(u32),
    Register(Register),
}

impl Instruction {
    /// Returns the size of the instruction in bytes.
    /// This is important for Thumb mode where most instructions are 2 bytes,
    /// but BL (Branch with Link) is 4 bytes (32-bit).
    pub fn size_bytes(&self, is_thumb: bool) -> u32 {
        if is_thumb {
            // In Thumb mode, BL is a 32-bit instruction (4 bytes)
            // All other Thumb instructions are 16-bit (2 bytes)
            match self {
                Instruction::Bl { .. } => 4,
                _ => 2,
            }
        } else {
            // ARM mode instructions are always 32-bit (4 bytes)
            4
        }
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::DataProcessing {
                op,
                condition,
                set_flags,
                rn,
                rd,
                operand2,
            } => {
                let s_suffix = if *set_flags { "S" } else { "" };
                let cond_suffix = condition.to_suffix();

                match (rn, rd) {
                    (Some(rn_reg), Some(rd_reg)) => {
                        write!(
                            f,
                            "{}{}{} {}, {}, {}",
                            op, cond_suffix, s_suffix, rd_reg, rn_reg, operand2
                        )
                    }
                    (Some(rn_reg), None) => {
                        write!(f, "{}{} {}, {}", op, cond_suffix, rn_reg, operand2)
                    }
                    (None, Some(rd_reg)) => {
                        write!(
                            f,
                            "{}{}{} {}, {}",
                            op, cond_suffix, s_suffix, rd_reg, operand2
                        )
                    }
                    (None, None) => {
                        write!(f, "{}{} {}", op, cond_suffix, operand2)
                    }
                }
            }
            Instruction::Ldr {
                condition,
                rd,
                rn,
                addressing,
                byte_access,
                target_addr,
            } => {
                let size = if *byte_access { "B" } else { "" };
                let addr_str = addressing.to_string_with_base(rn);
                match target_addr {
                    Some(addr) => write!(
                        f,
                        "LDR{}{} {}, {} ; {:#X}",
                        condition.to_suffix(),
                        size,
                        rd,
                        addr_str,
                        addr
                    ),
                    None => write!(
                        f,
                        "LDR{}{} {}, {}",
                        condition.to_suffix(),
                        size,
                        rd,
                        addr_str
                    ),
                }
            }
            Instruction::Str {
                condition,
                rd,
                rn,
                addressing,
                byte_access,
                target_addr,
            } => {
                let size = if *byte_access { "B" } else { "" };
                let addr_str = addressing.to_string_with_base(rn);
                match target_addr {
                    Some(addr) => write!(
                        f,
                        "STR{}{} {}, {} ; {:#X}",
                        condition.to_suffix(),
                        size,
                        rd,
                        addr_str,
                        addr
                    ),
                    None => write!(
                        f,
                        "STR{}{} {}, {}",
                        condition.to_suffix(),
                        size,
                        rd,
                        addr_str
                    ),
                }
            }
            Instruction::Ldm {
                condition,
                rn,
                registers,
                addressing_mode,
                writeback,
                user_mode,
            } => {
                let wb = if *writeback { "!" } else { "" };
                let user = if *user_mode { "^" } else { "" };
                let reg_list = format_register_list(registers);
                write!(
                    f,
                    "LDM{}{} {}{}, {}{}",
                    condition.to_suffix(),
                    addressing_mode,
                    rn,
                    wb,
                    reg_list,
                    user
                )
            }
            Instruction::Stm {
                condition,
                rn,
                registers,
                addressing_mode,
                writeback,
                user_mode,
            } => {
                let wb = if *writeback { "!" } else { "" };
                let user = if *user_mode { "^" } else { "" };
                let reg_list = format_register_list(registers);
                write!(
                    f,
                    "STM{}{} {}{}, {}{}",
                    condition.to_suffix(),
                    addressing_mode,
                    rn,
                    wb,
                    reg_list,
                    user
                )
            }
            Instruction::LdrH {
                condition,
                rd,
                rn,
                addressing,
            } => {
                write!(
                    f,
                    "LDRH{} {}, {}",
                    condition.to_suffix(),
                    rd,
                    addressing.to_string_with_base(rn)
                )
            }
            Instruction::StrH {
                condition,
                rd,
                rn,
                addressing,
            } => {
                write!(
                    f,
                    "STRH{} {}, {}",
                    condition.to_suffix(),
                    rd,
                    addressing.to_string_with_base(rn)
                )
            }
            Instruction::LdrSB {
                condition,
                rd,
                rn,
                addressing,
            } => {
                write!(
                    f,
                    "LDRSB{} {}, {}",
                    condition.to_suffix(),
                    rd,
                    addressing.to_string_with_base(rn)
                )
            }
            Instruction::LdrSH {
                condition,
                rd,
                rn,
                addressing,
            } => {
                write!(
                    f,
                    "LDRSH{} {}, {}",
                    condition.to_suffix(),
                    rd,
                    addressing.to_string_with_base(rn)
                )
            }
            Instruction::B { condition, target } => {
                write!(f, "B{} {:#X}", condition.to_suffix(), target)
            }
            Instruction::Bl { condition, target } => {
                write!(f, "BL{} {:#X}", condition.to_suffix(), target)
            }
            Instruction::Bx { condition, rm } => {
                write!(f, "BX{} {}", condition.to_suffix(), rm)
            }
            Instruction::Mul {
                condition,
                rd,
                rm,
                rs,
                set_flags,
            } => {
                let s = if *set_flags { "S" } else { "" };
                write!(
                    f,
                    "MUL{}{} {}, {}, {}",
                    condition.to_suffix(),
                    s,
                    rd,
                    rm,
                    rs
                )
            }
            Instruction::Mla {
                condition,
                rd,
                rm,
                rs,
                rn,
                set_flags,
            } => {
                let s = if *set_flags { "S" } else { "" };
                write!(
                    f,
                    "MLA{}{} {}, {}, {}, {}",
                    condition.to_suffix(),
                    s,
                    rd,
                    rm,
                    rs,
                    rn
                )
            }
            Instruction::Umull {
                condition,
                rdlo,
                rdhi,
                rm,
                rs,
                set_flags,
            } => {
                let s = if *set_flags { "S" } else { "" };
                write!(
                    f,
                    "UMULL{}{} {}, {}, {}, {}",
                    condition.to_suffix(),
                    s,
                    rdlo,
                    rdhi,
                    rm,
                    rs
                )
            }
            Instruction::Umlal {
                condition,
                rdlo,
                rdhi,
                rm,
                rs,
                set_flags,
            } => {
                let s = if *set_flags { "S" } else { "" };
                write!(
                    f,
                    "UMLAL{}{} {}, {}, {}, {}",
                    condition.to_suffix(),
                    s,
                    rdlo,
                    rdhi,
                    rm,
                    rs
                )
            }
            Instruction::Smull {
                condition,
                rdlo,
                rdhi,
                rm,
                rs,
                set_flags,
            } => {
                let s = if *set_flags { "S" } else { "" };
                write!(
                    f,
                    "SMULL{}{} {}, {}, {}, {}",
                    condition.to_suffix(),
                    s,
                    rdlo,
                    rdhi,
                    rm,
                    rs
                )
            }
            Instruction::Smlal {
                condition,
                rdlo,
                rdhi,
                rm,
                rs,
                set_flags,
            } => {
                let s = if *set_flags { "S" } else { "" };
                write!(
                    f,
                    "SMLAL{}{} {}, {}, {}, {}",
                    condition.to_suffix(),
                    s,
                    rdlo,
                    rdhi,
                    rm,
                    rs
                )
            }
            Instruction::Mrs {
                condition,
                rd,
                spsr,
            } => {
                let psr = if *spsr { "SPSR" } else { "CPSR" };
                write!(f, "MRS{} {}, {}", condition.to_suffix(), rd, psr)
            }
            Instruction::Msr {
                condition,
                fields,
                operand,
                spsr,
            } => {
                let psr = if *spsr { "SPSR" } else { "CPSR" };
                let psr_with_fields = if fields.is_empty() {
                    psr.to_string()
                } else {
                    format!("{}_{}", psr, fields)
                };
                match operand {
                    MsrOperand::Immediate(imm) => write!(
                        f,
                        "MSR{} {}, #{:#X}",
                        condition.to_suffix(),
                        psr_with_fields,
                        imm
                    ),
                    MsrOperand::Register(reg) => write!(
                        f,
                        "MSR{} {}, {}",
                        condition.to_suffix(),
                        psr_with_fields,
                        reg
                    ),
                }
            }
            Instruction::Swp {
                condition,
                rd,
                rm,
                rn,
                byte,
            } => {
                let size = if *byte { "B" } else { "" };
                write!(
                    f,
                    "SWP{}{} {}, {}, [{}]",
                    condition.to_suffix(),
                    size,
                    rd,
                    rm,
                    rn
                )
            }
            Instruction::Swi { condition, comment } => {
                write!(f, "SWI{} {:#06X}", condition.to_suffix(), comment)
            }
            Instruction::Adr {
                condition,
                rd,
                target,
            } => {
                write!(f, "ADR{} {}, {:#X}", condition.to_suffix(), rd, target)
            }
            Instruction::Unknown { condition, opcode } => {
                write!(f, "UNK{} {:#08X}", condition.to_suffix(), opcode)
            }
        }
    }
}

impl std::fmt::Display for Operand2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand2::Immediate { value, .. } => write!(f, "#{:#X}", value),
            Operand2::Register(reg) => write!(f, "{}", reg),
            Operand2::RegisterShifted {
                reg,
                shift_type,
                shift_amount,
            } => {
                if *shift_amount == 0 && *shift_type == ShiftType::Lsl {
                    // LSL #0 is just the register with no shift
                    write!(f, "{}", reg)
                } else if *shift_amount == 0 && *shift_type == ShiftType::Ror {
                    // ROR #0 is actually RRX (Rotate Right Extended)
                    write!(f, "{}, RRX", reg)
                } else {
                    write!(f, "{}, {} #{:#X}", reg, shift_type, shift_amount)
                }
            }
            Operand2::RegisterShiftedByRegister {
                reg,
                shift_type,
                shift_reg,
            } => {
                write!(f, "{}, {} {}", reg, shift_type, shift_reg)
            }
        }
    }
}

impl AddressingMode {
    fn to_string_with_base(&self, base: &Register) -> String {
        match self {
            AddressingMode::Offset { offset, writeback } => {
                let wb = if *writeback { "!" } else { "" };
                if *offset == 0 {
                    format!("[{}]{}", base, wb)
                } else {
                    let sign = if *offset >= 0 { "" } else { "-" };
                    let abs_offset = offset.unsigned_abs();
                    format!("[{}, #{}{:#X}]{}", base, sign, abs_offset, wb)
                }
            }
            AddressingMode::RegisterOffset {
                reg,
                shift,
                add,
                writeback,
            } => {
                let wb = if *writeback { "!" } else { "" };
                let sign = if *add { "" } else { "-" };
                match shift {
                    Some((shift_type, amount)) => {
                        format!(
                            "[{}, {}{}, {} #{:#X}]{}",
                            base, sign, reg, shift_type, amount, wb
                        )
                    }
                    None => format!("[{}, {}{}]{}", base, sign, reg, wb),
                }
            }
            AddressingMode::PostIndexed { offset } => {
                if *offset == 0 {
                    format!("[{}]", base)
                } else {
                    let sign = if *offset >= 0 { "" } else { "-" };
                    let abs_offset = offset.unsigned_abs();
                    format!("[{}], #{}{:#X}", base, sign, abs_offset)
                }
            }
            AddressingMode::PreIndexed { offset } => {
                if *offset == 0 {
                    format!("[{}]!", base)
                } else {
                    let sign = if *offset >= 0 { "" } else { "-" };
                    let abs_offset = offset.unsigned_abs();
                    format!("[{}, #{}{:#X}]!", base, sign, abs_offset)
                }
            }
        }
    }
}

pub struct Disassembler;

// Helper functions for common bit field extractions - inlined for performance
impl Disassembler {
    #[inline(always)]
    fn extract_condition(opcode: u32) -> Condition {
        Condition::from_bits((opcode >> 28) & 0xF)
    }

    #[inline(always)]
    fn extract_opcode_type(opcode: u32) -> u32 {
        (opcode >> 21) & 0xF
    }

    #[inline(always)]
    fn extract_rd(opcode: u32) -> u32 {
        (opcode >> 12) & 0xF
    }

    #[inline(always)]
    fn extract_rn(opcode: u32) -> u32 {
        (opcode >> 16) & 0xF
    }

    #[inline(always)]
    fn extract_rs(opcode: u32) -> u32 {
        (opcode >> 8) & 0xF
    }

    #[inline(always)]
    fn extract_rm(opcode: u32) -> u32 {
        opcode & 0xF
    }

    #[inline(always)]
    fn extract_s_bit(opcode: u32) -> bool {
        ((opcode >> 20) & 1) != 0
    }

    #[inline]
    fn get_data_processing_registers(
        op: &DataProcessingOp,
        rd: u32,
        rn: u32,
    ) -> (Option<Register>, Option<Register>) {
        // Optimize common matches! with direct enum comparisons
        let rn_reg = match op {
            DataProcessingOp::Mov | DataProcessingOp::Mvn => None,
            _ => Some(Register::from_u32(rn)),
        };

        let rd_reg = match op {
            DataProcessingOp::Tst
            | DataProcessingOp::Teq
            | DataProcessingOp::Cmp
            | DataProcessingOp::Cmn => {
                // For comparison instructions, normally Rd is not written
                // However, if Rd=PC, we need to preserve it for the edge case
                // where CPSR is restored from SPSR (unpredictable behavior)
                if rd == 15 {
                    Some(Register::PC)
                } else {
                    None
                }
            }
            _ => Some(Register::from_u32(rd)),
        };

        (rn_reg, rd_reg)
    }
}

/// Helper function to format register lists for LDM/STM instructions
fn format_register_list(registers: &RegisterList) -> String {
    let reg_vec = registers.to_vec();
    format!(
        "{{{}}}",
        reg_vec
            .iter()
            .map(|r| format!("{}", r))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

impl Disassembler {
    /// Returns the mnemonic string for the instruction at the given address (ARM mode)
    pub fn disassemble_with_reader<F>(addr: u32, read_fn: F) -> String
    where
        F: FnMut(u32) -> u32,
    {
        let instruction = Self::decode_with_reader(addr, read_fn);
        instruction.to_string()
    }

    /// Returns the Instruction struct for the instruction at the given address (ARM mode)
    pub fn decode_with_reader<F>(addr: u32, mut read_fn: F) -> Instruction
    where
        F: FnMut(u32) -> u32,
    {
        let opcode = read_fn(addr);
        Self::decode_opcode(opcode, addr, read_fn)
    }

    /// Returns the mnemonic string for the instruction at the given address (Thumb mode)
    pub fn disassemble_thumb_with_reader<F>(addr: u32, read_fn: F) -> String
    where
        F: FnMut(u32) -> u16,
    {
        let instruction = Self::decode_thumb_with_reader(addr, read_fn);
        instruction.to_string()
    }

    /// Returns the Instruction struct for the instruction at the given address (Thumb mode)
    pub fn decode_thumb_with_reader<F>(addr: u32, mut read_fn: F) -> Instruction
    where
        F: FnMut(u32) -> u16,
    {
        let opcode = read_fn(addr);
        Self::decode_thumb_opcode(opcode, addr, read_fn)
    }

    /// Returns the mnemonic string for the given opcode (backward compatibility)
    pub fn disassemble_opcode<F>(opcode: u32, pc: u32, read_fn: F) -> String
    where
        F: FnMut(u32) -> u32,
    {
        let instruction = Self::decode_opcode(opcode, pc, read_fn);
        instruction.to_string()
    }

    /// Returns the mnemonic string for the given Thumb opcode
    pub fn disassemble_thumb_opcode<F>(opcode: u16, pc: u32, read_fn: F) -> String
    where
        F: FnMut(u32) -> u16,
    {
        let instruction = Self::decode_thumb_opcode(opcode, pc, read_fn);
        instruction.to_string()
    }

    /// Decodes a single ARM opcode into an Instruction struct
    #[inline]
    pub fn decode_opcode<F>(opcode: u32, pc: u32, read_fn: F) -> Instruction
    where
        F: FnMut(u32) -> u32,
    {
        let condition = Self::extract_condition(opcode);
        let bits_27_25 = ((opcode >> 25) & 0b111) as u8;

        match bits_27_25 {
            0b000 => Self::decode_group_000(opcode, pc, condition, read_fn),
            0b001 => Self::decode_group_001(opcode, pc, condition),
            0b010 => Self::decode_group_010(opcode, pc, condition, read_fn),
            0b011 => Self::decode_group_011(opcode, condition),
            0b100 => Self::decode_group_100(opcode, condition),
            0b101 => Self::decode_group_101(opcode, pc, condition),
            0b110 => Self::decode_group_110(opcode, condition),
            0b111 => Self::decode_group_111(opcode, condition),
            _ => Instruction::Unknown { condition, opcode },
        }
    }

    /// Decodes a single Thumb opcode into an Instruction struct
    #[inline]
    pub fn decode_thumb_opcode<F>(opcode: u16, pc: u32, mut read_fn: F) -> Instruction
    where
        F: FnMut(u32) -> u16,
    {
        // Thumb instructions are always unconditional (condition = AL)
        let condition = Condition::AL;

        // Decode based on top bits of the instruction
        let bits_15_13 = (opcode >> 13) & 0b111;
        let bits_15_11 = (opcode >> 11) & 0b11111;
        let bits_15_12 = (opcode >> 12) & 0b1111;
        let bits_15_8 = (opcode >> 8) & 0xFF;

        match bits_15_13 {
            0b000 => {
                // Check bits 12-11 to distinguish between Format 1 and Format 2
                if bits_15_11 < 0b00011 {
                    // Format 1: LSL, LSR, ASR with immediate
                    Self::decode_thumb_shift_immediate(opcode, condition)
                } else {
                    // Format 2: ADD/SUB register/immediate
                    Self::decode_thumb_add_sub(opcode, condition)
                }
            }
            0b001 => {
                // Format 3: MOV/CMP/ADD/SUB immediate
                Self::decode_thumb_immediate(opcode, condition)
            }
            0b010 => {
                match bits_15_12 {
                    0b0100 => {
                        if (opcode >> 10) & 0b11 == 0b00 {
                            Self::decode_thumb_alu(opcode, condition)
                        } else if bits_15_11 == 0b01001 {
                            // Format 6: PC-relative load
                            Self::decode_thumb_pc_relative_load(opcode, pc, condition)
                        } else {
                            Self::decode_thumb_hireg_bx(opcode, pc, condition)
                        }
                    }
                    0b0101 => {
                        // Check if it's Format 8 (sign-extended) or Format 7 (register offset)
                        if (opcode >> 9) & 1 == 1 {
                            Self::decode_thumb_load_store_sign_extended(opcode, condition)
                        } else {
                            Self::decode_thumb_load_store_reg_offset(opcode, condition)
                        }
                    }
                    0b0110 | 0b0111 => Self::decode_thumb_load_store_word_imm(opcode, condition),
                    0b1000 | 0b1001 => {
                        Self::decode_thumb_load_store_halfword_imm(opcode, condition)
                    }
                    _ => Instruction::Unknown {
                        condition,
                        opcode: opcode as u32,
                    },
                }
            }
            0b011 => Self::decode_thumb_load_store_word_imm(opcode, condition),
            0b100 => {
                if bits_15_12 == 0b1000 {
                    // Format 8: Load/store halfword immediate
                    Self::decode_thumb_load_store_halfword_imm(opcode, condition)
                } else if bits_15_12 == 0b1001 {
                    // Format 11: SP-relative load/store
                    Self::decode_thumb_load_store_stack(opcode, condition)
                } else {
                    // This shouldn't happen for valid THUMB instructions in this range
                    Self::decode_thumb_load_store_stack(opcode, condition)
                }
            }
            0b101 => {
                if bits_15_12 == 0b1010 || bits_15_12 == 0b1011 {
                    // Format 13 must be checked before Format 12 because they overlap
                    if bits_15_8 == 0b10110000 {
                        // Format 13: ADD/SUB SP, #imm (bits[15:8] = 0xB0)
                        Self::decode_thumb_adjust_sp(opcode, condition)
                    } else if (0b10110001..=0b10110011).contains(&bits_15_8) {
                        // Format 12: ADD Rd, SP, #imm (bits[15:8] = 0xB1-0xB3)
                        Self::decode_thumb_add_sp_pc(opcode, pc, condition)
                    } else if (0b10110100..=0b10110111).contains(&bits_15_8) || 
                              (0b10111100..=0b10111111).contains(&bits_15_8) {
                        // Format 14: PUSH/POP (bits[15:8] = 0xB4-0xB7 for PUSH, 0xBC-0xBF for POP)
                        Self::decode_thumb_push_pop(opcode, condition)
                    } else {
                        // Format 12: ADD Rd, PC, #imm (bits[15:8] = 0xA0-0xAF)
                        Self::decode_thumb_add_sp_pc(opcode, pc, condition)
                    }
                } else {
                    Self::decode_thumb_load_store_multiple(opcode, condition)
                }
            }
            0b110 => {
                if bits_15_12 == 0b1100 {
                    // Format 15: Multiple load/store (0xC000-0xCFFF)
                    Self::decode_thumb_load_store_multiple(opcode, condition)
                } else if bits_15_12 == 0b1101 {
                    // Format 16: Conditional branch (0xD000-0xDEFF) or SWI (0xDF00-0xDFFF)
                    if bits_15_8 == 0b11011111 {
                        Self::decode_thumb_swi(opcode, condition)
                    } else {
                        Self::decode_thumb_conditional_branch(opcode, pc, condition)
                    }
                } else {
                    Self::decode_thumb_load_store_multiple(opcode, condition)
                }
            }
            0b111 => {
                if bits_15_12 == 0b1110 {
                    Self::decode_thumb_unconditional_branch(opcode, pc, condition)
                } else if bits_15_12 == 0b1111 {
                    Self::decode_thumb_long_branch_link(opcode, pc, condition, &mut read_fn)
                } else {
                    Instruction::Unknown {
                        condition,
                        opcode: opcode as u32,
                    }
                }
            }
            _ => Instruction::Unknown {
                condition,
                opcode: opcode as u32,
            },
        }
    }

    fn decode_group_000<F>(opcode: u32, pc: u32, condition: Condition, read_fn: F) -> Instruction
    where
        F: FnMut(u32) -> u32,
    {
        // Group 000: Data processing, multiply, extra load/store, status register operations

        // Check for MRS (Move from Status Register)
        if (opcode & 0x0FBF0FFF) == 0x010F0000 {
            let ps = (opcode >> 22) & 0b1;
            let rd = (opcode >> 12) & 0b1111;
            return Instruction::Mrs {
                condition,
                rd: Register::from_u32(rd),
                spsr: ps == 1,
            };
        }

        // Check for MSR (Move to Status Register)
        if (opcode & 0x0FB0F000) == 0x0120F000 && (opcode & 0x0FFFFFF0) != 0x012FFF10 {
            return Self::decode_msr(opcode, condition);
        }

        // Check for BX (Branch and Exchange)
        if (opcode & 0x0FFFFFF0) == 0x012FFF10 {
            let rm = opcode & 0xF;
            return Instruction::Bx {
                condition,
                rm: Register::from_u32(rm),
            };
        }

        // Check for multiply instructions
        if (opcode & 0x0FC000F0) == 0x00000090 {
            return Self::decode_multiply(opcode, condition);
        }

        // Check for multiply long instructions
        if (opcode & 0x0F8000F0) == 0x00800090 {
            return Self::decode_multiply_long(opcode, condition);
        }

        // Check for single data swap
        if (opcode & 0x0FB00FF0) == 0x01000090 {
            return Self::decode_swap(opcode, condition);
        }

        // Check for halfword/signed data transfer
        if (opcode & 0x0E000090) == 0x00000090 {
            return Self::decode_halfword_transfer(opcode, pc, condition);
        }

        // Data processing instructions
        Self::decode_data_processing(opcode, pc, condition, read_fn)
    }

    fn decode_group_001(opcode: u32, pc: u32, condition: Condition) -> Instruction {
        // Group 001: Data processing immediate, move immediate to status register

        // Check for MSR immediate (Move immediate to Status Register)
        // Pattern: xxxx 00 1 10 R 10 field_mask 1111 rotate imm8
        // Mask bits [24-23] = 10, bit [21] = 1, bit [20] = 0, bits [15-12] = 1111
        if (opcode & 0x01B0F000) == 0x0120F000 {
            return Self::decode_msr(opcode, condition);
        }

        // Check for undefined instruction space
        // This is the MSR pattern but with Rd != R15, which is reserved/undefined
        // Pattern: bits [27-26]=00, [25]=1, [24-23]=10, [21-20]=10, [15-12]!=1111
        if (opcode & 0x01F00000) == 0x01200000 && (opcode & 0x0000F000) != 0x0000F000 {
            return Instruction::Unknown { condition, opcode };
        }

        Self::decode_data_processing_immediate(opcode, pc, condition)
    }

    fn decode_group_010<F>(opcode: u32, pc: u32, condition: Condition, read_fn: F) -> Instruction
    where
        F: FnMut(u32) -> u32,
    {
        // Group 010: Load/store word and unsigned byte
        Self::decode_single_data_transfer(opcode, pc, condition, read_fn)
    }

    fn decode_group_011(opcode: u32, condition: Condition) -> Instruction {
        // Group 011: Load/store word and unsigned byte (register offset), undefined

        // Check for undefined instruction (bit 4 set)
        if (opcode & 0x00000010) != 0 {
            return Instruction::Unknown { condition, opcode };
        }

        Self::decode_single_data_transfer_register(opcode, condition)
    }

    fn decode_group_100(opcode: u32, condition: Condition) -> Instruction {
        // Group 100: Block data transfer (LDM/STM)
        Self::decode_block_data_transfer(opcode, condition)
    }

    fn decode_group_101(opcode: u32, pc: u32, condition: Condition) -> Instruction {
        // Group 101: Branch and branch with link
        Self::decode_branch(opcode, pc, condition)
    }

    fn decode_group_110(_opcode: u32, condition: Condition) -> Instruction {
        // Group 110: Coprocessor data transfer
        Instruction::Unknown {
            condition,
            opcode: _opcode,
        }
    }

    fn decode_group_111(opcode: u32, condition: Condition) -> Instruction {
        // Group 111: Coprocessor operations, software interrupt

        // Check for SWI (Software Interrupt)
        if (opcode & 0x0F000000) == 0x0F000000 {
            let comment = opcode & 0x00FFFFFF;
            return Instruction::Swi { condition, comment };
        }

        // Coprocessor operations
        Instruction::Unknown { condition, opcode }
    }

    fn decode_msr(opcode: u32, condition: Condition) -> Instruction {
        let ps = (opcode >> 22) & 0b1;
        let field_mask = (opcode >> 16) & 0xF;

        // Use the efficient MsrFields struct instead of String allocation
        let fields = MsrFields::from_mask(field_mask);

        let operand = if (opcode & (1 << 25)) != 0 {
            // Immediate operand
            let imm = opcode & 0xFF;
            let rotate = ((opcode >> 8) & 0xF) * 2;
            let rotated_imm = imm.rotate_right(rotate);
            MsrOperand::Immediate(rotated_imm)
        } else {
            // Register operand
            let rm = opcode & 0xF;
            MsrOperand::Register(Register::from_u32(rm))
        };

        Instruction::Msr {
            condition,
            fields,
            operand,
            spsr: ps == 1,
        }
    }

    fn decode_multiply(opcode: u32, condition: Condition) -> Instruction {
        let rd = Self::extract_rn(opcode); // rd is at bit 16
        let rn = Self::extract_rd(opcode); // rn is at bit 12
        let rs = Self::extract_rs(opcode);
        let rm = Self::extract_rm(opcode);
        let s_bit = Self::extract_s_bit(opcode);
        let a_bit = (opcode >> 21) & 1;

        if a_bit != 0 {
            Instruction::Mla {
                condition,
                rd: Register::from_u32(rd),
                rm: Register::from_u32(rm),
                rs: Register::from_u32(rs),
                rn: Register::from_u32(rn),
                set_flags: s_bit,
            }
        } else {
            Instruction::Mul {
                condition,
                rd: Register::from_u32(rd),
                rm: Register::from_u32(rm),
                rs: Register::from_u32(rs),
                set_flags: s_bit,
            }
        }
    }

    fn decode_multiply_long(opcode: u32, condition: Condition) -> Instruction {
        let rdhi = Self::extract_rn(opcode); // rdhi at bit 16
        let rdlo = Self::extract_rd(opcode); // rdlo at bit 12
        let rs = Self::extract_rs(opcode);
        let rm = Self::extract_rm(opcode);
        let set_flags = Self::extract_s_bit(opcode);
        let u_bit = (opcode >> 22) & 1;  // Bit 22: U (unsigned: 0=signed, 1=unsigned)
        let a_bit = (opcode >> 21) & 1;  // Bit 21: A (accumulate)
        let rdlo_reg = Register::from_u32(rdlo);
        let rdhi_reg = Register::from_u32(rdhi);
        let rm_reg = Register::from_u32(rm);
        let rs_reg = Register::from_u32(rs);

        match (u_bit, a_bit) {
            (0, 0) => Instruction::Umull {
                condition,
                rdlo: rdlo_reg,
                rdhi: rdhi_reg,
                rm: rm_reg,
                rs: rs_reg,
                set_flags,
            },
            (0, 1) => Instruction::Umlal {
                condition,
                rdlo: rdlo_reg,
                rdhi: rdhi_reg,
                rm: rm_reg,
                rs: rs_reg,
                set_flags,
            },
            (1, 0) => Instruction::Smull {
                condition,
                rdlo: rdlo_reg,
                rdhi: rdhi_reg,
                rm: rm_reg,
                rs: rs_reg,
                set_flags,
            },
            (1, 1) => Instruction::Smlal {
                condition,
                rdlo: rdlo_reg,
                rdhi: rdhi_reg,
                rm: rm_reg,
                rs: rs_reg,
                set_flags,
            },
            _ => unreachable!(),
        }
    }

    fn decode_swap(opcode: u32, condition: Condition) -> Instruction {
        let rn = (opcode >> 16) & 0xF;
        let rd = (opcode >> 12) & 0xF;
        let rm = opcode & 0xF;
        let b_bit = (opcode >> 22) & 1;

        Instruction::Swp {
            condition,
            rd: Register::from_u32(rd),
            rm: Register::from_u32(rm),
            rn: Register::from_u32(rn),
            byte: b_bit != 0,
        }
    }

    fn decode_halfword_transfer(opcode: u32, _pc: u32, condition: Condition) -> Instruction {
        let p_bit = (opcode >> 24) & 1;
        let u_bit = (opcode >> 23) & 1;
        let i_bit = (opcode >> 22) & 1;
        let w_bit = (opcode >> 21) & 1;
        let l_bit = (opcode >> 20) & 1;
        let rn = (opcode >> 16) & 0xF;
        let rd = (opcode >> 12) & 0xF;
        let sh = (opcode >> 5) & 0b11;

        let rd_reg = Register::from_u32(rd);
        let rn_reg = Register::from_u32(rn);

        let addressing = if i_bit != 0 {
            // Immediate offset
            let offset_8 = ((opcode >> 4) & 0xF0) | (opcode & 0xF);
            let offset = if u_bit != 0 {
                offset_8 as i32
            } else {
                -(offset_8 as i32)
            };

            if p_bit != 0 {
                AddressingMode::Offset {
                    offset,
                    writeback: w_bit != 0,
                }
            } else {
                AddressingMode::PostIndexed { offset }
            }
        } else {
            // Register offset
            let rm = opcode & 0xF;
            let rm_reg = Register::from_u32(rm);

            if p_bit != 0 {
                AddressingMode::RegisterOffset {
                    reg: rm_reg,
                    shift: None,
                    add: u_bit != 0,
                    writeback: w_bit != 0,
                }
            } else {
                // Post-indexed register offset - convert to equivalent form
                let offset = 0; // Placeholder - actual implementation would need register resolution
                AddressingMode::PostIndexed { offset }
            }
        };

        match (l_bit, sh) {
            (1, 0b01) => Instruction::LdrH {
                condition,
                rd: rd_reg,
                rn: rn_reg,
                addressing,
            },
            (0, 0b01) => Instruction::StrH {
                condition,
                rd: rd_reg,
                rn: rn_reg,
                addressing,
            },
            (1, 0b10) => Instruction::LdrSB {
                condition,
                rd: rd_reg,
                rn: rn_reg,
                addressing,
            },
            (1, 0b11) => Instruction::LdrSH {
                condition,
                rd: rd_reg,
                rn: rn_reg,
                addressing,
            },
            _ => Instruction::Unknown { condition, opcode },
        }
    }

    fn decode_data_processing<F>(
        opcode: u32,
        pc: u32,
        condition: Condition,
        mut read_fn: F,
    ) -> Instruction
    where
        F: FnMut(u32) -> u32,
    {
        let opcode_type = Self::extract_opcode_type(opcode);
        let s_bit = Self::extract_s_bit(opcode);
        let rn = Self::extract_rn(opcode);
        let rd = Self::extract_rd(opcode);

        let op = DataProcessingOp::from_opcode(opcode_type);

        let operand2 = Self::decode_operand2_struct(opcode);

        // Check for ADR pseudo-instruction (MOV with PC as source)
        if matches!(op, DataProcessingOp::Mov) && rn == 15 && (opcode & (1 << 25)) != 0
            && let Some(abs_addr) = Self::try_decode_adr(opcode, pc, &mut read_fn) {
                return Instruction::Adr {
                    condition,
                    rd: Register::from_u32(rd),
                    target: abs_addr,
                };
            }

        let (rn_reg, rd_reg) = Self::get_data_processing_registers(&op, rd, rn);

        Instruction::DataProcessing {
            op,
            condition,
            set_flags: s_bit,
            rn: rn_reg,
            rd: rd_reg,
            operand2,
        }
    }

    fn decode_data_processing_immediate(opcode: u32, pc: u32, condition: Condition) -> Instruction {
        let opcode_type = Self::extract_opcode_type(opcode);
        let s_bit = Self::extract_s_bit(opcode);
        let rn = Self::extract_rn(opcode);
        let rd = Self::extract_rd(opcode);

        let op = DataProcessingOp::from_opcode(opcode_type);

        let operand2 = Self::decode_operand2_struct(opcode);

        // Check for ADR pseudo-instruction
        if (matches!(op, DataProcessingOp::Add) || matches!(op, DataProcessingOp::Sub)) && rn == 15
            && let Some(abs_addr) = Self::try_decode_adr_immediate(opcode, pc) {
                return Instruction::Adr {
                    condition,
                    rd: Register::from_u32(rd),
                    target: abs_addr,
                };
            }

        let (rn_reg, rd_reg) = Self::get_data_processing_registers(&op, rd, rn);

        Instruction::DataProcessing {
            op,
            condition,
            set_flags: s_bit,
            rn: rn_reg,
            rd: rd_reg,
            operand2,
        }
    }
    fn decode_single_data_transfer<F>(
        opcode: u32,
        pc: u32,
        condition: Condition,
        _read_fn: F,
    ) -> Instruction
    where
        F: FnMut(u32) -> u32,
    {
        let i_bit = (opcode >> 25) & 1;
        let p_bit = (opcode >> 24) & 1;
        let u_bit = (opcode >> 23) & 1;
        let b_bit = (opcode >> 22) & 1;
        let w_bit = (opcode >> 21) & 1;
        let l_bit = (opcode >> 20) & 1;
        let rn = (opcode >> 16) & 0xF;
        let rd = (opcode >> 12) & 0xF;

        let rd_reg = Register::from_u32(rd);
        let rn_reg = Register::from_u32(rn);

        let addressing = if i_bit == 0 {
            // Immediate offset
            let offset = (opcode & 0xFFF) as i32;
            let signed_offset = if u_bit != 0 { offset } else { -offset };

            if p_bit != 0 {
                AddressingMode::Offset {
                    offset: signed_offset,
                    writeback: w_bit != 0,
                }
            } else {
                AddressingMode::PostIndexed {
                    offset: signed_offset,
                }
            }
        } else {
            // Register offset with optional shift
            let rm = opcode & 0xF;
            let shift_type = ShiftType::from_bits(opcode >> 5);
            let shift_imm = (opcode >> 7) & 0x1F;

            let shift = if shift_imm == 0 && shift_type == ShiftType::Lsl {
                None
            } else {
                Some((shift_type, shift_imm))
            };

            if p_bit != 0 {
                AddressingMode::RegisterOffset {
                    reg: Register::from_u32(rm),
                    shift,
                    add: u_bit != 0,
                    writeback: w_bit != 0,
                }
            } else {
                // Post-indexed - for simplicity, convert to offset form
                AddressingMode::PostIndexed { offset: 0 }
            }
        };

        // Calculate target address for PC-relative addressing
        let target_addr = if rn == 15 && i_bit == 0 && p_bit != 0 {
            let offset = opcode & 0xFFF;
            let effective_pc = pc + 8;
            let abs_addr = if u_bit != 0 {
                effective_pc + offset
            } else {
                effective_pc - offset
            };
            Some(abs_addr)
        } else {
            None
        };

        if l_bit != 0 {
            Instruction::Ldr {
                condition,
                rd: rd_reg,
                rn: rn_reg,
                addressing,
                byte_access: b_bit != 0,
                target_addr,
            }
        } else {
            Instruction::Str {
                condition,
                rd: rd_reg,
                rn: rn_reg,
                addressing,
                byte_access: b_bit != 0,
                target_addr,
            }
        }
    }

    fn decode_single_data_transfer_register(opcode: u32, condition: Condition) -> Instruction {
        let p_bit = (opcode >> 24) & 1;
        let u_bit = (opcode >> 23) & 1;
        let b_bit = (opcode >> 22) & 1;
        let w_bit = (opcode >> 21) & 1;
        let l_bit = (opcode >> 20) & 1;
        let rn = (opcode >> 16) & 0xF;
        let rd = (opcode >> 12) & 0xF;

        let rd_reg = Register::from_u32(rd);
        let rn_reg = Register::from_u32(rn);

        let rm = opcode & 0xF;
        let shift_type = ShiftType::from_bits(opcode >> 5);
        let shift_imm = (opcode >> 7) & 0x1F;

        let shift = if shift_imm == 0 && shift_type == ShiftType::Lsl {
            None
        } else {
            Some((shift_type, shift_imm))
        };

        let addressing = if p_bit != 0 {
            AddressingMode::RegisterOffset {
                reg: Register::from_u32(rm),
                shift,
                add: u_bit != 0,
                writeback: w_bit != 0,
            }
        } else {
            AddressingMode::PostIndexed { offset: 0 } // Simplified
        };

        if l_bit != 0 {
            Instruction::Ldr {
                condition,
                rd: rd_reg,
                rn: rn_reg,
                addressing,
                byte_access: b_bit != 0,
                target_addr: None,
            }
        } else {
            Instruction::Str {
                condition,
                rd: rd_reg,
                rn: rn_reg,
                addressing,
                byte_access: b_bit != 0,
                target_addr: None,
            }
        }
    }

    fn decode_block_data_transfer(opcode: u32, condition: Condition) -> Instruction {
        let p_bit = (opcode >> 24) & 1;
        let u_bit = (opcode >> 23) & 1;
        let s_bit = (opcode >> 22) & 1;
        let w_bit = (opcode >> 21) & 1;
        let l_bit = (opcode >> 20) & 1;
        let rn = (opcode >> 16) & 0xF;
        let register_list = opcode & 0xFFFF;

        // Use efficient enum instead of String allocation
        let addressing_mode = AddressingModeType::from_bits(p_bit, u_bit);

        // Use efficient RegisterList struct instead of Vec allocation
        let registers = RegisterList::from_mask(register_list as u16);

        let rn_reg = Register::from_u32(rn);

        if l_bit != 0 {
            Instruction::Ldm {
                condition,
                rn: rn_reg,
                registers,
                addressing_mode,
                writeback: w_bit != 0,
                user_mode: s_bit != 0,
            }
        } else {
            Instruction::Stm {
                condition,
                rn: rn_reg,
                registers,
                addressing_mode,
                writeback: w_bit != 0,
                user_mode: s_bit != 0,
            }
        }
    }

    fn decode_branch(opcode: u32, pc: u32, condition: Condition) -> Instruction {
        let l_bit = (opcode >> 24) & 1;
        let offset = (opcode & 0x00FFFFFF) as i32;

        // Sign extend 24-bit offset to 32-bit
        let offset = if (offset & 0x800000) != 0 {
            offset | 0xFF000000u32 as i32
        } else {
            offset
        };

        // Calculate target address (PC + 8 + offset * 4)
        let target = (pc as i32 + 8 + offset * 4) as u32;

        if l_bit != 0 {
            Instruction::Bl { condition, target }
        } else {
            Instruction::B { condition, target }
        }
    }

    fn decode_operand2_struct(opcode: u32) -> Operand2 {
        let i_bit = (opcode >> 25) & 1;

        if i_bit != 0 {
            // Immediate operand
            let imm = opcode & 0xFF;
            let rotate = ((opcode >> 8) & 0xF) * 2;
            let rotated_imm = imm.rotate_right(rotate);
            Operand2::Immediate { value: rotated_imm, rotation: rotate }
        } else {
            // Register operand with optional shift
            let rm = opcode & 0xF;
            let shift_type = ShiftType::from_bits(opcode >> 5);

            if (opcode & (1 << 4)) != 0 {
                // Register shift
                let rs = (opcode >> 8) & 0xF;
                Operand2::RegisterShiftedByRegister {
                    reg: Register::from_u32(rm),
                    shift_type,
                    shift_reg: Register::from_u32(rs),
                }
            } else {
                // Immediate shift
                let imm = (opcode >> 7) & 0x1F;
                
                // Handle special cases when shift amount is 0
                match shift_type {
                    ShiftType::Lsl => {
                        // LSL #0: No shift, just register
                        if imm == 0 {
                            Operand2::Register(Register::from_u32(rm))
                        } else {
                            Operand2::RegisterShifted {
                                reg: Register::from_u32(rm),
                                shift_type,
                                shift_amount: imm,
                            }
                        }
                    }
                    ShiftType::Lsr | ShiftType::Asr => {
                        // LSR #0 means LSR #32, ASR #0 means ASR #32
                        let actual_shift = if imm == 0 { 32 } else { imm };
                        Operand2::RegisterShifted {
                            reg: Register::from_u32(rm),
                            shift_type,
                            shift_amount: actual_shift,
                        }
                    }
                    ShiftType::Ror => {
                        // ROR #0 means RRX (rotate right extended, not implemented as separate type)
                        // For disassembly purposes, we'll represent it as ROR #0 which is technically RRX
                        Operand2::RegisterShifted {
                            reg: Register::from_u32(rm),
                            shift_type,
                            shift_amount: imm, // Keep as 0 to indicate RRX in execution
                        }
                    }
                }
            }
        }
    }

    fn try_decode_adr<F>(_opcode: u32, _pc: u32, _read_fn: &mut F) -> Option<u32>
    where
        F: FnMut(u32) -> u32,
    {
        // For register-based operand2, we can't easily determine if it's ADR
        None
    }

    fn try_decode_adr_immediate(opcode: u32, pc: u32) -> Option<u32> {
        let opcode_type = (opcode >> 21) & 0xF;
        let rn = (opcode >> 16) & 0xF;
        let rd = (opcode >> 12) & 0xF;
        let s_bit = (opcode >> 20) & 1;

        // ADR is only valid when:
        // 1. Rn is PC (15)
        // 2. Either Rd is not PC, OR (Rd is PC and S flag is 0)
        // 3. Operation is ADD or SUB
        // When Rd=PC and S=1, it's a special instruction that restores CPSR from SPSR
        if rn == 15 && (opcode_type == 0x4 || opcode_type == 0x2) && !(rd == 15 && s_bit == 1) {
            let imm = opcode & 0xFF;
            let rotate = ((opcode >> 8) & 0xF) * 2;
            let rotated_imm = imm.rotate_right(rotate);
            let effective_pc = pc + 8;

            let abs_addr = if opcode_type == 0x4 {
                effective_pc + rotated_imm
            } else {
                effective_pc - rotated_imm
            };

            Some(abs_addr)
        } else {
            None
        }
    }

    // ===== Thumb Instruction Decoders =====

    /// Format 1: Move shifted register (LSL, LSR, ASR)
    fn decode_thumb_shift_immediate(opcode: u16, condition: Condition) -> Instruction {
        let op = (opcode >> 11) & 0b11;
        let offset5 = (opcode >> 6) & 0x1F;
        let rs = (opcode >> 3) & 0x7;
        let rd = opcode & 0x7;

        // Only called for op values 0b00, 0b01, 0b10 (LSL, LSR, ASR)
        // op value 0b11 is handled by decode_thumb_add_sub
        let shift_type = match op {
            0b00 => ShiftType::Lsl,
            0b01 => ShiftType::Lsr,
            0b10 => ShiftType::Asr,
            _ => {
                // This should never happen with correct routing
                return Instruction::Unknown {
                    opcode: opcode as u32,
                    condition,
                };
            }
        };

        let operand2 = if offset5 == 0 && matches!(shift_type, ShiftType::Lsl) {
            Operand2::Register(Register::from_u32(rs as u32))
        } else {
            Operand2::RegisterShifted {
                reg: Register::from_u32(rs as u32),
                shift_type,
                shift_amount: offset5 as u32,
            }
        };

        Instruction::DataProcessing {
            op: DataProcessingOp::Mov,
            condition,
            set_flags: true,
            rn: None,
            rd: Some(Register::from_u32(rd as u32)),
            operand2,
        }
    }

    /// Format 2: Add/subtract (register or 3-bit immediate)
    fn decode_thumb_add_sub(opcode: u16, condition: Condition) -> Instruction {
        let op = (opcode >> 9) & 1;
        let is_imm = (opcode >> 10) & 1;
        let rn_or_imm = (opcode >> 6) & 0x7;
        let rs = (opcode >> 3) & 0x7;
        let rd = opcode & 0x7;

        let operand2 = if is_imm != 0 {
            Operand2::Immediate { value: rn_or_imm as u32, rotation: 0 }
        } else {
            Operand2::Register(Register::from_u32(rn_or_imm as u32))
        };

        let data_op = if op == 0 {
            DataProcessingOp::Add
        } else {
            DataProcessingOp::Sub
        };

        Instruction::DataProcessing {
            op: data_op,
            condition,
            set_flags: true,
            rn: Some(Register::from_u32(rs as u32)),
            rd: Some(Register::from_u32(rd as u32)),
            operand2,
        }
    }

    /// Format 3: Move/compare/add/subtract immediate (8-bit)
    fn decode_thumb_immediate(opcode: u16, condition: Condition) -> Instruction {
        let op = (opcode >> 11) & 0b11;
        let rd = (opcode >> 8) & 0x7;
        let imm8 = opcode & 0xFF;

        let (data_op, rn_reg) = match op {
            0b00 => (DataProcessingOp::Mov, None),
            0b01 => (DataProcessingOp::Cmp, Some(Register::from_u32(rd as u32))),
            0b10 => (DataProcessingOp::Add, Some(Register::from_u32(rd as u32))),
            0b11 => (DataProcessingOp::Sub, Some(Register::from_u32(rd as u32))),
            _ => unreachable!(),
        };

        let rd_reg = if matches!(data_op, DataProcessingOp::Cmp) {
            None
        } else {
            Some(Register::from_u32(rd as u32))
        };

        Instruction::DataProcessing {
            op: data_op,
            condition,
            set_flags: true,
            rn: rn_reg,
            rd: rd_reg,
            operand2: Operand2::Immediate { value: imm8 as u32, rotation: 0 },
        }
    }

    /// Format 4: ALU operations
    fn decode_thumb_alu(opcode: u16, condition: Condition) -> Instruction {
        let alu_op = (opcode >> 6) & 0xF;
        let rs = (opcode >> 3) & 0x7;
        let rd = opcode & 0x7;

        // For MUL, return a Mul instruction instead of DataProcessing
        if alu_op == 0xD {
            return Instruction::Mul {
                condition,
                rd: Register::from_u32(rd as u32),
                rm: Register::from_u32(rs as u32),
                rs: Register::from_u32(rd as u32),
                set_flags: true,
            };
        }

        let (data_op, has_rd) = match alu_op {
            0x0 => (DataProcessingOp::And, true),
            0x1 => (DataProcessingOp::Eor, true),
            0x2 => (DataProcessingOp::Mov, true), // LSL (register)
            0x3 => (DataProcessingOp::Mov, true), // LSR (register)
            0x4 => (DataProcessingOp::Mov, true), // ASR (register)
            0x5 => (DataProcessingOp::Adc, true),
            0x6 => (DataProcessingOp::Sbc, true),
            0x7 => (DataProcessingOp::Mov, true), // ROR (register)
            0x8 => (DataProcessingOp::Tst, false),
            0x9 => (DataProcessingOp::Rsb, true), // NEG
            0xA => (DataProcessingOp::Cmp, false),
            0xB => (DataProcessingOp::Cmn, false),
            0xC => (DataProcessingOp::Orr, true),
            0xE => (DataProcessingOp::Bic, true),
            0xF => (DataProcessingOp::Mvn, true),
            _ => unreachable!(),
        };

        // Determine operand2 based on operation
        let operand2 = match alu_op {
            0x2 => Operand2::RegisterShiftedByRegister {
                reg: Register::from_u32(rd as u32),
                shift_type: ShiftType::Lsl,
                shift_reg: Register::from_u32(rs as u32),
            },
            0x3 => Operand2::RegisterShiftedByRegister {
                reg: Register::from_u32(rd as u32),
                shift_type: ShiftType::Lsr,
                shift_reg: Register::from_u32(rs as u32),
            },
            0x4 => Operand2::RegisterShiftedByRegister {
                reg: Register::from_u32(rd as u32),
                shift_type: ShiftType::Asr,
                shift_reg: Register::from_u32(rs as u32),
            },
            0x7 => Operand2::RegisterShiftedByRegister {
                reg: Register::from_u32(rd as u32),
                shift_type: ShiftType::Ror,
                shift_reg: Register::from_u32(rs as u32),
            },
            0x9 => Operand2::Immediate { value: 0, rotation: 0 }, // NEG is RSB Rd, Rs, #0
            _ => Operand2::Register(Register::from_u32(rs as u32)),
        };

        let rn_reg = match alu_op {
            0x9 => Some(Register::from_u32(rs as u32)), // NEG uses rs as source
            0xF => None,                                // MVN has no rn
            _ => Some(Register::from_u32(rd as u32)),
        };

        Instruction::DataProcessing {
            op: data_op,
            condition,
            set_flags: true,
            rn: rn_reg,
            rd: if has_rd {
                Some(Register::from_u32(rd as u32))
            } else {
                None
            },
            operand2,
        }
    }

    /// Format 5: Hi register operations/branch exchange
    fn decode_thumb_hireg_bx(opcode: u16, _pc: u32, condition: Condition) -> Instruction {
        let op = (opcode >> 8) & 0b11;
        let h1 = (opcode >> 7) & 1;
        let h2 = (opcode >> 6) & 1;
        let rs_hs = ((opcode >> 3) & 0x7) | (h2 << 3);
        let rd_hd = (opcode & 0x7) | (h1 << 3);

        // BX/BLX
        if op == 0b11 {
            return Instruction::Bx {
                condition,
                rm: Register::from_u32(rs_hs as u32),
            };
        }

        let data_op = match op {
            0b00 => DataProcessingOp::Add,
            0b01 => DataProcessingOp::Cmp,
            0b10 => DataProcessingOp::Mov,
            _ => unreachable!(),
        };

        let has_rd = !matches!(data_op, DataProcessingOp::Cmp);

        Instruction::DataProcessing {
            op: data_op,
            condition,
            set_flags: false, // Hi register ops don't set flags
            rn: Some(Register::from_u32(rd_hd as u32)),
            rd: if has_rd {
                Some(Register::from_u32(rd_hd as u32))
            } else {
                None
            },
            operand2: Operand2::Register(Register::from_u32(rs_hs as u32)),
        }
    }

    /// Format 6: PC-relative load
    fn decode_thumb_pc_relative_load(opcode: u16, pc: u32, condition: Condition) -> Instruction {
        let rd = (opcode >> 8) & 0x7;
        let imm8 = opcode & 0xFF;
        let offset = (imm8 as u32) << 2;

        // PC is word-aligned (bits 1-0 are 0) for this calculation
        let base_pc = (pc + 4) & !3;
        let target_addr = base_pc.wrapping_add(offset);

        Instruction::Ldr {
            condition,
            rd: Register::from_u32(rd as u32),
            rn: Register::PC,
            addressing: AddressingMode::Offset {
                offset: offset as i32,
                writeback: false,
            },
            byte_access: false,
            target_addr: Some(target_addr),
        }
    }

    /// Format 7: Load/store with register offset
    fn decode_thumb_load_store_reg_offset(opcode: u16, condition: Condition) -> Instruction {
        let l_bit = (opcode >> 11) & 1;
        let b_bit = (opcode >> 10) & 1;
        let ro = (opcode >> 6) & 0x7;
        let rb = (opcode >> 3) & 0x7;
        let rd = opcode & 0x7;

        let addressing = AddressingMode::RegisterOffset {
            reg: Register::from_u32(ro as u32),
            shift: None,
            add: true,
            writeback: false,
        };

        if l_bit != 0 {
            Instruction::Ldr {
                condition,
                rd: Register::from_u32(rd as u32),
                rn: Register::from_u32(rb as u32),
                addressing,
                byte_access: b_bit != 0,
                target_addr: None,
            }
        } else {
            Instruction::Str {
                condition,
                rd: Register::from_u32(rd as u32),
                rn: Register::from_u32(rb as u32),
                addressing,
                byte_access: b_bit != 0,
                target_addr: None,
            }
        }
    }

    /// Format 8: Load/store sign-extended byte/halfword
    fn decode_thumb_load_store_sign_extended(opcode: u16, condition: Condition) -> Instruction {
        let h_bit = (opcode >> 11) & 1;
        let s_bit = (opcode >> 10) & 1;
        let ro = (opcode >> 6) & 0x7;
        let rb = (opcode >> 3) & 0x7;
        let rd = opcode & 0x7;

        let addressing = AddressingMode::RegisterOffset {
            reg: Register::from_u32(ro as u32),
            shift: None,
            add: true,
            writeback: false,
        };

        match (s_bit, h_bit) {
            (0, 0) => Instruction::StrH {
                condition,
                rd: Register::from_u32(rd as u32),
                rn: Register::from_u32(rb as u32),
                addressing,
            },
            (0, 1) => Instruction::LdrH {
                condition,
                rd: Register::from_u32(rd as u32),
                rn: Register::from_u32(rb as u32),
                addressing,
            },
            (1, 0) => Instruction::LdrSB {
                condition,
                rd: Register::from_u32(rd as u32),
                rn: Register::from_u32(rb as u32),
                addressing,
            },
            (1, 1) => Instruction::LdrSH {
                condition,
                rd: Register::from_u32(rd as u32),
                rn: Register::from_u32(rb as u32),
                addressing,
            },
            _ => unreachable!(),
        }
    }

    /// Format 9: Load/store with immediate offset (word/byte)
    fn decode_thumb_load_store_word_imm(opcode: u16, condition: Condition) -> Instruction {
        let b_bit = (opcode >> 12) & 1;
        let l_bit = (opcode >> 11) & 1;
        let offset5 = (opcode >> 6) & 0x1F;
        let rb = (opcode >> 3) & 0x7;
        let rd = opcode & 0x7;

        let offset = if b_bit != 0 {
            offset5 as i32 // Byte offset
        } else {
            (offset5 as i32) << 2 // Word offset (multiply by 4)
        };

        let addressing = AddressingMode::Offset {
            offset,
            writeback: false,
        };

        if l_bit != 0 {
            Instruction::Ldr {
                condition,
                rd: Register::from_u32(rd as u32),
                rn: Register::from_u32(rb as u32),
                addressing,
                byte_access: b_bit != 0,
                target_addr: None,
            }
        } else {
            Instruction::Str {
                condition,
                rd: Register::from_u32(rd as u32),
                rn: Register::from_u32(rb as u32),
                addressing,
                byte_access: b_bit != 0,
                target_addr: None,
            }
        }
    }

    /// Format 10: Load/store halfword with immediate offset
    fn decode_thumb_load_store_halfword_imm(opcode: u16, condition: Condition) -> Instruction {
        let l_bit = (opcode >> 11) & 1;
        let offset5 = (opcode >> 6) & 0x1F;
        let rb = (opcode >> 3) & 0x7;
        let rd = opcode & 0x7;

        let offset = (offset5 as i32) << 1; // Halfword offset (multiply by 2)

        let addressing = AddressingMode::Offset {
            offset,
            writeback: false,
        };

        if l_bit != 0 {
            Instruction::LdrH {
                condition,
                rd: Register::from_u32(rd as u32),
                rn: Register::from_u32(rb as u32),
                addressing,
            }
        } else {
            Instruction::StrH {
                condition,
                rd: Register::from_u32(rd as u32),
                rn: Register::from_u32(rb as u32),
                addressing,
            }
        }
    }

    /// Format 11: SP-relative load/store
    fn decode_thumb_load_store_stack(opcode: u16, condition: Condition) -> Instruction {
        let l_bit = (opcode >> 11) & 1;
        let rd = (opcode >> 8) & 0x7;
        let imm8 = opcode & 0xFF;
        let offset = (imm8 as i32) << 2; // Word offset

        let addressing = AddressingMode::Offset {
            offset,
            writeback: false,
        };

        if l_bit != 0 {
            Instruction::Ldr {
                condition,
                rd: Register::from_u32(rd as u32),
                rn: Register::SP,
                addressing,
                byte_access: false,
                target_addr: None,
            }
        } else {
            Instruction::Str {
                condition,
                rd: Register::from_u32(rd as u32),
                rn: Register::SP,
                addressing,
                byte_access: false,
                target_addr: None,
            }
        }
    }

    /// Format 12: Load address (ADD Rd, PC/SP, #imm)
    fn decode_thumb_add_sp_pc(opcode: u16, pc: u32, condition: Condition) -> Instruction {
        let sp_bit = (opcode >> 11) & 1;
        let rd = (opcode >> 8) & 0x7;
        let imm8 = opcode & 0xFF;
        let offset = (imm8 as u32) << 2;

        let source_reg = if sp_bit != 0 {
            Register::SP
        } else {
            Register::PC
        };

        // For PC-relative, calculate target address
        let target_addr = if sp_bit == 0 {
            let base_pc = (pc + 4) & !3;
            Some(base_pc.wrapping_add(offset))
        } else {
            None
        };

        if let Some(target) = target_addr {
            Instruction::Adr {
                condition,
                rd: Register::from_u32(rd as u32),
                target,
            }
        } else {
            Instruction::DataProcessing {
                op: DataProcessingOp::Add,
                condition,
                set_flags: false,
                rn: Some(source_reg),
                rd: Some(Register::from_u32(rd as u32)),
                operand2: Operand2::Immediate { value: offset, rotation: 0 },
            }
        }
    }

    /// Format 13: Add offset to Stack Pointer
    fn decode_thumb_adjust_sp(opcode: u16, condition: Condition) -> Instruction {
        let s_bit = (opcode >> 7) & 1;
        let imm7 = opcode & 0x7F;
        let offset = (imm7 as u32) << 2;

        let op = if s_bit != 0 {
            DataProcessingOp::Sub
        } else {
            DataProcessingOp::Add
        };

        Instruction::DataProcessing {
            op,
            condition,
            set_flags: false,
            rn: Some(Register::SP),
            rd: Some(Register::SP),
            operand2: Operand2::Immediate { value: offset, rotation: 0 },
        }
    }

    /// Format 14: Push/pop registers
    fn decode_thumb_push_pop(opcode: u16, condition: Condition) -> Instruction {
        let l_bit = (opcode >> 11) & 1;
        let r_bit = (opcode >> 8) & 1;
        let rlist = opcode & 0xFF;

        let mut register_mask = rlist;

        // Add PC (for POP) or LR (for PUSH) if R bit is set
        if r_bit != 0 {
            if l_bit != 0 {
                register_mask |= 1 << 15; // PC
            } else {
                register_mask |= 1 << 14; // LR
            }
        }

        let registers = RegisterList::from_mask(register_mask);

        // PUSH is STMDB (decrement before), POP is LDMIA (increment after)
        if l_bit != 0 {
            Instruction::Ldm {
                condition,
                rn: Register::SP,
                registers,
                addressing_mode: AddressingModeType::IA,
                writeback: true,
                user_mode: false,
            }
        } else {
            Instruction::Stm {
                condition,
                rn: Register::SP,
                registers,
                addressing_mode: AddressingModeType::DB,
                writeback: true,
                user_mode: false,
            }
        }
    }

    /// Format 15: Multiple load/store
    fn decode_thumb_load_store_multiple(opcode: u16, condition: Condition) -> Instruction {
        let l_bit = (opcode >> 11) & 1;
        let rb = (opcode >> 8) & 0x7;
        let rlist = opcode & 0xFF;

        let registers = RegisterList::from_mask(rlist);

        if l_bit != 0 {
            Instruction::Ldm {
                condition,
                rn: Register::from_u32(rb as u32),
                registers,
                addressing_mode: AddressingModeType::IA,
                writeback: true,
                user_mode: false,
            }
        } else {
            Instruction::Stm {
                condition,
                rn: Register::from_u32(rb as u32),
                registers,
                addressing_mode: AddressingModeType::IA,
                writeback: true,
                user_mode: false,
            }
        }
    }

    /// Format 16: Conditional branch
    fn decode_thumb_conditional_branch(opcode: u16, pc: u32, _condition: Condition) -> Instruction {
        let cond = (opcode >> 8) & 0xF;
        let offset8 = opcode & 0xFF;

        // Sign extend 8-bit offset to 32-bit
        let offset = if (offset8 & 0x80) != 0 {
            (offset8 as i32) | 0xFFFFFF00u32 as i32
        } else {
            offset8 as i32
        };

        let target = (pc as i32 + 4 + (offset << 1)) as u32;

        Instruction::B {
            condition: Condition::from_bits(cond as u32),
            target,
        }
    }

    /// Format 17: Software Interrupt
    fn decode_thumb_swi(opcode: u16, condition: Condition) -> Instruction {
        let comment = (opcode & 0xFF) as u32;
        Instruction::Swi { condition, comment }
    }

    /// Format 18: Unconditional branch
    fn decode_thumb_unconditional_branch(
        opcode: u16,
        pc: u32,
        condition: Condition,
    ) -> Instruction {
        let offset11 = opcode & 0x7FF;

        // Sign extend 11-bit offset to 32-bit
        let offset = if (offset11 & 0x400) != 0 {
            (offset11 as i32) | 0xFFFFF800u32 as i32
        } else {
            offset11 as i32
        };

        let target = (pc as i32 + 4 + (offset << 1)) as u32;

        Instruction::B { condition, target }
    }

    /// Format 19: Long branch with link
    /// Thumb BL is a 32-bit instruction split into two 16-bit parts:
    /// First half (H=0): Sets up high bits in LR
    /// Second half (H=1): Completes branch using LR + low bits
    fn decode_thumb_long_branch_link<F>(
        opcode: u16,
        pc: u32,
        condition: Condition,
        read_fn: &mut F,
    ) -> Instruction
    where
        F: FnMut(u32) -> u16,
    {
        let bits_15_11 = (opcode >> 11) & 0b11111;
        let offset11 = opcode & 0x7FF;

        if bits_15_11 == 0b11110 {
            // First half (H=0): Read ahead to get the second half and construct the full BL instruction
            let next_opcode = read_fn(pc.wrapping_add(2));
            
            // Check if next instruction is BL second half (bits [15:11] should be 0b11111)
            let next_bits_15_11 = (next_opcode >> 11) & 0b11111;
            if next_bits_15_11 == 0b11111 {
                // Extract offsets from both halves
                let offset_high = offset11;
                let offset_low = next_opcode & 0x7FF;
                
                // Sign extend the 11-bit high value to 23 bits (shifted left by 12)
                let sign_extended_high = if (offset_high & 0x400) != 0 {
                    (offset_high as i32 | 0xFFFFF800u32 as i32) << 12
                } else {
                    (offset_high as i32) << 12
                };
                
                // Combine: offset = sign_extend(offset_high:offset_low:0)
                // Note: offset_low is shifted by 1, not combined with sign_extended_high
                let offset = sign_extended_high + ((offset_low as i32) << 1);
                
                // Target address calculation:
                // In Thumb mode, PC for BL is ((address of first instruction & ~2) + 4)
                // However, instructions are always aligned in Thumb, so just add 4
                // But the actual formula from ARM docs: target = PC + offset where PC = addr_of_BL + 4
                // and the offset calculation makes the final address word-aligned (bit 1 is always 0 from the << 1)
                let base_pc = pc + 4;
                let target = (base_pc as i32 + offset) as u32;
                
                Instruction::Bl { condition, target }
            } else {
                // Invalid: first half without second half
                Instruction::Unknown {
                    condition,
                    opcode: opcode as u32,
                }
            }
        } else if bits_15_11 == 0b11111 {
            // Second half (H=1): This should have been handled when we decoded the first half
            // If we're here, it means we're decoding the second half in isolation
            // Try to look back to see if there's a first half
            let prev_opcode = read_fn(pc.wrapping_sub(2));
            
            // Check if previous instruction was BL first half (bits [15:11] should be 0b11110)
            let prev_bits_15_11 = (prev_opcode >> 11) & 0b11111;
            if prev_bits_15_11 == 0b11110 {
                // Extract offsets from both halves
                let offset_high = prev_opcode & 0x7FF;
                let offset_low = offset11;
                
                // Sign extend the 11-bit high value to 23 bits
                let sign_extended_high = if (offset_high & 0x400) != 0 {
                    (offset_high as i32 | 0xFFFFF800u32 as i32) << 12
                } else {
                    (offset_high as i32) << 12
                };
                
                // Combine: offset = sign_extend(offset_high:offset_low:0)
                let offset = sign_extended_high + ((offset_low as i32) << 1);
                
                // Target address is calculated from the first half's address
                // PC for the first instruction is (current PC - 2), then add 4 for PC offset
                let target = ((pc - 2 + 4) as i32 + offset) as u32;
                
                Instruction::Bl { condition, target }
            } else {
                // Invalid: second half without first half
                Instruction::Unknown {
                    condition,
                    opcode: opcode as u32,
                }
            }
        } else {
            // Invalid: not a BL instruction (bits [15:11] should be 11110 or 11111)
            Instruction::Unknown {
                condition,
                opcode: opcode as u32,
            }
        }
    }
}
