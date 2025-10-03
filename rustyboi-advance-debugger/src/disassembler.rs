use std::u8;

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
    /// Safe version with bounds checking for debug builds
    pub fn from_u32(reg: u32) -> Register {
        debug_assert!(reg <= 15, "Invalid register number: {}", reg);
        // Safety: ARM registers are always in range 0-15, and we validate in debug builds
        unsafe { Register::from_u32_unchecked(reg) }
    }

    /// Unsafe version without bounds checking for performance-critical hotpath
    #[inline]
    pub unsafe fn from_u32_unchecked(reg: u32) -> Register {
        unsafe { std::mem::transmute(reg as u8) }
    }

    /// Fallback safe version (kept for compatibility)
    pub fn from_u32_safe(reg: u32) -> Register {
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

    pub fn to_string(&self) -> String {
        format!("{}", self)
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
    Immediate(u32),
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

    pub fn to_string(&self) -> String {
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
        fields
    }
}

impl std::fmt::Display for MsrFields {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
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
            Operand2::Immediate(imm) => write!(f, "#{:#X}", imm),
            Operand2::Register(reg) => write!(f, "{}", reg),
            Operand2::RegisterShifted {
                reg,
                shift_type,
                shift_amount,
            } => {
                if *shift_amount == 0 && *shift_type == ShiftType::Lsl {
                    write!(f, "{}", reg)
                } else {
                    write!(f, "{}, {} #{}", reg, shift_type, shift_amount)
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
                    let abs_offset = offset.abs() as u32;
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
                            "[{}, {}{}, {} #{}]{}",
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
                    let abs_offset = offset.abs() as u32;
                    format!("[{}], #{}{:#X}", base, sign, abs_offset)
                }
            }
            AddressingMode::PreIndexed { offset } => {
                if *offset == 0 {
                    format!("[{}]!", base)
                } else {
                    let sign = if *offset >= 0 { "" } else { "-" };
                    let abs_offset = offset.abs() as u32;
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
            | DataProcessingOp::Cmn => None,
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
    /// Returns the mnemonic string for the instruction at the given address
    pub fn disassemble_with_reader<F>(addr: u32, read_fn: F) -> String
    where
        F: FnMut(u32) -> u32,
    {
        let instruction = Self::decode_with_reader(addr, read_fn);
        instruction.to_string()
    }

    /// Returns the Instruction struct for the instruction at the given address
    pub fn decode_with_reader<F>(addr: u32, mut read_fn: F) -> Instruction
    where
        F: FnMut(u32) -> u32,
    {
        let opcode = read_fn(addr);
        Self::decode_opcode(opcode, addr, |offset| read_fn(addr + offset))
    }

    /// Returns the mnemonic string for the given opcode (backward compatibility)
    pub fn disassemble_opcode<F>(opcode: u32, pc: u32, read_fn: F) -> String
    where
        F: FnMut(u32) -> u32,
    {
        let instruction = Self::decode_opcode(opcode, pc, read_fn);
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

        // Check for undefined instruction space
        if (opcode & 0x0E000000) == 0x02000000 && (opcode & 0x01200000) == 0x00200000 {
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
        let u_bit = (opcode >> 22) & 1;
        let a_bit = (opcode >> 21) & 1;
        let rdlo_reg = Register::from_u32(rdlo);
        let rdhi_reg = Register::from_u32(rdhi);
        let rm_reg = Register::from_u32(rm);
        let rs_reg = Register::from_u32(rs);

        match (u_bit, a_bit) {
            (0, 0) => Instruction::Smull {
                condition,
                rdlo: rdlo_reg,
                rdhi: rdhi_reg,
                rm: rm_reg,
                rs: rs_reg,
                set_flags,
            },
            (0, 1) => Instruction::Smlal {
                condition,
                rdlo: rdlo_reg,
                rdhi: rdhi_reg,
                rm: rm_reg,
                rs: rs_reg,
                set_flags,
            },
            (1, 0) => Instruction::Umull {
                condition,
                rdlo: rdlo_reg,
                rdhi: rdhi_reg,
                rm: rm_reg,
                rs: rs_reg,
                set_flags,
            },
            (1, 1) => Instruction::Umlal {
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
                let offset = if u_bit != 0 { 0 } else { 0 }; // Placeholder - actual implementation would need register resolution
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
        if matches!(op, DataProcessingOp::Mov) && rn == 15 && (opcode & (1 << 25)) != 0 {
            if let Some(abs_addr) = Self::try_decode_adr(opcode, pc, &mut read_fn) {
                return Instruction::Adr {
                    condition,
                    rd: Register::from_u32(rd),
                    target: abs_addr,
                };
            }
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
        {
            if let Some(abs_addr) = Self::try_decode_adr_immediate(opcode, pc) {
                return Instruction::Adr {
                    condition,
                    rd: Register::from_u32(rd),
                    target: abs_addr,
                };
            }
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
            let offset = (opcode & 0xFFF) as u32;
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
            Operand2::Immediate(rotated_imm)
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
                if imm == 0 && shift_type == ShiftType::Lsl {
                    Operand2::Register(Register::from_u32(rm))
                } else {
                    Operand2::RegisterShifted {
                        reg: Register::from_u32(rm),
                        shift_type,
                        shift_amount: imm,
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

        if rn == 15 && (opcode_type == 0x4 || opcode_type == 0x2) {
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
}
