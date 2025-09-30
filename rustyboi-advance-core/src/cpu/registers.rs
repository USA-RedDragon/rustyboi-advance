use serde::{Deserialize, Serialize};

/// A smart accessor for per-mode registers that acts like a u32
/// but automatically uses the current CPU mode
#[derive(Debug, Clone, Copy)]
pub struct CurrentModeRegisterAccess {
    value: u32,
}

impl CurrentModeRegisterAccess {
    fn new(register: &PerModeRegister, cpsr: u32) -> Self {
        Self {
            value: register.read_current(cpsr),
        }
    }

    pub fn get(self) -> u32 {
        self.value
    }

    pub fn saturating_sub(self, rhs: u32) -> u32 {
        self.value.saturating_sub(rhs)
    }

    pub fn saturating_add(self, rhs: u32) -> u32 {
        self.value.saturating_add(rhs)
    }
}

// Make CurrentModeRegisterAccess work like a u32 in most contexts
impl std::fmt::Display for CurrentModeRegisterAccess {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.value, f)
    }
}

impl std::fmt::UpperHex for CurrentModeRegisterAccess {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::UpperHex::fmt(&self.value, f)
    }
}

impl PartialEq<u32> for CurrentModeRegisterAccess {
    fn eq(&self, other: &u32) -> bool {
        self.value == *other
    }
}

impl PartialEq<CurrentModeRegisterAccess> for u32 {
    fn eq(&self, other: &CurrentModeRegisterAccess) -> bool {
        *self == other.value
    }
}

impl PartialOrd<u32> for CurrentModeRegisterAccess {
    fn partial_cmp(&self, other: &u32) -> Option<std::cmp::Ordering> {
        self.value.partial_cmp(other)
    }
}

impl PartialOrd<CurrentModeRegisterAccess> for u32 {
    fn partial_cmp(&self, other: &CurrentModeRegisterAccess) -> Option<std::cmp::Ordering> {
        self.partial_cmp(&other.value)
    }
}

impl From<CurrentModeRegisterAccess> for u32 {
    fn from(val: CurrentModeRegisterAccess) -> Self {
        val.value
    }
}

#[derive(Serialize, Deserialize, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    User = 0b10000,
    Fiq = 0b10001,
    Irq = 0b10010,
    Service = 0b10011,
    Abort = 0b10111,
    Undefined = 0b11011,
    System = 0b11111,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ModeValue {
    User = 1 << 4,
    Fiq = (1 << 4) | 1,
    Irq = (1 << 4) | (1 << 1),
    Service = (1 << 4) | (1 << 1) | 1,
    Abort = (1 << 4) | (1 << 2) | (1 << 1) | 1,
    Undefined = (1 << 4) | (1 << 3) | (1 << 1) | 1,
    System = (1 << 4) | (1 << 3) | (1 << 2) | (1 << 1) | 1,
}

impl From<u32> for Mode {
    fn from(cpsr_mode_bits: u32) -> Self {
        match ModeValue::from(cpsr_mode_bits & 0x1F) {
            ModeValue::User => Mode::User,
            ModeValue::Fiq => Mode::Fiq,
            ModeValue::Irq => Mode::Irq,
            ModeValue::Service => Mode::Service,
            ModeValue::Abort => Mode::Abort,
            ModeValue::Undefined => Mode::Undefined,
            ModeValue::System => Mode::System,
        }
    }
}

impl From<u32> for ModeValue {
    fn from(value: u32) -> Self {
        match value {
            v if v == (1 << 4) => ModeValue::User,
            v if v == ((1 << 4) | 1) => ModeValue::Fiq,
            v if v == ((1 << 4) | (1 << 1)) => ModeValue::Irq,
            v if v == ((1 << 4) | (1 << 1) | 1) => ModeValue::Service,
            v if v == ((1 << 4) | (1 << 2) | (1 << 1) | 1) => ModeValue::Abort,
            v if v == ((1 << 4) | (1 << 3) | (1 << 1) | 1) => ModeValue::Undefined,
            v if v == ((1 << 4) | (1 << 3) | (1 << 2) | (1 << 1) | 1) => ModeValue::System,
            _ => ModeValue::User, // Default to User mode for invalid values
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub struct PerModeRegister {
    pub fiq: u32,
    pub irq: u32,
    pub svc: u32,
    pub und: u32,
    pub abt: u32,
}

impl PerModeRegister {
    pub fn read(&self, mode: Mode) -> u32 {
        match mode {
            Mode::User | Mode::System => self.fiq, // User and System share the same registers
            Mode::Fiq => self.fiq,
            Mode::Irq => self.irq,
            Mode::Service => self.svc,
            Mode::Undefined => self.und,
            Mode::Abort => self.abt,
        }
    }

    pub fn write(&mut self, mode: Mode, value: u32) {
        match mode {
            Mode::User | Mode::System => self.fiq = value, // User and System share the same registers
            Mode::Fiq => self.fiq = value,
            Mode::Irq => self.irq = value,
            Mode::Service => self.svc = value,
            Mode::Undefined => self.und = value,
            Mode::Abort => self.abt = value,
        }
    }

    /// Get the current mode from CPSR
    fn get_current_mode(cpsr: u32) -> Mode {
        Mode::from(cpsr)
    }

    /// Read the register value for the current mode based on CPSR
    pub fn read_current(&self, cpsr: u32) -> u32 {
        let mode = Self::get_current_mode(cpsr);
        self.read(mode)
    }

    /// Write the register value for the current mode based on CPSR
    pub fn write_current(&mut self, cpsr: u32, value: u32) {
        let mode = Self::get_current_mode(cpsr);
        self.write(mode, value);
    }

    /// Saturating subtraction for the current mode
    pub fn saturating_sub_current(&self, cpsr: u32, rhs: u32) -> u32 {
        self.read_current(cpsr).saturating_sub(rhs)
    }

    /// Saturating addition for the current mode
    pub fn saturating_add_current(&self, cpsr: u32, rhs: u32) -> u32 {
        self.read_current(cpsr).saturating_add(rhs)
    }

    /// Check equality with a u32 for the current mode
    pub fn eq_current(&self, cpsr: u32, other: u32) -> bool {
        self.read_current(cpsr) == other
    }

    /// Compare with a u32 for the current mode
    pub fn cmp_current(&self, cpsr: u32, other: u32) -> std::cmp::Ordering {
        self.read_current(cpsr).cmp(&other)
    }
}

#[derive(Serialize, Deserialize, Clone, Copy)]
pub enum Flag {
    Negative = 0b1000_0000,
    Zero = 0b0100_0000,
    Carry = 0b0010_0000,
    Overflow = 0b0001_0000,
    IrqDisable = 0b0000_0100,
    FiqDisable = 0b0000_0010,
    ThumbState = 0b0000_0001,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Registers {
    pub r0: u32,
    pub r1: u32,
    pub r2: u32,
    pub r3: u32,
    pub r4: u32,
    pub r5: u32,
    pub r6: u32,
    pub r7: u32,
    pub r8: u32,
    pub r8_fiq: u32,
    pub r9: u32,
    pub r9_fiq: u32,
    pub r10: u32,
    pub r10_fiq: u32,
    pub r11: u32,
    pub r11_fiq: u32,
    pub r12: u32,
    pub r12_fiq: u32,
    pub sp: PerModeRegister, // Stack Pointer
    pub lr: PerModeRegister, // Link Register
    pub pc: u32,
    pub cpsr: u32,             // Current Program Status Register
    pub spsr: PerModeRegister, // Saved Program Status Register
}

impl Registers {
    pub fn new() -> Self {
        Registers {
            r0: 0,
            r1: 0,
            r2: 0,
            r3: 0,
            r4: 0,
            r5: 0,
            r6: 0,
            r7: 0,
            r8: 0,
            r8_fiq: 0,
            r9: 0,
            r9_fiq: 0,
            r10: 0,
            r10_fiq: 0,
            r11: 0,
            r11_fiq: 0,
            r12: 0,
            r12_fiq: 0,
            sp: PerModeRegister {
                fiq: 0,
                irq: 0,
                svc: 0,
                und: 0,
                abt: 0,
            },
            lr: PerModeRegister {
                fiq: 0,
                irq: 0,
                svc: 0,
                und: 0,
                abt: 0,
            },
            pc: 0,
            cpsr: 0x10, // Start in User mode
            spsr: PerModeRegister {
                fiq: 0,
                irq: 0,
                svc: 0,
                und: 0,
                abt: 0,
            },
        }
    }

    pub fn set_flag(&mut self, flag: Flag, value: bool) {
        if value {
            self.cpsr |= flag as u32;
        } else {
            self.cpsr &= !(flag as u32);
        }
    }

    pub fn get_flag(&self, flag: Flag) -> bool {
        (self.cpsr & (flag as u32)) != 0
    }

    /// Get the current CPU mode
    pub fn get_current_mode(&self) -> Mode {
        PerModeRegister::get_current_mode(self.cpsr)
    }

    /// Get the current stack pointer value
    pub fn get_sp(&self) -> u32 {
        self.sp.read_current(self.cpsr)
    }

    /// Set the current stack pointer value
    pub fn set_sp(&mut self, value: u32) {
        self.sp.write_current(self.cpsr, value);
    }

    /// Get the current link register value
    pub fn get_lr(&self) -> u32 {
        self.lr.read_current(self.cpsr)
    }

    /// Set the current link register value
    pub fn set_lr(&mut self, value: u32) {
        self.lr.write_current(self.cpsr, value);
    }

    /// Get the current SPSR value
    pub fn get_spsr(&self) -> u32 {
        self.spsr.read_current(self.cpsr)
    }

    /// Set the current SPSR value
    pub fn set_spsr(&mut self, value: u32) {
        self.spsr.write_current(self.cpsr, value);
    }

    /// Get a smart accessor for SP that acts like a u32
    pub fn sp(&self) -> CurrentModeRegisterAccess {
        CurrentModeRegisterAccess::new(&self.sp, self.cpsr)
    }

    /// Get a smart accessor for LR that acts like a u32
    pub fn lr(&self) -> CurrentModeRegisterAccess {
        CurrentModeRegisterAccess::new(&self.lr, self.cpsr)
    }

    /// Get a smart accessor for SPSR that acts like a u32
    pub fn spsr(&self) -> CurrentModeRegisterAccess {
        CurrentModeRegisterAccess::new(&self.spsr, self.cpsr)
    }
}
