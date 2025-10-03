use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone)]
pub struct Memory<const START: u32, const SIZE: usize> {
    #[serde(with = "serde_bytes")]
    data: [u8; SIZE],
}

impl<const START: u32, const SIZE: usize> Default for Memory<START, SIZE> {
    fn default() -> Self {
        Self::new()
    }
}

impl<const START: u32, const SIZE: usize> Memory<START, SIZE> {
    pub fn new() -> Self {
        Memory { data: [0; SIZE] }
    }

    fn normalize_addr(addr: u32) -> u32 {
        addr - START
    }

    pub fn as_slice(&self) -> &[u8] {
        &self.data
    }
}

pub trait Addressable {
    fn read(&self, addr: u32) -> u8;
    fn write(&mut self, addr: u32, value: u8);

    /// Calculate memory timing for a given address and access type
    /// Returns (wait_states, cycle_type)
    fn get_memory_timing(
        &self,
        addr: u32,
        is_sequential: bool,
    ) -> (u32, crate::cpu::arm7tdmi::CycleType) {
        use crate::cpu::arm7tdmi::WaitStateConfig;

        let config = WaitStateConfig::default(); // Could be passed as parameter in the future
        let timing = crate::memory::timing::get_memory_timing(addr, &config, is_sequential);

        (timing.wait_states, timing.cycle_type)
    }
}

impl dyn Addressable {
    pub fn read32(&self, addr: u32) -> u32 {
        let b0 = self.read(addr) as u32;
        let b1 = self.read(addr + 1) as u32;
        let b2 = self.read(addr + 2) as u32;
        let b3 = self.read(addr + 3) as u32;
        (b3 << 24) | (b2 << 16) | (b1 << 8) | b0
    }

    pub fn read16(&self, addr: u32) -> u16 {
        let b0 = self.read(addr) as u16;
        let b1 = self.read(addr + 1) as u16;
        (b1 << 8) | b0
    }

    pub fn write32(&mut self, addr: u32, value: u32) {
        self.write(addr, (value & 0xFF) as u8);
        self.write(addr + 1, ((value >> 8) & 0xFF) as u8);
        self.write(addr + 2, ((value >> 16) & 0xFF) as u8);
        self.write(addr + 3, ((value >> 24) & 0xFF) as u8);
    }

    pub fn write16(&mut self, addr: u32, value: u16) {
        self.write(addr, (value & 0xFF) as u8);
        self.write(addr + 1, ((value >> 8) & 0xFF) as u8);
    }

    /// Read 32-bit value with timing information
    pub fn read32_timed(
        &self,
        addr: u32,
        is_sequential: bool,
    ) -> (u32, u32, crate::cpu::arm7tdmi::CycleType) {
        let value = self.read32(addr);
        let (wait_states, cycle_type) = self.get_memory_timing(addr, is_sequential);
        (value, wait_states, cycle_type)
    }

    /// Read 16-bit value with timing information
    pub fn read16_timed(
        &self,
        addr: u32,
        is_sequential: bool,
    ) -> (u16, u32, crate::cpu::arm7tdmi::CycleType) {
        let value = self.read16(addr);
        let (wait_states, cycle_type) = self.get_memory_timing(addr, is_sequential);
        (value, wait_states, cycle_type)
    }

    /// Read 8-bit value with timing information
    pub fn read8_timed(
        &self,
        addr: u32,
        is_sequential: bool,
    ) -> (u8, u32, crate::cpu::arm7tdmi::CycleType) {
        let value = self.read(addr);
        let (wait_states, cycle_type) = self.get_memory_timing(addr, is_sequential);
        (value, wait_states, cycle_type)
    }
}

impl<const START: u32, const SIZE: usize> Addressable for Memory<START, SIZE> {
    fn read(&self, addr: u32) -> u8 {
        let offset = Self::normalize_addr(addr);
        self.data[offset as usize]
    }

    fn write(&mut self, addr: u32, value: u8) {
        let offset = Self::normalize_addr(addr);
        self.data[offset as usize] = value;
    }
}
