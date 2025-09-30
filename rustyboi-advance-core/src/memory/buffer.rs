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
}

impl dyn Addressable {
    pub fn read32(&self, addr: u32) -> u32 {
        let b0 = self.read(addr) as u32;
        let b1 = self.read(addr + 1) as u32;
        let b2 = self.read(addr + 2) as u32;
        let b3 = self.read(addr + 3) as u32;
        (b3 << 24) | (b2 << 16) | (b1 << 8) | b0
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
