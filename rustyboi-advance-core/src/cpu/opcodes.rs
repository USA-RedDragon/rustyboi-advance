use crate::{cpu, cpu::registers, memory, memory::Addressable};

pub fn nop(_cpu: &mut cpu::ARM7TDMI, _mmio: &mut memory::mmio::Mmio) -> u32 {
    4
}
