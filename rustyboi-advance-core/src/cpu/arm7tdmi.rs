use crate::{cpu::registers, memory, memory::Addressable};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone)]
pub struct ARM7TDMI {
    pub registers: registers::Registers,
    pub halted: bool,
    pub stopped: bool,
}

impl ARM7TDMI {
    pub fn new() -> Self {
        ARM7TDMI {
            registers: registers::Registers::new(),
            halted: false,
            stopped: false,
        }
    }

    pub fn step(&mut self, mmio: &mut memory::mmio::Mmio) -> u32 {
        let mut cycles = 0;

        // If halted, check if we should exit halt state
        if self.halted {
            // CPU is halted and no interrupt is pending, consume 1 cycle and return
            return 1;
        }

        let opcode = (mmio as &dyn Addressable).read32(self.registers.pc);
        self.registers.pc += 4;
        self.execute(opcode, mmio) + cycles
    }

    fn execute(&mut self, opcode: u32, mmio: &mut memory::mmio::Mmio) -> u32 {
        match opcode {
            _ => {
                unimplemented!("Unimplemented ARM opcode: 0x{:08X}", opcode);
                1 // Default to 1 cycle for unimplemented opcodes
            }
        }
    }
}
