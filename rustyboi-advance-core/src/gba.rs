use crate::cartridge;
use crate::cpu;
use crate::memory;
use crate::memory::Addressable;
use crate::ppu;

use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::fs;
use std::io;

#[derive(Serialize, Deserialize)]
pub struct GBA {
    cpu: cpu::ARM7TDMI,
    mmio: memory::mmio::Mmio,
    ppu: ppu::Ppu,
    #[serde(skip, default)]
    skip_bios: bool,
    #[serde(skip, default)]
    breakpoints: HashSet<u32>,
}

impl GBA {
    pub fn new() -> Self {
        GBA {
            cpu: cpu::ARM7TDMI::new(),
            mmio: memory::mmio::Mmio::new(),
            ppu: ppu::Ppu::new(),
            skip_bios: false,
            breakpoints: HashSet::new(),
        }
    }

    pub fn skip_bios(&mut self) {
        self.skip_bios = true;
    }

    pub fn insert(&mut self, cartridge: cartridge::Cartridge) {
        self.mmio.insert_cartridge(cartridge);
    }

    pub fn load_bios(&mut self, path: &str) -> Result<(), std::io::Error> {
        self.mmio.load_bios(path)?;
        Ok(())
    }

    /// Check if a ROM cartridge is loaded
    pub fn has_rom(&self) -> bool {
        self.mmio.get_cartridge().is_some()
    }

    /// Check if a BIOS is loaded
    pub fn has_bios(&self) -> bool {
        self.mmio.has_bios()
    }

    pub fn step_instruction(&mut self) -> (bool, u32) {
        // Check for breakpoint at current PC before executing
        let pc = self.cpu.registers.pc;
        if self.breakpoints.contains(&pc) {
            // Breakpoint hit - don't execute instruction and return (empty audio, breakpoint hit)
            return (true, 0);
        }

        // Execute one CPU instruction and step PPU accordingly
        let cycles = self.cpu.step(&mut self.mmio);

        for _ in 0..cycles {
            self.ppu.step(&mut self.mmio);
        }

        (false, cycles) // No breakpoint hit
    }

    pub fn run_until_frame(&mut self) -> ([u8; ppu::FRAMEBUFFER_SIZE], bool) {
        let mut cpu_cycles_this_frame = 0u32;
        // Normal GBA frame should be 70224 cycles (154 scanlines × 456 cycles)
        // If we exceed this, we assume PPU is disabled or stuck
        // and return to avoid audio buildup
        const MAX_CYCLES_PER_FRAME: u32 = 70224;

        loop {
            let (breakpoint_hit, cycles) = self.step_instruction();
            cpu_cycles_this_frame += cycles;

            if breakpoint_hit {
                // Breakpoint hit - return current frame and indicate breakpoint hit
                return (self.ppu.get_frame(), true);
            }

            // Check if PPU has completed a frame
            if self.ppu.has_frame() {
                return (self.ppu.get_frame(), false);
            }

            // If PPU is disabled or taking too long, cap the cycles to prevent audio buildup
            if cpu_cycles_this_frame >= MAX_CYCLES_PER_FRAME {
                // PPU disabled or stuck - return after reasonable cycle count to maintain timing
                return (self.ppu.get_frame(), false);
            }
        }
    }

    pub fn get_current_frame(&mut self) -> [u8; ppu::FRAMEBUFFER_SIZE] {
        self.ppu.get_frame()
    }

    pub fn get_cpu_registers(&self) -> &cpu::registers::Registers {
        &self.cpu.registers
    }

    pub fn read_memory(&self, address: u32) -> u8 {
        self.mmio.read(address)
    }

    pub fn get_ppu(&self) -> &ppu::Ppu {
        &self.ppu
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub fn from_state_file(path: &str) -> Result<Self, io::Error> {
        let saved_state = fs::read_to_string(path)?;
        let gb = serde_json::from_str(&saved_state)?;
        Ok(gb)
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub fn to_state_file(&self, path: &str) -> Result<(), io::Error> {
        let serialized = serde_json::to_string(&self)?;
        fs::write(path, serialized)?;
        Ok(())
    }

    pub fn reset(&mut self) {
        self.mmio.reset();
        self.ppu = ppu::Ppu::new();
        self.cpu.halted = false;
        self.cpu.stopped = false;
        if self.skip_bios {
            self.skip_bios();
        } else {
            self.cpu.registers = cpu::registers::Registers::new();
        }
    }

    // Breakpoint management methods
    pub fn add_breakpoint(&mut self, address: u32) {
        self.breakpoints.insert(address);
    }

    pub fn remove_breakpoint(&mut self, address: u32) {
        self.breakpoints.remove(&address);
    }

    pub fn get_breakpoints(&self) -> &HashSet<u32> {
        &self.breakpoints
    }
}
