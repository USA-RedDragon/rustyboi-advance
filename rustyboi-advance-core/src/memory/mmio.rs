use crate::cartridge;
use crate::memory;
use crate::memory::Addressable;
use serde::{Deserialize, Serialize};

use std::fs;
use std::io;

pub const EMPTY_BYTE: u8 = 0xFF;

pub const BIOS_START: u32 = 0x00000000;
pub const BIOS_END: u32 = 0x00003FFF;
pub const BIOS_SIZE: usize = (BIOS_END - BIOS_START + 1) as usize;

pub const WRAM_BOARD_START: u32 = 0x02000000;
pub const WRAM_BOARD_END: u32 = 0x0203FFFF;
pub const WRAM_BOARD_SIZE: usize = (WRAM_BOARD_END - WRAM_BOARD_START + 1) as usize;

pub const WRAM_CHIP_START: u32 = 0x03000000;
pub const WRAM_CHIP_END: u32 = 0x03007FFF;
pub const WRAM_CHIP_SIZE: usize = (WRAM_CHIP_END - WRAM_CHIP_START + 1) as usize;

pub const IO_REGISTERS_START: u32 = 0x04000000;
pub const IO_REGISTERS_END: u32 = 0x040003FE;
pub const IO_REGISTERS_SIZE: usize = (IO_REGISTERS_END - IO_REGISTERS_START + 1) as usize;

pub const PALETTE_RAM_START: u32 = 0x05000000;
pub const PALETTE_RAM_END: u32 = 0x050003FF;
pub const PALETTE_RAM_SIZE: usize = (PALETTE_RAM_END - PALETTE_RAM_START + 1) as usize;

pub const VRAM_START: u32 = 0x06000000;
pub const VRAM_END: u32 = 0x06017FFF;
pub const VRAM_SIZE: usize = (VRAM_END - VRAM_START + 1) as usize;

pub const OAM_START: u32 = 0x07000000;
pub const OAM_END: u32 = 0x070003FF;
pub const OAM_SIZE: usize = (OAM_END - OAM_START + 1) as usize;

pub const GAME_PAK_ROM_WAIT_STATE_0_START: u32 = 0x08000000;
pub const GAME_PAK_ROM_WAIT_STATE_0_END: u32 = 0x09FFFFFF;
pub const GAME_PAK_ROM_WAIT_STATE_0_SIZE: usize =
    (GAME_PAK_ROM_WAIT_STATE_0_END - GAME_PAK_ROM_WAIT_STATE_0_START + 1) as usize;
pub const GAME_PAK_ROM_WAIT_STATE_1_START: u32 = 0x0A000000;
pub const GAME_PAK_ROM_WAIT_STATE_1_END: u32 = 0x0BFFFFFF;
pub const GAME_PAK_ROM_WAIT_STATE_1_SIZE: usize =
    (GAME_PAK_ROM_WAIT_STATE_1_END - GAME_PAK_ROM_WAIT_STATE_1_START + 1) as usize;
pub const GAME_PAK_ROM_WAIT_STATE_2_START: u32 = 0x0C000000;
pub const GAME_PAK_ROM_WAIT_STATE_2_END: u32 = 0x0DFFFFFF;
pub const GAME_PAK_ROM_WAIT_STATE_2_SIZE: usize =
    (GAME_PAK_ROM_WAIT_STATE_2_END - GAME_PAK_ROM_WAIT_STATE_2_START + 1) as usize;

pub const GAME_PAK_RAM_START: u32 = 0x0E000000;
pub const GAME_PAK_RAM_END: u32 = 0x0E00FFFF;
pub const GAME_PAK_RAM_SIZE: usize = (GAME_PAK_RAM_END - GAME_PAK_RAM_START + 1) as usize;

#[derive(Serialize, Deserialize, Clone)]
pub struct Mmio {
    #[serde(skip, default)]
    bios: Option<memory::Memory<BIOS_START, BIOS_SIZE>>,
    wram_board: memory::Memory<WRAM_BOARD_START, WRAM_BOARD_SIZE>,
    wram_chip: memory::Memory<WRAM_CHIP_START, WRAM_CHIP_SIZE>,
    io_registers: memory::Memory<IO_REGISTERS_START, IO_REGISTERS_SIZE>,
    palette_ram: memory::Memory<PALETTE_RAM_START, PALETTE_RAM_SIZE>,
    vram: memory::Memory<VRAM_START, VRAM_SIZE>,
    oam: memory::Memory<OAM_START, OAM_SIZE>,
    #[serde(skip, default)]
    cartridge: Option<cartridge::Cartridge>,
}

impl Mmio {
    pub fn new() -> Self {
        Mmio {
            bios: None,
            cartridge: None,
            wram_board: memory::Memory::new(),
            wram_chip: memory::Memory::new(),
            io_registers: memory::Memory::new(),
            palette_ram: memory::Memory::new(),
            vram: memory::Memory::new(),
            oam: memory::Memory::new(),
        }
    }

    pub fn reset(&mut self) {
        let mut new = Self::new();
        self.bios.clone_into(&mut new.bios);
        self.cartridge.clone_into(&mut new.cartridge);
        *self = new;
    }

    pub fn insert_cartridge(&mut self, cartridge: cartridge::Cartridge) {
        self.cartridge = Some(cartridge);
    }

    pub fn get_cartridge(&self) -> Option<&cartridge::Cartridge> {
        self.cartridge.as_ref()
    }

    pub fn load_bios(&mut self, path: &str) -> Result<(), io::Error> {
        let data = fs::read(path)?;
        let mut bios = memory::Memory::new();
        if data.len() < BIOS_SIZE {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "BIOS file too small",
            ));
        }
        if data.len() > BIOS_SIZE {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "BIOS file too large",
            ));
        }
        for (i, &byte) in data.iter().take(BIOS_SIZE).enumerate() {
            bios.write(BIOS_START + i as u32, byte);
        }
        self.bios = Some(bios);
        Ok(())
    }

    pub fn has_bios(&self) -> bool {
        self.bios.is_some()
    }
}

impl memory::Addressable for Mmio {
    fn read(&self, addr: u32) -> u8 {
        match addr {
            BIOS_START..=BIOS_END => {
                if let Some(bios) = &self.bios {
                    bios.read(addr)
                } else {
                    EMPTY_BYTE
                }
            }
            WRAM_BOARD_START..=WRAM_BOARD_END => self.wram_board.read(addr),
            WRAM_CHIP_START..=WRAM_CHIP_END => self.wram_chip.read(addr),
            IO_REGISTERS_START..=IO_REGISTERS_END => self.io_registers.read(addr),
            PALETTE_RAM_START..=PALETTE_RAM_END => self.palette_ram.read(addr),
            VRAM_START..=VRAM_END => self.vram.read(addr),
            OAM_START..=OAM_END => self.oam.read(addr),
            GAME_PAK_ROM_WAIT_STATE_0_START..=GAME_PAK_ROM_WAIT_STATE_0_END => {
                if let Some(cart) = &self.cartridge {
                    cart.read(addr)
                } else {
                    EMPTY_BYTE
                }
            }
            GAME_PAK_ROM_WAIT_STATE_1_START..=GAME_PAK_ROM_WAIT_STATE_1_END => {
                if let Some(cart) = &self.cartridge {
                    cart.read(addr)
                } else {
                    EMPTY_BYTE
                }
            }
            GAME_PAK_ROM_WAIT_STATE_2_START..=GAME_PAK_ROM_WAIT_STATE_2_END => {
                if let Some(cart) = &self.cartridge {
                    cart.read(addr)
                } else {
                    EMPTY_BYTE
                }
            }
            GAME_PAK_RAM_START..=GAME_PAK_RAM_END => {
                if let Some(cart) = &self.cartridge {
                    cart.read(addr)
                } else {
                    EMPTY_BYTE
                }
            }
            _ => EMPTY_BYTE,
        }
    }

    fn write(&mut self, addr: u32, value: u8) {
        match addr {
            WRAM_BOARD_START..=WRAM_BOARD_END => self.wram_board.write(addr, value),
            WRAM_CHIP_START..=WRAM_CHIP_END => self.wram_chip.write(addr, value),
            IO_REGISTERS_START..=IO_REGISTERS_END => self.io_registers.write(addr, value),
            PALETTE_RAM_START..=PALETTE_RAM_END => self.palette_ram.write(addr, value),
            VRAM_START..=VRAM_END => self.vram.write(addr, value),
            OAM_START..=OAM_END => self.oam.write(addr, value),
            GAME_PAK_RAM_START..=GAME_PAK_RAM_END => {
                if let Some(cart) = &mut self.cartridge {
                    cart.write(addr, value);
                }
            }
            _ => {}
        }
    }
}
