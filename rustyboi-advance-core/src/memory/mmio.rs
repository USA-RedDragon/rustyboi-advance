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
    pub dma: memory::dma::DmaController,
}

impl Default for Mmio {
    fn default() -> Self {
        Self::new()
    }
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
            dma: memory::dma::DmaController::new(),
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
            // WRAM Board - 256KB mirrored every 256KB in 0x02000000-0x02FFFFFF
            0x02000000..=0x02FFFFFF => {
                let mirrored_addr = WRAM_BOARD_START + (addr & (WRAM_BOARD_SIZE as u32 - 1));
                self.wram_board.read(mirrored_addr)
            }
            // WRAM Chip - 32KB mirrored every 32KB in 0x03000000-0x03FFFFFF  
            0x03000000..=0x03FFFFFF => {
                let mirrored_addr = WRAM_CHIP_START + (addr & (WRAM_CHIP_SIZE as u32 - 1));
                self.wram_chip.read(mirrored_addr)
            }
            // I/O Registers - mirrored every 1KB in 0x04000000-0x04FFFFFF
            0x04000000..=0x04FFFFFF => {
                let offset = addr & 0x3FF; // Mirror every 1KB
                if offset <= (IO_REGISTERS_END - IO_REGISTERS_START) {
                    self.read_dma_register(offset)
                } else {
                    EMPTY_BYTE
                }
            }
            // Palette RAM - 1KB mirrored every 1KB in 0x05000000-0x05FFFFFF
            0x05000000..=0x05FFFFFF => {
                let mirrored_addr = PALETTE_RAM_START + (addr & (PALETTE_RAM_SIZE as u32 - 1));
                self.palette_ram.read(mirrored_addr)
            }
            // VRAM - 96KB with complex mirroring in 0x06000000-0x06FFFFFF
            0x06000000..=0x06FFFFFF => {
                let offset = addr & 0x1FFFF; // 128KB address space
                if offset < VRAM_SIZE as u32 {
                    self.vram.read(VRAM_START + offset)
                } else {
                    // Mirror the upper 32KB region
                    let mirrored_offset = offset & 0x17FFF; // Mirror within 96KB
                    self.vram.read(VRAM_START + mirrored_offset)
                }
            }
            // OAM - 1KB mirrored every 1KB in 0x07000000-0x07FFFFFF
            0x07000000..=0x07FFFFFF => {
                let mirrored_addr = OAM_START + (addr & (OAM_SIZE as u32 - 1));
                self.oam.read(mirrored_addr)
            }
            // Game Pak ROM regions - all mirror the same cartridge data
            GAME_PAK_ROM_WAIT_STATE_0_START..=GAME_PAK_ROM_WAIT_STATE_0_END |
            GAME_PAK_ROM_WAIT_STATE_1_START..=GAME_PAK_ROM_WAIT_STATE_1_END |
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
            // WRAM Board - 256KB mirrored every 256KB in 0x02000000-0x02FFFFFF
            0x02000000..=0x02FFFFFF => {
                let mirrored_addr = WRAM_BOARD_START + (addr & (WRAM_BOARD_SIZE as u32 - 1));
                self.wram_board.write(mirrored_addr, value);
            }
            // WRAM Chip - 32KB mirrored every 32KB in 0x03000000-0x03FFFFFF
            0x03000000..=0x03FFFFFF => {
                let mirrored_addr = WRAM_CHIP_START + (addr & (WRAM_CHIP_SIZE as u32 - 1));
                self.wram_chip.write(mirrored_addr, value);
            }
            // I/O Registers - mirrored every 1KB in 0x04000000-0x04FFFFFF
            0x04000000..=0x04FFFFFF => {
                let offset = addr & 0x3FF; // Mirror every 1KB
                if offset <= (IO_REGISTERS_END - IO_REGISTERS_START) {
                    self.write_dma_register(offset, value);
                }
            }
            // Palette RAM - byte writes duplicate to both bytes of halfword
            0x05000000..=0x05FFFFFF => {
                let mirrored_addr = PALETTE_RAM_START + (addr & (PALETTE_RAM_SIZE as u32 - 1));
                let aligned_addr = mirrored_addr & !1;
                self.palette_ram.write(aligned_addr, value);
                self.palette_ram.write(aligned_addr + 1, value);
            }
            // VRAM - byte writes have special behavior based on display mode
            0x06000000..=0x06FFFFFF => {
                let offset = addr & 0x1FFFF;
                let actual_offset = if offset < VRAM_SIZE as u32 {
                    offset
                } else {
                    offset & 0x17FFF
                };
                let actual_addr = VRAM_START + actual_offset;
                
                // Get display mode from DISPCNT register
                let dispcnt = (self.io_registers.read(IO_REGISTERS_START + 1) as u16) << 8 | 
                             (self.io_registers.read(IO_REGISTERS_START) as u16);
                let display_mode = dispcnt & 0x7;
                
                match display_mode {
                    3..=5 => {
                        // In bitmap modes, byte writes to VRAM duplicate the byte
                        if actual_offset >= 0x14000 {
                            return; // Ignore writes beyond valid range
                        }
                        let aligned_addr = actual_addr & !1;
                        self.vram.write(aligned_addr, value);
                        self.vram.write(aligned_addr + 1, value);
                    }
                    0..=2 => {
                        // In tiled modes, byte writes to VRAM also duplicate
                        if actual_offset >= 0x10000 {
                            return; // Ignore writes beyond valid range
                        }
                        let aligned_addr = actual_addr & !1;
                        self.vram.write(aligned_addr, value);
                        self.vram.write(aligned_addr + 1, value);
                    }
                    _ => {
                        // For other modes, treat as normal
                        if actual_offset >= 0x10000 {
                            return;
                        }
                        let aligned_addr = actual_addr & !1;
                        self.vram.write(aligned_addr, value);
                        self.vram.write(aligned_addr + 1, value);
                    }
                }
            }
            // OAM - byte writes are ignored (GBA hardware quirk)
            0x07000000..=0x07FFFFFF => {
                // Ignore byte writes to OAM
            }
            GAME_PAK_RAM_START..=GAME_PAK_RAM_END => {
                if let Some(cart) = &mut self.cartridge {
                    cart.write(addr, value);
                }
            }
            _ => {}
        }
    }
}

impl Mmio {
    /// Handle reads from DMA registers
    fn read_dma_register(&self, offset: u32) -> u8 {
        use memory::dma::DmaChannel;
        
        match offset {
            // DMA0 registers (0xB0-0xBB)
            0xBA => (self.dma.read_cnt_h(DmaChannel::Dma0) & 0xFF) as u8,
            0xBB => ((self.dma.read_cnt_h(DmaChannel::Dma0) >> 8) & 0xFF) as u8,
            
            // DMA1 registers (0xBC-0xC7)
            0xC6 => (self.dma.read_cnt_h(DmaChannel::Dma1) & 0xFF) as u8,
            0xC7 => ((self.dma.read_cnt_h(DmaChannel::Dma1) >> 8) & 0xFF) as u8,
            
            // DMA2 registers (0xC8-0xD3)
            0xD2 => (self.dma.read_cnt_h(DmaChannel::Dma2) & 0xFF) as u8,
            0xD3 => ((self.dma.read_cnt_h(DmaChannel::Dma2) >> 8) & 0xFF) as u8,
            
            // DMA3 registers (0xD4-0xDF)
            0xDE => (self.dma.read_cnt_h(DmaChannel::Dma3) & 0xFF) as u8,
            0xDF => ((self.dma.read_cnt_h(DmaChannel::Dma3) >> 8) & 0xFF) as u8,
            
            _ => self.io_registers.read(IO_REGISTERS_START + offset),
        }
    }
    
    /// Handle writes to DMA registers
    fn write_dma_register(&mut self, offset: u32, value: u8) {
        use memory::dma::DmaChannel;
        
        match offset {
            // DMA0SAD (0xB0-0xB3) - Source Address
            0xB0 => {
                let current = self.dma.get_channel(DmaChannel::Dma0).source_addr;
                self.dma.write_sad(DmaChannel::Dma0, (current & 0xFFFFFF00) | (value as u32));
            }
            0xB1 => {
                let current = self.dma.get_channel(DmaChannel::Dma0).source_addr;
                self.dma.write_sad(DmaChannel::Dma0, (current & 0xFFFF00FF) | ((value as u32) << 8));
            }
            0xB2 => {
                let current = self.dma.get_channel(DmaChannel::Dma0).source_addr;
                self.dma.write_sad(DmaChannel::Dma0, (current & 0xFF00FFFF) | ((value as u32) << 16));
            }
            0xB3 => {
                let current = self.dma.get_channel(DmaChannel::Dma0).source_addr;
                self.dma.write_sad(DmaChannel::Dma0, (current & 0x00FFFFFF) | ((value as u32) << 24));
            }
            
            // DMA0DAD (0xB4-0xB7) - Destination Address
            0xB4 => {
                let current = self.dma.get_channel(DmaChannel::Dma0).dest_addr;
                self.dma.write_dad(DmaChannel::Dma0, (current & 0xFFFFFF00) | (value as u32));
            }
            0xB5 => {
                let current = self.dma.get_channel(DmaChannel::Dma0).dest_addr;
                self.dma.write_dad(DmaChannel::Dma0, (current & 0xFFFF00FF) | ((value as u32) << 8));
            }
            0xB6 => {
                let current = self.dma.get_channel(DmaChannel::Dma0).dest_addr;
                self.dma.write_dad(DmaChannel::Dma0, (current & 0xFF00FFFF) | ((value as u32) << 16));
            }
            0xB7 => {
                let current = self.dma.get_channel(DmaChannel::Dma0).dest_addr;
                self.dma.write_dad(DmaChannel::Dma0, (current & 0x00FFFFFF) | ((value as u32) << 24));
            }
            
            // DMA0CNT_L (0xB8-0xB9) - Word Count
            0xB8 => {
                let current = self.dma.get_channel(DmaChannel::Dma0).word_count;
                self.dma.write_cnt_l(DmaChannel::Dma0, (current & 0xFF00) | (value as u16));
            }
            0xB9 => {
                let current = self.dma.get_channel(DmaChannel::Dma0).word_count;
                self.dma.write_cnt_l(DmaChannel::Dma0, (current & 0x00FF) | ((value as u16) << 8));
            }
            
            // DMA0CNT_H (0xBA-0xBB) - Control
            0xBA => {
                let current = self.dma.read_cnt_h(DmaChannel::Dma0);
                self.dma.write_cnt_h(DmaChannel::Dma0, (current & 0xFF00) | (value as u16));
            }
            0xBB => {
                let current = self.dma.read_cnt_h(DmaChannel::Dma0);
                self.dma.write_cnt_h(DmaChannel::Dma0, (current & 0x00FF) | ((value as u16) << 8));
            }
            
            // DMA1 registers (0xBC-0xC7)
            0xBC..=0xBF => {
                let byte_offset = offset - 0xBC;
                let current = self.dma.get_channel(DmaChannel::Dma1).source_addr;
                let shift = byte_offset * 8;
                let mask = !(0xFF << shift);
                self.dma.write_sad(DmaChannel::Dma1, (current & mask) | ((value as u32) << shift));
            }
            0xC0..=0xC3 => {
                let byte_offset = offset - 0xC0;
                let current = self.dma.get_channel(DmaChannel::Dma1).dest_addr;
                let shift = byte_offset * 8;
                let mask = !(0xFF << shift);
                self.dma.write_dad(DmaChannel::Dma1, (current & mask) | ((value as u32) << shift));
            }
            0xC4 => {
                let current = self.dma.get_channel(DmaChannel::Dma1).word_count;
                self.dma.write_cnt_l(DmaChannel::Dma1, (current & 0xFF00) | (value as u16));
            }
            0xC5 => {
                let current = self.dma.get_channel(DmaChannel::Dma1).word_count;
                self.dma.write_cnt_l(DmaChannel::Dma1, (current & 0x00FF) | ((value as u16) << 8));
            }
            0xC6 => {
                let current = self.dma.read_cnt_h(DmaChannel::Dma1);
                self.dma.write_cnt_h(DmaChannel::Dma1, (current & 0xFF00) | (value as u16));
            }
            0xC7 => {
                let current = self.dma.read_cnt_h(DmaChannel::Dma1);
                self.dma.write_cnt_h(DmaChannel::Dma1, (current & 0x00FF) | ((value as u16) << 8));
            }
            
            // DMA2 registers (0xC8-0xD3)
            0xC8..=0xCB => {
                let byte_offset = offset - 0xC8;
                let current = self.dma.get_channel(DmaChannel::Dma2).source_addr;
                let shift = byte_offset * 8;
                let mask = !(0xFF << shift);
                self.dma.write_sad(DmaChannel::Dma2, (current & mask) | ((value as u32) << shift));
            }
            0xCC..=0xCF => {
                let byte_offset = offset - 0xCC;
                let current = self.dma.get_channel(DmaChannel::Dma2).dest_addr;
                let shift = byte_offset * 8;
                let mask = !(0xFF << shift);
                self.dma.write_dad(DmaChannel::Dma2, (current & mask) | ((value as u32) << shift));
            }
            0xD0 => {
                let current = self.dma.get_channel(DmaChannel::Dma2).word_count;
                self.dma.write_cnt_l(DmaChannel::Dma2, (current & 0xFF00) | (value as u16));
            }
            0xD1 => {
                let current = self.dma.get_channel(DmaChannel::Dma2).word_count;
                self.dma.write_cnt_l(DmaChannel::Dma2, (current & 0x00FF) | ((value as u16) << 8));
            }
            0xD2 => {
                let current = self.dma.read_cnt_h(DmaChannel::Dma2);
                self.dma.write_cnt_h(DmaChannel::Dma2, (current & 0xFF00) | (value as u16));
            }
            0xD3 => {
                let current = self.dma.read_cnt_h(DmaChannel::Dma2);
                self.dma.write_cnt_h(DmaChannel::Dma2, (current & 0x00FF) | ((value as u16) << 8));
            }
            
            // DMA3 registers (0xD4-0xDF)
            0xD4..=0xD7 => {
                let byte_offset = offset - 0xD4;
                let current = self.dma.get_channel(DmaChannel::Dma3).source_addr;
                let shift = byte_offset * 8;
                let mask = !(0xFF << shift);
                self.dma.write_sad(DmaChannel::Dma3, (current & mask) | ((value as u32) << shift));
            }
            0xD8..=0xDB => {
                let byte_offset = offset - 0xD8;
                let current = self.dma.get_channel(DmaChannel::Dma3).dest_addr;
                let shift = byte_offset * 8;
                let mask = !(0xFF << shift);
                self.dma.write_dad(DmaChannel::Dma3, (current & mask) | ((value as u32) << shift));
            }
            0xDC => {
                let current = self.dma.get_channel(DmaChannel::Dma3).word_count;
                self.dma.write_cnt_l(DmaChannel::Dma3, (current & 0xFF00) | (value as u16));
            }
            0xDD => {
                let current = self.dma.get_channel(DmaChannel::Dma3).word_count;
                self.dma.write_cnt_l(DmaChannel::Dma3, (current & 0x00FF) | ((value as u16) << 8));
            }
            0xDE => {
                let current = self.dma.read_cnt_h(DmaChannel::Dma3);
                self.dma.write_cnt_h(DmaChannel::Dma3, (current & 0xFF00) | (value as u16));
            }
            0xDF => {
                let current = self.dma.read_cnt_h(DmaChannel::Dma3);
                self.dma.write_cnt_h(DmaChannel::Dma3, (current & 0x00FF) | ((value as u16) << 8));
            }
            
            _ => {
                self.io_registers.write(IO_REGISTERS_START + offset, value);
            }
        }
    }

    /// Write 16-bit value (for STRH instruction)
    pub fn write16(&mut self, addr: u32, value: u16) {
        match addr {
            // Palette RAM with mirroring
            0x05000000..=0x05FFFFFF => {
                let mirrored_addr = PALETTE_RAM_START + (addr & (PALETTE_RAM_SIZE as u32 - 1));
                self.palette_ram.write(mirrored_addr, (value & 0xFF) as u8);
                self.palette_ram.write(mirrored_addr + 1, ((value >> 8) & 0xFF) as u8);
            }
            // VRAM with mirroring
            0x06000000..=0x06FFFFFF => {
                let offset = addr & 0x1FFFF;
                if offset < VRAM_SIZE as u32 {
                    self.vram.write(VRAM_START + offset, (value & 0xFF) as u8);
                    self.vram.write(VRAM_START + offset + 1, ((value >> 8) & 0xFF) as u8);
                } else {
                    let mirrored_offset = offset & 0x17FFF;
                    self.vram.write(VRAM_START + mirrored_offset, (value & 0xFF) as u8);
                    self.vram.write(VRAM_START + mirrored_offset + 1, ((value >> 8) & 0xFF) as u8);
                }
            }
            // OAM with mirroring (halfword writes work normally)
            0x07000000..=0x07FFFFFF => {
                let mirrored_addr = OAM_START + (addr & (OAM_SIZE as u32 - 1));
                self.oam.write(mirrored_addr, (value & 0xFF) as u8);
                self.oam.write(mirrored_addr + 1, ((value >> 8) & 0xFF) as u8);
            }
            // All other addresses use normal byte writes
            _ => {
                self.write(addr, (value & 0xFF) as u8);
                self.write(addr + 1, ((value >> 8) & 0xFF) as u8);
            }
        }
    }

    /// Write 32-bit value (for STR instruction)
    pub fn write32(&mut self, addr: u32, value: u32) {
        match addr {
            // Palette RAM with mirroring
            0x05000000..=0x05FFFFFF => {
                let mirrored_addr = PALETTE_RAM_START + (addr & (PALETTE_RAM_SIZE as u32 - 1));
                self.palette_ram.write(mirrored_addr, (value & 0xFF) as u8);
                self.palette_ram.write(mirrored_addr + 1, ((value >> 8) & 0xFF) as u8);
                self.palette_ram.write(mirrored_addr + 2, ((value >> 16) & 0xFF) as u8);
                self.palette_ram.write(mirrored_addr + 3, ((value >> 24) & 0xFF) as u8);
            }
            // VRAM with mirroring
            0x06000000..=0x06FFFFFF => {
                let offset = addr & 0x1FFFF;
                if offset < VRAM_SIZE as u32 {
                    self.vram.write(VRAM_START + offset, (value & 0xFF) as u8);
                    self.vram.write(VRAM_START + offset + 1, ((value >> 8) & 0xFF) as u8);
                    self.vram.write(VRAM_START + offset + 2, ((value >> 16) & 0xFF) as u8);
                    self.vram.write(VRAM_START + offset + 3, ((value >> 24) & 0xFF) as u8);
                } else {
                    let mirrored_offset = offset & 0x17FFF;
                    self.vram.write(VRAM_START + mirrored_offset, (value & 0xFF) as u8);
                    self.vram.write(VRAM_START + mirrored_offset + 1, ((value >> 8) & 0xFF) as u8);
                    self.vram.write(VRAM_START + mirrored_offset + 2, ((value >> 16) & 0xFF) as u8);
                    self.vram.write(VRAM_START + mirrored_offset + 3, ((value >> 24) & 0xFF) as u8);
                }
            }
            // OAM with mirroring (word writes work normally)
            0x07000000..=0x07FFFFFF => {
                let mirrored_addr = OAM_START + (addr & (OAM_SIZE as u32 - 1));
                self.oam.write(mirrored_addr, (value & 0xFF) as u8);
                self.oam.write(mirrored_addr + 1, ((value >> 8) & 0xFF) as u8);
                self.oam.write(mirrored_addr + 2, ((value >> 16) & 0xFF) as u8);
                self.oam.write(mirrored_addr + 3, ((value >> 24) & 0xFF) as u8);
            }
            // All other addresses use normal byte writes
            _ => {
                self.write(addr, (value & 0xFF) as u8);
                self.write(addr + 1, ((value >> 8) & 0xFF) as u8);
                self.write(addr + 2, ((value >> 16) & 0xFF) as u8);
                self.write(addr + 3, ((value >> 24) & 0xFF) as u8);
            }
        }
    }
}
