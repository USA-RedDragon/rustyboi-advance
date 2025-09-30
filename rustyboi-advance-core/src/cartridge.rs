use crate::memory;
use crate::memory::buffer::Addressable;
use crate::memory::mmio;
use serde::{Deserialize, Serialize};

use std::fs;
use std::fs::{File, OpenOptions};
use std::io;
use std::io::{Read, Seek, SeekFrom, Write};
use zip::ZipArchive;

const MAX_ROM_SIZE: usize = 32 * 1024 * 1024; // 32 MB max ROM size

#[derive(Serialize, Deserialize)]
pub struct Cartridge {
    // ROM file path (for determining .sav file location)
    #[serde(skip)]
    rom_path: Option<String>,
    // Open file handle for save file (for battery-backed cartridges)
    #[serde(skip)]
    save_file: Option<File>,
    ram: memory::Memory<{ mmio::GAME_PAK_RAM_START }, { mmio::GAME_PAK_RAM_SIZE }>,
    rom: Vec<u8>,
}

impl Clone for Cartridge {
    fn clone(&self) -> Self {
        Cartridge {
            rom_path: self.rom_path.clone(),
            save_file: None, // Do not clone the file handle
            ram: self.ram.clone(),
            rom: self.rom.clone(),
        }
    }
}

impl Cartridge {
    fn extract_rom_from_zip_archive<R: Read + Seek>(
        mut archive: ZipArchive<R>,
    ) -> Result<Vec<u8>, io::Error> {
        for i in 0..archive.len() {
            let mut file = archive.by_index(i)?;
            let name = file.name().to_lowercase();
            if name.ends_with(".gba") {
                let mut rom_data = Vec::with_capacity(file.size() as usize);
                file.read_to_end(&mut rom_data)?;
                println!("Found GBA ROM in zip: {}", file.name());
                return Ok(rom_data);
            }
        }

        Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "No suitable ROM file found in zip archive",
        ))
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn extract_rom_from_zip(path: &str) -> Result<Vec<u8>, io::Error> {
        let file = File::open(path)?;
        let archive = ZipArchive::new(file)?;
        Self::extract_rom_from_zip_archive(archive)
    }

    fn extract_rom_from_zip_bytes(data: &[u8]) -> Result<Vec<u8>, io::Error> {
        use std::io::Cursor;

        let cursor = Cursor::new(data);
        let archive = ZipArchive::new(cursor)?;
        Self::extract_rom_from_zip_archive(archive)
    }

    pub fn load_from_bytes(data: &[u8]) -> Result<Self, io::Error> {
        let rom_data = if data.len() >= 4 && &data[0..4] == b"PK\x03\x04" {
            &Self::extract_rom_from_zip_bytes(data)?
        } else {
            data
        };

        if rom_data.len() > MAX_ROM_SIZE {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "ROM size out of bounds",
            ));
        }

        let mut cartridge = Cartridge {
            rom_path: None,
            save_file: None,
            ram: memory::Memory::new(),
            rom: rom_data.to_vec(),
        };

        // Try to load existing save file or create new one
        cartridge.load_or_create_save_file()?;

        Ok(cartridge)
    }

    pub fn load_from_path(path: &str) -> Result<Self, io::Error> {
        let data = if path.to_lowercase().ends_with(".zip") {
            #[cfg(not(target_arch = "wasm32"))]
            {
                Self::extract_rom_from_zip(path)?
            }
            #[cfg(target_arch = "wasm32")]
            {
                // For WASM, read the zip file and extract from bytes
                let zip_data = fs::read(path)?;
                Self::extract_rom_from_zip_bytes(&zip_data)?
            }
        } else {
            fs::read(path)?
        };
        Self::load_from_bytes(&data)
    }

    /// Get the save file path for this cartridge
    fn get_save_file_path(&self) -> Option<String> {
        self.rom_path.as_ref().map(|path| {
            // Replace the extension with .sav
            let mut save_path = path.clone();
            if let Some(dot_pos) = save_path.rfind('.') {
                save_path.truncate(dot_pos);
            }
            save_path.push_str(".sav");
            save_path
        })
    }

    /// Load save file data into RAM if it exists, or create empty save file (only for battery-backed RAM)
    fn load_or_create_save_file(&mut self) -> Result<(), io::Error> {
        if let Some(save_path) = self.get_save_file_path() {
            if std::path::Path::new(&save_path).exists() {
                // Load existing save file
                let loaded_data = fs::read(&save_path)?;
                if loaded_data.len() <= mmio::GAME_PAK_RAM_SIZE {
                    // Load data into RAM buffer
                    for (i, &byte) in loaded_data.iter().enumerate() {
                        self.ram.write(mmio::GAME_PAK_RAM_START + i as u32, byte);
                    }
                } else {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "Save file size out of bounds",
                    ));
                }
            } else {
                // Create new save file with current RAM data
                fs::write(&save_path, self.ram.as_slice())?;
            }

            // Open file handle for efficient writing
            self.save_file = Some(OpenOptions::new().write(true).open(&save_path)?);
        }
        Ok(())
    }

    fn write_ram_byte(&mut self, offset: usize, value: u8) -> Result<(), io::Error> {
        // Write to RAM buffer (offset is already wrapped by caller)
        self.ram
            .write(mmio::GAME_PAK_RAM_START + offset as u32, value);

        // Also write to save file if we have one open
        if let Some(ref mut file) = self.save_file {
            file.seek(SeekFrom::Start(offset as u64))?;
            file.write_all(&[value])?;
            file.flush()?; // Ensure immediate write
        }
        Ok(())
    }
}

impl memory::Addressable for Cartridge {
    fn read(&self, addr: u32) -> u8 {
        match addr {
            mmio::GAME_PAK_RAM_START..=mmio::GAME_PAK_RAM_END => self.ram.read(addr),
            mmio::GAME_PAK_ROM_WAIT_STATE_0_START..=mmio::GAME_PAK_ROM_WAIT_STATE_0_END => {
                let index: usize = (addr - mmio::GAME_PAK_ROM_WAIT_STATE_0_START) as usize;
                if index < self.rom.len() {
                    self.rom[index]
                } else {
                    mmio::EMPTY_BYTE
                }
            }
            mmio::GAME_PAK_ROM_WAIT_STATE_1_START..=mmio::GAME_PAK_ROM_WAIT_STATE_1_END => {
                let index: usize = (addr - mmio::GAME_PAK_ROM_WAIT_STATE_1_START) as usize;
                if index < self.rom.len() {
                    self.rom[index]
                } else {
                    mmio::EMPTY_BYTE
                }
            }
            mmio::GAME_PAK_ROM_WAIT_STATE_2_START..=mmio::GAME_PAK_ROM_WAIT_STATE_2_END => {
                let index: usize = (addr - mmio::GAME_PAK_ROM_WAIT_STATE_2_START) as usize;
                if index < self.rom.len() {
                    self.rom[index]
                } else {
                    mmio::EMPTY_BYTE
                }
            }
            _ => mmio::EMPTY_BYTE, // Out of bounds
        }
    }

    fn write(&mut self, addr: u32, value: u8) {
        match addr {
            mmio::GAME_PAK_RAM_START..=mmio::GAME_PAK_RAM_END => {
                let offset: usize = (addr - mmio::GAME_PAK_RAM_START) as usize;
                let _ = self.write_ram_byte(offset, value);
            }
            mmio::GAME_PAK_ROM_WAIT_STATE_0_START..=mmio::GAME_PAK_ROM_WAIT_STATE_0_END => {
                // ROM area is read-only, ignore writes
            }
            mmio::GAME_PAK_ROM_WAIT_STATE_1_START..=mmio::GAME_PAK_ROM_WAIT_STATE_1_END => {
                // ROM area is read-only, ignore writes
            }
            mmio::GAME_PAK_ROM_WAIT_STATE_2_START..=mmio::GAME_PAK_ROM_WAIT_STATE_2_END => {
                // ROM area is read-only, ignore writes
            }
            _ => {
                // Out of bounds, ignore
            }
        }
    }
}
