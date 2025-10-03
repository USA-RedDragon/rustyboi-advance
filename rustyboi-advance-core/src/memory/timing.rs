use crate::cpu::arm7tdmi::{CycleType, WaitStateConfig};
use crate::memory::mmio;

/// Memory timing information for cycle calculation
#[derive(Debug, Clone, Copy)]
pub struct MemoryTiming {
    /// Number of wait states for this access
    pub wait_states: u32,
    /// The cycle type used for this access
    pub cycle_type: CycleType,
}

/// Calculate memory timing for a given address and access type
pub fn get_memory_timing(addr: u32, config: &WaitStateConfig, is_sequential: bool) -> MemoryTiming {
    match addr {
        // BIOS - System ROM
        mmio::BIOS_START..=mmio::BIOS_END => MemoryTiming {
            wait_states: 0, // 1 cycle base for BIOS
            cycle_type: if is_sequential {
                CycleType::S
            } else {
                CycleType::N
            },
        },

        // EWRAM - External Working RAM (on-board)
        mmio::WRAM_BOARD_START..=mmio::WRAM_BOARD_END => MemoryTiming {
            wait_states: 0, // 1 cycle base
            cycle_type: if is_sequential {
                CycleType::S
            } else {
                CycleType::N
            },
        },

        // IWRAM - Internal Working RAM (on-chip)
        mmio::WRAM_CHIP_START..=mmio::WRAM_CHIP_END => MemoryTiming {
            wait_states: 0, // 1 cycle base
            cycle_type: if is_sequential {
                CycleType::S
            } else {
                CycleType::N
            },
        },

        // I/O Registers, Palette RAM, VRAM, OAM
        mmio::IO_REGISTERS_START..=mmio::IO_REGISTERS_END
        | mmio::PALETTE_RAM_START..=mmio::PALETTE_RAM_END
        | mmio::VRAM_START..=mmio::VRAM_END
        | mmio::OAM_START..=mmio::OAM_END => MemoryTiming {
            wait_states: 0, // 1 cycle base
            cycle_type: if is_sequential {
                CycleType::S
            } else {
                CycleType::N
            },
        },

        // ROM Wait State 0
        mmio::GAME_PAK_ROM_WAIT_STATE_0_START..=mmio::GAME_PAK_ROM_WAIT_STATE_0_END => {
            let (first, seq) = config.rom_ws0;
            MemoryTiming {
                wait_states: if is_sequential { seq } else { first },
                cycle_type: if is_sequential {
                    CycleType::S
                } else {
                    CycleType::N
                },
            }
        }

        // ROM Wait State 1
        mmio::GAME_PAK_ROM_WAIT_STATE_1_START..=mmio::GAME_PAK_ROM_WAIT_STATE_1_END => {
            let (first, seq) = config.rom_ws1;
            MemoryTiming {
                wait_states: if is_sequential { seq } else { first },
                cycle_type: if is_sequential {
                    CycleType::S
                } else {
                    CycleType::N
                },
            }
        }

        // ROM Wait State 2
        mmio::GAME_PAK_ROM_WAIT_STATE_2_START..=mmio::GAME_PAK_ROM_WAIT_STATE_2_END => {
            let (first, seq) = config.rom_ws2;
            MemoryTiming {
                wait_states: if is_sequential { seq } else { first },
                cycle_type: if is_sequential {
                    CycleType::S
                } else {
                    CycleType::N
                },
            }
        }

        // SRAM - Game Pak RAM
        mmio::GAME_PAK_RAM_START..=mmio::GAME_PAK_RAM_END => MemoryTiming {
            wait_states: config.sram,
            cycle_type: CycleType::N,
        },

        // Unknown/unmapped region
        _ => MemoryTiming {
            wait_states: 1,
            cycle_type: CycleType::N,
        },
    }
}
