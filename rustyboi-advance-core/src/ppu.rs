use crate::memory::mmio;

use serde::{Deserialize, Serialize};

pub const FRAMEBUFFER_SIZE: usize = 240 * 160 * 4; // Width * Height * RGBA

#[derive(Serialize, Deserialize, Clone)]
pub enum State {
    NoOperation,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Ppu {
    disabled: bool,
    state: State,
    ticks: u128,

    have_frame: bool,
    #[serde(with = "serde_bytes")]
    fb_a: [u8; FRAMEBUFFER_SIZE],
    #[serde(with = "serde_bytes")]
    fb_b: [u8; FRAMEBUFFER_SIZE],
}

impl Ppu {
    pub fn new() -> Self {
        Ppu {
            disabled: true,
            state: State::NoOperation,
            ticks: 0,
            fb_a: [0; FRAMEBUFFER_SIZE],
            fb_b: [0; FRAMEBUFFER_SIZE],
            have_frame: false,
        }
    }

    pub fn step(&mut self, mmio: &mut mmio::Mmio) {
        if self.disabled {
            return;
        }
        self.ticks += 1;
    }

    pub fn is_disabled(&self) -> bool {
        self.disabled
    }

    pub fn get_state(&self) -> &State {
        &self.state
    }

    pub fn get_ticks(&self) -> u128 {
        self.ticks
    }

    pub fn has_frame(&self) -> bool {
        self.have_frame
    }

    pub fn get_frame(&mut self) -> [u8; FRAMEBUFFER_SIZE] {
        self.have_frame = false;
        self.fb_b
    }
}
