use crate::memory::mmio;

use serde::{Deserialize, Serialize};

pub const FRAMEBUFFER_SIZE: usize = 240 * 160 * 4; // Width * Height * RGBA
pub const NUM_PIXELS: usize = 240 * 160;

#[derive(Serialize, Deserialize, Clone)]
pub struct PpuConfig {
    pub debug: bool,
    pub scale: f32,
}

impl Default for PpuConfig {
    fn default() -> Self {
        PpuConfig {
            debug: false,
            scale: 1.0,
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Ppu {
    disabled: bool,
    ticks: u128,
    config: PpuConfig,

    // PPU state
    cycle: u32,
    pixel_index: u32,
    scanline_index: u16, // Changed from u8 to u16 to avoid overflow issues
    h_blank: bool,
    v_blank: bool,

    // Frame buffers - double buffering
    have_frame: bool,
    #[serde(with = "serde_bytes")]
    fb_a: [u8; FRAMEBUFFER_SIZE], // Rendering buffer
    #[serde(with = "serde_bytes")]
    fb_b: [u8; FRAMEBUFFER_SIZE], // Display buffer
}

impl Default for Ppu {
    fn default() -> Self {
        Self::new()
    }
}

impl Ppu {
    pub fn new() -> Self {
        Self::with_config(PpuConfig::default())
    }

    pub fn with_config(config: PpuConfig) -> Self {
        Ppu {
            disabled: false, // Enable PPU by default now
            ticks: 0,
            config,
            
            // Initialize PPU state
            cycle: 0,
            pixel_index: 0,
            scanline_index: 0,
            h_blank: false,
            v_blank: false,
            
            // Initialize frame buffers
            have_frame: false,
            fb_a: [0; FRAMEBUFFER_SIZE], // Rendering buffer
            fb_b: [0; FRAMEBUFFER_SIZE], // Display buffer
        }
    }

    pub fn step(&mut self, mmio: &mut mmio::Mmio) {
        use crate::memory::Addressable;
        
        if self.disabled {
            return;
        }
        
        self.ticks += 1;
        
        // Read DISPCNT to check display mode
        let disp_cnt = (mmio.read(mmio::IO_REGISTERS_START + 1) as u16) << 8 | 
                       (mmio.read(mmio::IO_REGISTERS_START) as u16);
        let display_mode = disp_cnt & 0x7;
        
        // Skip if invalid display mode
        if display_mode > 5 {
            return;
        }
        
        if self.config.debug {
            println!("PPU Cycle: {}", self.cycle);
        }
        
        let mut newly_h_blank = false;
        let mut newly_not_h_blank = false;
        let mut newly_v_blank = false;
        let mut newly_not_v_blank = false;
        
        // Every 4 cycles is a pixel
        if self.cycle % 4 == 0 {
            if self.config.debug {
                println!("Pixel");
            }
            self.pixel_index += 1;
        }
        
        // Every 240+68 pixels is a scanline (240 visible + 68 H-blank)
        if self.pixel_index > 240 + 68 {
            // Scanline is done
            if self.config.debug {
                println!("Scanline");
            }
            self.scanline_index += 1;
            // Update VCOUNT register (0x04000006) - only lower 8 bits
            mmio.write(mmio::IO_REGISTERS_START + 0x06, self.scanline_index as u8);
            self.pixel_index = 0;
            newly_not_h_blank = true;
            self.h_blank = false;
        } else if self.pixel_index == 240 {
            // Enter H-blank
            newly_h_blank = true;
            self.h_blank = true;
        }
        
        // Every 160+68 scanlines is a frame (160 visible + 68 V-blank)  
        if self.scanline_index >= 160 + 68 {
            // Frame is done
            if self.config.debug {
                println!("Frame");
            }
            // Just mark that we have a frame ready - rendering happens when requested
            self.have_frame = true;
            self.v_blank = false;
            self.h_blank = false;
            newly_not_v_blank = true;
            newly_not_h_blank = true;
            self.scanline_index = 0;
            self.cycle = 0;
        } else if self.scanline_index == 160 {
            // Enter V-blank
            self.v_blank = true;
            newly_v_blank = true;
        }
        
        self.cycle += 1;
        
        // Update DISPSTAT register (0x04000004)
        let mut dispstat = mmio.read(mmio::IO_REGISTERS_START + 0x04);
        
        if newly_h_blank {
            dispstat |= 0x02; // Set H-blank flag
        }
        
        if newly_not_h_blank {
            dispstat &= 0xFD; // Clear H-blank flag
        }
        
        if newly_v_blank {
            dispstat |= 0x01; // Set V-blank flag
        }
        
        if newly_not_v_blank {
            dispstat &= 0xFE; // Clear V-blank flag
        }
        
        mmio.write(mmio::IO_REGISTERS_START + 0x04, dispstat);
    }

    pub fn is_disabled(&self) -> bool {
        self.disabled
    }

    pub fn get_ticks(&self) -> u128 {
        self.ticks
    }

    pub fn has_frame(&self) -> bool {
        self.have_frame
    }

    pub fn get_frame(&mut self, mmio: &mmio::Mmio) -> [u8; FRAMEBUFFER_SIZE] {
        // If we have a new frame ready, render it to fb_a and swap buffers
        if self.have_frame {
            if let Some(rendered_frame) = self.frame_buffer(mmio) {
                // Copy rendered frame to fb_a
                if rendered_frame.len() == FRAMEBUFFER_SIZE {
                    self.fb_a.copy_from_slice(&rendered_frame);
                } else {
                    // Handle upscaled frames - just take what fits
                    let copy_size = rendered_frame.len().min(FRAMEBUFFER_SIZE);
                    self.fb_a[..copy_size].copy_from_slice(&rendered_frame[..copy_size]);
                }
                // Swap buffers - fb_b becomes the new display buffer
                std::mem::swap(&mut self.fb_a, &mut self.fb_b);
            }
            self.have_frame = false;
        }
        
        self.fb_b // Return the display buffer
    }

    pub fn dump_vram(&self, mmio: &mmio::Mmio) -> String {
        use crate::memory::Addressable;
        
        let mut output = String::new();
        for i in (mmio::VRAM_START..=mmio::VRAM_END).step_by(16) {
            output += &format!("0x{:08X}: ", i);
            for j in 0..16 {
                if i + j <= mmio::VRAM_END {
                    output += &format!("{:02X} ", mmio.read(i + j));
                } else {
                    break;
                }
            }
            output += "\n";
        }
        output
    }

    pub fn frame_ready(&self) -> bool {
        self.have_frame
    }

    pub fn clear_frame_ready(&mut self) {
        self.have_frame = false;
    }

    pub fn is_h_blank(&self) -> bool {
        self.h_blank
    }

    pub fn is_v_blank(&self) -> bool {
        self.v_blank
    }

    pub fn get_display_mode(&self, mmio: &mmio::Mmio) -> u8 {
        use crate::memory::Addressable;
        
        // Read DISPCNT register (0x04000000)
        let disp_cnt = (mmio.read(mmio::IO_REGISTERS_START + 1) as u16) << 8 | 
                       (mmio.read(mmio::IO_REGISTERS_START) as u16);
        
        // Extract display mode (bits 0-2)
        (disp_cnt & 0x7) as u8
    }

    pub fn get_cycle(&self) -> u32 {
        self.cycle
    }

    pub fn get_pixel_index(&self) -> u32 {
        self.pixel_index
    }

    pub fn get_scanline_index(&self) -> u16 {
        self.scanline_index
    }

    pub fn is_debug_enabled(&self) -> bool {
        self.config.debug
    }

    pub fn get_scale(&self) -> f32 {
        self.config.scale
    }

    pub fn frame_buffer(&self, mmio: &mmio::Mmio) -> Option<Vec<u8>> {
        use crate::memory::Addressable;
        
        // Read DISPCNT register (0x04000000)
        let disp_cnt = (mmio.read(mmio::IO_REGISTERS_START + 1) as u16) << 8 | 
                       (mmio.read(mmio::IO_REGISTERS_START) as u16);
        
        // Extract display mode (bits 0-2)
        let display_mode = disp_cnt & 0x7;
        
        let original_render = match display_mode {
            0 => None, // Mode 0: Tiled 240x160 8-bpp with 4 backgrounds - Not implemented yet
            1 => None, // Mode 1: Tiled 240x160 8-bpp with 3 backgrounds - Not implemented yet
            2 => None, // Mode 2: Tiled 240x160 8-bpp with 2 backgrounds - Not implemented yet
            3 => Some(self.render_mode_3(mmio)), // Mode 3: Bitmap 240x160 16-bpp with 1 background
            4 => Some(self.render_mode_4(mmio)), // Mode 4: Bitmap 240x160 8-bpp with 2 backgrounds
            5 => Some(self.render_mode_5(mmio)), // Mode 5: Bitmap 160x128 16-bpp with 2 backgrounds
            _ => {
                panic!("Invalid display mode: {}", display_mode);
            }
        };
        
        if let Some(render) = original_render {
            Some(self.upscale(&render))
        } else {
            None
        }
    }

    fn render_mode_3(&self, mmio: &mmio::Mmio) -> Vec<u8> {
        use crate::memory::Addressable;
        
        // Mode 3: 240x160 16-bpp bitmap
        let mut frame_buffer = vec![0u8; FRAMEBUFFER_SIZE];
        
        for y in 0..160 {
            for x in 0..240 {
                let vram_offset = (y * 240 + x) * 2; // 2 bytes per pixel (16-bpp)
                let vram_addr = mmio::VRAM_START + vram_offset as u32;
                
                // Read 16-bit color value (little endian)
                let color16 = (mmio.read(vram_addr + 1) as u16) << 8 | 
                             (mmio.read(vram_addr) as u16);
                
                // Convert RGB555 to RGBA8888
                let r = ((color16 & 0x1F) << 3) as u8;
                let g = (((color16 >> 5) & 0x1F) << 3) as u8;
                let b = (((color16 >> 10) & 0x1F) << 3) as u8;
                let a = 255u8;
                
                let fb_offset = (y * 240 + x) * 4;
                frame_buffer[fb_offset] = r;
                frame_buffer[fb_offset + 1] = g;
                frame_buffer[fb_offset + 2] = b;
                frame_buffer[fb_offset + 3] = a;
            }
        }
        
        frame_buffer
    }

    fn render_mode_4(&self, mmio: &mmio::Mmio) -> Vec<u8> {
        use crate::memory::Addressable;
        
        // Mode 4: 240x160 8-bpp indexed bitmap
        let mut frame_buffer = vec![0u8; FRAMEBUFFER_SIZE];
        
        for y in 0..160 {
            for x in 0..240 {
                let vram_offset = y * 240 + x; // 1 byte per pixel (8-bpp)
                let vram_addr = mmio::VRAM_START + vram_offset as u32;
                
                // Read palette index
                let palette_index = mmio.read(vram_addr) as u32;
                
                // Read color from palette RAM (2 bytes per entry)
                let palette_addr = mmio::PALETTE_RAM_START + palette_index * 2;
                let color16 = (mmio.read(palette_addr + 1) as u16) << 8 | 
                             (mmio.read(palette_addr) as u16);
                
                // Convert RGB555 to RGBA8888
                let r = ((color16 & 0x1F) << 3) as u8;
                let g = (((color16 >> 5) & 0x1F) << 3) as u8;
                let b = (((color16 >> 10) & 0x1F) << 3) as u8;
                let a = 255u8;
                
                let fb_offset = (y * 240 + x) * 4;
                frame_buffer[fb_offset] = r;
                frame_buffer[fb_offset + 1] = g;
                frame_buffer[fb_offset + 2] = b;
                frame_buffer[fb_offset + 3] = a;
            }
        }
        
        frame_buffer
    }

    fn render_mode_5(&self, mmio: &mmio::Mmio) -> Vec<u8> {
        use crate::memory::Addressable;
        
        // Mode 5: 160x128 16-bpp bitmap - centered in 240x160 frame
        let mut frame_buffer = vec![0u8; FRAMEBUFFER_SIZE];
        
        // Calculate centering offsets
        let x_offset = (240 - 160) / 2; // 40 pixels from left
        let y_offset = (160 - 128) / 2; // 16 pixels from top
        
        for y in 0..128 {
            for x in 0..160 {
                let vram_offset = (y * 160 + x) * 2; // 2 bytes per pixel (16-bpp)
                let vram_addr = mmio::VRAM_START + vram_offset as u32;
                
                // Read 16-bit color value (little endian)
                let color16 = (mmio.read(vram_addr + 1) as u16) << 8 | 
                             (mmio.read(vram_addr) as u16);
                
                // Convert RGB555 to RGBA8888
                let r = ((color16 & 0x1F) << 3) as u8;
                let g = (((color16 >> 5) & 0x1F) << 3) as u8;
                let b = (((color16 >> 10) & 0x1F) << 3) as u8;
                let a = 255u8;
                
                // Place in centered position in 240x160 frame
                let fb_x = x + x_offset;
                let fb_y = y + y_offset;
                let fb_offset = (fb_y * 240 + fb_x) * 4;
                
                frame_buffer[fb_offset] = r;
                frame_buffer[fb_offset + 1] = g;
                frame_buffer[fb_offset + 2] = b;
                frame_buffer[fb_offset + 3] = a;
            }
        }
        
        frame_buffer
    }

    fn upscale(&self, render: &[u8]) -> Vec<u8> {
        // TODO: Implement proper bicubic interpolation like the Go version
        // For now, implement simple nearest-neighbor scaling
        
        let scale = self.config.scale;
        if scale == 1.0 {
            return render.to_vec();
        }
        
        let target_width = (240.0 * scale) as usize;
        let target_height = (160.0 * scale) as usize;
        let mut upscaled = vec![0u8; target_width * target_height * 4];
        
        for y in 0..target_height {
            for x in 0..target_width {
                // Nearest neighbor sampling
                let src_x = (x as f32 / scale) as usize;
                let src_y = (y as f32 / scale) as usize;
                
                if src_x < 240 && src_y < 160 {
                    let src_offset = (src_y * 240 + src_x) * 4;
                    let dst_offset = (y * target_width + x) * 4;
                    
                    upscaled[dst_offset] = render[src_offset];         // R
                    upscaled[dst_offset + 1] = render[src_offset + 1]; // G
                    upscaled[dst_offset + 2] = render[src_offset + 2]; // B
                    upscaled[dst_offset + 3] = render[src_offset + 3]; // A
                }
            }
        }
        
        upscaled
    }
}
