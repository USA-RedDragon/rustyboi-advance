use crate::memory::mmio;

use serde::{Deserialize, Serialize};

pub const FRAMEBUFFER_SIZE: usize = 240 * 160 * 4; // Width * Height * RGBA

// Timing constants
pub const SCREEN_WIDTH: usize = 240;
pub const SCREEN_HEIGHT: usize = 160;
pub const TOTAL_LINES: usize = 228; // 160 visible + 68 vblank
pub const CYCLES_PER_LINE: usize = 1232;
pub const HBLANK_START: usize = 1006;
pub const VISIBLE_CYCLES: usize = 960;

// Background rendering timing
pub const BG_START_CYCLE: usize = 31;
pub const SPRITE_START_CYCLE: usize = 40;
pub const COMPOSITE_START_CYCLE: usize = 46;

// PPU register addresses
pub const REG_DISPCNT: u32 = 0x4000000;
pub const REG_DISPSTAT: u32 = 0x4000004;
pub const REG_VCOUNT: u32 = 0x4000006;
pub const REG_BG0CNT: u32 = 0x4000008;
pub const REG_BG1CNT: u32 = 0x400000A;
pub const REG_BG2CNT: u32 = 0x400000C;
pub const REG_BG3CNT: u32 = 0x400000E;
pub const REG_BG0HOFS: u32 = 0x4000010;
pub const REG_BG0VOFS: u32 = 0x4000012;
pub const REG_BG1HOFS: u32 = 0x4000014;
pub const REG_BG1VOFS: u32 = 0x4000016;
pub const REG_BG2HOFS: u32 = 0x4000018;
pub const REG_BG2VOFS: u32 = 0x400001A;
pub const REG_BG3HOFS: u32 = 0x400001C;
pub const REG_BG3VOFS: u32 = 0x400001E;
pub const REG_BG2PA: u32 = 0x4000020;
pub const REG_BG2PB: u32 = 0x4000022;
pub const REG_BG2PC: u32 = 0x4000024;
pub const REG_BG2PD: u32 = 0x4000026;
pub const REG_BG2X: u32 = 0x4000028;
pub const REG_BG2Y: u32 = 0x400002C;
pub const REG_BG3PA: u32 = 0x4000030;
pub const REG_BG3PB: u32 = 0x4000032;
pub const REG_BG3PC: u32 = 0x4000034;
pub const REG_BG3PD: u32 = 0x4000036;
pub const REG_BG3X: u32 = 0x4000038;
pub const REG_BG3Y: u32 = 0x400003C;
pub const REG_WIN0H: u32 = 0x4000040;
pub const REG_WIN1H: u32 = 0x4000042;
pub const REG_WIN0V: u32 = 0x4000044;
pub const REG_WIN1V: u32 = 0x4000046;
pub const REG_WININ: u32 = 0x4000048;
pub const REG_WINOUT: u32 = 0x400004A;
pub const REG_MOSAIC: u32 = 0x400004C;
pub const REG_BLDCNT: u32 = 0x4000050;
pub const REG_BLDALPHA: u32 = 0x4000052;
pub const REG_BLDY: u32 = 0x4000054;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum VideoMode {
    Mode0, // BG0-BG3 text
    Mode1, // BG0-BG1 text, BG2 affine
    Mode2, // BG2-BG3 affine
    Mode3, // BG2 bitmap 240x160 16bpp
    Mode4, // BG2 bitmap 240x160 8bpp
    Mode5, // BG2 bitmap 160x128 16bpp
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum BgState {
    Idle,
    FetchMap,
    FetchTile,
    FetchBitmap,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct BackgroundRenderer {
    pub state: BgState,
    pub enabled: bool,
    pub start_cycle: usize,
    pub current_tile: usize,
    pub current_pixel: usize,
    pub cycles_in_tile: usize,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum SpriteState {
    Idle,
    FetchOamAttributes,
    FetchMatrix,
    FetchVram,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct SpriteRenderer {
    pub state: SpriteState,
    pub current_oam: usize,
    pub current_sprite_pixel: usize,
    pub active_sprites: Vec<usize>,
    pub oam_stall: bool,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum CompositeState {
    Idle,
    Compositing,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Compositor {
    pub state: CompositeState,
    pub current_pixel: usize,
    pub pram_cycle: usize, // 0-3 for A-B- pattern
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum PpuState {
    Visible,
    HBlank,
    VBlank,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct CycleState {
    pub scanline: usize,
    pub cycle: usize,
    pub state: PpuState,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LayerType {
    Background,
    Sprite,
    Backdrop,
}

#[derive(Debug, Clone, Copy)]
pub struct LayerInfo {
    layer_type: LayerType,
    layer_id: usize,
    palette_index: Option<u8>,
    priority: u8,
    color: Option<u16>, // Direct color for Mode 3
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Ppu {
    disabled: bool,

    // Cycle tracking
    cycle_state: CycleState,

    // Video mode (cached from DISPCNT for performance)
    video_mode: VideoMode,

    // Rendering stages
    bg_renderers: [BackgroundRenderer; 4],
    sprite_renderer: SpriteRenderer,
    compositor: Compositor,

    // Frame buffers
    have_frame: bool,
    #[serde(with = "serde_bytes")]
    fb_a: [u8; FRAMEBUFFER_SIZE],
    #[serde(with = "serde_bytes")]
    fb_b: [u8; FRAMEBUFFER_SIZE],
}

impl Default for Ppu {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for BackgroundRenderer {
    fn default() -> Self {
        Self {
            state: BgState::Idle,
            enabled: false,
            start_cycle: BG_START_CYCLE,
            current_tile: 0,
            current_pixel: 0,
            cycles_in_tile: 0,
        }
    }
}

impl Default for SpriteRenderer {
    fn default() -> Self {
        Self {
            state: SpriteState::Idle,
            current_oam: 0,
            current_sprite_pixel: 0,
            active_sprites: Vec::new(),
            oam_stall: false,
        }
    }
}

impl Default for Compositor {
    fn default() -> Self {
        Self {
            state: CompositeState::Idle,
            current_pixel: 0,
            pram_cycle: 0,
        }
    }
}

impl Ppu {
    pub fn new() -> Self {
        Ppu {
            disabled: true,
            cycle_state: CycleState {
                scanline: 0,
                cycle: 0,
                state: PpuState::Visible,
            },
            video_mode: VideoMode::Mode0,
            bg_renderers: [
                BackgroundRenderer::default(),
                BackgroundRenderer::default(),
                BackgroundRenderer::default(),
                BackgroundRenderer::default(),
            ],
            sprite_renderer: SpriteRenderer::default(),
            compositor: Compositor::default(),
            fb_a: [0; FRAMEBUFFER_SIZE],
            fb_b: [0; FRAMEBUFFER_SIZE],
            have_frame: false,
        }
    }

    #[inline]
    pub fn step(&mut self, mmio: &mut mmio::Mmio) {
        if self.disabled {
            return;
        }

        // Update registers from MMIO
        self.update_registers(mmio);

        // Process current scanline and cycle
        self.process_cycle(mmio);

        // Advance to next cycle
        self.advance_cycle();
    }

    #[inline]
    fn update_registers(&mut self, mmio: &mut mmio::Mmio) {
        use crate::memory::Addressable;

        let mmio_addr: &dyn Addressable = mmio;
        let dispcnt = mmio_addr.read16(REG_DISPCNT);
        let dispstat = mmio_addr.read16(REG_DISPSTAT);

        // Check if PPU is being enabled/disabled (bit 7 of DISPCNT)
        let should_be_enabled = (dispcnt >> 7) & 1 == 0; // 0 = enabled, 1 = forced blank
        if should_be_enabled && self.disabled {
            self.disabled = false;
        } else if !should_be_enabled && !self.disabled {
            self.disabled = true;
        }

        // Update video mode
        self.video_mode = match dispcnt & 0x7 {
            0 => VideoMode::Mode0,
            1 => VideoMode::Mode1,
            2 => VideoMode::Mode2,
            3 => VideoMode::Mode3,
            4 => VideoMode::Mode4,
            5 => VideoMode::Mode5,
            _ => VideoMode::Mode0,
        };

        // Update background renderers based on enabled backgrounds
        for i in 0..4 {
            self.bg_renderers[i].enabled = (dispcnt >> (8 + i)) & 1 != 0;

            // Update start cycle based on horizontal scrolling for text modes
            if matches!(self.video_mode, VideoMode::Mode0)
                || (matches!(self.video_mode, VideoMode::Mode1) && i < 2)
            {
                let hofs = self.get_bg_hofs_register(mmio, i);
                // Start time: 31 - 4 * (BG[x]HOFS mod 8)
                let sub_tile_offset = (hofs % 8) as usize;
                self.bg_renderers[i].start_cycle =
                    BG_START_CYCLE.saturating_sub(4 * sub_tile_offset);
            }
        }

        // Update VCOUNT
        let vcount = self.cycle_state.scanline as u16;
        let mmio_addr_mut: &mut dyn Addressable = mmio;
        mmio_addr_mut.write16(REG_VCOUNT, vcount);

        // Update DISPSTAT flags
        let mut new_dispstat = dispstat;

        // V-Blank flag (bit 0)
        if self.cycle_state.scanline >= SCREEN_HEIGHT {
            new_dispstat |= 1;
        } else {
            new_dispstat &= !1;
        }

        // H-Blank flag (bit 1)
        if self.cycle_state.cycle >= HBLANK_START {
            new_dispstat |= 2;
        } else {
            new_dispstat &= !2;
        }

        // V-Counter match flag (bit 2)
        let lyc = (new_dispstat >> 8) & 0xFF;
        if vcount == lyc {
            new_dispstat |= 4;
        } else {
            new_dispstat &= !4;
        }

        mmio_addr_mut.write16(REG_DISPSTAT, new_dispstat);
    }

    #[inline]
    fn get_bg_hofs_register(&self, mmio: &mmio::Mmio, bg_idx: usize) -> u16 {
        use crate::memory::Addressable;

        let mmio_addr: &dyn Addressable = mmio;
        match bg_idx {
            0 => mmio_addr.read16(REG_BG0HOFS),
            1 => mmio_addr.read16(REG_BG1HOFS),
            2 => mmio_addr.read16(REG_BG2HOFS),
            3 => mmio_addr.read16(REG_BG3HOFS),
            _ => 0,
        }
    }

    #[inline]
    fn process_cycle(&mut self, mmio: &mut mmio::Mmio) {
        match self.cycle_state.state {
            PpuState::Visible => {
                if self.cycle_state.scanline < SCREEN_HEIGHT {
                    self.process_visible_cycle(mmio);
                }
            }
            PpuState::HBlank => {
                self.process_hblank_cycle(mmio);
            }
            PpuState::VBlank => {
                self.process_vblank_cycle(mmio);
            }
        }
    }

    #[inline]
    fn process_visible_cycle(&mut self, mmio: &mut mmio::Mmio) {
        // Background rendering
        if self.cycle_state.cycle >= BG_START_CYCLE && self.cycle_state.cycle < HBLANK_START {
            self.process_background_rendering(mmio);
        }

        // Sprite rendering
        if self.cycle_state.cycle >= SPRITE_START_CYCLE {
            self.process_sprite_rendering(mmio);
        }

        // Compositing
        if self.cycle_state.cycle >= COMPOSITE_START_CYCLE && self.cycle_state.cycle < HBLANK_START
        {
            self.process_compositing(mmio);
        }
    }

    #[inline]
    fn process_background_rendering(&mut self, _mmio: &mut mmio::Mmio) {
        // This will be implemented in the next step
        // For now, just update states based on video mode and timing
        match self.video_mode {
            VideoMode::Mode0 => self.process_mode0_backgrounds(),
            VideoMode::Mode1 => self.process_mode1_backgrounds(),
            VideoMode::Mode2 => self.process_mode2_backgrounds(),
            VideoMode::Mode3 | VideoMode::Mode4 | VideoMode::Mode5 => {
                self.process_bitmap_background()
            }
        }
    }

    #[inline]
    fn process_sprite_rendering(&mut self, mmio: &mut mmio::Mmio) {
        use crate::memory::Addressable;

        // Sprite rendering starts at cycle 40 of previous scanline
        // and continues until h-blank or cycle 40 of current line
        let sprite_cycle = if self.cycle_state.cycle >= SPRITE_START_CYCLE {
            self.cycle_state.cycle - SPRITE_START_CYCLE
        } else {
            // Previous scanline sprite rendering
            return;
        };

        // Check if we should continue sprite rendering based on DISPCNT.bit5
        let mmio_addr: &dyn Addressable = mmio;
        let dispcnt = mmio_addr.read16(REG_DISPCNT);
        let h_blank_free_oam = (dispcnt >> 5) & 1 != 0;
        let max_sprite_cycle = if h_blank_free_oam {
            HBLANK_START - SPRITE_START_CYCLE
        } else {
            1192 // Until cycle 40 of next scanline
        };

        if sprite_cycle >= max_sprite_cycle {
            return;
        }

        self.process_sprite_pipeline(mmio, sprite_cycle);
    }

    #[inline]
    fn process_sprite_pipeline(&mut self, mmio: &mut mmio::Mmio, sprite_cycle: usize) {
        use crate::memory::Addressable;

        let sprite = &mut self.sprite_renderer;

        match sprite.state {
            SpriteState::Idle => {
                // Start processing OAM #0 if we're at the beginning
                if sprite_cycle == 0 {
                    sprite.current_oam = 0;
                    sprite.state = SpriteState::FetchOamAttributes;
                }
            }
            SpriteState::FetchOamAttributes => {
                self.process_oam_fetch_stage(mmio, sprite_cycle);
            }
            SpriteState::FetchMatrix => {
                self.process_matrix_fetch_stage(mmio, sprite_cycle);
            }
            SpriteState::FetchVram => {
                self.process_vram_fetch_stage(mmio, sprite_cycle);
            }
        }
    }

    #[inline]
    fn process_oam_fetch_stage(&mut self, mmio: &mut mmio::Mmio, sprite_cycle: usize) {
        use crate::memory::Addressable;

        let oam_cycle = sprite_cycle % 12; // OAM processing cycles
        let current_oam = self.sprite_renderer.current_oam;

        match oam_cycle {
            0 => {
                // Fetch sprite attributes #0 and #1 (32-bit read)
                let oam_addr = 0x07000000u32 + (current_oam as u32 * 8);
                let mmio_addr: &dyn Addressable = mmio;
                let attr01 = mmio_addr.read32(oam_addr);

                // Check if sprite is enabled and intersects current scanline
                let obj_mode = (attr01 >> 10) & 0x3;
                let is_disabled = obj_mode == 2;

                if !is_disabled && self.sprite_intersects_scanline(attr01) {
                    // Sprite should be rendered, continue to attribute #2
                    self.sprite_renderer.active_sprites.push(current_oam);
                }
            }
            2 => {
                // Fetch attribute #2 if sprite is active
                if self.sprite_renderer.active_sprites.contains(&current_oam) {
                    let oam_addr = 0x07000000u32 + (current_oam as u32 * 8) + 4;
                    let mmio_addr: &dyn Addressable = mmio;
                    let _attr2 = mmio_addr.read16(oam_addr);
                }
            }
            4 | 6 | 8 | 10 => {
                // Fetch matrix components for affine sprites
                if self.sprite_renderer.active_sprites.contains(&current_oam)
                    && self.is_sprite_affine(current_oam)
                {
                    self.sprite_renderer.state = SpriteState::FetchMatrix;
                }
            }
            _ => {}
        }

        // Move to next OAM entry
        if oam_cycle == 11 {
            self.sprite_renderer.current_oam += 1;
            if self.sprite_renderer.current_oam >= 128 {
                // All OAM entries processed
                self.sprite_renderer.current_oam = 0;
                self.sprite_renderer.state = SpriteState::FetchVram;
            }
        }
    }

    #[inline]
    fn process_matrix_fetch_stage(&mut self, mmio: &mut mmio::Mmio, _sprite_cycle: usize) {
        use crate::memory::Addressable;

        // Fetch 2x2 matrix components for affine sprites
        // PA, PB, PC, PD in cycles 4, 6, 8, 10
        let matrix_index = self.sprite_renderer.current_oam / 4; // Each matrix is shared by 4 sprites
        let matrix_base = 0x07000000u32 + 0x6 + (matrix_index as u32 * 0x20);

        let mmio_addr: &dyn Addressable = mmio;
        let _pa = mmio_addr.read16(matrix_base);
        let _pb = mmio_addr.read16(matrix_base + 8);
        let _pc = mmio_addr.read16(matrix_base + 16);
        let _pd = mmio_addr.read16(matrix_base + 24);

        // Matrix fetched, proceed to VRAM stage
        self.sprite_renderer.state = SpriteState::FetchVram;
    }

    #[inline]
    fn process_vram_fetch_stage(&mut self, mmio: &mut mmio::Mmio, sprite_cycle: usize) {
        use crate::memory::Addressable;

        let sprite = &mut self.sprite_renderer;

        // Process active sprites for VRAM fetching
        for &oam_idx in &sprite.active_sprites.clone() {
            if self.is_sprite_affine(oam_idx) {
                self.process_affine_sprite_vram(mmio, oam_idx, sprite_cycle);
            } else {
                self.process_regular_sprite_vram(mmio, oam_idx, sprite_cycle);
            }
        }
    }

    #[inline]
    fn process_regular_sprite_vram(
        &mut self,
        mmio: &mut mmio::Mmio,
        oam_idx: usize,
        sprite_cycle: usize,
    ) {
        use crate::memory::Addressable;

        // Regular sprites: width/2 16-bit VRAM reads (every 2 cycles)
        // Each read fetches 2 pixels
        if sprite_cycle % 2 == 0 {
            let oam_addr = 0x07000000u32 + (oam_idx as u32 * 8);
            let mmio_addr: &dyn Addressable = mmio;
            let _attr01 = mmio_addr.read32(oam_addr);
            let attr2 = mmio_addr.read16(oam_addr + 4);

            // Calculate VRAM address based on attributes
            let tile_number = attr2 & 0x3FF;
            let _vram_addr = 0x06010000u32 + (tile_number as u32 * 32);

            // Fetch tile data (placeholder)
            // let tile_data = mmio_addr.read16(vram_addr);
        }
    }

    #[inline]
    fn process_affine_sprite_vram(
        &mut self,
        mmio: &mut mmio::Mmio,
        oam_idx: usize,
        sprite_cycle: usize,
    ) {
        use crate::memory::Addressable;

        // Affine sprites: width VRAM reads (every 2 cycles after initial 2 cycles)
        // Each read fetches 1 pixel
        if sprite_cycle >= 2 && sprite_cycle % 2 == 0 {
            let oam_addr = 0x07000000u32 + (oam_idx as u32 * 8);
            let mmio_addr: &dyn Addressable = mmio;
            let _attr01 = mmio_addr.read32(oam_addr);
            let attr2 = mmio_addr.read16(oam_addr + 4);

            // Calculate VRAM address for affine sprite
            let tile_number = attr2 & 0x3FF;
            let _vram_addr = 0x06010000u32 + (tile_number as u32 * 32);

            // Apply affine transformation and fetch pixel
            // let pixel_data = mmio_addr.read16(vram_addr);
        }
    }

    #[inline]
    // Helper functions for sprite processing
    fn sprite_intersects_scanline(&self, attr01: u32) -> bool {
        let y_pos = (attr01 & 0xFF) as u8;
        let shape = (attr01 >> 14) & 0x3;
        let size = (attr01 >> 14) & 0x3;

        // Calculate sprite height based on shape and size
        let height = match (shape, size) {
            (0, 0) => 8,  // Square 8x8
            (0, 1) => 16, // Square 16x16
            (0, 2) => 32, // Square 32x32
            (0, 3) => 64, // Square 64x64
            (1, 0) => 8,  // Horizontal 16x8
            (1, 1) => 8,  // Horizontal 32x8
            (1, 2) => 16, // Horizontal 32x16
            (1, 3) => 32, // Horizontal 64x32
            (2, 0) => 16, // Vertical 8x16
            (2, 1) => 32, // Vertical 8x32
            (2, 2) => 32, // Vertical 16x32
            (2, 3) => 64, // Vertical 32x64
            _ => 8,
        };

        let current_line = self.cycle_state.scanline as u8;
        current_line >= y_pos && current_line < y_pos.wrapping_add(height as u8)
    }

    #[inline]
    fn is_sprite_affine(&self, _oam_idx: usize) -> bool {
        // This would check the affine flag in OAM attributes
        // For now, return false (placeholder)
        false
    }

    #[inline]
    fn process_compositing(&mut self, mmio: &mut mmio::Mmio) {
        use crate::memory::Addressable;

        // Compositing starts at cycle 46 and processes one pixel every 4 cycles
        if self.cycle_state.cycle < COMPOSITE_START_CYCLE {
            return;
        }

        let composite_cycle = self.cycle_state.cycle - COMPOSITE_START_CYCLE;

        // Process one pixel every 4 cycles
        if composite_cycle % 4 == 0 {
            let pixel_x = composite_cycle / 4;
            if pixel_x < SCREEN_WIDTH {
                self.process_pixel_composite(mmio, pixel_x);
            }
        }
    }

    #[inline]
    fn process_pixel_composite(&mut self, mmio: &mut mmio::Mmio, pixel_x: usize) {
        use crate::memory::Addressable;

        let compositor = &mut self.compositor;
        compositor.current_pixel = pixel_x;

        // Get the top two opaque layers for this pixel
        let (top_layer, second_layer) = self.get_top_layers(pixel_x);

        // PRAM access pattern: A-B- A-B- A-B- ...
        let pram_cycle = (pixel_x % 4) / 2; // 0 for A cycles, 1 for B cycles

        match pram_cycle {
            0 => {
                // Top-most layer PRAM fetch
                if let Some(layer_info) = top_layer {
                    self.fetch_layer_color(mmio, layer_info, true);
                }
            }
            1 => {
                // Second top-most layer PRAM fetch (if conditions are met)
                if let Some(layer_info) = second_layer {
                    if self.should_fetch_second_layer(&layer_info) {
                        self.fetch_layer_color(mmio, layer_info, false);
                    }
                }
            }
            _ => {}
        }

        // Composite the final pixel color
        self.composite_final_pixel(pixel_x, top_layer, second_layer);
    }

    #[inline]
    fn get_top_layers(&self, _pixel_x: usize) -> (Option<LayerInfo>, Option<LayerInfo>) {
        // This would determine the top two opaque layers for the given pixel
        // by checking background pixels, sprite pixels, and backdrop
        // For now, return placeholders
        (None, None)
    }

    #[inline]
    fn should_fetch_second_layer(&self, layer_info: &LayerInfo) -> bool {
        // Check the conditions for second layer PRAM access:
        // 1. not (PPU is in Mode 3 and the layer is BG2)
        // 2. Alpha-blending is enabled
        // 3. The top-most layer is selected as the first blend target
        // 4. The second top-most layer selected as the second blend target

        // Condition 1: Mode 3 BG2 special case
        if matches!(self.video_mode, VideoMode::Mode3)
            && layer_info.layer_type == LayerType::Background
            && layer_info.layer_id == 2
        {
            return false;
        }

        // Check alpha blending configuration from BLDCNT register
        let bldcnt = self.get_bldcnt_register();
        let alpha_blend_enabled = (bldcnt & 0x3) == 1;

        if !alpha_blend_enabled {
            return false;
        }

        // Check if layers are selected as blend targets
        // This would need more detailed implementation
        true
    }

    #[inline]
    fn fetch_layer_color(
        &mut self,
        mmio: &mut mmio::Mmio,
        layer_info: LayerInfo,
        is_top_layer: bool,
    ) {
        use crate::memory::Addressable;

        match layer_info.layer_type {
            LayerType::Background => {
                // For palette-based backgrounds, fetch from PRAM
                if layer_info.palette_index.is_some() {
                    let palette_addr =
                        0x05000000u32 + (layer_info.palette_index.unwrap() as u32 * 2);
                    let mmio_addr: &dyn Addressable = mmio;
                    let _color = mmio_addr.read16(palette_addr);

                    // Store color for compositing
                    if is_top_layer {
                        // Store top layer color
                    } else {
                        // Store second layer color
                    }
                }
            }
            LayerType::Sprite => {
                // Sprite palette access
                if layer_info.palette_index.is_some() {
                    let palette_addr =
                        0x05000200u32 + (layer_info.palette_index.unwrap() as u32 * 2);
                    let mmio_addr: &dyn Addressable = mmio;
                    let _color = mmio_addr.read16(palette_addr);
                }
            }
            LayerType::Backdrop => {
                // Backdrop color from palette index 0
                let mmio_addr: &dyn Addressable = mmio;
                let _color = mmio_addr.read16(0x05000000u32);
            }
        }
    }

    #[inline]
    fn composite_final_pixel(
        &mut self,
        pixel_x: usize,
        _top_layer: Option<LayerInfo>,
        _second_layer: Option<LayerInfo>,
    ) {
        // Composite the final pixel based on the fetched colors and blending mode
        // For now, just set a placeholder color
        let scanline = self.cycle_state.scanline;
        if scanline < SCREEN_HEIGHT && pixel_x < SCREEN_WIDTH {
            let fb_index = (scanline * SCREEN_WIDTH + pixel_x) * 4;
            if fb_index + 3 < FRAMEBUFFER_SIZE {
                // RGBA placeholder - would use actual composited color
                self.fb_a[fb_index] = 0x80; // R
                self.fb_a[fb_index + 1] = 0x80; // G
                self.fb_a[fb_index + 2] = 0x80; // B
                self.fb_a[fb_index + 3] = 0xFF; // A
            }
        }
    }

    #[inline]
    fn get_bldcnt_register(&self) -> u16 {
        // This would read the BLDCNT register for blending configuration
        // For now, return 0 (no blending)
        0
    }

    #[inline]
    fn process_mode0_backgrounds(&mut self) {
        // Mode 0: BG0-BG3 text mode
        // Each background has specific timing patterns for 4BPP/8BPP

        for bg_idx in 0..4 {
            if !self.is_bg_enabled(bg_idx) {
                continue;
            }

            let bg = &mut self.bg_renderers[bg_idx];
            let cycle_offset = self.cycle_state.cycle.saturating_sub(bg.start_cycle);

            // Text mode: 32 cycles per tile, renders 301 tiles per scanline normally
            // Pattern repeats every 32 cycles
            let tile_cycle = cycle_offset % 32;

            match bg_idx {
                0 => self.process_bg0_text_cycle(tile_cycle),
                1 => self.process_bg1_text_cycle(tile_cycle),
                2 => self.process_bg2_text_cycle(tile_cycle),
                3 => self.process_bg3_text_cycle(tile_cycle),
                _ => unreachable!(),
            }
        }
    }

    #[inline]
    fn process_mode1_backgrounds(&mut self) {
        // Mode 1: BG0-BG1 text, BG2 affine
        for bg_idx in 0..2 {
            if self.is_bg_enabled(bg_idx) {
                let bg = &mut self.bg_renderers[bg_idx];
                let cycle_offset = self.cycle_state.cycle.saturating_sub(bg.start_cycle);
                let tile_cycle = cycle_offset % 32;

                match bg_idx {
                    0 => self.process_bg0_text_cycle(tile_cycle),
                    1 => self.process_bg1_text_cycle(tile_cycle),
                    _ => unreachable!(),
                }
            }
        }

        // BG2 affine
        if self.is_bg_enabled(2) {
            self.process_bg2_affine_cycle();
        }
    }

    #[inline]
    fn process_mode2_backgrounds(&mut self) {
        // Mode 2: BG2-BG3 affine tilemap
        // Every 4 cycles: one BG3 pixel, then one BG2 pixel
        let cycle_in_pattern = self.cycle_state.cycle % 4;

        match cycle_in_pattern {
            0 => {
                // BG3: Fetch map entry
                if self.is_bg_enabled(3) {
                    self.bg_renderers[3].state = BgState::FetchMap;
                }
            }
            1 => {
                // BG3: Fetch tile data
                if self.is_bg_enabled(3) {
                    self.bg_renderers[3].state = BgState::FetchTile;
                }
            }
            2 => {
                // BG2: Fetch map entry
                if self.is_bg_enabled(2) {
                    self.bg_renderers[2].state = BgState::FetchMap;
                }
            }
            3 => {
                // BG2: Fetch tile data
                if self.is_bg_enabled(2) {
                    self.bg_renderers[2].state = BgState::FetchTile;
                }
            }
            _ => {}
        }
    }

    #[inline]
    fn process_bitmap_background(&mut self) {
        // Modes 3-5: Bitmap modes (BG2 only)
        // Every 4 cycles: fetch one bitmap pixel
        if self.cycle_state.cycle % 4 == 3 && self.is_bg_enabled(2) {
            self.bg_renderers[2].state = BgState::FetchBitmap;
            self.bg_renderers[2].current_pixel += 1;
        }
    }

    #[inline]
    // Text mode background cycle processing
    fn process_bg0_text_cycle(&mut self, tile_cycle: usize) {
        // BG0: M--- T--- ---- ---- ---- T--- ---- ---- (4BPP)
        // BG0: M--- T--- ---- T--- ---- T--- ---- T--- (8BPP)
        match tile_cycle {
            0 => self.bg_renderers[0].state = BgState::FetchMap,
            4 => self.bg_renderers[0].state = BgState::FetchTile,
            20 => self.bg_renderers[0].state = BgState::FetchTile,
            12 | 16 | 24 | 28 => {
                // Additional tile fetches for 8BPP mode
                if self.is_bg_8bpp(0) {
                    self.bg_renderers[0].state = BgState::FetchTile;
                }
            }
            _ => self.bg_renderers[0].state = BgState::Idle,
        }
    }

    #[inline]
    fn process_bg1_text_cycle(&mut self, tile_cycle: usize) {
        // BG1: -M-- -T-- ---- ---- ---- -T-- ---- ---- (4BPP)
        // BG1: -M-- -T-- ---- -T-- ---- -T-- ---- -T-- (8BPP)
        match tile_cycle {
            1 => self.bg_renderers[1].state = BgState::FetchMap,
            5 => self.bg_renderers[1].state = BgState::FetchTile,
            21 => self.bg_renderers[1].state = BgState::FetchTile,
            13 | 17 | 25 | 29 => {
                if self.is_bg_8bpp(1) {
                    self.bg_renderers[1].state = BgState::FetchTile;
                }
            }
            _ => self.bg_renderers[1].state = BgState::Idle,
        }
    }

    #[inline]
    fn process_bg2_text_cycle(&mut self, tile_cycle: usize) {
        // BG2: --M- --T- ---- ---- ---- --T- ---- ---- (4BPP)
        // BG2: --M- --T- ---- --T- ---- --T- ---- --T- (8BPP)
        match tile_cycle {
            2 => self.bg_renderers[2].state = BgState::FetchMap,
            6 => self.bg_renderers[2].state = BgState::FetchTile,
            22 => self.bg_renderers[2].state = BgState::FetchTile,
            14 | 18 | 26 | 30 => {
                if self.is_bg_8bpp(2) {
                    self.bg_renderers[2].state = BgState::FetchTile;
                }
            }
            _ => self.bg_renderers[2].state = BgState::Idle,
        }
    }

    #[inline]
    fn process_bg3_text_cycle(&mut self, tile_cycle: usize) {
        // BG3: ---M ---T ---- ---- ---- ---T ---- ---- (4BPP)
        // BG3: ---M ---T ---- ---T ---- ---T ---- ---T (8BPP)
        match tile_cycle {
            3 => self.bg_renderers[3].state = BgState::FetchMap,
            7 => self.bg_renderers[3].state = BgState::FetchTile,
            23 => self.bg_renderers[3].state = BgState::FetchTile,
            15 | 19 | 27 | 31 => {
                if self.is_bg_8bpp(3) {
                    self.bg_renderers[3].state = BgState::FetchTile;
                }
            }
            _ => self.bg_renderers[3].state = BgState::Idle,
        }
    }

    #[inline]
    fn process_bg2_affine_cycle(&mut self) {
        // Affine mode: pixel-by-pixel rendering every 4 cycles
        if self.cycle_state.cycle % 4 == 0 {
            self.bg_renderers[2].state = BgState::FetchMap;
        } else if self.cycle_state.cycle % 4 == 1 {
            self.bg_renderers[2].state = BgState::FetchTile;
        }
    }

    // Helper functions
    #[inline]
    fn is_bg_enabled(&self, bg_idx: usize) -> bool {
        // Check if background is enabled (updated from DISPCNT in update_registers)
        if bg_idx < 4 {
            self.bg_renderers[bg_idx].enabled
        } else {
            false
        }
    }

    #[inline]
    fn is_bg_8bpp(&self, _bg_idx: usize) -> bool {
        // This would need to check the BGxCNT registers
        // For now, assume 4BPP (placeholder)
        false
    }

    #[inline]
    pub fn set_enabled(&mut self, enabled: bool) {
        self.disabled = !enabled;
    }

    #[inline]
    pub fn is_in_vblank(&self) -> bool {
        self.cycle_state.scanline >= SCREEN_HEIGHT
    }

    #[inline]
    pub fn is_in_hblank(&self) -> bool {
        self.cycle_state.cycle >= HBLANK_START
    }

    #[inline]
    pub fn get_current_scanline(&self) -> usize {
        self.cycle_state.scanline
    }

    #[inline]
    pub fn get_current_cycle(&self) -> usize {
        self.cycle_state.cycle
    }

    // Debug information methods
    #[inline]
    pub fn get_bg_renderer_state(&self, bg_idx: usize) -> Option<&BackgroundRenderer> {
        if bg_idx < 4 {
            Some(&self.bg_renderers[bg_idx])
        } else {
            None
        }
    }

    #[inline]
    pub fn get_sprite_renderer_state(&self) -> &SpriteRenderer {
        &self.sprite_renderer
    }

    #[inline]
    pub fn get_compositor_state(&self) -> &Compositor {
        &self.compositor
    }

    #[inline]
    pub fn get_ppu_state(&self) -> &PpuState {
        &self.cycle_state.state
    }

    #[inline]
    fn process_hblank_cycle(&mut self, _mmio: &mut mmio::Mmio) {
        // H-blank processing
    }

    #[inline]
    fn process_vblank_cycle(&mut self, _mmio: &mut mmio::Mmio) {
        // V-blank processing
        if self.cycle_state.scanline == 227 && self.cycle_state.cycle == SPRITE_START_CYCLE {
            // Sprite rendering for line 0 starts during line 227
            self.process_sprite_rendering(_mmio);
        }
    }

    #[inline]
    fn advance_cycle(&mut self) {
        self.cycle_state.cycle += 1;

        if self.cycle_state.cycle >= CYCLES_PER_LINE {
            self.cycle_state.cycle = 0;
            self.cycle_state.scanline += 1;

            if self.cycle_state.scanline >= TOTAL_LINES {
                self.cycle_state.scanline = 0;
                self.have_frame = true;
                // Swap frame buffers
                std::mem::swap(&mut self.fb_a, &mut self.fb_b);
            }
        }

        // Update PPU state based on position
        if self.cycle_state.scanline < SCREEN_HEIGHT {
            if self.cycle_state.cycle < HBLANK_START {
                self.cycle_state.state = PpuState::Visible;
            } else {
                self.cycle_state.state = PpuState::HBlank;
            }
        } else {
            self.cycle_state.state = PpuState::VBlank;
        }
    }

    #[inline]
    pub fn is_disabled(&self) -> bool {
        self.disabled
    }

    #[inline]
    pub fn get_cycle_state(&self) -> &CycleState {
        &self.cycle_state
    }

    #[inline]
    pub fn get_video_mode(&self) -> &VideoMode {
        &self.video_mode
    }

    #[inline]
    pub fn get_scanline(&self) -> usize {
        self.cycle_state.scanline
    }

    #[inline]
    pub fn get_cycle(&self) -> usize {
        self.cycle_state.cycle
    }

    #[inline]
    pub fn has_frame(&self) -> bool {
        self.have_frame
    }

    #[inline]
    pub fn get_frame(&mut self) -> [u8; FRAMEBUFFER_SIZE] {
        self.have_frame = false;
        self.fb_b
    }
}
