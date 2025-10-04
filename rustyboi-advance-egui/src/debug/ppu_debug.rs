use crate::ui::Gui;
use egui::Context;
use rustyboi_advance_core_lib::{gba, ppu};

impl Gui {
    pub(crate) fn render_ppu_debug_panel(&mut self, ctx: &Context, gba: Option<&gba::GBA>) {
        if let Some(gba_ref) = gba {
            let ppu = gba_ref.get_ppu();
            egui::Window::new("PPU Debug")
                .default_pos([640.0, 50.0])
                .default_size([250.0, 600.0])
                .collapsible(true)
                .resizable(false)
                .frame(egui::Frame::window(&ctx.style()).fill(crate::ui::PANEL_BACKGROUND))
                .show(ctx, |ui| {
                    ui.set_width(230.0);

                    // PPU Status
                    ui.monospace(
                        egui::RichText::new(format!(
                            "Disabled: {}",
                            if ppu.is_disabled() { "YES" } else { "NO" }
                        ))
                        .color(if ppu.is_disabled() {
                            egui::Color32::LIGHT_RED
                        } else {
                            egui::Color32::LIGHT_GREEN
                        }),
                    );

                    // Video Mode
                    let video_mode_str = match ppu.get_video_mode() {
                        ppu::VideoMode::Mode0 => "Mode 0 (Text BG0-3)",
                        ppu::VideoMode::Mode1 => "Mode 1 (Text BG0-1, Affine BG2)",
                        ppu::VideoMode::Mode2 => "Mode 2 (Affine BG2-3)",
                        ppu::VideoMode::Mode3 => "Mode 3 (Bitmap 240x160 16bpp)",
                        ppu::VideoMode::Mode4 => "Mode 4 (Bitmap 240x160 8bpp)",
                        ppu::VideoMode::Mode5 => "Mode 5 (Bitmap 160x128 16bpp)",
                    };
                    ui.monospace(
                        egui::RichText::new(format!("Video Mode: {}", video_mode_str))
                            .color(egui::Color32::LIGHT_BLUE),
                    );

                    // Scanline and Cycle Information
                    ui.monospace(
                        egui::RichText::new(format!("Scanline: {}", ppu.get_current_scanline()))
                            .color(egui::Color32::WHITE),
                    );
                    ui.monospace(
                        egui::RichText::new(format!("Cycle: {}", ppu.get_current_cycle()))
                            .color(egui::Color32::WHITE),
                    );

                    // PPU State
                    let cycle_state = ppu.get_cycle_state();
                    let ppu_state_str = match cycle_state.state {
                        ppu::PpuState::Visible => "Visible",
                        ppu::PpuState::HBlank => "H-Blank",
                        ppu::PpuState::VBlank => "V-Blank",
                    };
                    ui.monospace(
                        egui::RichText::new(format!("PPU State: {}", ppu_state_str))
                            .color(match cycle_state.state {
                                ppu::PpuState::Visible => egui::Color32::LIGHT_GREEN,
                                ppu::PpuState::HBlank => egui::Color32::YELLOW,
                                ppu::PpuState::VBlank => egui::Color32::LIGHT_RED,
                            }),
                    );

                    // Frame status
                    ui.monospace(
                        egui::RichText::new(format!(
                            "Has Frame: {}",
                            if ppu.has_frame() { "YES" } else { "NO" }
                        ))
                        .color(if ppu.has_frame() {
                            egui::Color32::LIGHT_GREEN
                        } else {
                            egui::Color32::GRAY
                        }),
                    );

                    ui.separator();
                    
                    // Background Renderers Status
                    ui.small(
                        egui::RichText::new("Background Renderers")
                            .color(egui::Color32::LIGHT_GRAY),
                    );
                    
                    for i in 0..4 {
                        if let Some(bg_renderer) = ppu.get_bg_renderer_state(i) {
                            let bg_state_str = match bg_renderer.state {
                                ppu::BgState::Idle => "Idle",
                                ppu::BgState::FetchMap => "Fetch Map",
                                ppu::BgState::FetchTile => "Fetch Tile",
                                ppu::BgState::FetchBitmap => "Fetch Bitmap",
                            };
                            
                            ui.monospace(
                                egui::RichText::new(format!(
                                    "BG{}: {} | {}",
                                    i,
                                    if bg_renderer.enabled { "ON" } else { "OFF" },
                                    bg_state_str
                                ))
                                .color(if bg_renderer.enabled {
                                    egui::Color32::LIGHT_GREEN
                                } else {
                                    egui::Color32::GRAY
                                })
                                .size(10.0),
                            );
                        }
                    }

                    ui.separator();
                    
                    // Sprite Renderer Status
                    ui.small(
                        egui::RichText::new("Sprite Renderer")
                            .color(egui::Color32::LIGHT_GRAY),
                    );
                    
                    let sprite_renderer = ppu.get_sprite_renderer_state();
                    let sprite_state_str = match sprite_renderer.state {
                        ppu::SpriteState::Idle => "Idle",
                        ppu::SpriteState::FetchOamAttributes => "Fetch OAM",
                        ppu::SpriteState::FetchMatrix => "Fetch Matrix",
                        ppu::SpriteState::FetchVram => "Fetch VRAM",
                    };
                    
                    ui.monospace(
                        egui::RichText::new(format!("State: {}", sprite_state_str))
                            .color(egui::Color32::WHITE)
                            .size(10.0),
                    );
                    ui.monospace(
                        egui::RichText::new(format!("Current OAM: {}", sprite_renderer.current_oam))
                            .color(egui::Color32::WHITE)
                            .size(10.0),
                    );
                    ui.monospace(
                        egui::RichText::new(format!("Active Sprites: {}", sprite_renderer.active_sprites.len()))
                            .color(egui::Color32::WHITE)
                            .size(10.0),
                    );

                    ui.separator();
                    
                    // Compositor Status
                    ui.small(
                        egui::RichText::new("Compositor")
                            .color(egui::Color32::LIGHT_GRAY),
                    );
                    
                    let compositor = ppu.get_compositor_state();
                    let compositor_state_str = match compositor.state {
                        ppu::CompositeState::Idle => "Idle",
                        ppu::CompositeState::Compositing => "Compositing",
                    };
                    
                    ui.monospace(
                        egui::RichText::new(format!("State: {}", compositor_state_str))
                            .color(egui::Color32::WHITE)
                            .size(10.0),
                    );
                    ui.monospace(
                        egui::RichText::new(format!("Current Pixel: {}", compositor.current_pixel))
                            .color(egui::Color32::WHITE)
                            .size(10.0),
                    );
                });
        }
    }
}
