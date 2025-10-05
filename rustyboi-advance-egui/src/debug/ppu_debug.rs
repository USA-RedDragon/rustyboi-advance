use crate::ui::Gui;
use egui::Context;
use rustyboi_advance_core_lib::gba;

impl Gui {
    pub(crate) fn render_ppu_debug_panel(&mut self, ctx: &Context, gba: Option<&gba::GBA>) {
        if let Some(gba_ref) = gba {
            let ppu = gba_ref.get_ppu();
            egui::Window::new("PPU Debug")
                .default_pos([640.0, 50.0])
                .default_size([250.0, 500.0])
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

                    ui.monospace(
                        egui::RichText::new(format!("Frame Ready: {}", ppu.frame_ready()))
                            .color(egui::Color32::WHITE),
                    );
                    ui.monospace(
                        egui::RichText::new(format!("H-Blank: {}, V-Blank: {}", 
                            if ppu.is_h_blank() { "Yes" } else { "No" },
                            if ppu.is_v_blank() { "Yes" } else { "No" }))
                            .color(egui::Color32::WHITE),
                    );
                    
                    let display_mode = gba_ref.get_ppu_display_mode();
                    let mode_description = match display_mode {
                        0 => "Mode 0: Tiled 240x160 8-bpp with 4 backgrounds",
                        1 => "Mode 1: Tiled 240x160 8-bpp with 3 backgrounds", 
                        2 => "Mode 2: Tiled 240x160 8-bpp with 2 backgrounds",
                        3 => "Mode 3: Bitmap 240x160 16-bpp with 1 background",
                        4 => "Mode 4: Bitmap 240x160 8-bpp with 2 backgrounds",
                        5 => "Mode 5: Bitmap 160x128 16-bpp with 2 backgrounds",
                        _ => "Invalid display mode",
                    };
                    ui.monospace(
                        egui::RichText::new(format!("Display: {}", mode_description))
                            .color(egui::Color32::YELLOW),
                    );
                    
                    ui.monospace(
                        egui::RichText::new(format!("Ticks: {}", ppu.get_ticks()))
                            .color(egui::Color32::WHITE),
                    );
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
                    ui.small(
                        egui::RichText::new("Timing Information")
                            .color(egui::Color32::LIGHT_BLUE),
                    );
                    
                    // Add cycle and timing information
                    ui.monospace(
                        egui::RichText::new(format!("Cycle: {}", gba_ref.get_ppu_cycle()))
                            .color(egui::Color32::WHITE),
                    );
                    ui.monospace(
                        egui::RichText::new(format!("Pixel Index: {}", gba_ref.get_ppu_pixel_index()))
                            .color(egui::Color32::WHITE),
                    );
                    ui.monospace(
                        egui::RichText::new(format!("Scanline: {}/228", gba_ref.get_ppu_scanline_index()))
                            .color(egui::Color32::WHITE),
                    );
                    
                    // Add progress bars for visual representation
                    let pixel_progress = gba_ref.get_ppu_pixel_index() as f32 / 308.0; // 240 visible + 68 H-blank
                    ui.add(egui::ProgressBar::new(pixel_progress)
                        .text(format!("Pixel: {}/308", gba_ref.get_ppu_pixel_index()))
                        .desired_width(200.0));
                    
                    let scanline_progress = gba_ref.get_ppu_scanline_index() as f32 / 228.0; // 160 visible + 68 V-blank
                    ui.add(egui::ProgressBar::new(scanline_progress)
                        .text(format!("Scanline: {}/228", gba_ref.get_ppu_scanline_index()))
                        .desired_width(200.0));

                    ui.separator();
                    ui.small(
                        egui::RichText::new("Register Information")
                            .color(egui::Color32::LIGHT_BLUE),
                    );
                    
                    // Display register values
                    let dispcnt = gba_ref.get_ppu_dispcnt();
                    ui.monospace(
                        egui::RichText::new(format!("DISPCNT: 0x{:04X}", dispcnt))
                            .color(egui::Color32::WHITE),
                    );
                    
                    let dispstat = gba_ref.get_ppu_dispstat();
                    ui.monospace(
                        egui::RichText::new(format!("DISPSTAT: 0x{:02X}", dispstat))
                            .color(egui::Color32::WHITE),
                    );
                    
                    let vcount = gba_ref.get_ppu_vcount();
                    ui.monospace(
                        egui::RichText::new(format!("VCOUNT: {}", vcount))
                            .color(egui::Color32::WHITE),
                    );

                    ui.separator();
                    ui.small(
                        egui::RichText::new("Configuration")
                            .color(egui::Color32::LIGHT_BLUE),
                    );
                    
                    ui.monospace(
                        egui::RichText::new(format!("Debug Mode: {}", 
                            if gba_ref.get_ppu_debug_enabled() { "ON" } else { "OFF" }))
                            .color(if gba_ref.get_ppu_debug_enabled() {
                                egui::Color32::LIGHT_GREEN
                            } else {
                                egui::Color32::GRAY
                            }),
                    );
                    
                    ui.monospace(
                        egui::RichText::new(format!("Scale Factor: {:.1}x", gba_ref.get_ppu_scale()))
                            .color(egui::Color32::WHITE),
                    );

                    ui.separator();
                    ui.small(
                        egui::RichText::new("PPU Debug Information")
                            .color(egui::Color32::LIGHT_GRAY),
                    );
                });
        }
    }
}
