use crate::ui::Gui;
use egui::Context;
use rustyboi_advance_core_lib::{gba, ppu};

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

                    let state_str = match ppu.get_state() {
                        ppu::State::NoOperation => "No Operation",
                    };
                    ui.monospace(
                        egui::RichText::new(format!("State: {}", state_str))
                            .color(egui::Color32::WHITE),
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
                        egui::RichText::new("PPU Debug Information")
                            .color(egui::Color32::LIGHT_GRAY),
                    );
                });
        }
    }
}
