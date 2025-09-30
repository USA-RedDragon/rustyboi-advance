use crate::ui::Gui;
use egui::Context;
use rustyboi_advance_core_lib::{cpu, gba};

impl Gui {
    pub(crate) fn render_stack_explorer_panel(
        &mut self,
        ctx: &Context,
        registers: Option<&cpu::registers::Registers>,
        gb: Option<&gba::GBA>,
    ) {
        if let Some(regs) = registers
            && let Some(gb_ref) = gb
        {
            egui::Window::new("Stack Explorer")
                .default_pos([220.0, 50.0])
                .default_size([180.0, 400.0])
                .collapsible(true)
                .resizable(false)
                .frame(egui::Frame::window(&ctx.style()).fill(crate::ui::PANEL_BACKGROUND))
                .show(ctx, |ui| {
                    ui.set_width(160.0);

                    let sp = regs.sp();
                    ui.monospace(
                        egui::RichText::new(format!("SP: {:08X}", sp)).color(egui::Color32::YELLOW),
                    );

                    if ui.button("↑ Scroll Up").clicked() && self.stack_scroll_offset < 100 {
                        // Reasonable upper limit
                        self.stack_scroll_offset = self.stack_scroll_offset.saturating_add(1);
                    }

                    ui.separator();

                    // Show stack contents around SP with scroll offset
                    let sp_addr: u32 = sp.into(); // Convert to u32 for address arithmetic
                    let base_start = sp_addr.saturating_sub(16); // 4 entries above SP (16 bytes, 4 bytes per ARM word)
                    let scroll_adjustment = (self.stack_scroll_offset as i32) * 4; // 4 bytes per scroll step for ARM
                    let start_addr = if scroll_adjustment >= 0 {
                        base_start.saturating_sub(scroll_adjustment as u32)
                    } else {
                        base_start.saturating_add((-scroll_adjustment) as u32)
                    };
                    let end_addr = std::cmp::min(start_addr.saturating_add(32), 0xFFFFFFF); // Show 8 entries (32 bytes)

                    for addr in (start_addr..=end_addr).step_by(4) {
                        // ARM uses 4-byte words
                        let val1 = gb_ref.read_memory(addr);
                        let val2 = gb_ref.read_memory(addr + 1);
                        let val3 = gb_ref.read_memory(addr + 2);
                        let val4 = gb_ref.read_memory(addr + 3);
                        let word_val = (val1 as u32)
                            | ((val2 as u32) << 8)
                            | ((val3 as u32) << 16)
                            | ((val4 as u32) << 24);

                        let color = if addr == sp_addr {
                            egui::Color32::YELLOW // Highlight current SP
                        } else if addr < sp_addr {
                            egui::Color32::LIGHT_GRAY // Above SP (older entries)
                        } else {
                            egui::Color32::GRAY // Below SP (unused)
                        };

                        let marker = if addr == sp_addr { "→" } else { " " };
                        ui.monospace(
                            egui::RichText::new(format!(
                                "{} {:08X}: {:08X}",
                                marker, addr, word_val
                            ))
                            .color(color),
                        );
                    }

                    ui.separator();

                    if ui.button("↓ Scroll Down").clicked() && self.stack_scroll_offset > -100 {
                        // Reasonable lower limit
                        self.stack_scroll_offset = self.stack_scroll_offset.saturating_sub(1);
                    }

                    // Reset button
                    ui.horizontal(|ui| {
                        if ui.button("Center on SP").clicked() {
                            self.stack_scroll_offset = 0;
                        }
                        ui.small(
                            egui::RichText::new(format!("Offset: {}", self.stack_scroll_offset))
                                .color(egui::Color32::LIGHT_GRAY),
                        );
                    });

                    ui.separator();
                    ui.small(
                        egui::RichText::new("Yellow = SP position")
                            .color(egui::Color32::LIGHT_GRAY),
                    );
                });
        }
    }
}
