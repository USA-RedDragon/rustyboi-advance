use crate::actions::GuiAction;
use crate::file_dialog::{self, FileDialogBuilder};
use egui::Context;
use rustyboi_advance_core_lib::{cpu, gba};
use std::env;
use std::sync::{Arc, Mutex};
use web_time::{SystemTime, UNIX_EPOCH};

pub const PANEL_BACKGROUND: egui::Color32 = egui::Color32::from_rgba_premultiplied(64, 64, 64, 220);

pub struct Gui {
    error_message: Option<String>,
    status_message: Option<String>,
    show_cpu_registers: bool,
    show_stack_explorer: bool,
    show_memory_explorer: bool,
    show_ppu_debug: bool,
    show_keybind_settings: bool,
    show_breakpoint_panel: bool,
    breakpoint_address_input: String,
    pub(super) stack_scroll_offset: i32,
    pub(super) memory_explorer_address: String,
    pub(super) memory_explorer_parsed_address: u32,
    pub(super) memory_scroll_offset: i16,
    pub(super) step_count: u32,
    // Button hold state tracking
    pub(super) step_cycles_held_frames: u32,
    pub(super) step_frames_held_frames: u32,
    // CPU register view mode selection
    pub(super) selected_cpu_mode: CpuModeSelection,
    // File dialog result tracking
    pending_dialog_result: Arc<Mutex<Option<GuiAction>>>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum CpuModeSelection {
    Auto,
    User,
    Fiq,
    Irq,
    Service,
    Abort,
    Undefined,
    System,
}

impl CpuModeSelection {
    pub fn to_mode(self, current_mode: cpu::registers::Mode) -> cpu::registers::Mode {
        match self {
            CpuModeSelection::Auto => current_mode,
            CpuModeSelection::User => cpu::registers::Mode::User,
            CpuModeSelection::Fiq => cpu::registers::Mode::Fiq,
            CpuModeSelection::Irq => cpu::registers::Mode::Irq,
            CpuModeSelection::Service => cpu::registers::Mode::Service,
            CpuModeSelection::Abort => cpu::registers::Mode::Abort,
            CpuModeSelection::Undefined => cpu::registers::Mode::Undefined,
            CpuModeSelection::System => cpu::registers::Mode::System,
        }
    }
}

impl Default for Gui {
    fn default() -> Self {
        Self::new()
    }
}

impl Gui {
    pub fn new() -> Self {
        Self {
            error_message: None,
            status_message: None,
            show_cpu_registers: true,
            show_stack_explorer: false,
            show_memory_explorer: false,
            show_ppu_debug: false,
            show_keybind_settings: false,
            show_breakpoint_panel: false,
            breakpoint_address_input: String::from("00000000"),
            stack_scroll_offset: 0,
            memory_explorer_address: String::from("00000000"),
            memory_explorer_parsed_address: 0x00000000,
            memory_scroll_offset: 0,
            step_count: 1,
            step_cycles_held_frames: 0,
            step_frames_held_frames: 0,
            selected_cpu_mode: CpuModeSelection::Auto,
            pending_dialog_result: Arc::new(Mutex::new(None)),
        }
    }

    /// Create the UI using egui.
    pub fn ui(
        &mut self,
        ctx: &Context,
        paused: bool,
        registers: Option<&cpu::registers::Registers>,
        gba: Option<&gba::GBA>,
    ) -> (Option<GuiAction>, bool) {
        let mut action = None;
        let mut any_menu_open = false;

        // Check for pending dialog results first
        if let Ok(mut pending) = self.pending_dialog_result.try_lock()
            && let Some(pending_action) = pending.take()
        {
            action = Some(pending_action);
        }

        self.render_menu_bar(ctx, &mut action, &mut any_menu_open, paused);
        self.render_debug_panels(ctx, registers, gba, &mut action, paused);
        self.render_status_panel(ctx);
        self.render_error_panel(ctx, &mut action);

        (action, any_menu_open)
    }

    fn render_menu_bar(
        &mut self,
        ctx: &Context,
        action: &mut Option<GuiAction>,
        any_menu_open: &mut bool,
        paused: bool,
    ) {
        egui::TopBottomPanel::top("menubar_container").show(ctx, |ui| {
            egui::MenuBar::new().ui(ui, |ui| {
                ui.menu_button("File", |ui| {
                    *any_menu_open = true;
                    if ui.button("Load ROM").clicked() {
                        let mut dialog = file_dialog::new()
                            .add_filter("Game Boy Advance ROM", &["gba", "zip"])
                            .add_filter("All Files", &["*"]);
                        if env::current_dir().is_ok() {
                            dialog = dialog.set_directory(env::current_dir().unwrap());
                        }
                        let result_holder = Arc::clone(&self.pending_dialog_result);
                        dialog.pick_file(move |file_data| {
                            if let Some(file_data) = file_data
                                && let Ok(mut pending) = result_holder.lock()
                            {
                                *pending = Some(GuiAction::LoadRom(file_data));
                            }
                        });
                        ui.close_kind(egui::UiKind::Menu);
                    }
                    ui.separator();
                    if ui.button("Save State").clicked() {
                        let timestamp = SystemTime::now()
                            .duration_since(UNIX_EPOCH)
                            .unwrap()
                            .as_secs();
                        let file_name = format!("save_{}", timestamp);
                        let mut dialog = file_dialog::new()
                            .add_filter("RustyBoi Advance Save State", &["rustyboiadvancesave"])
                            .set_file_name(file_name);
                        if env::current_dir().is_ok() {
                            dialog = dialog.set_directory(env::current_dir().unwrap());
                        }
                        let result_holder = Arc::clone(&self.pending_dialog_result);
                        dialog.save_file(move |path| {
                            if let Some(path) = path
                                && let Ok(mut pending) = result_holder.lock()
                            {
                                *pending = Some(GuiAction::SaveState(path));
                            }
                        });
                        ui.close_kind(egui::UiKind::Menu);
                    }
                    if ui.button("Load State").clicked() {
                        let mut dialog = file_dialog::new()
                            .add_filter("RustyBoi Advance Save State", &["rustyboiadvancesave"])
                            .add_filter("All Files", &["*"]);
                        if env::current_dir().is_ok() {
                            dialog = dialog.set_directory(env::current_dir().unwrap());
                        }
                        let result_holder = Arc::clone(&self.pending_dialog_result);
                        dialog.pick_file(move |file_data| {
                            if let Some(file_data) = file_data
                                && let Ok(mut pending) = result_holder.lock()
                            {
                                *pending = Some(GuiAction::LoadState(file_data));
                            }
                        });
                        ui.close_kind(egui::UiKind::Menu);
                    }
                    ui.separator();
                    if ui.button("Exit").clicked() {
                        *action = Some(GuiAction::Exit);
                        ui.close_kind(egui::UiKind::Menu);
                    }
                });

                ui.menu_button("Emulation", |ui| {
                    *any_menu_open = true;
                    if ui.button("Restart").clicked() {
                        *action = Some(GuiAction::Restart);
                        ui.close_kind(egui::UiKind::Menu);
                    }
                    ui.separator();
                    let pause_text = if paused { "Resume" } else { "Pause" };
                    if ui.button(pause_text).clicked() {
                        *action = Some(GuiAction::TogglePause);
                        ui.close_kind(egui::UiKind::Menu);
                    }
                });

                ui.menu_button("Debug", |ui| {
                    *any_menu_open = true;
                    ui.checkbox(&mut self.show_cpu_registers, "CPU Registers");
                    ui.checkbox(&mut self.show_stack_explorer, "Stack Explorer");
                    ui.checkbox(&mut self.show_memory_explorer, "Memory Explorer");
                    ui.checkbox(&mut self.show_ppu_debug, "PPU Debug");
                    ui.separator();
                    ui.checkbox(&mut self.show_breakpoint_panel, "Breakpoint Manager");
                });

                ui.menu_button("Settings", |ui| {
                    *any_menu_open = true;
                    ui.checkbox(&mut self.show_keybind_settings, "Keybind Settings");
                });
            });
        });
    }

    fn render_debug_panels(
        &mut self,
        ctx: &Context,
        registers: Option<&cpu::registers::Registers>,
        gba: Option<&gba::GBA>,
        action: &mut Option<GuiAction>,
        paused: bool,
    ) {
        if self.show_cpu_registers {
            self.render_cpu_registers_panel(ctx, registers, gba, action, paused);
        }

        if self.show_stack_explorer {
            self.render_stack_explorer_panel(ctx, registers, gba);
        }

        if self.show_memory_explorer {
            self.render_memory_explorer_panel(ctx, gba);
        }

        if self.show_ppu_debug {
            self.render_ppu_debug_panel(ctx, gba);
        }

        if self.show_keybind_settings {
            self.render_keybind_settings_panel(ctx);
        }

        if self.show_breakpoint_panel {
            self.render_breakpoint_panel(ctx, action, gba);
        }
    }

    fn render_status_panel(&mut self, ctx: &Context) {
        if let Some(status_msg) = &self.status_message.clone() {
            let mut clear_status = false;
            egui::TopBottomPanel::bottom("status_panel").show(ctx, |ui| {
                ui.horizontal(|ui| {
                    ui.label("✅");
                    ui.label(status_msg);
                    if ui.button("✕").clicked() {
                        clear_status = true;
                    }
                });
            });

            if clear_status {
                self.status_message = None;
            }
        }
    }

    fn render_error_panel(&mut self, ctx: &Context, action: &mut Option<GuiAction>) {
        if let Some(error_msg) = &self.error_message.clone() {
            egui::CentralPanel::default().show(ctx, |ui| {
                ui.heading("🚨 Emulator Crashed");
                ui.separator();

                ui.label("The emulator has encountered a fatal error and has stopped running.");
                ui.label("The GUI remains open for debugging purposes.");

                ui.add_space(10.0);

                ui.label("Error Details:");
                ui.group(|ui| {
                    ui.add(
                        egui::TextEdit::multiline(&mut error_msg.as_str())
                            .desired_width(f32::INFINITY)
                            .desired_rows(6)
                            .font(egui::TextStyle::Monospace),
                    );
                });

                ui.add_space(10.0);

                ui.horizontal(|ui| {
                    if ui.button("🔄 Restart Emulation").clicked() {
                        *action = Some(GuiAction::Restart);
                    }

                    if ui.button("Clear Error (Debug Mode)").clicked() {
                        *action = Some(GuiAction::ClearError);
                    }
                });
            });
        }
    }

    pub fn set_error(&mut self, error_message: String) {
        self.error_message = Some(error_message);
    }

    pub fn clear_error(&mut self) {
        self.error_message = None;
    }

    pub fn set_status(&mut self, status_message: String) {
        self.status_message = Some(status_message);
    }

    fn render_breakpoint_panel(
        &mut self,
        ctx: &Context,
        action: &mut Option<GuiAction>,
        gba: Option<&gba::GBA>,
    ) {
        egui::Window::new("Breakpoint Manager")
            .default_width(300.0)
            .frame(egui::Frame::window(&ctx.style()).fill(PANEL_BACKGROUND))
            .show(ctx, |ui| {
                ui.heading("Breakpoints");
                ui.separator();

                // Input for new breakpoint address
                ui.horizontal(|ui| {
                    ui.label("Address:");
                    ui.add(
                        egui::TextEdit::singleline(&mut self.breakpoint_address_input)
                            .desired_width(80.0)
                            .font(egui::TextStyle::Monospace),
                    );

                    if ui.button("Add").clicked() {
                        // Parse the address from hex string
                        if let Ok(address) = u32::from_str_radix(
                            self.breakpoint_address_input.trim_start_matches("0x"),
                            16,
                        ) {
                            *action = Some(GuiAction::SetBreakpoint(address));
                            self.breakpoint_address_input = String::from("00000000");
                        }
                    }
                });

                ui.small("Enter address in hex format (e.g., 0100, FFAA)");
                ui.separator();

                // Display current breakpoints if we have access to GB
                if let Some(gba) = gba {
                    ui.label("Active Breakpoints:");
                    ui.separator();

                    let breakpoints: Vec<u32> = gba.get_breakpoints().iter().cloned().collect();
                    if breakpoints.is_empty() {
                        ui.label("No breakpoints set");
                    } else {
                        // Sort breakpoints for consistent display
                        let mut sorted_breakpoints = breakpoints.clone();
                        sorted_breakpoints.sort();

                        for &address in &sorted_breakpoints {
                            ui.horizontal(|ui| {
                                ui.monospace(format!("{:08X}", address));
                                if ui.small_button("✕").clicked() {
                                    *action = Some(GuiAction::RemoveBreakpoint(address));
                                }
                            });
                        }

                        ui.separator();
                        if ui.button("Clear All").clicked() {
                            // Remove all breakpoints by sending individual remove actions
                            // We'll handle this in the main loop
                            for &address in &breakpoints {
                                *action = Some(GuiAction::RemoveBreakpoint(address));
                            }
                        }
                    }

                    ui.separator();
                    ui.small("Click ✕ to remove a breakpoint");
                } else {
                    ui.label("Emulator not available");
                }
            });
    }
}
