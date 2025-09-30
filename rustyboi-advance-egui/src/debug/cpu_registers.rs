use crate::actions::GuiAction;
use crate::ui::Gui;
use egui::Context;
use rustyboi_advance_core_lib::{cpu, gba};
use rustyboi_advance_debugger_lib::disassembler::Disassembler;

impl Gui {
    pub(crate) fn render_cpu_registers_panel(
        &mut self,
        ctx: &Context,
        registers: Option<&cpu::registers::Registers>,
        gb: Option<&gba::GBA>,
        action: &mut Option<GuiAction>,
        paused: bool,
    ) {
        if let Some(regs) = registers
            && let Some(gb_ref) = gb
        {
            egui::Window::new("ARM7TDMI Registers")
                .default_pos([10.0, 50.0])
                .default_size([320.0, 500.0])
                .collapsible(true)
                .resizable(false)
                .frame(egui::Frame::window(&ctx.style()).fill(crate::ui::PANEL_BACKGROUND))
                .show(ctx, |ui| {
                    ui.set_width(280.0);

                    // ARM7TDMI General Purpose Registers (R0-R12)
                    ui.small(egui::RichText::new("General Registers:").color(egui::Color32::LIGHT_GRAY));
                    ui.monospace(
                        egui::RichText::new(format!("R0: {:08X}  R1: {:08X}", regs.r0, regs.r1))
                            .color(egui::Color32::WHITE),
                    );
                    ui.monospace(
                        egui::RichText::new(format!("R2: {:08X}  R3: {:08X}", regs.r2, regs.r3))
                            .color(egui::Color32::WHITE),
                    );
                    ui.monospace(
                        egui::RichText::new(format!("R4: {:08X}  R5: {:08X}", regs.r4, regs.r5))
                            .color(egui::Color32::WHITE),
                    );
                    ui.monospace(
                        egui::RichText::new(format!("R6: {:08X}  R7: {:08X}", regs.r6, regs.r7))
                            .color(egui::Color32::WHITE),
                    );
                    ui.monospace(
                        egui::RichText::new(format!("R8: {:08X}  R9: {:08X}", regs.r8, regs.r9))
                            .color(egui::Color32::WHITE),
                    );
                    ui.monospace(
                        egui::RichText::new(format!("R10:{:08X}  R11:{:08X}", regs.r10, regs.r11))
                            .color(egui::Color32::WHITE),
                    );
                    ui.monospace(
                        egui::RichText::new(format!("R12:{:08X}", regs.r12))
                            .color(egui::Color32::WHITE),
                    );
                    ui.separator();

                    // Special Registers
                    ui.small(egui::RichText::new("Special Registers:").color(egui::Color32::LIGHT_GRAY));
                    
                    // Get current CPU mode for SP/LR display
                    let current_mode = cpu::registers::Mode::from(regs.cpsr);
                    
                    let current_sp = regs.sp();
                    let current_lr = regs.lr();
                    
                    ui.monospace(
                        egui::RichText::new(format!("SP: {:08X}  LR: {:08X}", current_sp, current_lr))
                            .color(egui::Color32::LIGHT_BLUE),
                    );

                    // CPSR flags
                    let n_flag = regs.get_flag(cpu::registers::Flag::Negative);
                    let z_flag = regs.get_flag(cpu::registers::Flag::Zero);
                    let c_flag = regs.get_flag(cpu::registers::Flag::Carry);
                    let v_flag = regs.get_flag(cpu::registers::Flag::Overflow);
                    let i_flag = regs.get_flag(cpu::registers::Flag::IrqDisable);
                    let f_flag = regs.get_flag(cpu::registers::Flag::FiqDisable);
                    let t_flag = regs.get_flag(cpu::registers::Flag::ThumbState);

                    ui.horizontal(|ui| {
                        ui.monospace(
                            egui::RichText::new(format!("N:{}", if n_flag { "1" } else { "0" }))
                                .color(if n_flag {
                                    egui::Color32::LIGHT_RED
                                } else {
                                    egui::Color32::GRAY
                                }),
                        );
                        ui.monospace(
                            egui::RichText::new(format!("Z:{}", if z_flag { "1" } else { "0" }))
                                .color(if z_flag {
                                    egui::Color32::LIGHT_GREEN
                                } else {
                                    egui::Color32::GRAY
                                }),
                        );
                        ui.monospace(
                            egui::RichText::new(format!("C:{}", if c_flag { "1" } else { "0" }))
                                .color(if c_flag {
                                    egui::Color32::YELLOW
                                } else {
                                    egui::Color32::GRAY
                                }),
                        );
                        ui.monospace(
                            egui::RichText::new(format!("V:{}", if v_flag { "1" } else { "0" }))
                                .color(if v_flag {
                                    egui::Color32::LIGHT_BLUE
                                } else {
                                    egui::Color32::GRAY
                                }),
                        );
                    });
                    
                    ui.horizontal(|ui| {
                        ui.monospace(
                            egui::RichText::new(format!("I:{}", if i_flag { "1" } else { "0" }))
                                .color(if i_flag {
                                    egui::Color32::ORANGE
                                } else {
                                    egui::Color32::GRAY
                                }),
                        );
                        ui.monospace(
                            egui::RichText::new(format!("F:{}", if f_flag { "1" } else { "0" }))
                                .color(if f_flag {
                                    egui::Color32::ORANGE
                                } else {
                                    egui::Color32::GRAY
                                }),
                        );
                        ui.monospace(
                            egui::RichText::new(format!("T:{}", if t_flag { "1" } else { "0" }))
                                .color(if t_flag {
                                    egui::Color32::LIGHT_BLUE
                                } else {
                                    egui::Color32::GRAY
                                }),
                        );
                        
                        // Show current CPU mode
                        let mode_str = match current_mode {
                            cpu::registers::Mode::User => "USR",
                            cpu::registers::Mode::Fiq => "FIQ",
                            cpu::registers::Mode::Irq => "IRQ",
                            cpu::registers::Mode::Service => "SVC",
                            cpu::registers::Mode::Abort => "ABT",
                            cpu::registers::Mode::Undefined => "UND",
                            cpu::registers::Mode::System => "SYS",
                        };
                        ui.monospace(
                            egui::RichText::new(format!("Mode:{}", mode_str))
                                .color(egui::Color32::LIGHT_YELLOW),
                        );
                    });
                    ui.separator();
                    ui.monospace(
                        egui::RichText::new(format!("PC: {:08X}", regs.pc.saturating_sub(4))) // ARM instructions are 4 bytes
                            .color(egui::Color32::WHITE),
                    );
                    ui.separator();

                    // Instruction viewer around PC
                    ui.small(egui::RichText::new("Instructions:").color(egui::Color32::LIGHT_GRAY));
                    let pc = regs.pc;
                    let display_pc = pc.saturating_sub(0); // Show the instruction that was just executed

                    // Display exactly 5 instructions starting from the current PC
                    let mut addr = display_pc;
                    let mut instruction_count = 0;
                    const MAX_INSTRUCTIONS: usize = 5;

                    while instruction_count < MAX_INSTRUCTIONS {
                        // ARM/Thumb instruction length depends on CPU state
                        let is_thumb = regs.get_flag(cpu::registers::Flag::ThumbState);
                        let instruction_length = if is_thumb { 2 } else { 4 }; // Thumb=16bit, ARM=32bit
                        
                        let (mnemonic, _) = Disassembler::disassemble_with_reader(addr, |a| {
                            let mut word = 0u32;
                            for i in 0..4 {
                                word |= (gb_ref.read_memory(a + i) as u32) << (i * 8);
                            }
                            word
                        });

                        let color = if addr == display_pc {
                            egui::Color32::YELLOW // Highlight the instruction that was just executed
                        } else if addr < display_pc {
                            egui::Color32::LIGHT_GRAY // Before executed instruction
                        } else {
                            egui::Color32::GRAY // After executed instruction (upcoming)
                        };

                        let marker = if addr == display_pc { "→" } else { " " };

                        // Show instruction bytes based on ARM/Thumb state
                        let bytes = if is_thumb {
                            // Thumb: 16-bit instruction
                            format!(
                                "{:02X}{:02X}",
                                gb_ref.read_memory(addr + 1),
                                gb_ref.read_memory(addr)
                            )
                        } else {
                            // ARM: 32-bit instruction
                            format!(
                                "{:02X}{:02X}{:02X}{:02X}",
                                gb_ref.read_memory(addr + 3),
                                gb_ref.read_memory(addr + 2),
                                gb_ref.read_memory(addr + 1),
                                gb_ref.read_memory(addr)
                            )
                        };

                        ui.monospace(
                            egui::RichText::new(format!(
                                "{} {:08X}: {:8} {}",
                                marker, addr, bytes, mnemonic
                            ))
                            .color(color),
                        );

                        addr += instruction_length;
                        instruction_count += 1;
                    }
                    ui.separator();

                    // Step controls
                    ui.small(
                        egui::RichText::new("Step Controls:").color(egui::Color32::LIGHT_GRAY),
                    );

                    // Slider for step count
                    ui.horizontal(|ui| {
                        ui.label("Steps:");
                        ui.add(
                            egui::Slider::new(&mut self.step_count, 1..=100).text("instructions"),
                        );
                    });

                    ui.separator();

                    // Step buttons
                    ui.horizontal(|ui| {
                        if paused {
                            // Step Cycles button with hold functionality
                            let cycles_response = ui.button("Step Cycles");
                            if cycles_response.clicked() {
                                // Initial press - execute immediately
                                *action = Some(GuiAction::StepCycles(self.step_count));
                                self.step_cycles_held_frames = 0;
                            } else if cycles_response.is_pointer_button_down_on() {
                                // Button is being held down
                                self.step_cycles_held_frames += 1;
                                // After 15 frames (250ms at 60fps), start repeating every 4 frames (67ms at 60fps)
                                if self.step_cycles_held_frames > 15
                                    && (self.step_cycles_held_frames - 15).is_multiple_of(4)
                                {
                                    *action = Some(GuiAction::StepCycles(self.step_count));
                                }
                            } else {
                                // Button released - reset state
                                self.step_cycles_held_frames = 0;
                            }

                            // Step Frames button with hold functionality
                            let frames_response = ui.button("Step Frames");
                            if frames_response.clicked() {
                                // Initial press - execute immediately
                                *action = Some(GuiAction::StepFrames(self.step_count));
                                self.step_frames_held_frames = 0;
                            } else if frames_response.is_pointer_button_down_on() {
                                // Button is being held down
                                self.step_frames_held_frames += 1;
                                // After 15 frames (250ms at 60fps), start repeating every 4 frames (67ms at 60fps)
                                if self.step_frames_held_frames > 15
                                    && (self.step_frames_held_frames - 15).is_multiple_of(4)
                                {
                                    *action = Some(GuiAction::StepFrames(self.step_count));
                                }
                            } else {
                                // Button released - reset state
                                self.step_frames_held_frames = 0;
                            }
                        } else {
                            ui.add_enabled(false, egui::Button::new("Step Cycles"));
                            ui.add_enabled(false, egui::Button::new("Step Frames"));
                        }
                    });

                    if !paused {
                        ui.small(
                            egui::RichText::new("(Pause to enable stepping)")
                                .color(egui::Color32::GRAY),
                        );
                    }

                    ui.separator();
                    ui.small(
                        egui::RichText::new("F = step frame | N = step cycle")
                            .color(egui::Color32::LIGHT_GRAY),
                    );
                    ui.small(
                        egui::RichText::new("Flags: N=Negative Z=Zero C=Carry V=Overflow")
                            .color(egui::Color32::LIGHT_GRAY),
                    );
                    ui.small(
                        egui::RichText::new("I=IRQ Disable F=FIQ Disable T=Thumb Mode")
                            .color(egui::Color32::LIGHT_GRAY),
                    );
                });
        }
    }
}
