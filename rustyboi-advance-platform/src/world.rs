use rustyboi_advance_core_lib::{cartridge, gba, ppu};
use rustyboi_advance_egui_lib::actions::FileData;

#[cfg(not(target_arch = "wasm32"))]
use std::time::Duration;
use web_time::Instant;

#[cfg(target_arch = "wasm32")]
use crate::config;

pub struct World {
    pub gba: gba::GBA,
    frame: Option<[u8; ppu::FRAMEBUFFER_SIZE]>,
    pub error_state: Option<String>,
    pub is_paused: bool,
    pub step_single_frame: bool,
    pub step_single_cycle: bool,
    pub step_multiple_cycles: Option<u32>,
    pub step_multiple_frames: Option<u32>,
    current_rom_path: Option<String>,
    current_bios_path: Option<String>,
    // FPS and performance tracking
    frame_times: Vec<Instant>,
    #[cfg(not(target_arch = "wasm32"))]
    last_title_update: Instant,
    #[cfg(not(target_arch = "wasm32"))]
    // Frame timing for 60fps
    last_frame_time: Instant,
    // Breakpoint status
    breakpoint_hit: bool,
    // Track if emulator was auto-paused due to missing ROM/BIOS
    pub auto_paused_no_content: bool,
}

impl World {
    #[cfg(target_arch = "wasm32")]
    pub fn new(gba: gba::GBA, config: Option<config::CleanConfig>) -> Self {
        let (rom_path, bios_path, start_paused) = if let Some(cfg) = config {
            (cfg.rom, cfg.bios, false) // WASM doesn't support start_paused config
        } else {
            (None, None, false)
        };

        Self::new_with_paths(gba, rom_path, bios_path, start_paused)
    }

    pub fn new_with_paths(
        gba: gba::GBA,
        rom_path: Option<String>,
        bios_path: Option<String>,
        start_paused: bool,
    ) -> Self {
        #[cfg(not(target_arch = "wasm32"))]
        let now = Instant::now();

        // Check if both ROM and BIOS are missing - if so, start paused
        // Also respect the user's start_paused config option
        let should_start_paused = start_paused || (!gb.has_rom() && !gb.has_bios());

        Self {
            gba,
            frame: None,
            error_state: None,
            is_paused: should_start_paused,
            step_single_frame: false,
            step_single_cycle: false,
            step_multiple_cycles: None,
            step_multiple_frames: None,
            current_rom_path: rom_path,
            current_bios_path: bios_path,
            frame_times: Vec::with_capacity(60), // Store last 60 frame times for FPS calculation
            #[cfg(not(target_arch = "wasm32"))]
            last_title_update: now,
            #[cfg(not(target_arch = "wasm32"))]
            last_frame_time: now,
            breakpoint_hit: false,
            auto_paused_no_content: should_start_paused,
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub fn save_state(&self, path: std::path::PathBuf) -> Result<String, std::io::Error> {
        let filename = path.to_string_lossy().to_string();
        self.gba.to_state_file(&filename)?;
        println!("Game state saved to: {}", filename);
        Ok(filename)
    }

    pub fn load_state(
        &mut self,
        file_data: FileData,
    ) -> Result<String, Box<dyn std::error::Error>> {
        // Save the current ROM and BIOS paths before loading state
        let saved_rom_path = self.current_rom_path.clone();
        let saved_bios_path = self.current_bios_path.clone();

        // Load the new state and get filename
        let filename = match file_data {
            #[cfg(not(target_arch = "wasm32"))]
            FileData::Path(path) => {
                let filename = path.to_string_lossy().to_string();
                self.gba = gba::GBA::from_state_file(&filename)?;
                filename
            }
            #[cfg(target_arch = "wasm32")]
            FileData::Contents { name, data } => {
                // For WASM, parse the data directly
                let saved_state = String::from_utf8(data)?;
                self.gba = serde_json::from_str(&saved_state)?;
                name
            }
        };

        // Reload the ROM if we had one loaded
        if let Some(rom_path) = saved_rom_path {
            match cartridge::Cartridge::load_from_path(&rom_path) {
                Ok(cartridge) => {
                    self.gba.insert(cartridge);
                    self.current_rom_path = Some(rom_path);
                    println!("Reloaded ROM: {}", self.current_rom_path.as_ref().unwrap());
                }
                Err(e) => {
                    println!("Warning: Failed to reload ROM {}: {}", rom_path, e);
                    self.current_rom_path = None;
                }
            }
        }

        // Reload the BIOS if we had one loaded
        if let Some(bios_path) = saved_bios_path {
            match self.gba.load_bios(&bios_path) {
                Ok(_) => {
                    self.current_bios_path = Some(bios_path);
                    println!(
                        "Reloaded BIOS: {}",
                        self.current_bios_path.as_ref().unwrap()
                    );
                }
                Err(e) => {
                    println!("Warning: Failed to reload BIOS {}: {}", bios_path, e);
                    self.current_bios_path = None;
                }
            }
        }

        // Clear any error state
        self.error_state = None;

        // Clear the current frame
        self.frame = None;

        // If emulator was auto-paused due to no content and state has content, unpause it
        if self.auto_paused_no_content && (self.gba.has_rom() || self.gba.has_bios()) {
            self.is_paused = false;
            self.auto_paused_no_content = false;
        }

        println!("Game state loaded from: {}", filename);
        Ok(filename)
    }

    pub fn load_rom(&mut self, file_data: FileData) -> Result<String, Box<dyn std::error::Error>> {
        let (filename, cartridge, has_file_path) = match file_data {
            #[cfg(not(target_arch = "wasm32"))]
            FileData::Path(path) => {
                let filename = path.to_string_lossy().to_string();
                let cartridge = cartridge::Cartridge::load_from_path(&filename)?;
                (filename, cartridge, true)
            }
            #[cfg(target_arch = "wasm32")]
            FileData::Contents { name, data } => {
                // For WASM, load from memory
                let cartridge = cartridge::Cartridge::load_from_bytes(&data)?;
                (name, cartridge, false)
            }
        };
        self.gba.insert(cartridge);

        // Track the current ROM path
        self.current_rom_path = if has_file_path {
            Some(filename.clone())
        } else {
            None // No file path for WASM content
        };

        // Reset the emulator to a clean state after loading the ROM
        self.gba.reset();

        // Clear any error state
        self.error_state = None;

        // Clear the current frame
        self.frame = None;

        // If emulator was auto-paused due to no content, unpause it now
        if self.auto_paused_no_content {
            self.is_paused = false;
            self.auto_paused_no_content = false;
        }

        println!("ROM loaded from: {}", filename);
        Ok(filename)
    }

    pub fn toggle_pause(&mut self) {
        self.is_paused = !self.is_paused;
    }

    pub fn pause(&mut self) {
        self.is_paused = true;
    }

    pub fn resume(&mut self) {
        self.is_paused = false;
    }

    pub fn restart(&mut self) {
        // Reset the Game Boy to its initial state
        self.gba.reset();

        // Clear any error state
        self.error_state = None;

        // Clear the current frame
        self.frame = None;

        // Reset pause state
        self.is_paused = false;
    }

    pub fn clear_error(&mut self) {
        self.error_state = None;
    }

    pub fn draw(&mut self, frame: &mut [u8]) -> bool {
        if let Some(data) = self.frame.as_ref() {
            frame.copy_from_slice(data);
            self.frame = None;
            true
        } else {
            false
        }
    }

    fn run_until_frame(&mut self) -> Option<[u8; ppu::FRAMEBUFFER_SIZE]> {
        let result =
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| self.gba.run_until_frame()));

        match result {
            Ok((frame, _breakpoint_hit)) => Some(frame),
            Err(panic_info) => {
                // Convert panic info to a string for debugging
                let error_msg = if let Some(s) = panic_info.downcast_ref::<&str>() {
                    format!("Emulator panic: {}", s)
                } else if let Some(s) = panic_info.downcast_ref::<String>() {
                    format!("Emulator panic: {}", s)
                } else {
                    "Emulator panic: Unknown error".to_string()
                };

                println!("Game Boy emulator crashed: {}", error_msg);
                None
            }
        }
    }

    fn run_until_frame_with_breakpoints(&mut self) -> (Option<[u8; ppu::FRAMEBUFFER_SIZE]>, bool) {
        let result =
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| self.gba.run_until_frame()));

        match result {
            Ok((frame, breakpoint_hit)) => (Some(frame), breakpoint_hit),
            Err(panic_info) => {
                // Convert panic info to a string for debugging
                let error_msg = if let Some(s) = panic_info.downcast_ref::<&str>() {
                    format!("Emulator panic: {}", s)
                } else if let Some(s) = panic_info.downcast_ref::<String>() {
                    format!("Emulator panic: {}", s)
                } else {
                    "Emulator panic: Unknown error".to_string()
                };

                println!("Game Boy emulator crashed: {}", error_msg);
                (None, false)
            }
        }
    }

    pub fn update(&mut self) {
        // Handle single frame stepping
        if self.step_single_frame {
            self.step_single_frame = false;
            match self.run_until_frame() {
                Some(frame) => {
                    self.frame = Some(frame);
                }
                None => {
                    self.error_state = Some("Emulator crashed during frame step".to_string());
                    self.frame = None;
                }
            }
            return;
        }

        // Handle single cycle stepping
        if self.step_single_cycle {
            self.step_single_cycle = false;
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                let (_breakpoint_hit, _cycles) = self.gba.step_instruction();
                // For cycle stepping, we need to get the current frame even if incomplete
                self.gba.get_current_frame()
            }));
            match result {
                Ok(frame) => {
                    self.frame = Some(frame);
                }
                Err(panic_info) => {
                    let error_msg = if let Some(s) = panic_info.downcast_ref::<&str>() {
                        format!("Emulator panic during cycle step: {}", s)
                    } else if let Some(s) = panic_info.downcast_ref::<String>() {
                        format!("Emulator panic during cycle step: {}", s)
                    } else {
                        "Emulator panic during cycle step: Unknown error".to_string()
                    };
                    self.error_state = Some(error_msg);
                    self.frame = None;
                }
            }
            return;
        }

        // Handle multiple cycle stepping
        if let Some(count) = self.step_multiple_cycles.take() {
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                for _ in 0..count {
                    let (_breakpoint_hit, _cycles) = self.gba.step_instruction();
                }
                self.gba.get_current_frame()
            }));
            match result {
                Ok(frame) => {
                    self.frame = Some(frame);
                }
                Err(panic_info) => {
                    let error_msg = if let Some(s) = panic_info.downcast_ref::<&str>() {
                        format!("Emulator panic during multi-cycle step ({}): {}", count, s)
                    } else if let Some(s) = panic_info.downcast_ref::<String>() {
                        format!("Emulator panic during multi-cycle step ({}): {}", count, s)
                    } else {
                        format!(
                            "Emulator panic during multi-cycle step ({}): Unknown error",
                            count
                        )
                    };
                    self.error_state = Some(error_msg);
                    self.frame = None;
                }
            }
            return;
        }

        // Handle multiple frame stepping
        if let Some(count) = self.step_multiple_frames.take() {
            let mut success = true;
            let mut final_frame = None;

            for _ in 0..count {
                match self.run_until_frame() {
                    Some(_) => {} // Continue to next frame
                    None => {
                        success = false;
                        break;
                    }
                }
            }

            if success {
                final_frame = Some(self.gba.get_current_frame());
            }

            match final_frame {
                Some(frame) => {
                    self.frame = Some(frame);
                }
                None => {
                    self.error_state = Some(format!(
                        "Emulator crashed during multi-frame step ({})",
                        count
                    ));
                    self.frame = None;
                }
            }
            return;
        }

        // Skip updating if we're in an error state or paused
        if self.error_state.is_some() || self.is_paused {
            return;
        }

        #[cfg(not(target_arch = "wasm32"))]
        {
            // Frame timing: target 60fps (16.75ms per frame)
            const TARGET_FRAME_TIME: Duration = Duration::from_micros(16750); // ~59.7 fps
            let now = Instant::now();
            let elapsed_since_last_frame = now.duration_since(self.last_frame_time);

            // Only update if enough time has passed
            if elapsed_since_last_frame < TARGET_FRAME_TIME {
                let remaining = TARGET_FRAME_TIME - elapsed_since_last_frame;

                // Sleep for most of the remaining time (not supported in WASM)
                #[cfg(not(target_arch = "wasm32"))]
                if remaining > Duration::from_micros(100) {
                    std::thread::sleep(remaining - Duration::from_micros(50));
                }
                // Spin for precision
                while self.last_frame_time.elapsed() < TARGET_FRAME_TIME {
                    std::hint::spin_loop();
                }
            } else if elapsed_since_last_frame.as_millis() > 25 {
                // Frame took too long
                println!(
                    "Slow frame: {}ms (target: {}ms)",
                    elapsed_since_last_frame.as_millis(),
                    TARGET_FRAME_TIME.as_millis()
                );
            }

            self.last_frame_time = Instant::now();
        }

        // Use breakpoint-aware version if we have any breakpoints set
        if self.gba.get_breakpoints().is_empty() {
            // No breakpoints - use regular version for better performance
            match self.run_until_frame() {
                Some(frame_data) => {
                    self.frame = Some(frame_data);
                    self.update_performance_metrics();
                }
                None => {
                    self.error_state = Some("Emulator crashed".to_string());
                    println!(
                        "Game Boy emulator crashed: {}",
                        self.error_state.as_ref().unwrap()
                    );
                    self.frame = None;
                }
            }
        } else {
            // We have breakpoints - use breakpoint-aware version
            let (frame_result, breakpoint_hit) = self.run_until_frame_with_breakpoints();
            match frame_result {
                Some(frame_data) => {
                    self.frame = Some(frame_data);
                    self.update_performance_metrics();

                    // If a breakpoint was hit, pause emulation
                    if breakpoint_hit {
                        self.is_paused = true;
                        self.breakpoint_hit = true;
                        println!(
                            "Breakpoint hit at PC: {:08X}",
                            self.gba.get_cpu_registers().pc
                        );
                    }
                }
                None => {
                    self.error_state = Some("Emulator crashed".to_string());
                    println!(
                        "Game Boy emulator crashed: {}",
                        self.error_state.as_ref().unwrap()
                    );
                    self.frame = None;
                }
            }
        }
    }

    fn update_performance_metrics(&mut self) {
        let now = Instant::now();

        // Track frame times for FPS calculation
        self.frame_times.push(now);

        // Keep only the last 60 frame times (1 second at 60 FPS)
        if self.frame_times.len() > 60 {
            self.frame_times.remove(0);
        }
    }

    fn get_fps(&self) -> f64 {
        let frame_count = self.frame_times.len();
        if frame_count < 2 {
            return 0.0;
        }

        let duration = self.frame_times[frame_count - 1].duration_since(self.frame_times[0]);
        if duration.as_secs_f64() == 0.0 {
            return 0.0;
        }

        (frame_count as f64 - 1.0) / duration.as_secs_f64()
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub fn update_window_title(&mut self, window: &winit::window::Window, is_paused: bool) {
        let now = Instant::now();

        // Update title every 500ms to avoid excessive updates
        if now.duration_since(self.last_title_update).as_millis() >= 500 {
            let fps = self.get_fps();

            let title = if self.error_state.is_some() {
                format!("RustyBoi Advance - ERROR | {:.1} FPS", fps)
            } else if is_paused {
                format!("RustyBoi Advance - PAUSED | {:.1} FPS", fps)
            } else {
                format!("RustyBoi Advance | {:.1} FPS", fps)
            };

            window.set_title(&title);
            self.last_title_update = now;
        }
    }

    // Breakpoint management methods
    pub fn add_breakpoint(&mut self, address: u32) {
        self.gba.add_breakpoint(address);
    }

    pub fn remove_breakpoint(&mut self, address: u32) {
        self.gba.remove_breakpoint(address);
    }

    pub fn check_and_clear_breakpoint_hit(&mut self) -> bool {
        let hit = self.breakpoint_hit;
        self.breakpoint_hit = false;
        hit
    }
}
