use egui_wgpu::wgpu::SurfaceError;
use egui_wgpu::{ScreenDescriptor, wgpu};
use std::sync::Arc;
use web_time::{Duration, Instant};
use winit::application::ApplicationHandler;
use winit::dpi::PhysicalSize;
use winit::event::WindowEvent;
use winit::event_loop::ActiveEventLoop;
use winit::keyboard::KeyCode;
use winit::window::{Window, WindowId};

use crate::app_state;
use crate::config;
use crate::input::InputHandler;
use crate::world;
use rustyboi_advance_core_lib::gba;
use rustyboi_advance_egui_lib::{Gui, actions::GuiAction};

#[cfg(target_arch = "wasm32")]
use std::cell::RefCell;
#[cfg(target_arch = "wasm32")]
use std::rc::Rc;

const WIDTH: u32 = 240;
const HEIGHT: u32 = 160;

pub struct App {
    instance: wgpu::Instance,
    state: Option<app_state::AppState>,
    window: Option<Arc<Window>>,
    #[cfg(target_arch = "wasm32")]
    initializing: bool,
    #[cfg(target_arch = "wasm32")]
    shared_state: Rc<RefCell<Option<app_state::AppState>>>,
    // Game Boy emulator components
    gui: Option<Gui>,
    world: Option<world::World>,
    gameboy_has_rendered_frame: bool,
    input: Option<InputHandler>,
    config: Option<config::CleanConfig>,
    // Debounce timing for F and N keys
    f_key_press_time: Option<Instant>,
    n_key_press_time: Option<Instant>,
    f_key_processed_initial: bool,
    n_key_processed_initial: bool,
    f_last_repeat_time: Option<Instant>,
    n_last_repeat_time: Option<Instant>,
    manually_paused: bool,
    user_paused: bool,

    // PERFORMANCE: Reusable framebuffer to avoid per-frame allocations
    reusable_framebuffer: Vec<u8>,
    texture_handle: Option<egui::TextureHandle>,
    image: Option<egui::ColorImage>,
}

impl App {
    #[cfg(target_arch = "wasm32")]
    pub fn new() -> Self {
        // Create default config and Game Boy for WASM
        let config = config::CleanConfig::default();
        let mut gb = gba::GBA::new();
        gb.skip_bios();
        let world = world::World::new(gb, Some(config.clone()));
        let input = InputHandler::new();

        let instance = egui_wgpu::wgpu::Instance::new(&wgpu::InstanceDescriptor::default());
        Self {
            instance,
            state: None,
            window: None,
            initializing: false,
            shared_state: Rc::new(RefCell::new(None)),
            gui: Some(Gui::new()),
            world: Some(world),
            gameboy_has_rendered_frame: false,
            input: Some(input),
            config: Some(config),
            f_key_press_time: None,
            n_key_press_time: None,
            f_key_processed_initial: false,
            n_key_processed_initial: false,
            f_last_repeat_time: None,
            n_last_repeat_time: None,
            manually_paused: false,
            user_paused: false,
            reusable_framebuffer: vec![0u8; WIDTH as usize * HEIGHT as usize * 4],
            texture_handle: None,
            image: None,
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub fn new_with_gb(gb: gba::GBA, config: config::CleanConfig) -> Self {
        let instance = egui_wgpu::wgpu::Instance::new(&wgpu::InstanceDescriptor::default());

        // Create World instance
        let world = world::World::new_with_paths(gb, config.rom.clone(), config.bios.clone());
        let should_start_paused = world.is_paused;

        Self {
            instance,
            state: None,
            window: None,
            #[cfg(target_arch = "wasm32")]
            initializing: false,
            #[cfg(target_arch = "wasm32")]
            shared_state: Rc::new(RefCell::new(None)),
            gui: Some(Gui::new()),
            world: Some(world),
            gameboy_has_rendered_frame: false,
            input: Some(InputHandler::new()),
            config: Some(config),
            f_key_press_time: None,
            n_key_press_time: None,
            f_key_processed_initial: false,
            n_key_processed_initial: false,
            f_last_repeat_time: None,
            n_last_repeat_time: None,
            manually_paused: should_start_paused,
            user_paused: should_start_paused,
            reusable_framebuffer: vec![0u8; WIDTH as usize * HEIGHT as usize * 4],
            texture_handle: None,
            image: None,
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    async fn set_window(&mut self, window: Window) {
        let window = Arc::new(window);

        let (initial_width, initial_height) = {
            #[cfg(not(target_arch = "wasm32"))]
            {
                let scale = self.config.as_ref().map(|c| c.scale as u32).unwrap_or(7);
                let width = WIDTH * scale;
                let height = HEIGHT * scale;
                let _ = window.request_inner_size(PhysicalSize::new(width, height));
                (width, height)
            }
            #[cfg(target_arch = "wasm32")]
            {
                // For WASM, don't request window size - let CSS handle it
                // Just use the current window size
                let current_size = window.inner_size();
                (current_size.width, current_size.height)
            }
        };

        let surface = self
            .instance
            .create_surface(window.clone())
            .expect("Failed to create surface!");

        let state = app_state::AppState::new(
            &self.instance,
            surface,
            &window,
            initial_width,
            initial_height,
        )
        .await;

        // Game Boy rendering will be handled directly in handle_redraw

        self.window.get_or_insert(window);
        self.state.get_or_insert(state);

        // Trigger a resize event now that the WASM state is ready
        #[cfg(target_arch = "wasm32")]
        {
            if let Some(window) = web_sys::window() {
                if let Ok(event) = web_sys::Event::new("resize") {
                    let _ = window.dispatch_event(&event);
                    web_sys::console::log_1(&"WASM state ready - triggered resize event".into());
                }
            }
        }
    }

    fn handle_resized(&mut self, width: u32, height: u32) {
        if width > 0 && height > 0
            && let Some(state) = self.state.as_mut() {
                state.resize_surface(width, height);
            }
    }

    fn handle_redraw(&mut self) {
        // Attempt to handle minimizing window
        if let Some(window) = self.window.as_ref()
            && let Some(min) = window.is_minimized()
                && min {
                    println!("Window is minimized");
                    return;
                }

        // For WASM, check if async initialization is complete
        #[cfg(target_arch = "wasm32")]
        if self.state.is_none() {
            // Check if the shared state has been initialized
            if let Ok(shared_state) = self.shared_state.try_borrow() {
                if shared_state.is_some() {
                    // Move the state from shared to local
                    drop(shared_state);
                    if let Ok(mut shared_state) = self.shared_state.try_borrow_mut() {
                        if let Some(state) = shared_state.take() {
                            self.state = Some(state);

                            // Trigger a resize event now that the WASM state is ready
                            if let Some(window) = web_sys::window() {
                                if let Ok(event) = web_sys::Event::new("resize") {
                                    let _ = window.dispatch_event(&event);
                                    web_sys::console::log_1(
                                        &"WASM state ready (shared) - triggered resize event"
                                            .into(),
                                    );
                                }
                            }
                        }
                    }
                }
            }

            // If still no state, skip rendering this frame
            if self.state.is_none() {
                web_sys::console::log_1(&"WASM state not ready, skipping frame".into());
                return;
            }
        }

        let state = match self.state.as_mut() {
            Some(state) => state,
            None => return, // No state available, skip rendering
        };

        let screen_descriptor = ScreenDescriptor {
            size_in_pixels: [state.surface_config.width, state.surface_config.height],
            pixels_per_point: self.window.as_ref().unwrap().scale_factor() as f32
                * state.scale_factor,
        };

        let window = self.window.as_ref().unwrap();

        // Game Boy emulator rendering

        let gui_action = if self.gui.is_some() {
            // Update Game Boy emulation
            if let Some(world) = &mut self.world {
                world.update();
            }

            // Game Boy framebuffer will be handled in egui rendering section

            let gui_paused_state = if let Some(world) = &self.world {
                self.manually_paused || world.error_state.is_some()
            } else {
                true
            };

            // Update window title with performance metrics
            #[cfg(not(target_arch = "wasm32"))]
            if let Some(world) = &mut self.world {
                world.update_window_title(window, gui_paused_state);
            }

            // Prepare GUI using rustyboi-egui::Gui
            let gui_result = if let Some(gui) = &mut self.gui {
                // Start egui frame
                state.egui_renderer.begin_frame(window);

                // Set dark theme to avoid white backgrounds
                state
                    .egui_renderer
                    .context()
                    .set_visuals(egui::Visuals::dark());

                // Render GUI and get actions (with optional Game Boy data)
                let (registers, gb_ref) = if let Some(world) = &self.world {
                    (Some(world.gb.get_cpu_registers()), Some(&world.gb))
                } else {
                    (None, None)
                };

                let (gui_action, any_menu_open) = gui.ui(
                    state.egui_renderer.context(),
                    gui_paused_state,
                    registers,
                    gb_ref,
                );

                // Render Game Boy screen
                if let Some(world_mut) = &mut self.world {
                    // PERFORMANCE: Reuse framebuffer to avoid per-frame allocation
                    self.reusable_framebuffer.fill(0);
                    let has_new_frame = world_mut.draw(&mut self.reusable_framebuffer);

                    // Create or update texture every frame for proper frame rate
                    if has_new_frame || self.texture_handle.is_none() {
                        // PERFORMANCE: Always use direct RGBA conversion to avoid Color32 intermediate buffer
                        if self.texture_handle.is_none() {
                            // Initial texture creation - only happens once
                            let image = egui::ColorImage::from_rgba_unmultiplied(
                                [WIDTH as usize, HEIGHT as usize],
                                &self.reusable_framebuffer,
                            );

                            self.texture_handle = Some(state.egui_renderer.context().load_texture(
                                "gameboy_screen",
                                image,
                                egui::TextureOptions::NEAREST,
                            ));
                            self.image = None;
                        } else if has_new_frame {
                            // Update existing texture - use direct RGBA conversion
                            if let Some(handle) = &mut self.texture_handle {
                                let image = egui::ColorImage::from_rgba_unmultiplied(
                                    [WIDTH as usize, HEIGHT as usize],
                                    &self.reusable_framebuffer,
                                );
                                handle.set(image, egui::TextureOptions::NEAREST);
                            }
                        }
                        self.gameboy_has_rendered_frame = true;
                    }

                    // Only render Game Boy screen if we have both a texture AND have rendered at least one frame
                    if let Some(texture_handle) = &self.texture_handle
                        && self.gameboy_has_rendered_frame {
                            egui::CentralPanel::default()
                                .frame(egui::Frame::NONE.fill(egui::Color32::TRANSPARENT))
                                .show(state.egui_renderer.context(), |ui| {
                                    // Get available space and calculate scale to fit
                                    let available_rect = ui.available_rect_before_wrap();
                                    let config_scale =
                                        self.config.as_ref().map(|c| c.scale as f32).unwrap_or(5.0);

                                    // Calculate ideal size based on config scale
                                    let ideal_width = WIDTH as f32 * config_scale;
                                    let ideal_height = HEIGHT as f32 * config_scale;

                                    // Scale down if needed to fit in available space, but keep aspect ratio
                                    let scale_x = available_rect.width() / ideal_width;
                                    let scale_y = available_rect.height() / ideal_height;
                                    let final_scale = scale_x.min(scale_y).min(1.0) * config_scale;

                                    let final_size = egui::Vec2::new(
                                        WIDTH as f32 * final_scale,
                                        HEIGHT as f32 * final_scale,
                                    );

                                    ui.with_layout(
                                        egui::Layout::centered_and_justified(
                                            egui::Direction::TopDown,
                                        ),
                                        |ui| {
                                            ui.add_sized(
                                                final_size,
                                                egui::Image::new(texture_handle)
                                                    .fit_to_exact_size(final_size),
                                            );
                                        },
                                    );
                                });
                        }
                }
                (gui_action, any_menu_open)
            } else {
                // If no world or gui, still start egui frame for fallback UI
                state.egui_renderer.begin_frame(window);

                egui::Window::new("RustyBoi")
                    .resizable(true)
                    .default_open(true)
                    .show(state.egui_renderer.context(), |ui| {
                        ui.label("Game Boy emulator ready");
                        ui.label("Load a ROM to start playing");
                    });

                (None, false)
            };

            let (gui_action, menu_open) = gui_result;

            // Auto-pause when menu is open, but respect manual pause state
            let should_be_paused = self.manually_paused || menu_open;
            if let Some(world) = &mut self.world {
                if should_be_paused != world.is_paused {
                    if should_be_paused {
                        world.pause();
                    } else {
                        // Only auto-resume if not manually paused, no error, AND not auto-paused due to no content
                        if !self.user_paused
                            && world.error_state.is_none()
                            && !world.auto_paused_no_content
                        {
                            world.resume();
                        }
                    }
                }

                // Check for breakpoint hits and notify user
                if world.check_and_clear_breakpoint_hit() {
                    self.manually_paused = true; // Ensure we stay paused
                    self.user_paused = true; // User should explicitly resume
                    // The GUI component will handle showing this in its UI
                }

                // Update manually_paused to include error state
                self.manually_paused = self.user_paused || world.error_state.is_some();
            }

            // Finish egui frame and render everything
            let surface_texture = state.surface.get_current_texture();

            match surface_texture {
                Err(SurfaceError::Outdated) => {
                    println!("wgpu surface outdated");
                    return;
                }
                Err(err) => {
                    println!("Surface error: {:?}", err);
                    return;
                }
                Ok(surface_texture) => {
                    let surface_view = surface_texture
                        .texture
                        .create_view(&wgpu::TextureViewDescriptor::default());

                    let mut encoder = state
                        .device
                        .create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });

                    // No background clearing - let the Game Boy screen show through

                    // Then render egui on top with Load to preserve the background
                    state.egui_renderer.end_frame_and_draw(
                        &state.device,
                        &state.queue,
                        &mut encoder,
                        window,
                        &surface_view,
                        screen_descriptor,
                    );

                    state.queue.submit(Some(encoder.finish()));
                    surface_texture.present();
                }
            }

            gui_action
        } else {
            None
        };

        // Handle GUI actions
        if let Some(action) = gui_action {
            // Move gui_action handling outside of the state borrow scope
            let action_copy = action;
            match action_copy {
                GuiAction::LoadRom(file_data) => {
                    if let Some(world) = &mut self.world {
                        match world.load_rom(file_data) {
                            Ok(_msg) => {
                                #[cfg(target_arch = "wasm32")]
                                web_sys::console::log_1(
                                    &format!("WASM ROM loaded successfully: {}", _msg).into(),
                                );
                            }
                            Err(_err) => {
                                #[cfg(target_arch = "wasm32")]
                                web_sys::console::log_1(
                                    &format!("WASM ROM load error: {}", _err).into(),
                                );
                            }
                        }
                    }
                }
                GuiAction::SaveState(_path) => {
                    #[cfg(not(target_arch = "wasm32"))]
                    if let Some(world) = &self.world {
                        match world.save_state(_path) {
                            Ok(_msg) => {
                                // Success handling
                            }
                            Err(_err) => {
                                // Error handling
                            }
                        }
                    }
                }
                GuiAction::LoadState(file_data) => {
                    if let Some(world) = &mut self.world {
                        match world.load_state(file_data) {
                            Ok(_msg) => {
                                // Success handling
                            }
                            Err(_err) => {
                                // Error handling
                            }
                        }
                    }
                }
                GuiAction::Exit => {
                    window.request_redraw();
                    std::process::exit(0);
                }
                GuiAction::StepCycles(cycles) => {
                    if let Some(world) = &mut self.world {
                        world.step_multiple_cycles = Some(cycles);
                    }
                }
                GuiAction::StepFrames(frames) => {
                    if let Some(world) = &mut self.world {
                        world.step_multiple_frames = Some(frames);
                    }
                }
                GuiAction::Restart => {
                    if let Some(world) = &mut self.world {
                        world.restart();
                    }
                }
                GuiAction::TogglePause => {
                    if let Some(world) = &mut self.world {
                        world.toggle_pause();
                        // Update our tracking of user pause state
                        self.user_paused = world.is_paused;
                        self.manually_paused = self.user_paused || world.error_state.is_some();
                    }
                }
                GuiAction::ClearError => {
                    if let Some(world) = &mut self.world {
                        world.clear_error();
                        // Update pause state after clearing error
                        self.manually_paused = self.user_paused || world.error_state.is_some();
                    }
                }
                GuiAction::SetBreakpoint(address) => {
                    if let Some(world) = &mut self.world {
                        world.add_breakpoint(address);
                    }
                }
                GuiAction::RemoveBreakpoint(address) => {
                    if let Some(world) = &mut self.world {
                        world.remove_breakpoint(address);
                    }
                }
            }
        }

        // Request another redraw to keep the emulator running continuously
        if let Some(window) = &self.window {
            window.request_redraw();
        }
    }

    fn handle_input_events(&mut self, event_loop: &ActiveEventLoop) {
        if let Some(input) = &self.input {
            if input.key_pressed(KeyCode::Escape) || input.close_requested() {
                event_loop.exit();
                return;
            }

            // Handle F key for frame stepping with debounce
            if input.key_pressed(KeyCode::KeyF) {
                if let Some(world) = &mut self.world
                    && (self.manually_paused || world.error_state.is_some()) {
                        // Initial press - execute immediately
                        world.step_single_frame = true;
                        let now = Instant::now();
                        self.f_key_press_time = Some(now);
                        self.f_last_repeat_time = Some(now);
                        self.f_key_processed_initial = true;
                        if let Some(window) = &self.window {
                            window.request_redraw();
                        }
                    }
            } else if input.key_held(KeyCode::KeyF) {
                if let Some(world) = &mut self.world
                    && (self.manually_paused || world.error_state.is_some())
                        && let Some(press_time) = self.f_key_press_time
                    {
                        // Check if debounce period has elapsed
                        const DEBOUNCE_DURATION: Duration = Duration::from_millis(250);
                        const REPEAT_INTERVAL: Duration = Duration::from_millis(67);
                        if press_time.elapsed() >= DEBOUNCE_DURATION {
                            // Check if enough time has passed since last repeat
                            if let Some(last_repeat) = self.f_last_repeat_time
                                && last_repeat.elapsed() >= REPEAT_INTERVAL
                            {
                                world.step_single_frame = true;
                                self.f_last_repeat_time = Some(Instant::now());
                                if let Some(window) = &self.window {
                                    window.request_redraw();
                                }
                            }
                        }
                    }
            } else {
                // Key released - reset state
                self.f_key_press_time = None;
                self.f_key_processed_initial = false;
                self.f_last_repeat_time = None;
            }

            // Handle N key for cycle stepping with debounce
            if input.key_pressed(KeyCode::KeyN) {
                if let Some(world) = &mut self.world
                    && (self.manually_paused || world.error_state.is_some()) {
                        // Initial press - execute immediately
                        world.step_single_cycle = true;
                        let now = Instant::now();
                        self.n_key_press_time = Some(now);
                        self.n_last_repeat_time = Some(now);
                        self.n_key_processed_initial = true;
                        if let Some(window) = &self.window {
                            window.request_redraw();
                        }
                    }
            } else if input.key_held(KeyCode::KeyN) {
                if let Some(world) = &mut self.world
                    && (self.manually_paused || world.error_state.is_some())
                        && let Some(press_time) = self.n_key_press_time
                    {
                        const DEBOUNCE_DURATION: Duration = Duration::from_millis(250);
                        const REPEAT_INTERVAL: Duration = Duration::from_millis(67);
                        // Check if debounce period has elapsed
                        if press_time.elapsed() >= DEBOUNCE_DURATION {
                            // Check if enough time has passed since last repeat
                            if let Some(last_repeat) = self.n_last_repeat_time
                                && last_repeat.elapsed() >= REPEAT_INTERVAL
                            {
                                world.step_single_cycle = true;
                                self.n_last_repeat_time = Some(Instant::now());
                                if let Some(window) = &self.window {
                                    window.request_redraw();
                                }
                            }
                        }
                    }
            } else {
                // Key released - reset state
                self.n_key_press_time = None;
                self.n_key_processed_initial = false;
                self.n_last_repeat_time = None;
            }

            // Handle Game Boy input based on keybinds
            if let Some(world) = &mut self.world {
                world.update();
                if let Some(window) = &self.window {
                    window.request_redraw();
                }
            }
        }
    }
}

impl ApplicationHandler for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        #[cfg(target_arch = "wasm32")]
        let window = {
            use wasm_bindgen::JsCast;
            use winit::platform::web::WindowAttributesExtWebSys;

            // Get the existing canvas element
            let canvas = web_sys::window()
                .and_then(|win| win.document())
                .and_then(|doc| doc.get_element_by_id("rustyboi-canvas"))
                .and_then(|element| element.dyn_into::<web_sys::HtmlCanvasElement>().ok())
                .expect("Failed to find canvas element with id 'rustyboi-canvas'");

            let window_attributes = Window::default_attributes().with_canvas(Some(canvas));

            event_loop.create_window(window_attributes).unwrap()
        };

        #[cfg(not(target_arch = "wasm32"))]
        let window = event_loop
            .create_window(Window::default_attributes())
            .unwrap();

        #[cfg(not(target_arch = "wasm32"))]
        {
            pollster::block_on(self.set_window(window));
            // Request initial redraw to start the rendering loop
            if let Some(window) = self.window.as_ref() {
                window.request_redraw();
            }
        }

        #[cfg(target_arch = "wasm32")]
        {
            // For WASM, store the window and start async initialization
            let window = Arc::new(window);
            let scale = self.config.as_ref().map(|c| c.scale as u32).unwrap_or(7);
            let initial_width = 160 * scale;
            let initial_height = 144 * scale;
            let _ = window.request_inner_size(PhysicalSize::new(initial_width, initial_height));

            // Log window size for debugging
            let actual_size = window.inner_size();
            web_sys::console::log_1(
                &format!(
                    "WASM window size: {}x{}",
                    actual_size.width, actual_size.height
                )
                .into(),
            );

            self.window = Some(window.clone());

            // Calculate current viewport size first
            let current_size = web_sys::window()
                .and_then(|win| {
                    let width = win.inner_width().ok()?.as_f64()? as u32;
                    let height = win.inner_height().ok()?.as_f64()? as u32;
                    // Ensure minimum size
                    let width = width.max(800);
                    let height = height.max(600);
                    Some(PhysicalSize::new(width, height))
                })
                .unwrap_or(PhysicalSize::new(800, 600));

            web_sys::console::log_1(
                &format!(
                    "WASM detected viewport size: {}x{}",
                    current_size.width, current_size.height
                )
                .into(),
            );

            // For WASM, add the canvas to the DOM and set up proper sizing
            use wasm_bindgen::JsCast;

            // Set up window resize handler
            use std::rc::Rc;
            let window_clone = Rc::new(window.clone());
            let closure = wasm_bindgen::closure::Closure::wrap(Box::new({
                let window = window_clone.clone();
                move |_e: web_sys::Event| {
                    // Get current viewport size
                    let new_size = web_sys::window()
                        .and_then(|win| {
                            let width = win.inner_width().ok()?.as_f64()? as u32;
                            let height = win.inner_height().ok()?.as_f64()? as u32;
                            Some(PhysicalSize::new(width, height))
                        })
                        .unwrap_or(PhysicalSize::new(1360, 768));

                    let _ = window.request_inner_size(new_size);
                }
            }) as Box<dyn FnMut(_)>);

            web_sys::window()
                .unwrap()
                .add_event_listener_with_callback("resize", closure.as_ref().unchecked_ref())
                .unwrap();
            closure.forget();

            // Don't override canvas size - let JavaScript handle sizing
            web_sys::console::log_1(
                &"WASM skipping window size request to preserve canvas sizing".into(),
            );

            // Start async initialization
            if !self.initializing {
                self.initializing = true;
                let instance = self.instance.clone();
                let shared_state = self.shared_state.clone();
                wasm_bindgen_futures::spawn_local(async move {
                    let surface = instance
                        .create_surface(window.clone())
                        .expect("Failed to create surface!");

                    let actual_size = window.inner_size();
                    let width = if actual_size.width > 0 {
                        actual_size.width
                    } else {
                        initial_width
                    };
                    let height = if actual_size.height > 0 {
                        actual_size.height
                    } else {
                        initial_height
                    };

                    let state =
                        app_state::AppState::new(&instance, surface, &window, width, height).await;

                    // Set the shared state
                    *shared_state.borrow_mut() = Some(state);

                    // Request multiple redraws to ensure rendering starts
                    window.request_redraw();

                    // Use web_sys::setTimeout for delayed redraw
                    let window_clone = window.clone();
                    let closure = wasm_bindgen::closure::Closure::wrap(Box::new(move || {
                        window_clone.request_redraw();
                    })
                        as Box<dyn FnMut()>);

                    web_sys::window()
                        .unwrap()
                        .set_timeout_with_callback_and_timeout_and_arguments_0(
                            closure.as_ref().unchecked_ref(),
                            100,
                        )
                        .unwrap();
                    closure.forget();
                });
            }
        }
    }

    fn window_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        window_id: WindowId,
        event: WindowEvent,
    ) {
        // Update input handler and handle Game Boy inputs
        if let Some(input) = &mut self.input {
            // Create a proper event for the input handler
            let winit_event: winit::event::Event<()> = winit::event::Event::WindowEvent {
                window_id,
                event: event.clone(),
            };
            if input.update(&winit_event) {
                self.handle_input_events(event_loop);
            }
        }

        // Let egui process the event, but only if state exists
        let mut needs_repaint = false;
        if let (Some(state), Some(window)) = (self.state.as_mut(), self.window.as_ref()) {
            needs_repaint = state.egui_renderer.handle_input(window, &event);
        }

        match event {
            WindowEvent::CloseRequested => {
                println!("The close button was pressed; stopping");
                event_loop.exit();
            }
            WindowEvent::RedrawRequested => {
                self.handle_redraw();
            }
            WindowEvent::Resized(new_size) => {
                self.handle_resized(new_size.width, new_size.height);
                // Request redraw on resize
                if let Some(window) = self.window.as_ref() {
                    window.request_redraw();
                }
            }
            _ => {
                // For other events, request redraw only if egui needs it
                if needs_repaint
                    && let Some(window) = self.window.as_ref() {
                        window.request_redraw();
                    }
            }
        }
    }
}
