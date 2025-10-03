#[cfg(not(target_arch = "wasm32"))]
use crate::config;
#[cfg(not(target_arch = "wasm32"))]
use rustyboi_advance_core_lib::gba;

use winit::event_loop::{ControlFlow, EventLoop};

#[cfg(not(target_arch = "wasm32"))]
pub fn run_with_gui(
    gba: gba::GBA,
    config: config::CleanConfig,
) -> Result<(), Box<dyn std::error::Error>> {
    let event_loop = EventLoop::new().unwrap();
    event_loop.set_control_flow(ControlFlow::Poll);
    let mut app = crate::app::App::new_with_gba(gba, config);
    event_loop
        .run_app(&mut app)
        .map_err(|e| Box::new(e) as Box<dyn std::error::Error>)
}

#[cfg(target_arch = "wasm32")]
pub async fn run_with_gui_async() {
    let event_loop = EventLoop::new().unwrap();
    event_loop.set_control_flow(ControlFlow::Poll);
    let mut app = crate::app::App::new();
    event_loop.run_app(&mut app).expect("Failed to run app");
}

pub async fn run() {
    #[cfg(target_arch = "wasm32")]
    {
        std::panic::set_hook(Box::new(console_error_panic_hook::hook));
        // Only initialize logger if it hasn't been initialized yet
        let _ = console_log::init_with_level(log::Level::Trace);

        wasm_bindgen_futures::spawn_local(run_with_gui_async());
    }

    #[cfg(not(target_arch = "wasm32"))]
    {
        use crate::config;
        use clap::Parser;
        use rustyboi_advance_core_lib::{cartridge, gba};

        let config = config::RawConfig::parse().clean();

        let mut gba = gba::GBA::new();

        if let Some(state) = config.state.as_ref() {
            gba = gba::GBA::from_state_file(state).expect("Failed to load state file");
        }

        if let Some(rom) = config.rom.as_ref() {
            let cartridge =
                cartridge::Cartridge::load_from_path(rom).expect("Failed to load ROM file");
            gba.insert(cartridge);
        }

        if let Some(bios) = config.bios.as_ref() {
            gba.load_bios(bios).expect("Failed to load BIOS file");
        }

        if config.skip_bios {
            gba.skip_bios();
        }

        match run_with_gui(gba, config) {
            Ok(_) => {}
            Err(e) => eprintln!("Error: {}", e),
        }
    }
}
