#![warn(clippy::all)]
#![forbid(unsafe_code)]

mod app;
mod app_state;
mod config;
mod egui_renderer;
mod framework;
mod input;
mod run;
mod world;

pub use crate::run::run;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

// Export the run function for WASM
#[cfg(target_arch = "wasm32")]
#[wasm_bindgen(start)]
pub fn start() {
    wasm_bindgen_futures::spawn_local(run::run());
}
