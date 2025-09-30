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


fn main() {
    pollster::block_on(run::run());
}
