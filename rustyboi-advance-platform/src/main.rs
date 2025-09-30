#![warn(clippy::all)]
#![forbid(unsafe_code)]

mod app;
mod config;
mod framework;
mod input;
mod renderer;
mod run;

use pollster;

fn main() {
    pollster::block_on(run::run());
}
