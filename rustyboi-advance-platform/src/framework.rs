use egui_wgpu::ScreenDescriptor;
use winit::window::Window;

pub struct Framework {
    egui_state: egui_winit::State,
    screen_descriptor: ScreenDescriptor,
}

impl Framework {
    pub fn handle_event(&mut self, window: &Window, event: &winit::event::WindowEvent) {
        let _ = self.egui_state.on_window_event(window, event);
    }

    pub fn scale_factor(&mut self, scale_factor: f64) {
        self.screen_descriptor.pixels_per_point = scale_factor as f32;
    }
}
