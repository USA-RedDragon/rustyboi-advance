use clap::Parser;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct RawConfig {
    /// BIOS file path, optional
    #[arg(short, long)]
    bios: Option<String>,

    /// ROM file path, optional
    #[arg(short, long)]
    rom: Option<String>,

    /// Save state file path to load on startup, optional
    #[arg(long)]
    state: Option<String>,

    /// Scale factor for GUI
    #[arg(short, long, default_value_t = 5)]
    scale: u8,

    /// Skip BIOS on startup
    #[arg(long, default_value_t = false)]
    skip_bios: bool,
}

#[derive(Clone)]
pub struct CleanConfig {
    // path to BIOS file
    pub bios: Option<String>,
    // path to ROM file
    pub rom: Option<String>,
    #[cfg(not(target_arch = "wasm32"))]
    // path to save state to load on startup
    pub state: Option<String>,
    // GUI scale factor
    pub scale: u8,
    #[cfg(not(target_arch = "wasm32"))]
    // skip BIOS on startup
    pub skip_bios: bool,
}

impl RawConfig {
    pub fn clean(self) -> CleanConfig {
        let mut _skip_bios = self.skip_bios;
        #[cfg(not(target_arch = "wasm32"))]
        {
            if self.bios.is_none() {
                _skip_bios = true;
            }
        }

        CleanConfig {
            bios: self.bios,
            rom: self.rom,
            #[cfg(not(target_arch = "wasm32"))]
            state: self.state,
            scale: self.scale,
            #[cfg(not(target_arch = "wasm32"))]
            skip_bios: _skip_bios,
        }
    }
}

impl CleanConfig {
    #[cfg(target_arch = "wasm32")]
    /// Get default configuration by parsing empty arguments with clap
    pub fn default() -> Self {
        use clap::Parser;
        let raw_config = RawConfig::parse_from(std::iter::empty::<&str>());
        raw_config.clean()
    }
}
