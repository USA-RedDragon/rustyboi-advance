use serde::{Deserialize, Serialize};

/// DMA channel number (0-3)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DmaChannel {
    Dma0 = 0,
    Dma1 = 1,
    Dma2 = 2,
    Dma3 = 3,
}

impl DmaChannel {
    pub fn from_index(index: usize) -> Option<Self> {
        match index {
            0 => Some(DmaChannel::Dma0),
            1 => Some(DmaChannel::Dma1),
            2 => Some(DmaChannel::Dma2),
            3 => Some(DmaChannel::Dma3),
            _ => None,
        }
    }
}

/// Destination address control
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DestAddrControl {
    Increment = 0,
    Decrement = 1,
    Fixed = 2,
    IncrementReload = 3,
}

impl From<u16> for DestAddrControl {
    fn from(value: u16) -> Self {
        match (value >> 5) & 0x3 {
            0 => DestAddrControl::Increment,
            1 => DestAddrControl::Decrement,
            2 => DestAddrControl::Fixed,
            3 => DestAddrControl::IncrementReload,
            _ => unreachable!(),
        }
    }
}

/// Source address control
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SourceAddrControl {
    Increment = 0,
    Decrement = 1,
    Fixed = 2,
}

impl From<u16> for SourceAddrControl {
    fn from(value: u16) -> Self {
        match (value >> 7) & 0x3 {
            0 => SourceAddrControl::Increment,
            1 => SourceAddrControl::Decrement,
            2 => SourceAddrControl::Fixed,
            _ => SourceAddrControl::Fixed, // 3 is prohibited, treat as fixed
        }
    }
}

/// DMA start timing
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DmaStartTiming {
    Immediate = 0,
    VBlank = 1,
    HBlank = 2,
    Special = 3, // DMA0=Prohibited, DMA1/2=Sound FIFO, DMA3=Video Capture
}

impl From<u16> for DmaStartTiming {
    fn from(value: u16) -> Self {
        match (value >> 12) & 0x3 {
            0 => DmaStartTiming::Immediate,
            1 => DmaStartTiming::VBlank,
            2 => DmaStartTiming::HBlank,
            3 => DmaStartTiming::Special,
            _ => unreachable!(),
        }
    }
}

/// DMA control register structure
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct DmaControl {
    pub dest_addr_control: DestAddrControl,
    pub source_addr_control: SourceAddrControl,
    pub repeat: bool,
    pub transfer_32bit: bool, // false = 16bit, true = 32bit
    pub game_pak_drq: bool,   // DMA3 only
    pub start_timing: DmaStartTiming,
    pub irq_enable: bool,
    pub enable: bool,
}

impl Default for DmaControl {
    fn default() -> Self {
        Self {
            dest_addr_control: DestAddrControl::Increment,
            source_addr_control: SourceAddrControl::Increment,
            repeat: false,
            transfer_32bit: false,
            game_pak_drq: false,
            start_timing: DmaStartTiming::Immediate,
            irq_enable: false,
            enable: false,
        }
    }
}

impl From<u16> for DmaControl {
    fn from(value: u16) -> Self {
        Self {
            dest_addr_control: DestAddrControl::from(value),
            source_addr_control: SourceAddrControl::from(value),
            repeat: (value & (1 << 9)) != 0,
            transfer_32bit: (value & (1 << 10)) != 0,
            game_pak_drq: (value & (1 << 11)) != 0,
            start_timing: DmaStartTiming::from(value),
            irq_enable: (value & (1 << 14)) != 0,
            enable: (value & (1 << 15)) != 0,
        }
    }
}

impl From<DmaControl> for u16 {
    fn from(ctrl: DmaControl) -> Self {
        let mut value = 0u16;
        value |= (ctrl.dest_addr_control as u16) << 5;
        value |= (ctrl.source_addr_control as u16) << 7;
        if ctrl.repeat {
            value |= 1 << 9;
        }
        if ctrl.transfer_32bit {
            value |= 1 << 10;
        }
        if ctrl.game_pak_drq {
            value |= 1 << 11;
        }
        value |= (ctrl.start_timing as u16) << 12;
        if ctrl.irq_enable {
            value |= 1 << 14;
        }
        if ctrl.enable {
            value |= 1 << 15;
        }
        value
    }
}

/// Individual DMA channel state
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DmaChannelState {
    pub channel: DmaChannel,
    
    // Written registers (not modified during transfer)
    pub source_addr: u32,
    pub dest_addr: u32,
    pub word_count: u16,
    pub control: DmaControl,
    
    // Internal working registers
    internal_source: u32,
    internal_dest: u32,
    internal_count: u32,
    
    // State tracking
    pub active: bool,
    pub pending: bool, // Waiting for trigger (VBlank, HBlank, etc.)
}

impl DmaChannelState {
    pub fn new(channel: DmaChannel) -> Self {
        Self {
            channel,
            source_addr: 0,
            dest_addr: 0,
            word_count: 0,
            control: DmaControl::default(),
            internal_source: 0,
            internal_dest: 0,
            internal_count: 0,
            active: false,
            pending: false,
        }
    }
    
    /// Called when enable bit changes from 0 to 1
    pub fn reload(&mut self) {
        // Mask addresses according to GBA specifications
        let source_mask = match self.channel {
            DmaChannel::Dma0 => 0x07FFFFFF, // Internal memory only
            _ => 0x0FFFFFFF,                // Any memory
        };
        
        let dest_mask = match self.channel {
            DmaChannel::Dma3 => 0x0FFFFFFF, // Any memory
            _ => 0x07FFFFFF,                // Internal memory only
        };
        
        self.internal_source = self.source_addr & source_mask;
        self.internal_dest = self.dest_addr & dest_mask;
        
        // Set word count - 0 means max
        self.internal_count = if self.word_count == 0 {
            match self.channel {
                DmaChannel::Dma3 => 0x10000, // 64K for DMA3
                _ => 0x4000,                 // 16K for DMA0-2
            }
        } else {
            self.word_count as u32
        };
    }
    
    /// Reload just the count and optionally dest address (for repeat mode)
    pub fn reload_for_repeat(&mut self) {
        self.internal_count = if self.word_count == 0 {
            match self.channel {
                DmaChannel::Dma3 => 0x10000,
                _ => 0x4000,
            }
        } else {
            self.word_count as u32
        };
        
        // Reload dest address if using Increment+Reload mode
        if matches!(self.control.dest_addr_control, DestAddrControl::IncrementReload) {
            let dest_mask = match self.channel {
                DmaChannel::Dma3 => 0x0FFFFFFF,
                _ => 0x07FFFFFF,
            };
            self.internal_dest = self.dest_addr & dest_mask;
        }
    }
    
    /// Check if this DMA should start based on timing
    pub fn should_trigger(&self, timing: DmaStartTiming) -> bool {
        self.control.enable && self.control.start_timing == timing && self.pending
    }
}

/// DMA controller managing all 4 channels
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DmaController {
    channels: [DmaChannelState; 4],
    
    /// Track if any DMA is currently active
    pub active_channel: Option<DmaChannel>,
    
    /// IRQ flags for each channel
    pub irq_flags: [bool; 4],
}

impl Default for DmaController {
    fn default() -> Self {
        Self::new()
    }
}

impl DmaController {
    pub fn new() -> Self {
        Self {
            channels: [
                DmaChannelState::new(DmaChannel::Dma0),
                DmaChannelState::new(DmaChannel::Dma1),
                DmaChannelState::new(DmaChannel::Dma2),
                DmaChannelState::new(DmaChannel::Dma3),
            ],
            active_channel: None,
            irq_flags: [false; 4],
        }
    }
    
    /// Get a reference to a DMA channel
    pub fn get_channel(&self, channel: DmaChannel) -> &DmaChannelState {
        &self.channels[channel as usize]
    }
    
    /// Get a mutable reference to a DMA channel
    pub fn get_channel_mut(&mut self, channel: DmaChannel) -> &mut DmaChannelState {
        &mut self.channels[channel as usize]
    }
    
    /// Write to DMA source address register
    pub fn write_sad(&mut self, channel: DmaChannel, value: u32) {
        let ch = self.get_channel_mut(channel);
        ch.source_addr = value;
    }
    
    /// Write to DMA destination address register
    pub fn write_dad(&mut self, channel: DmaChannel, value: u32) {
        let ch = self.get_channel_mut(channel);
        ch.dest_addr = value;
    }
    
    /// Write to DMA word count register
    pub fn write_cnt_l(&mut self, channel: DmaChannel, value: u16) {
        let ch = self.get_channel_mut(channel);
        ch.word_count = value;
    }
    
    /// Write to DMA control register
    pub fn write_cnt_h(&mut self, channel: DmaChannel, value: u16) {
        let ch = self.get_channel_mut(channel);
        let old_enable = ch.control.enable;
        ch.control = DmaControl::from(value);
        
        // If enable bit changed from 0 to 1, reload internal registers
        if !old_enable && ch.control.enable {
            ch.reload();
            
            // For immediate transfers, mark as pending
            if ch.control.start_timing == DmaStartTiming::Immediate {
                ch.pending = true;
            } else {
                // For other timings, mark as pending and wait for trigger
                ch.pending = true;
            }
        }
        
        // If disabled, clear active and pending flags
        if !ch.control.enable {
            ch.active = false;
            ch.pending = false;
        }
    }
    
    /// Read DMA control register
    pub fn read_cnt_h(&self, channel: DmaChannel) -> u16 {
        u16::from(self.get_channel(channel).control)
    }
    
    /// Trigger DMAs with specific timing (called by PPU/other systems)
    pub fn trigger(&mut self, timing: DmaStartTiming) {
        for channel in &mut self.channels {
            if channel.should_trigger(timing) {
                channel.active = true;
            }
        }
    }
    
    /// Check if any DMA is ready to run
    pub fn has_pending_dma(&self) -> bool {
        self.channels.iter().any(|ch| ch.pending && ch.control.start_timing == DmaStartTiming::Immediate)
            || self.channels.iter().any(|ch| ch.active)
    }
    
    /// Get the highest priority active/pending DMA channel
    pub fn get_highest_priority_channel(&mut self) -> Option<DmaChannel> {
        // DMA0 has highest priority, DMA3 has lowest
        for i in 0..4 {
            let ch = &self.channels[i];
            if ch.active || (ch.pending && ch.control.start_timing == DmaStartTiming::Immediate) {
                return Some(ch.channel);
            }
        }
        None
    }
    
    /// Check if any DMA IRQ is pending
    pub fn has_irq(&self, channel: DmaChannel) -> bool {
        self.irq_flags[channel as usize]
    }
    
    /// Clear DMA IRQ flag
    pub fn clear_irq(&mut self, channel: DmaChannel) {
        self.irq_flags[channel as usize] = false;
    }
    
    /// Check if CPU should be paused (any DMA active)
    pub fn is_cpu_paused(&self) -> bool {
        self.channels.iter().any(|ch| ch.active)
    }
}

/// Standalone function to run DMA transfers, avoiding borrow checker issues
/// This function takes separate mutable references to the DMA controller and MMIO
pub fn run_dma_transfers(
    dma: &mut DmaController,
    mmio: &mut crate::memory::mmio::Mmio,
    wait_config: &crate::cpu::arm7tdmi::WaitStateConfig,
) -> u32 {
    let mut total_cycles = 0;
    
    loop {
        // Get highest priority channel
        let channel = match dma.get_highest_priority_channel() {
            Some(ch) => ch,
            None => break,
        };
        
        let ch = &mut dma.channels[channel as usize];
        
        // Mark as active if it was pending
        if ch.pending && !ch.active {
            ch.active = true;
            ch.pending = false;
        }
        
        // Special handling for Sound FIFO DMA (DMA1/2 with Special timing)
        if matches!(channel, DmaChannel::Dma1 | DmaChannel::Dma2)
            && ch.control.start_timing == DmaStartTiming::Special
        {
            // Sound FIFO always transfers 4 units of 32 bits (16 bytes)
            // regardless of word count and transfer type settings
            let dest = ch.internal_dest;
            
            // Verify destination is FIFO_A or FIFO_B
            if dest == 0x040000A0 || dest == 0x040000A4 {
                for _ in 0..4 {
                    let value = (mmio as &dyn crate::memory::Addressable).read32(ch.internal_source);
                    mmio.write32(dest, value);
                    
                    // Source address increments, dest stays fixed
                    match ch.control.source_addr_control {
                        SourceAddrControl::Increment => ch.internal_source = ch.internal_source.wrapping_add(4),
                        SourceAddrControl::Decrement => ch.internal_source = ch.internal_source.wrapping_sub(4),
                        SourceAddrControl::Fixed => {}
                    }
                    
                    total_cycles += 4; // Simplified timing for FIFO
                }
                
                // Mark as complete, will be retriggered by audio
                ch.active = false;
                
                if ch.control.repeat {
                    ch.pending = true; // Will be triggered again by sound
                } else {
                    ch.control.enable = false;
                }
                
                if ch.control.irq_enable {
                    dma.irq_flags[channel as usize] = true;
                }
                
                continue;
            }
        }
        
        // Normal DMA transfer
        let cycles = transfer_unit_impl(dma, mmio, channel, wait_config);
        total_cycles += cycles;
        
        let ch = &mut dma.channels[channel as usize];
        
        // Check if transfer is complete
        if ch.internal_count == 0 {
            ch.active = false;
            
            // Handle repeat mode
            if ch.control.repeat {
                ch.reload_for_repeat();
                ch.pending = true; // Wait for next trigger
            } else {
                // Clear enable bit
                ch.control.enable = false;
                ch.pending = false;
            }
            
            // Trigger IRQ if enabled
            if ch.control.irq_enable {
                dma.irq_flags[channel as usize] = true;
            }
        }
    }
    
    total_cycles
}

/// Internal helper to transfer a single unit
fn transfer_unit_impl(
    dma: &mut DmaController,
    mmio: &mut crate::memory::mmio::Mmio,
    channel: DmaChannel,
    wait_config: &crate::cpu::arm7tdmi::WaitStateConfig,
) -> u32 {
    let ch = &mut dma.channels[channel as usize];
    
    if ch.internal_count == 0 {
        return 0;
    }
    
    // Determine transfer size
    let transfer_size = if ch.control.transfer_32bit { 4 } else { 2 };
    
    // Read from source
    let value = if ch.control.transfer_32bit {
        (mmio as &dyn crate::memory::Addressable).read32(ch.internal_source)
    } else {
        (mmio as &dyn crate::memory::Addressable).read16(ch.internal_source) as u32
    };
    
    // Write to destination
    if ch.control.transfer_32bit {
        mmio.write32(ch.internal_dest, value);
    } else {
        mmio.write16(ch.internal_dest, value as u16);
    }
    
    // Update addresses based on control settings
    match ch.control.source_addr_control {
        SourceAddrControl::Increment => ch.internal_source = ch.internal_source.wrapping_add(transfer_size),
        SourceAddrControl::Decrement => ch.internal_source = ch.internal_source.wrapping_sub(transfer_size),
        SourceAddrControl::Fixed => {}
    }
    
    match ch.control.dest_addr_control {
        DestAddrControl::Increment | DestAddrControl::IncrementReload => {
            ch.internal_dest = ch.internal_dest.wrapping_add(transfer_size);
        }
        DestAddrControl::Decrement => {
            ch.internal_dest = ch.internal_dest.wrapping_sub(transfer_size);
        }
        DestAddrControl::Fixed => {}
    }
    
    // Decrement count
    ch.internal_count -= 1;
    
    // Calculate cycle timing
    // First unit is 2N, subsequent units are 2S
    // For simplicity, we'll calculate actual wait states based on memory regions
    let is_sequential = ch.internal_count > 0;
    
    let read_timing = crate::memory::timing::get_memory_timing(ch.internal_source, wait_config, is_sequential);
    let write_timing = crate::memory::timing::get_memory_timing(ch.internal_dest, wait_config, is_sequential);
    
    // Total cycles = read cycles + write cycles
    let mut cycles = 2; // Base cycles
    cycles += read_timing.wait_states;
    cycles += write_timing.wait_states;
    
    // Add internal processing cycles if both source and dest are in game pak
    if ch.internal_source >= 0x08000000 && ch.internal_dest >= 0x08000000 {
        cycles += 2; // Additional 2I cycles
    }
    
    cycles
}
