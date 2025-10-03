use crate::{cpu::registers, memory};
use rustyboi_advance_debugger_lib::disassembler::{Disassembler, Instruction};
use serde::{Deserialize, Serialize};

/// Cycle types for ARM7TDMI timing
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CycleType {
    /// N-cycle: Normal cycle, access to memory
    N,
    /// S-cycle: Sequential cycle, consecutive memory access
    S,
    /// I-cycle: Internal cycle, no memory access
    I,
}

/// Pipeline stages for ARM7TDMI
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PipelineStage {
    Fetch,
    Decode,
    Execute,
}

/// Pipeline instruction container
#[derive(Debug, Clone)]
pub struct PipelineInstruction {
    pub instruction: Option<Instruction>,
    /// Program counter when this instruction was fetched
    pub pc: u32,
    /// Whether this is a Thumb instruction
    pub is_thumb: bool,
}

impl Default for PipelineInstruction {
    fn default() -> Self {
        Self {
            instruction: None,
            pc: 0,
            is_thumb: false,
        }
    }
}

/// Pipeline state for the ARM7TDMI 3-stage pipeline
#[derive(Debug, Clone)]
pub struct Pipeline {
    /// Fetch stage - instruction currently being fetched
    pub fetch: Option<PipelineInstruction>,
    /// Decode stage - instruction currently being decoded
    pub decode: Option<PipelineInstruction>,
    /// Execute stage - instruction currently being executed
    pub execute: Option<PipelineInstruction>,
    /// Pipeline flush flag
    pub flush_pending: bool,
}

impl Default for Pipeline {
    fn default() -> Self {
        Self {
            fetch: None,
            decode: None,
            execute: None,
            flush_pending: false,
        }
    }
}

/// Wait state configuration for different memory regions
#[derive(Debug, Clone)]
pub struct WaitStateConfig {
    /// Wait states for ROM WS0 (first access, sequential access)
    pub rom_ws0: (u32, u32),
    /// Wait states for ROM WS1 (first access, sequential access)
    pub rom_ws1: (u32, u32),
    /// Wait states for ROM WS2 (first access, sequential access)
    pub rom_ws2: (u32, u32),
    /// Wait states for SRAM (single value applies to all accesses)
    pub sram: u32,
    /// Prefetch buffer enabled flag
    pub prefetch_enabled: bool,
}

impl Default for WaitStateConfig {
    fn default() -> Self {
        Self {
            // Default values from GBA hardware manual
            rom_ws0: (4, 2), // 4N + 2S cycles by default
            rom_ws1: (4, 4), // 4N + 4S cycles by default
            rom_ws2: (4, 8), // 4N + 8S cycles by default
            sram: 8,         // 8 cycles by default
            prefetch_enabled: false,
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub struct ARM7TDMI {
    pub registers: registers::Registers,
    pub halted: bool,
    pub stopped: bool,

    /// 3-stage pipeline state
    #[serde(skip)]
    pub pipeline: Pipeline,

    /// Wait state configuration
    #[serde(skip)]
    pub wait_config: WaitStateConfig,

    /// Last memory access address for sequential detection
    last_access_addr: Option<u32>,

    /// Accumulated cycle counts for debugging and timing
    pub cycle_counts: CycleCounts,
}

/// Cycle counting for performance analysis and debugging
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CycleCounts {
    pub total_cycles: u64,
    pub n_cycles: u64,
    pub s_cycles: u64,
    pub i_cycles: u64,
    pub instructions_executed: u64,
}

impl Default for ARM7TDMI {
    fn default() -> Self {
        Self::new()
    }
}

impl ARM7TDMI {
    pub fn new() -> Self {
        ARM7TDMI {
            registers: registers::Registers::new(),
            halted: false,
            stopped: false,
            pipeline: Pipeline::default(),
            wait_config: WaitStateConfig::default(),
            last_access_addr: None,
            cycle_counts: CycleCounts::default(),
        }
    }

    pub fn step(&mut self, mmio: &mut memory::mmio::Mmio) -> u32 {
        // If halted, check if we should exit halt state
        if self.halted {
            // CPU is halted and no interrupt is pending, consume 1 cycle and return
            self.add_cycles(CycleType::I, 0);
            return 1;
        }

        // If stopped, no cycles are consumed
        if self.stopped {
            return 0;
        }

        // Advance the pipeline by one stage
        self.advance_pipeline(mmio)
    }

    fn execute(&mut self, instruction: Instruction, _mmio: &mut memory::mmio::Mmio) -> u32 {
        {
            unimplemented!("Unimplemented ARM opcode: {}", instruction.to_string());
        }
    }

    /// Flush the pipeline (used during branches, exceptions, etc.)
    pub fn flush_pipeline(&mut self) {
        self.pipeline.fetch = None;
        self.pipeline.decode = None;
        self.pipeline.execute = None;
        self.pipeline.flush_pending = false;
    }

    /// Mark pipeline for flush on next cycle
    pub fn request_pipeline_flush(&mut self) {
        self.pipeline.flush_pending = true;
    }

    /// Check if the next memory access would be sequential
    fn is_sequential_access(&self, addr: u32) -> bool {
        if let Some(last_addr) = self.last_access_addr {
            // Sequential if accessing the next word (4 bytes for ARM, 2 for Thumb)
            let is_thumb = self.registers.get_flag(registers::Flag::ThumbState);
            let expected_offset = if is_thumb { 2 } else { 4 };
            addr == last_addr.wrapping_add(expected_offset)
        } else {
            false
        }
    }

    /// Update cycle counts based on cycle type and wait states
    fn add_cycles(&mut self, cycle_type: CycleType, wait_states: u32) {
        let total_cycles = 1 + wait_states; // Base cycle + wait states

        self.cycle_counts.total_cycles += total_cycles as u64;

        match cycle_type {
            CycleType::N => self.cycle_counts.n_cycles += total_cycles as u64,
            CycleType::S => self.cycle_counts.s_cycles += total_cycles as u64,
            CycleType::I => self.cycle_counts.i_cycles += total_cycles as u64,
        }
    }

    /// Perform a memory read with proper timing calculation
    fn read_memory_timed(&mut self, mmio: &mut memory::mmio::Mmio, addr: u32) -> (u32, u32) {
        let is_sequential = self.is_sequential_access(addr);
        let timing = memory::timing::get_memory_timing(addr, &self.wait_config, is_sequential);
        let value = (mmio as &dyn memory::Addressable).read32(addr);

        self.add_cycles(timing.cycle_type, timing.wait_states);
        self.last_access_addr = Some(addr);

        (value, 1 + timing.wait_states)
    }

    /// Advance the pipeline by one stage
    fn advance_pipeline(&mut self, mmio: &mut memory::mmio::Mmio) -> u32 {
        let mut total_cycles = 0;

        // Handle pipeline flush if requested
        if self.pipeline.flush_pending {
            self.flush_pipeline();
            return 0; // Flush takes no additional cycles, but refill will
        }

        // Execute stage - execute the instruction currently in execute stage
        if let Some(execute_instr) = self.pipeline.execute.take() {
            total_cycles += self.execute(
                execute_instr
                    .instruction
                    .expect("No instruction in execute stage"),
                mmio,
            );
            self.cycle_counts.instructions_executed += 1;
        }

        // Decode -> Execute: Move decoded instruction to execute
        self.pipeline.execute = self.pipeline.decode.take();

        // Fetch -> Decode: Move fetched instruction to decode
        self.pipeline.decode = self.pipeline.fetch.take();

        // Fetch new instruction
        let pc = self.registers.pc;
        let (instruction, fetch_cycles) = self.read_memory_timed(mmio, pc);

        // Decode the instruction for structured access (only for ARM mode for now)
        let decoded_instruction = if !self.registers.get_flag(registers::Flag::ThumbState) {
            Some(Disassembler::decode_opcode(instruction, pc, |offset| {
                (mmio as &dyn memory::Addressable).read32(pc + offset)
            }))
        } else {
            None // TODO: Add Thumb instruction decoding
        };

        self.pipeline.fetch = Some(PipelineInstruction {
            instruction: decoded_instruction,
            pc,
            is_thumb: self.registers.get_flag(registers::Flag::ThumbState),
        });

        // Increment PC for next fetch
        let pc_increment = if self.registers.get_flag(registers::Flag::ThumbState) {
            2
        } else {
            4
        };
        self.registers.pc = self.registers.pc.wrapping_add(pc_increment);

        total_cycles + fetch_cycles
    }

    /// Initialize the pipeline by filling it with the first few instructions
    /// This should be called after reset or when starting execution
    pub fn prime_pipeline(&mut self, mmio: &mut memory::mmio::Mmio) {
        // Clear any existing pipeline state
        self.flush_pipeline();

        // Save the current PC since we don't want to advance it during priming
        let original_pc = self.registers.pc;

        // Manually fill the pipeline with the first 3 instructions
        // without advancing the actual PC for execution
        let is_thumb = self.registers.get_flag(registers::Flag::ThumbState);
        let instruction_size = if is_thumb { 2 } else { 4 };

        // Fill Execute stage (PC + 0)
        let (execute_instr, _) = self.read_memory_timed(mmio, original_pc);
        let execute_decoded = if !is_thumb {
            Some(Disassembler::decode_opcode(
                execute_instr,
                original_pc,
                |offset| (mmio as &dyn memory::Addressable).read32(original_pc + offset),
            ))
        } else {
            None // TODO: Add Thumb instruction decoding
        };
        self.pipeline.execute = Some(PipelineInstruction {
            instruction: execute_decoded,
            pc: original_pc,
            is_thumb,
        });

        // Fill Decode stage (PC + 4/2)
        let decode_pc = original_pc.wrapping_add(instruction_size);
        let (decode_instr, _) = self.read_memory_timed(mmio, decode_pc);
        let decode_decoded = if !is_thumb {
            Some(Disassembler::decode_opcode(
                decode_instr,
                decode_pc,
                |offset| (mmio as &dyn memory::Addressable).read32(decode_pc + offset),
            ))
        } else {
            None // TODO: Add Thumb instruction decoding
        };
        self.pipeline.decode = Some(PipelineInstruction {
            instruction: decode_decoded,
            pc: decode_pc,
            is_thumb,
        });

        // Fill Fetch stage (PC + 8/4)
        let fetch_pc = original_pc.wrapping_add(instruction_size * 2);
        let (fetch_instr, _) = self.read_memory_timed(mmio, fetch_pc);
        let fetch_decoded = if !is_thumb {
            Some(Disassembler::decode_opcode(
                fetch_instr,
                fetch_pc,
                |offset| (mmio as &dyn memory::Addressable).read32(fetch_pc + offset),
            ))
        } else {
            None // TODO: Add Thumb instruction decoding
        };
        self.pipeline.fetch = Some(PipelineInstruction {
            instruction: fetch_decoded,
            pc: fetch_pc,
            is_thumb,
        });
    }

    /// Get current pipeline state for debugging
    pub fn get_pipeline_state(
        &self,
    ) -> (
        &Option<PipelineInstruction>,
        &Option<PipelineInstruction>,
        &Option<PipelineInstruction>,
    ) {
        (
            &self.pipeline.fetch,
            &self.pipeline.decode,
            &self.pipeline.execute,
        )
    }

    /// Reset cycle counts
    pub fn reset_cycle_counts(&mut self) {
        self.cycle_counts = CycleCounts::default();
    }

    /// Configure wait states (e.g., from WAITCNT register)
    pub fn configure_wait_states(&mut self, config: WaitStateConfig) {
        self.wait_config = config;
    }

    /// Add internal cycles (for instructions that do internal processing)
    pub fn add_internal_cycles(&mut self, count: u32) {
        self.add_cycles(CycleType::I, count.saturating_sub(1)); // subtract 1 because add_cycles adds 1 base
    }

    /// Handle branch instruction - flushes pipeline and updates PC
    pub fn branch_to(&mut self, target_addr: u32) {
        self.registers.pc = target_addr;
        self.request_pipeline_flush();
        self.last_access_addr = None; // Next access won't be sequential
    }
}
