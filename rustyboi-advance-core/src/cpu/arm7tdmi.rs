use crate::{cpu::registers::{self, Flag}, memory};
use rustyboi_advance_debugger_lib::disassembler::{Disassembler, Instruction};
use serde::{Deserialize, Serialize};

// Exception vector addresses
const VECTOR_UNDEFINED: u32 = 0x00000004;
const VECTOR_SWI: u32 = 0x00000008;
const VECTOR_PREFETCH_ABORT: u32 = 0x0000000C;
const VECTOR_DATA_ABORT: u32 = 0x00000010;
const VECTOR_IRQ: u32 = 0x00000018;
const VECTOR_FIQ: u32 = 0x0000001C;

// Mode bits mask
const MODE_BITS_MASK: u32 = 0x1F;

// ARM/Thumb instruction sizes
const ARM_INSTRUCTION_SIZE: u32 = 4;
const THUMB_INSTRUCTION_SIZE: u32 = 2;

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
#[derive(Default)]
pub struct PipelineInstruction {
    pub instruction: Option<Instruction>,
    /// Program counter when this instruction was fetched
    pub pc: u32,
    /// Whether this is a Thumb instruction
    pub is_thumb: bool,
}


/// Pipeline state for the ARM7TDMI 3-stage pipeline
#[derive(Debug, Clone)]
#[derive(Default)]
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

    #[inline]
    pub fn step(&mut self, mmio: &mut memory::mmio::Mmio) -> u32 {
        // Check for interrupts before executing the next instruction
        // In a real GBA, we would check the interrupt controller here
        // For now, we'll add a method that can be called externally
        // TODO: Integrate with interrupt controller when available

        // If halted, check if we should exit halt state
        if self.halted {
            // CPU is halted - check for interrupts
            // For now, just consume 1 cycle
            // TODO: Check interrupt flags and exit halt if interrupt pending
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

    #[inline]
    /// Flush the pipeline (used during branches, exceptions, etc.)
    pub fn flush_pipeline(&mut self) {
        self.pipeline.fetch = None;
        self.pipeline.decode = None;
        self.pipeline.execute = None;
        self.pipeline.flush_pending = false;
    }

    /// Mark pipeline for flush on next cycle
    #[inline]
    pub fn request_pipeline_flush(&mut self) {
        self.pipeline.flush_pending = true;
    }

    #[inline]
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

    #[inline]
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

    #[inline]
    /// Perform a memory read with proper timing calculation
    fn read_memory_timed(&mut self, mmio: &mut memory::mmio::Mmio, addr: u32) -> (u32, u32) {
        let is_sequential = self.is_sequential_access(addr);
        let timing = memory::timing::get_memory_timing(addr, &self.wait_config, is_sequential);
        let value = (mmio as &dyn memory::Addressable).read32(addr);

        self.add_cycles(timing.cycle_type, timing.wait_states);
        self.last_access_addr = Some(addr);

        (value, 1 + timing.wait_states)
    }

    #[inline]
    /// Advance the pipeline by one stage
    fn advance_pipeline(&mut self, mmio: &mut memory::mmio::Mmio) -> u32 {
        let mut total_cycles = 0;

        // Handle pipeline flush if requested (before executing anything)
        if self.pipeline.flush_pending {
            self.flush_pipeline();
            // After flush, prime the pipeline from the new PC location
            // This will account for actual memory timing based on the target address
            let refill_cycles = self.prime_pipeline(mmio);
            return refill_cycles;
        }

        // Execute stage - execute the instruction currently in execute stage
        if let Some(execute_instr) = self.pipeline.execute.take() {
            if let Some(instruction) = execute_instr.instruction {
                total_cycles += self.dispatch_instruction(&instruction, execute_instr.pc, mmio);
                self.cycle_counts.instructions_executed += 1;
            } else {
                // No decoded instruction - likely Thumb mode which is not yet implemented
                if execute_instr.is_thumb {
                    panic!(
                        "Attempted to execute Thumb instruction at PC={:#010X}, but Thumb mode is not yet implemented",
                        execute_instr.pc
                    );
                } else {
                    panic!(
                        "No instruction decoded in execute stage at PC={:#010X}",
                        execute_instr.pc
                    );
                }
            }
        }

        // Check if instruction execution triggered a pipeline flush
        // If so, handle it immediately and don't advance the pipeline further
        if self.pipeline.flush_pending {
            self.flush_pipeline();
            // Prime the pipeline and account for actual memory timing
            let refill_cycles = self.prime_pipeline(mmio);
            return total_cycles + refill_cycles;
        }

        // Decode -> Execute: Move decoded instruction to execute
        self.pipeline.execute = self.pipeline.decode.take();

        // Fetch -> Decode: Move fetched instruction to decode
        self.pipeline.decode = self.pipeline.fetch.take();

        // Fetch new instruction
        let pc = self.registers.pc;
        let is_thumb = self.registers.get_flag(registers::Flag::ThumbState);

        // Decode the instruction for structured access
        let (decoded_instruction, fetch_cycles) = if !is_thumb {
            // ARM mode: read 32-bit instruction
            let (instruction, cycles) = self.read_memory_timed(mmio, pc);
            let decoded = Some(Disassembler::decode_opcode(instruction, pc, |offset| {
                (mmio as &dyn memory::Addressable).read32(pc + offset)
            }));
            (decoded, cycles)
        } else {
            // Thumb mode: read 16-bit instruction
            let (instruction, cycles) = self.read_memory_timed(mmio, pc);
            let thumb_opcode = (instruction & 0xFFFF) as u16;
            let decoded = Some(Disassembler::decode_thumb_opcode(
                thumb_opcode,
                pc,
                |addr| {
                    let word = (mmio as &dyn memory::Addressable).read32(addr);
                    (word & 0xFFFF) as u16
                },
            ));
            (decoded, cycles)
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
    /// Returns the number of cycles used for the pipeline refill
    pub fn prime_pipeline(&mut self, mmio: &mut memory::mmio::Mmio) -> u32 {
        // Clear any existing pipeline state
        self.flush_pipeline();

        // Track cycles used for pipeline refill
        let cycles_before = self.cycle_counts.total_cycles;

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
                |addr| (mmio as &dyn memory::Addressable).read32(addr),
            ))
        } else {
            let thumb_opcode = (execute_instr & 0xFFFF) as u16;
            Some(Disassembler::decode_thumb_opcode(
                thumb_opcode,
                original_pc,
                |addr| {
                    let word = (mmio as &dyn memory::Addressable).read32(addr);
                    (word & 0xFFFF) as u16
                },
            ))
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
                |addr| (mmio as &dyn memory::Addressable).read32(addr),
            ))
        } else {
            let thumb_opcode = (decode_instr & 0xFFFF) as u16;
            Some(Disassembler::decode_thumb_opcode(
                thumb_opcode,
                decode_pc,
                |addr| {
                    let word = (mmio as &dyn memory::Addressable).read32(addr);
                    (word & 0xFFFF) as u16
                },
            ))
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
                |addr| (mmio as &dyn memory::Addressable).read32(addr),
            ))
        } else {
            let thumb_opcode = (fetch_instr & 0xFFFF) as u16;
            Some(Disassembler::decode_thumb_opcode(
                thumb_opcode,
                fetch_pc,
                |addr| {
                    let word = (mmio as &dyn memory::Addressable).read32(addr);
                    (word & 0xFFFF) as u16
                },
            ))
        };
        self.pipeline.fetch = Some(PipelineInstruction {
            instruction: fetch_decoded,
            pc: fetch_pc,
            is_thumb,
        });

        // Update PC to point to the next instruction to be fetched
        // PC should be 12 bytes ahead of execute stage (8 + 4) in ARM mode, or 6 bytes ahead (4 + 2) in Thumb mode
        self.registers.pc = original_pc.wrapping_add(instruction_size * 3);

        // Return the actual cycles used for pipeline refill
        // This will be 2S + 1N typically, but depends on actual memory timings
        (self.cycle_counts.total_cycles - cycles_before) as u32
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
    #[inline]
    pub fn reset_cycle_counts(&mut self) {
        self.cycle_counts = CycleCounts::default();
    }

    /// Configure wait states (e.g., from WAITCNT register)
    #[inline]
    pub fn configure_wait_states(&mut self, config: WaitStateConfig) {
        self.wait_config = config;
    }

    /// Add internal cycles (for instructions that do internal processing)
    #[inline]
    pub fn add_internal_cycles(&mut self, count: u32) {
        self.add_cycles(CycleType::I, count.saturating_sub(1)); // subtract 1 because add_cycles adds 1 base
    }

    /// Handle branch instruction - flushes pipeline and updates PC
    #[inline]
    pub fn branch_to(&mut self, target_addr: u32) {
        self.registers.pc = target_addr;
        self.request_pipeline_flush();
        self.last_access_addr = None; // Next access won't be sequential
    }



    /// Enter an exception
    /// This handles the state saving and mode switching for all exception types
    #[inline]
    pub fn enter_exception(
        &mut self,
        exception_vector: u32,
        new_mode: registers::Mode,
        _disable_irq: bool,
        disable_fiq: bool,
    ) {
        // Save current CPSR to SPSR of the new mode
        let current_cpsr = self.registers.cpsr;
        let is_thumb = self.registers.get_flag(registers::Flag::ThumbState);

        // Calculate return address based on exception type and current mode
        // In ARM mode, PC is 8 ahead; in Thumb mode, PC is 4 ahead
        let return_addr = match exception_vector {
            VECTOR_SWI | VECTOR_UNDEFINED => self.registers.pc,
            VECTOR_IRQ | VECTOR_FIQ => {
                if is_thumb {
                    self.registers.pc.wrapping_sub(THUMB_INSTRUCTION_SIZE)
                } else {
                    self.registers.pc.wrapping_sub(ARM_INSTRUCTION_SIZE)
                }
            }
            VECTOR_PREFETCH_ABORT => {
                if is_thumb {
                    self.registers.pc.wrapping_sub(THUMB_INSTRUCTION_SIZE)
                } else {
                    self.registers.pc.wrapping_sub(ARM_INSTRUCTION_SIZE)
                }
            }
            VECTOR_DATA_ABORT => {
                self.registers.pc
            }
            _ => self.registers.pc,
        };

        // Switch to the new mode
        let new_cpsr = (current_cpsr & !MODE_BITS_MASK) | (new_mode as u32);
        let mut new_cpsr = new_cpsr | Flag::IrqDisable as u32;
        if disable_fiq {
            new_cpsr |= Flag::FiqDisable as u32;
        }

        // Set Thumb bit to 0 (exceptions always execute in ARM mode)
        new_cpsr &= !(1 << 5);

        self.registers.cpsr = new_cpsr;

        // Now that we're in the new mode, save SPSR and LR
        self.registers.set_spsr(current_cpsr);
        self.registers.set_lr(return_addr);

        // Branch to exception vector
        self.branch_to(exception_vector);
    }

    #[inline]
    pub fn software_interrupt(&mut self) {
        self.enter_exception(
            VECTOR_SWI,
            registers::Mode::Service,
            true,
            false,
        );
    }

    #[inline]
    pub fn irq_exception(&mut self) {
        self.enter_exception(
            VECTOR_IRQ,
            registers::Mode::Irq,
            true,
            false,
        );
    }

    #[inline]
    pub fn fiq_exception(&mut self) {
        self.enter_exception(
            VECTOR_FIQ,
            registers::Mode::Fiq,
            true,
            true,
        );
    }

    /// Check for pending interrupts and handle them
    /// Returns true if an interrupt was serviced
    #[inline]
    pub fn check_interrupts(&mut self, irq_pending: bool, fiq_pending: bool) -> bool {
        let cpsr = self.registers.cpsr;
        let irq_disabled = (cpsr & Flag::IrqDisable as u32) != 0;
        let fiq_disabled = (cpsr & Flag::FiqDisable as u32) != 0;

        // FIQ has higher priority than IRQ
        if fiq_pending && !fiq_disabled {
            self.fiq_exception();
            return true;
        }

        if irq_pending && !irq_disabled {
            self.irq_exception();
            return true;
        }

        false
    }
}
