use criterion::{BatchSize, BenchmarkId, Criterion, criterion_group, criterion_main};
use rustyboi_advance_debugger_lib::disassembler::Disassembler;
use rustyboi_advance_debugger_lib::test_data::PERF_TEST_OPCODES;
use std::hint::black_box;

/// Benchmark decoding individual instruction types
fn bench_decode_instruction_types(c: &mut Criterion) {
    let mut group = c.benchmark_group("decode_instruction_types");

    for (opcode, desc) in PERF_TEST_OPCODES.iter() {
        group.bench_with_input(BenchmarkId::new("decode", desc), opcode, |b, &opcode| {
            b.iter(|| {
                // Benchmark the core decode function
                Disassembler::decode_opcode(
                    black_box(opcode),
                    black_box(0x08000000),           // Typical GBA ROM start
                    |_offset| black_box(0x00000000), // Dummy read function
                )
            });
        });
    }

    group.finish();
}

/// Benchmark decoding a batch of mixed instructions (hotpath simulation)
fn bench_decode_mixed_batch(c: &mut Criterion) {
    let mut group = c.benchmark_group("decode_mixed_batch");

    // Create batches of different sizes
    let batch_sizes = [10, 50, 100, 500];

    for &batch_size in &batch_sizes {
        group.bench_with_input(
            BenchmarkId::new("mixed_instructions", batch_size),
            &batch_size,
            |b, &batch_size| {
                b.iter_batched(
                    || {
                        // Setup: Create a batch of random instructions from our samples
                        let mut batch = Vec::with_capacity(batch_size);
                        for i in 0..batch_size {
                            let opcode_index = i % PERF_TEST_OPCODES.len();
                            batch.push(PERF_TEST_OPCODES[opcode_index].0);
                        }
                        batch
                    },
                    |batch| {
                        // Benchmark: Decode all instructions in the batch
                        for (i, &opcode) in batch.iter().enumerate() {
                            let pc = 0x08000000 + (i as u32 * 4);
                            black_box(Disassembler::decode_opcode(
                                black_box(opcode),
                                black_box(pc),
                                |_offset| black_box(0x00000000),
                            ));
                        }
                    },
                    BatchSize::SmallInput,
                );
            },
        );
    }

    group.finish();
}

/// Benchmark worst-case scenarios (complex instructions)
fn bench_decode_complex_instructions(c: &mut Criterion) {
    let mut group = c.benchmark_group("decode_complex_instructions");

    // Complex instructions that exercise different code paths
    let complex_opcodes = &[
        (0xe89f07ff, "LDM with many registers"),
        (0xe92d47ff, "STM with many registers"),
        (0xe59f1234, "LDR PC-relative with large offset"),
        (0xe129f00f, "MSR with all fields"),
        (0xe1a01002, "MOV with shift by register"),
    ];

    for &(opcode, desc) in complex_opcodes.iter() {
        group.bench_function(desc, |b| {
            b.iter(|| {
                Disassembler::decode_opcode(
                    black_box(opcode),
                    black_box(0x08000000),
                    |_offset| black_box(0x12345678), // Return some data for PC-relative loads
                )
            });
        });
    }

    group.finish();
}

/// Benchmark the old vs new register creation methods
fn bench_register_creation(c: &mut Criterion) {
    let mut group = c.benchmark_group("register_creation");

    group.bench_function("from_u32_safe", |b| {
        b.iter(|| {
            for i in 0..16 {
                black_box(
                    rustyboi_advance_debugger_lib::disassembler::Register::from_u32(
                        black_box(i),
                    ),
                );
            }
        });
    });

    group.bench_function("from_u32_optimized", |b| {
        b.iter(|| {
            for i in 0..16 {
                black_box(
                    rustyboi_advance_debugger_lib::disassembler::Register::from_u32(black_box(i)),
                );
            }
        });
    });

    group.finish();
}

/// Benchmark realistic emulator workload
fn bench_emulator_workload(c: &mut Criterion) {
    c.bench_function("emulator_hotpath_simulation", |b| {
        // Simulate a realistic mix of instructions that might appear in GBA code
        let realistic_opcodes = [
            0xe3a01000, // MOV R1, #0 (initialization)
            0xe2801001, // ADD R1, R0, #1 (increment)
            0xe1510002, // CMP R1, R2 (comparison)
            0x1a000001, // BNE +1 (conditional branch)
            0xe5901000, // LDR R1, [R0] (memory load)
            0xe5802000, // STR R2, [R0] (memory store)
            0xe1a02001, // MOV R2, R1 (register move)
            0xe0812002, // ADD R2, R1, R2 (arithmetic)
        ];

        b.iter(|| {
            let mut pc = 0x08000000u32;

            // Simulate processing a small block of instructions
            for &opcode in realistic_opcodes.iter() {
                black_box(Disassembler::decode_opcode(
                    black_box(opcode),
                    black_box(pc),
                    |_offset| black_box(0x00000000),
                ));
                pc = pc.wrapping_add(4);
            }
        });
    });
}

criterion_group!(
    benches,
    bench_decode_instruction_types,
    bench_decode_mixed_batch,
    bench_decode_complex_instructions,
    bench_register_creation,
    bench_emulator_workload
);
criterion_main!(benches);
