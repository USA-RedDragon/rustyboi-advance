pub mod disassembler;
pub mod test_data;

pub use disassembler::{
    AddressingMode, Condition, DataProcessingOp, Instruction, MsrOperand, Operand2, Register,
};

// Include tests
#[cfg(test)]
#[path = "disassembler_tests.rs"]
mod tests;
