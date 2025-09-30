pub struct Disassembler;

impl Disassembler {
    /// Returns (mnemonic, instruction_length)
    pub fn disassemble_with_reader<F>(addr: u32, mut read_fn: F) -> (String, u32)
    where
        F: FnMut(u32) -> u32,
    {
        let opcode = read_fn(addr);
        Self::disassemble_opcode(opcode, addr, |offset| read_fn(addr + offset))
    }

    fn disassemble_opcode<F>(opcode: u32, _pc: u32, mut _read_fn: F) -> (String, u32)
    where
        F: FnMut(u32) -> u32,
    {
        (format!("UNK 0x{:08X}", opcode), 4)
    }
}
