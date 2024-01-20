use std::io::Cursor;

use super::{error::DisassemblyError, opcode::read_opcode};
use crate::runtime::opcode::Opcode;

pub fn disassemble(bytes: &[u8]) -> Result<Box<[(u64, Opcode)]>, DisassemblyError> {
    let mut cursor = Cursor::new(bytes);

    let mut opcodes = vec![];
    loop {
        let offset = cursor.position();
        if offset as usize >= bytes.len() {
            break;
        }

        let opcode = match read_opcode(&mut cursor) {
            Ok(opcode) => opcode,
            Err(error) => return Err(DisassemblyError::IllegalOpcode { offset, error }),
        };
        opcodes.push((offset, opcode));
    }

    Ok(opcodes.into_boxed_slice())
}
