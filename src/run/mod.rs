use std::io::{Cursor, Read};

use byteorder::ReadBytesExt;
use colored::Colorize;

use crate::run::opcode::Opcode;

use self::vm::VM;

pub mod opcode;
mod vm;

pub fn run(program: Vec<Opcode>) {
    let mut vm = VM::new(program.as_slice());
    vm.run();
}

pub fn disassemble(program: Vec<Opcode>) -> Option<()> {
    let mut cursor = Cursor::new(program.as_slice());

    loop {
        let offset = cursor.position();
        let byte = match cursor.read_u8() {
            Ok(b) => b,
            Err(_) => break,
        };

        let opcode = match opcode::name(byte) {
            Some(name) => name,
            None => {
                println!("{offset:0>8} | {byte:02x} {:>16}", "illegal".bright_red());
                continue;
            }
        };

        match byte {
            opcode::ld_num_const => {
                let value = cursor.read_f64::<byteorder::LE>().ok()?;
                println!(
                    "{offset:0width$} | {byte:02x} {:>16} {value:+.10e}",
                    opcode.bold(),
                    width = 8
                );
            }
            opcode::ld_loc | opcode::st_loc => {
                let count = cursor.read_u8().ok()?;
                println!(
                    "{offset:0width$} | {byte:02x} {:>16} {count}",
                    opcode.bold(),
                    width = 8
                );
            }
            opcode::jmp | opcode::jmp_if => {
                let to = cursor.read_u32::<byteorder::LE>().ok()?;
                println!(
                    "{offset:0width$} | {byte:02x} {:>16} -> {to:0width$}",
                    opcode.bold(),
                    width = 8
                );
            }
            opcode::entry_point => {
                let to = cursor.read_u32::<byteorder::LE>().ok()?;
                println!(
                    "{offset:0width$} | !! {} -> {to:0width$}",
                    "entry-point".bold(),
                    width = 8
                );
            }
            opcode::function => {
                let n = cursor.read_u16::<byteorder::LE>().ok()?;

                let mut bytes = vec![0; n as usize];
                cursor.read_exact(&mut bytes).ok()?;
                let name = String::from_utf8(bytes).unwrap();

                let param_count = cursor.read_u8().ok()?;
                let local_count = cursor.read_u8().ok()?;

                println!(
                    "{offset:0width$} | :: {} ({} params, {} locals)",
                    name.bold(),
                    param_count.to_string().bold(),
                    local_count.to_string().bold(),
                    width = 8
                );
            }
            opcode::call => {
                let fp = cursor.read_u32::<byteorder::LE>().ok()?;

                let mut cursor_alt = cursor.clone();
                cursor_alt.set_position(fp as u64);
                cursor_alt.read_u8().ok()?;

                let n = cursor_alt.read_u16::<byteorder::LE>().ok()?;

                let mut bytes = vec![0; n as usize];
                cursor_alt.read_exact(&mut bytes).ok()?;
                let name = String::from_utf8(bytes).unwrap();

                println!(
                    "{offset:0width$} | {byte:02x} {:>16} -> {} ({fp:0width$})",
                    opcode.bold(),
                    name.bold(),
                    width = 8
                );
            }
            _ => {
                println!(
                    "{offset:0width$} | {byte:02x} {:>16}",
                    opcode.bold(),
                    width = 8
                );
            }
        }
    }

    Some(())
}
