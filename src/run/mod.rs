use colored::Colorize;

use crate::run::opcode::Opcode;

use self::vm::VM;

mod opcode;
mod vm;

pub fn run(program: Vec<u8>) {
    let mut vm = VM::new(program);
    vm.run();
}

pub fn disassemble(program: Vec<u8>) {
    let mut ip: usize = 0;
    while let Some(&byte) = program.get(ip) {
        let offset = ip;
        ip += 1;

        let opcode = match Opcode::try_from(byte) {
            Ok(opcode) => opcode,
            Err(_) => {
                println!("{offset:0>8} | {byte:02x} {:>16}", "illegal".bright_red());
                continue;
            }
        };

        match opcode {
            Opcode::ld_num_const => {
                let bytes: [u8; 8] = program[ip..ip + 8].try_into().unwrap();
                let value = f64::from_be_bytes(bytes);
                ip += 8;
                println!(
                    "{offset:0width$} | {byte:02x} {:>16} {:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x} ({value:+.10e})",
                    format!("{opcode:?}").bold(),
                    bytes[0], bytes[1], bytes[2], bytes[3],
                    bytes[4], bytes[5], bytes[6], bytes[7],
                    width = 8
                );
            }
            _ => {
                println!(
                    "{offset:0width$} | {byte:02x} {:>16}",
                    format!("{opcode:?}").bold(),
                    width = 8
                );
            }
        }
    }
}
