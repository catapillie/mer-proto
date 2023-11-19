use colored::Colorize;

use crate::run::opcode::Opcode;

use self::vm::VM;

pub mod opcode;
mod vm;

pub fn run(program: Vec<Opcode>) {
    let mut vm = VM::new(program);
    vm.run();
}

pub fn disassemble(program: Vec<Opcode>) {
    let mut ip: usize = 0;
    while let Some(&byte) = program.get(ip) {
        let offset = ip;
        ip += 1;

        let opcode = match opcode::name(byte) {
            Some(name) => name,
            None => {
                println!("{offset:0>8} | {byte:02x} {:>16}", "illegal".bright_red());
                continue;
            },
        };

        match byte {
            opcode::ld_num_const => {
                let bytes: [u8; 8] = program[ip..ip + 8].try_into().unwrap();
                let value = f64::from_le_bytes(bytes);
                ip += 8;
                println!(
                    "{offset:0width$} | {byte:02x} {:>16} {value:+.10e} ({:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x})",
                    opcode.bold(),
                    bytes[0], bytes[1], bytes[2], bytes[3],
                    bytes[4], bytes[5], bytes[6], bytes[7],
                    width = 8
                );
            }
            opcode::ld_loc | opcode::st_loc => {
                let count = program[ip];
                ip += 1;
                println!(
                    "{offset:0width$} | {byte:02x} {:>16} {count}",
                    opcode.bold(),
                    width = 8
                );
            }
            opcode::jmp | opcode::jmp_if => {
                let to = u32::from_le_bytes(program[ip..ip + 4].try_into().unwrap());
                ip += 4;
                println!(
                    "{offset:0width$} | {byte:02x} {:>16} -> {to:0width$}",
                    opcode.bold(),
                    width = 8
                );
            }
            opcode::entry_point => {
                let to = u32::from_le_bytes(program[ip..ip + 4].try_into().unwrap());
                ip += 4;
                println!(
                    "{offset:0width$} | !! {} -> {to:0width$}",
                    "entry-point".bold(),
                    width = 8
                 );
             }
            opcode::function => {
                let n = u16::from_le_bytes(program[ip..ip + 2].try_into().unwrap()) as usize;
                ip += 2;

                let bytes = &program[ip..ip + n];
                let name = String::from_utf8(bytes.to_vec()).unwrap();
                ip += n;

                let param_count = program[ip];
                ip += 1;

                let local_count = program[ip];
                ip += 1;

                println!(
                    "{offset:0width$} | :: {} ({} params, {} locals)",
                    name.bold(),
                    param_count.to_string().bold(),
                    local_count.to_string().bold(),
                    width = 8
                );
            },
            opcode::call => {
                let fp = u32::from_le_bytes(program[ip..ip + 4].try_into().unwrap());
                ip += 4;

                let mut ip_alt = 1 + fp as usize;
                let n = u16::from_le_bytes(program[ip_alt..ip_alt + 2].try_into().unwrap()) as usize;
                ip_alt += 2;

                let bytes = &program[ip_alt..ip_alt + n];
                let name = String::from_utf8(bytes.to_vec()).unwrap();

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
}
