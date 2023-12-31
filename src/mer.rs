use std::{collections::HashMap, fs, path::PathBuf, process};

use cmd::{Command, CompileCommand, DisassembleCommand, RunCommand};
use colored::Colorize;

use merlib::{
    binaries,
    com::{self, AnalysisStage},
    runtime::{Opcode, VM},
};

mod cli;
mod cmd;
mod msg;

mod tests;

fn main() {
    match cli::parse_command() {
        Command::Compile(command) => compile(command),
        Command::Run(command) => run(command),
        Command::Disassemble(command) => dis(command),
        Command::Version => msg::info(format!("mer {}", env!("CARGO_PKG_VERSION"))),
        Command::Help(Some(command)) => match command.as_str() {
            "com" => msg::show_com_man(),
            "run" => msg::show_run_man(),
            "dis" => msg::show_dis_man(),
            "version" => msg::show_version_man(),
            "help" => msg::show_help_man(),
            _ => {
                msg::error(format!("invalid command '{command}'"));
                msg::help(format!(
                    "run {} for a list of commands",
                    "mer help".bold().underline()
                ));
                process::exit(1);
            }
        },
        Command::Help(None) => msg::show_man(),
        Command::Unknown(command) => {
            msg::error(format!("invalid command '{command}'"));
            msg::help(format!(
                "run {} for more help",
                "mer help".bold().underline()
            ));
            process::exit(1);
        }
        Command::Error => {
            msg::error("failed to parse command, nothing happens");
            process::exit(1);
        }
    }
}

fn compile(command: CompileCommand) {
    match command {
        CompileCommand::Go(path) => {
            let source = match fs::read_to_string(&path) {
                Ok(source) => source,
                Err(e) => {
                    msg::error(format!(
                        "could not read file {}:\n      {}",
                        path.bold(),
                        e.to_string().italic()
                    ));
                    process::exit(e.raw_os_error().unwrap_or(1));
                }
            };

            let abt = match com::analyse_program(&source) {
                AnalysisStage::Ok(abt, diagnostics) => {
                    let diagnostics = diagnostics.done();
                    if diagnostics.is_empty() {
                        msg::ok("analysis finished normally")
                    } else {
                        let len = diagnostics.len();
                        msg::warn(format!("analysis finished with {len} errors"));
                        for diagnostic in diagnostics.into_iter() {
                            com::print_diagnostic(&path, &source, &diagnostic);
                            println!();
                        }
                    }
                    abt
                }
                AnalysisStage::CannotCompile(diagnostics) => {
                    for diagnostic in diagnostics.done().into_iter() {
                        com::print_diagnostic(&path, &source, &diagnostic);
                        println!();
                    }
                    msg::error("cannot compile with errors -- aborting");
                    process::exit(1);
                }
            };

            let bytecode = match com::compile_to_bytecode(abt) {
                Ok(bytecode) => {
                    msg::ok("bytecode generation finished normally");
                    bytecode
                }
                Err(err) => {
                    msg::error(format!(
                        "bytecode generation failed:\n    {}",
                        err.to_string().italic(),
                    ));
                    process::exit(1);
                }
            };

            let out_path = PathBuf::from(path)
                .with_extension("out")
                .to_string_lossy()
                .to_string();
            match com::write_bytecode(&out_path, &bytecode) {
                Ok(_) => {}
                Err(err) => {
                    msg::error(format!(
                        "something went wrong when writing bytecode into {}:\n{}",
                        out_path.bold().underline(),
                        err.to_string().italic(),
                    ));
                }
            }

            msg::ok(format!(
                "compilation finished successfully to {}",
                out_path.bold().underline(),
            ));
            process::exit(0);
        }
        CompileCommand::NoPath => {
            msg::error("no path provided");
            msg::help(format!(
                "run {} for more help on compilation",
                "mer help com".bold().underline()
            ));
            process::exit(1);
        }
    }
}

fn run(command: RunCommand) {
    match command {
        RunCommand::Go(ref path) => {
            let program = match fs::read(path) {
                Ok(program) => program,
                Err(e) => {
                    msg::error(format!(
                        "could not read file {}:\n      {}",
                        path.bold(),
                        e.to_string().italic()
                    ));
                    process::exit(e.raw_os_error().unwrap_or(1));
                }
            };

            match VM::new(&program).run::<()>() {
                Ok(()) => (),
                Err(error) => {
                    msg::error(format!("run-time error: {}", error.to_string().bold()));
                    process::exit(1);
                }
            }
        }
        RunCommand::NoPath => {
            msg::error("no path provided");
            msg::help(format!(
                "run {} for more help on running programs",
                "mer help run".bold().underline()
            ));
            process::exit(1);
        }
    }
}

fn dis(command: DisassembleCommand) {
    match command {
        DisassembleCommand::Go(ref path) => {
            let bytes = match fs::read(path) {
                Ok(program) => program,
                Err(e) => {
                    msg::error(format!(
                        "could not read file {}:\n      {}",
                        path.bold(),
                        e.to_string().italic()
                    ));
                    process::exit(e.raw_os_error().unwrap_or(1));
                }
            };

            let opcodes = match binaries::disassemble(&bytes) {
                Ok(opcodes) => opcodes,
                Err(err) => {
                    msg::error(format!(
                        "failed to disassemble program: {}",
                        err.to_string().bold()
                    ));
                    process::exit(1);
                }
            };

            let mut map = HashMap::new();
            for (offset, opcode) in opcodes.iter() {
                map.insert(offset, opcode.clone());
            }

            println!();
            println!("         ╥");
            for (offset, opcode) in opcodes.iter() {
                match opcode {
                    Opcode::entry_point(addr) => {
                        let Some(Opcode::function(name, _, _)) = map.get(&(*addr as u64)) else {
                            unreachable!()
                        };
                        println!(
                            "{offset:0>8} ╟─── !! entry-point -> {} [{addr:0>8}]",
                            name.bold()
                        )
                    }
                    Opcode::function(name, param_count, local_count) => {
                        println!(
                            "{offset:0>8} ║ :: {} ({} params, {} locals)",
                            name.bold(),
                            param_count.to_string().bold(),
                            local_count.to_string().bold()
                        )
                    }
                    Opcode::call(addr) => {
                        let Some(Opcode::function(name, _, _)) = map.get(&(*addr as u64)) else {
                            unreachable!()
                        };
                        println!(
                            "{offset:0>8} ║ {op:>20} -> {} [{addr:0>8}]",
                            name.bold(),
                            op = opcode.name(),
                        )
                    }
                    Opcode::jmp(addr) | Opcode::jmp_if(addr) => println!(
                        "{offset:0>8} ║ {op:>20} -> {}",
                        format!("{addr:0>8}").bold(),
                        op = opcode.name()
                    ),
                    Opcode::ld_loc(loc) | Opcode::st_loc(loc) => {
                        println!(
                            "{offset:0>8} ║ {op:>20} {}",
                            loc.to_string().bold(),
                            op = opcode.name(),
                        )
                    }
                    Opcode::ld_u8(num) => println!(
                        "{offset:0>8} ║ {op:>20} {}",
                        num.to_string().bold(),
                        op = opcode.name()
                    ),
                    Opcode::ld_u16(num) => println!(
                        "{offset:0>8} ║ {op:>20} {}",
                        num.to_string().bold(),
                        op = opcode.name()
                    ),
                    Opcode::ld_u32(num) => println!(
                        "{offset:0>8} ║ {op:>20} {}",
                        num.to_string().bold(),
                        op = opcode.name()
                    ),
                    Opcode::ld_u64(num) => println!(
                        "{offset:0>8} ║ {op:>20} {}",
                        num.to_string().bold(),
                        op = opcode.name()
                    ),
                    Opcode::ld_i8(num) => println!(
                        "{offset:0>8} ║ {op:>20} {}",
                        format!("{num:+}").bold(),
                        op = opcode.name()
                    ),
                    Opcode::ld_i16(num) => println!(
                        "{offset:0>8} ║ {op:>20} {}",
                        format!("{num:+}").bold(),
                        op = opcode.name()
                    ),
                    Opcode::ld_i32(num) => println!(
                        "{offset:0>8} ║ {op:>20} {}",
                        format!("{num:+}").bold(),
                        op = opcode.name()
                    ),
                    Opcode::ld_i64(num) => println!(
                        "{offset:0>8} ║ {op:>20} {}",
                        format!("{num:+}").bold(),
                        op = opcode.name()
                    ),
                    Opcode::ld_f32(num) => println!(
                        "{offset:0>8} ║ {op:>20} {}",
                        format!("{num:8.8}").bold(),
                        op = opcode.name()
                    ),
                    Opcode::ld_f64(num) => println!(
                        "{offset:0>8} ║ {op:>20} {}",
                        format!("{num:8.8}").bold(),
                        op = opcode.name()
                    ),
                    Opcode::dbg(ty)
                    | Opcode::add(ty)
                    | Opcode::sub(ty)
                    | Opcode::mul(ty)
                    | Opcode::div(ty)
                    | Opcode::rem(ty)
                    | Opcode::eq(ty)
                    | Opcode::ne(ty)
                    | Opcode::le(ty)
                    | Opcode::lt(ty)
                    | Opcode::ge(ty)
                    | Opcode::gt(ty)
                    | Opcode::bitand(ty)
                    | Opcode::bitor(ty)
                    | Opcode::bitxor(ty)
                    | Opcode::neg(ty) => {
                        println!(
                            "{offset:0>8} ║ {op:>20} <{}>",
                            ty.to_string().bold(),
                            op = opcode.name(),
                        )
                    }
                    _ => {
                        println!("{offset:0>8} ║ {op:>20}", op = opcode.name(),)
                    }
                }
            }
            println!("         ╨");
        }
        DisassembleCommand::NoPath => {
            msg::error("no path provided");
            msg::help(format!(
                "run {} for more help on disassembling programs",
                "mer help dis".bold().underline()
            ));
            process::exit(1);
        }
    }
}
