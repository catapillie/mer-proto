use std::{fs, path::PathBuf, process};

use cmd::{Command, CompileCommand, DisassembleCommand, RunCommand};
use colored::Colorize;

use merlib::{
    binaries,
    com::{self, AnalysisStage},
    runtime::VM,
};

mod cli;
mod cmd;
mod msg;

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
                    msg::error(format!("failed to disassemble program: {}", err.to_string().bold()));
                    process::exit(1);
                },
            };

            for (offset, opcode) in opcodes.iter() {
                println!("{offset} {opcode:?}");
            }
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
