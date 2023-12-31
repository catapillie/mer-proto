use std::{fs, process};

use cmd::{Command, CompileCommand, DisassembleCommand, RunCommand};
use colored::Colorize;

use merlib::{
    com,
    runtime::{self, VM},
};

mod cli;
mod cmd;
mod msg;

fn main() {
    match cli::parse_command() {
        Command::Compile(command) => run_compile(command),
        Command::Run(command) => run_run(command),
        Command::Disassemble(command) => run_disassemble(command),
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

fn run_compile(command: CompileCommand) {
    match command {
        CompileCommand::Go(ref path) => {
            let source = match fs::read_to_string(path) {
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

            com::compile_then_write(path, source);
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

fn run_run(command: RunCommand) {
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

            todo!();
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

fn run_disassemble(command: DisassembleCommand) {
    match command {
        DisassembleCommand::Go(ref path) => {
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
                    msg::error(error.to_string());
                    process::exit(1);
                }
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
