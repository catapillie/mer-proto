use pico_args::Arguments;

use crate::cmd::{CheckCommand, Command, CompileCommand, DisassembleCommand, RunCommand};

pub fn parse_command() -> Command {
    let mut args = Arguments::from_env();

    match args.subcommand() {
        Ok(Some(command)) => match command.as_str() {
            "com" => parse_compile_command(args),
            "check" => parse_check_command(args),
            "run" => parse_run_command(args),
            "dis" => parse_dis_command(args),
            "help" => parse_help_command(args),
            "version" => Command::Version,
            _ => Command::Unknown(command),
        },
        Ok(None) => Command::Help(None),
        Err(_) => Command::Error,
    }
}

fn parse_compile_command(mut args: Arguments) -> Command {
    match args.opt_free_from_str::<String>() {
        Ok(Some(path)) => Command::Compile(CompileCommand::Go(path)),
        Ok(None) => Command::Compile(CompileCommand::NoPath),
        Err(_) => Command::Error,
    }
}

fn parse_check_command(mut args: Arguments) -> Command {
    match args.opt_free_from_str::<String>() {
        Ok(Some(path)) => Command::Check(CheckCommand::Go(path)),
        Ok(None) => Command::Check(CheckCommand::NoPath),
        Err(_) => Command::Error,
    }
}

fn parse_run_command(mut args: Arguments) -> Command {
    match args.opt_free_from_str::<String>() {
        Ok(Some(path)) => Command::Run(RunCommand::Go(path)),
        Ok(None) => Command::Run(RunCommand::NoPath),
        Err(_) => Command::Error,
    }
}

fn parse_dis_command(mut args: Arguments) -> Command {
    match args.opt_free_from_str::<String>() {
        Ok(Some(path)) => Command::Disassemble(DisassembleCommand::Go(path)),
        Ok(None) => Command::Disassemble(DisassembleCommand::NoPath),
        Err(_) => Command::Error,
    }
}

fn parse_help_command(mut args: Arguments) -> Command {
    match args.opt_free_from_str::<String>() {
        Ok(s) => Command::Help(s),
        Err(_) => Command::Error,
    }
}
