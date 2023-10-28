use pico_args::Arguments;

use crate::cmd::{Command, CompileCommand};

pub fn parse_command() -> Command {
    let mut args = Arguments::from_env();

    match args.subcommand() {
        Ok(Some(command)) => match command.as_str() {
            "com" => parse_compile_command(args),
            "version" => Command::Version,
            "help" => parse_help_command(args),
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

fn parse_help_command(mut args: Arguments) -> Command {
    match args.opt_free_from_str::<String>() {
        Ok(s) => Command::Help(s),
        Err(_) => Command::Error,
    }
}
