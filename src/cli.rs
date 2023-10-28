use pico_args::Arguments;

use crate::cmd::Command;

pub fn parse_command() -> Command {
    let mut args = Arguments::from_env();

    match args.subcommand() {
        Ok(Some(command)) => match command.as_str() {
            "version" => Command::Version,
            "help" => Command::Help,
            _ => Command::Unknown(command),
        },
        Ok(None) => Command::Help,
        Err(_) => Command::Error,
    }
}
