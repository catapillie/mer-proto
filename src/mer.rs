use std::process;

use colored::Colorize;

mod cli;
mod cmd;
mod msg;

fn main() {
    match cli::parse_command() {
        cmd::Command::Version => msg::info(format!("mer {}", env!("CARGO_PKG_VERSION"))),
        cmd::Command::Help => msg::show_man(),
        cmd::Command::Unknown(string) => {
            msg::error(format!("invalid command '{string}'"));
            msg::help(format!(
                "run {} for more help",
                "mer help".bold().underline()
            ));
            process::exit(1);
        }
        cmd::Command::Error => {
            msg::error("failed to parse command, nothing happens");
            process::exit(1);
        }
    }
}
