use colored::Colorize;

pub fn ok(msg: impl Into<String>) {
    println!("{} {}", "ok".bright_green(), msg.into());
}

pub fn error(msg: impl Into<String>) {
    println!("{} {}", "error".bright_red(), msg.into());
}

pub fn warn(msg: impl Into<String>) {
    println!("{} {}", "warning".bright_yellow(), msg.into());
}

pub fn info(msg: impl Into<String>) {
    println!("{} {}", "info".bright_magenta(), msg.into());
}

pub fn help(msg: impl Into<String>) {
    println!("{} {}", "help".cyan(), msg.into());
}

pub fn show_man() {
    println!("{}", "USAGE".bold().underline());
    println!("    {} <{}>", "mer".bold(), "command".bold());
    println!("{}", "COMMANDS".bold().underline());
    println!("    {}    | show version then exit", "version".bold());
    println!("    {}       | print this help message then exit", "help".bold());
}
