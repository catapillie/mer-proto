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

pub fn show_com_man() {
    println!("{}", "USAGE".bold().underline());
    println!("    {} <{}>", "mer com".bold(), "path".bold());
    println!("{}", "DESCRIPTION".bold().underline());
    println!("    compiles a program to bytecode, given the path to the source code");
    println!("{}", "ARGUMENTS".bold().underline());
    println!("    {}    the path to file containing the source code", "path".bold());
}

pub fn show_version_man() {
    println!("{}", "USAGE".bold().underline());
    println!("    {} ", "mer version".bold());
    println!("{}", "DESCRIPTION".bold().underline());
    println!("    shows version then exits");
}

pub fn show_help_man() {
    println!("{}", "USAGE".bold().underline());
    println!("    {} [{}]", "mer help".bold(), "command".bold());
    println!("{}", "DESCRIPTION".bold().underline());
    println!("    displays general help, or if provided, help for a specified command");
    println!("{}", "ARGUMENTS".bold().underline());
    println!("    {}    optional subcommand to get help from", "command".bold());
}

pub fn show_man() {
    println!("{}", "USAGE".bold().underline());
    println!("    {} <{}>", "mer".bold(), "command".bold());
    println!("    {} [{}]", "mer help".bold(), "command".bold());
    println!("{}", "COMMANDS".bold().underline());
    println!("    {}        compile a program to bytecode", "com".bold());
    println!("    {}    show version then exit", "version".bold());
    println!("    {}       show help then exit", "help".bold());
}
