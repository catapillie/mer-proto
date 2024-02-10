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
