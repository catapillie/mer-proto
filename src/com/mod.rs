use std::process;

use colored::Colorize;

use crate::msg;

use self::parser::Parser;

mod asts;
mod ops;
mod parser;
mod tokens;

pub fn compile(source: String) {
    let parser = Parser::new(source.as_str());
    let (_, errors) = parser.parse();

    if errors.is_empty() {
        msg::ok("parsing finished successfully");
        return;
    }

    msg::error("parsing finished abnormally");
    for err in errors {
        println!("    * {}", err.to_string().bold());
    }

    process::exit(1);
}
