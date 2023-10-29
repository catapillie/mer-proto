use self::parser::Parser;
use crate::msg;
use colored::Colorize;
use std::process;

mod ast;
mod cursor;
mod errors;
mod parser;
mod pos;
mod span;
mod tokens;

pub fn compile(source: String) {
    let parser = Parser::init(source.as_str());
    let (ast, errors) = parser.parse_program();

    println!("{ast:#?}");

    if !errors.is_empty() {
        msg::error(
            format!("parsing finished with {} error(s)", errors.len())
                .bold()
                .to_string(),
        );
        for error in errors {
            println!("      {error}");
        }
        process::exit(1);
    }

    msg::ok("parsing finish successfully");
    process::exit(0);
}
