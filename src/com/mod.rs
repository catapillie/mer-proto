use self::{parser::Parser, errors::ParseError, span::Span};
use crate::{com::codegen::Codegen, msg};
use colored::Colorize;
use std::{
    fs,
    process::{self},
};

mod ast;
mod codegen;
mod cursor;
mod errors;
mod parser;
mod pos;
mod span;
mod tokens;

pub fn compile(source: String) {
    let vec = source.lines().collect::<Vec<_>>();
    let lines = vec.as_slice();

    let parser = Parser::init(source.as_str());
    let (ast, errors) = parser.parse_program();

    if !errors.is_empty() {
        msg::error(
            format!("parsing finished with {} error(s)", errors.len())
                .bold()
                .to_string(),
        );
        for (error, span) in errors {
            print_err(lines, error, span);
        }
        process::exit(1);
    }

    msg::ok("parsing finished successfully");

    let result = Codegen::new().gen(&ast);

    const PATH: &str = "a.out";

    match result {
        Ok(bytes) => {
            match fs::write(PATH, bytes) {
                Ok(_) => msg::ok(format!(
                    "compilation to {} finished successfully",
                    PATH.bold()
                )),
                Err(e) => {
                    msg::error(format!(
                        "compilation succeeded then failed when writing to {}:\n      {}",
                        PATH,
                        e.to_string().italic()
                    ));
                    process::exit(e.raw_os_error().unwrap_or(1));
                }
            };
        }
        _ => {
            msg::error("compilation finished abnormally");
            process::exit(1);
        }
    }

    process::exit(0);
}

fn print_err(lines: &[&str], error: ParseError, span: Span) {
    assert!(span.from.line == span.to.line);

    let from  = span.from.column;
    let to = span.to.column;

    let line_index = span.from.line;
    let line = lines[line_index];
    let chars = line.chars();

    msg::error(error.to_string());

    print!("{:>4} │ ", line_index + 1);
    print!("{}", chars.clone().take(from).collect::<String>());
    print!("{}", chars.clone().skip(from).take(to-from).collect::<String>().bright_red());
    print!("{}", chars.clone().skip(to).collect::<String>());
    println!("");

    print!("     │{}", " ".repeat(span.from.column));
    print!("{}", "└".bright_red());
    print!("{}", "─".repeat(span.to.column - span.from.column).bright_red());
    print!("{}", "┘".bright_red());
    println!();
}