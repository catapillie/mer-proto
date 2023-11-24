use self::{parser::Parser, diagnostics::Diagnostic};
use crate::{com::{codegen::Codegen, diagnostics::Diagnostics}, msg};
use colored::Colorize;
use std::{
    fs,
    process::{self},
};

mod ast;
mod codegen;
mod cursor;
mod diagnostics;
mod parser;
mod pos;
mod span;
mod tokens;

pub fn compile(source: String) {
    let lines = source.lines().collect::<Vec<_>>();

    let mut diagnostics = Diagnostics::new();

    let parser = Parser::new(source.as_str(), &mut diagnostics);
    let ast = parser.parse_program();

    let diagnostics = diagnostics.done();

    if !diagnostics.is_empty() {
        msg::error(
            format!("parsing finished with {} error(s)", diagnostics.len())
                .bold()
                .to_string(),
        );
        for diagnostic in diagnostics {
            print_diagnostic(lines.as_slice(), diagnostic);
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

// TODO: make it work with TAB characters
fn print_diagnostic(lines: &[&str], diagnostic: Diagnostic) {
    let msg = diagnostic.kind.msg();
    let span = diagnostic.span;

    assert!(span.from.line == span.to.line);

    let from  = span.from.column;
    let to = span.to.column;

    let line_index = span.from.line;
    let prev_line = lines.get(line_index - 1);
    let next_line = lines.get(line_index + 1);
    let chars = lines[line_index].chars();

    msg::error(msg);

    if let Some(line) = prev_line {
        println!("{:>4} │ {line}", line_index);
    }

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

    if let Some(line) = next_line {
        println!("{:>4} │ {line}", line_index + 2);
    }
}
