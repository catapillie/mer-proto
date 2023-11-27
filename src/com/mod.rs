use self::{diagnostics::Diagnostic, parser::Parser};
use crate::{
    com::{
        analyser::Analyser,
        codegen::Codegen,
        diagnostics::{Diagnostics, Severity},
    },
    msg,
};
use colored::{Color, Colorize};
use std::{
    fs,
    process::{self},
};

mod abt;
mod analyser;
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

    let ast = Parser::new(source.as_str(), &mut diagnostics).parse_program();
    Analyser::new(&mut diagnostics).analyse_program(&ast);

    let diagnostics = diagnostics.done();
    if !diagnostics.is_empty() {
        for diagnostic in diagnostics {
            print_diagnostic(lines.as_slice(), diagnostic);
        }
        process::exit(1);
    }
    msg::ok("analysis finished successfully");

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
    let color = match diagnostic.severity {
        Severity::Error => {
            msg::error(msg);
            Color::BrightRed
        }
        Severity::Warning => {
            msg::warn(msg);
            Color::BrightYellow
        }
    };

    let Some(span) = diagnostic.span else {
        return;
    };

    assert!(span.from.line == span.to.line);

    let from = span.from.column;
    let to = span.to.column;
    let width = to - from;

    let line_index = span.from.line;
    let prev_line = if line_index == 0 {
        None
    } else {
        lines.get(line_index - 1)
    };
    let next_line = lines.get(line_index + 1);
    let chars = lines[line_index].chars();

    if let Some(line) = prev_line {
        println!("{:>4} │ {line}", line_index);
    }

    let current_line = lines[line_index];
    if !current_line.is_empty() {
        print!("{:>4} │ ", line_index + 1);
        print!("{}", chars.clone().take(from).collect::<String>());
        print!(
            "{}",
            chars
                .clone()
                .skip(from)
                .take(to - from)
                .collect::<String>()
                .color(color)
        );
        print!("{}", chars.clone().skip(to).collect::<String>());
    } else {
        println!("{:>4} │ {current_line}", line_index + 1);
    }
    println!();

    if width > 1 {
        print!("     │{}", " ".repeat(span.from.column));
        print!("{}", "└".color(color));
        print!(
            "{}",
            "─".repeat(span.to.column - span.from.column).color(color)
        );
        print!("{}", "┘".color(color));
    } else {
        print!("     │{}", " ".repeat(span.from.column + 2 - width));
        print!("{}", "↑ here".color(color));
    }
    println!();

    if let Some(line) = next_line {
        println!("{:>4} │ {line}", line_index + 2);
    }
}
