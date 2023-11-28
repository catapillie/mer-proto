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

pub fn compile(path: &str, source: String) {
    let lines = source.lines().collect::<Vec<_>>();

    let mut diagnostics = Diagnostics::new();

    let ast = Parser::new(source.as_str(), &mut diagnostics).parse_program();
    Analyser::new(&mut diagnostics).analyse_program(&ast);

    let diagnostics = diagnostics.done();
    if !diagnostics.is_empty() {
        let mut fatal = false;
        for diagnostic in diagnostics {
            print_diagnostic(path, lines.as_slice(), &diagnostic);
            if matches!(diagnostic.severity, Severity::Error) {
                fatal = true;
            }
        }

        if fatal {
            process::exit(1);
        }
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
fn print_diagnostic(path: &str, lines: &[&str], diagnostic: &Diagnostic) {
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
        println!("{}", format!("    --> {}", path).cyan());
        return;
    };

    let first_line = span.from.line;
    let last_line = span.to.line;

    let max_line_num_len = (last_line + 2).to_string().len();

    let is_one_line = first_line == last_line;

    println!(
        "{}",
        format!(" {:>max_line_num_len$}--> {}:{}", " ", path, span).cyan()
    );
    println!(" {:>max_line_num_len$} ╥", " ");

    let display_line = |line_index: usize, line: &str, color: Color| {
        println!(
            " {:>max_line_num_len$} {} {}",
            line_index.to_string().color(color),
            "║".color(color),
            line.color(color)
        );
    };

    let display_colored_span = |index: usize, from: Option<usize>, to: Option<usize>| {
        let line = lines[index];
        let chars = line.chars();

        let from = from.unwrap_or(0);
        let to = to.unwrap_or(line.len());
        let w = to - from;

        let before: String = chars.clone().take(from).collect();
        let at: String = chars.clone().skip(from).take(w).collect();
        let after: String = chars.clone().skip(to).collect();

        print!(
            " {:>max_line_num_len$} {} ",
            (index + 1).to_string().color(color),
            "║".color(color)
        );
        print!("{before}{}{after}", at.color(color));
        println!();
    };

    if first_line > 0 {
        if let Some(line) = lines.get(first_line - 1) {
            if !line.trim().is_empty() {
                display_line(first_line, line, Color::White);
            }
        }
    }

    if is_one_line {
        let from = span.from.column;
        let to = span.to.column;
        let w = to - from;
        
        display_colored_span(first_line, Some(span.from.column), Some(span.to.column));

        print!(" {:>max_line_num_len$} {}", " ", "║".color(color));
        if from > 0 {
            print!("{:>from$}", " ");
        }
        if w > 1 {
            print!("{}", format!("└{}┘", "─".repeat(w)).color(color));
        } else {
            print!("{:>s$}{}", " ", "↑".color(color), s = 2 - w);
            if w == 0 {
                print!(" {}", "here".color(color))
            }
        }
        println!();
    } else {
        display_colored_span(first_line, Some(span.from.column), None);
        for i in (first_line + 1)..last_line {
            let Some(line) = lines.get(i) else {
                continue;
            };

            display_line(i + 1, line, color);
        }
        display_colored_span(last_line, None, Some(span.to.column));
        // println!(" {:>max_line_num_len$} {}", " ", "╟".color(color));
    }

    if let Some(line) = lines.get(last_line + 1) {
        if !line.trim().is_empty() {
            display_line(last_line + 2, line, Color::White);
        }
    }

    println!(" {:>max_line_num_len$} ╨", " ");
}
