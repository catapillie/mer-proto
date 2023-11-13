use self::parser::Parser;
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

    msg::ok("parsing finished successfully");

    const PATH: &str = "a.out";

    let result = {
        let mut file = fs::OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(PATH)
            .unwrap_or_else(|e| {
                msg::error(format!(
                    "could not write to file file {}:\n      {}",
                    PATH.bold(),
                    e.to_string().italic()
                ));
                process::exit(e.raw_os_error().unwrap_or(1));
            });

        let mut codegen = Codegen::new(&mut file);
        codegen.gen(&ast)
    };

    match result {
        Ok(_) => {
            msg::ok(format!(
                "compilation to {} finished successfully",
                PATH.bold()
            ));
        }
        _ => {
            msg::error("compilation finished abnormally");
            process::exit(1);
        }
    }

    process::exit(0);
}
