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
