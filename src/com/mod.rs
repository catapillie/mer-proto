use self::{
    abt::ProgramAbt, analysis::Analyser, codegen::Codegen, diagnostics::Diagnostic, parser::Parser,
};
use crate::{
    com::diagnostics::{Diagnostics, Severity},
    msg,
};
use colored::{Color, Colorize};
use std::{
    collections::BTreeMap,
    fs::{self},
    path::PathBuf,
    process::{self},
};

pub mod abt;
mod analysis;
mod codegen;
mod cursor;
mod diagnostics;
mod parser;
mod pos;
mod printer;
mod span;
mod syntax;
mod tokens;

pub fn compile_to_bytecode(path: &str, source: String) -> Vec<u8> {
    let Some(abt) = analyse(path, source) else {
        process::exit(1);
    };

    match Codegen::new().gen(&abt) {
        Ok(vec) => vec,
        Err(e) => {
            msg::error(format!(
                "io error occurred during bytecode generation:\n      {}",
                e.to_string().italic()
            ));
            process::exit(1);
        }
    }
}

pub fn compile_then_write(path: &str, source: String) {
    let program = compile_to_bytecode(path, source);

    let mut path_buf = PathBuf::from(path);
    path_buf.set_extension("out");
    let out_path = path_buf.to_string_lossy().into_owned();

    match fs::write(path_buf, program.as_slice()) {
        Ok(()) => msg::ok(format!(
            "compilation to '{}' finished successfully",
            out_path.bold()
        )),
        Err(e) => {
            msg::error(format!(
                "compilation was successful, but couldn't write to file '{}':\n      {}",
                out_path.bold(),
                e.to_string().italic()
            ));
            process::exit(1);
        }
    }

    process::exit(0);
}

pub fn analyse(path: &str, source: String) -> Option<ProgramAbt> {
    let lines = source.lines().collect::<Vec<_>>();

    let mut diagnostics = Diagnostics::new();

    let ast = Parser::new(source.as_str(), &mut diagnostics).parse_program();
    let abt = Analyser::new(&mut diagnostics).analyse_program(&ast);

    let diagnostics = diagnostics.done();
    if !diagnostics.is_empty() {
        let mut fatal = false;
        for diagnostic in diagnostics {
            printer::print_diagnostic(path, lines.as_slice(), &diagnostic);
            println!();
            if matches!(diagnostic.severity, Severity::Error) {
                fatal = true;
            }
        }

        if fatal {
            msg::error("cannot compile with errors -- aborting");
            return None;
        }
    }
    msg::ok("analysis finished successfully");

    todo!()
}
