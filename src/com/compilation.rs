use std::{fs, io, path::Path};

use crate::diagnostics::{DiagnosticList, Severity};

use super::{
    abt::{Program, TypeAbt},
    analysis::Analyser,
    codegen::Codegen,
    parser::Parser,
};

pub enum AnalysisStage {
    Ok(Program, DiagnosticList),
    CannotCompile(DiagnosticList),
}

pub fn analyse_program(source: &str) -> AnalysisStage {
    analyse_program_with_type(source, TypeAbt::Unit)
}

pub fn analyse_program_with_type(source: &str, expected_type: TypeAbt) -> AnalysisStage {
    let mut diagnostics = DiagnosticList::new();

    let ast = Parser::new(source, &mut diagnostics).parse_program();
    let abt = Analyser::new(&mut diagnostics).analyse_program(&ast, expected_type);

    let is_fatal = diagnostics.iter().any(|d| d.severity == Severity::Error);
    if is_fatal {
        AnalysisStage::CannotCompile(diagnostics)
    } else {
        AnalysisStage::Ok(abt, diagnostics)
    }
}

pub fn compile_to_bytecode(abt: Program) -> io::Result<Vec<u8>> {
    Codegen::new().gen(&abt)
}

pub fn write_bytecode(path: impl AsRef<Path>, bytecode: &[u8]) -> io::Result<()> {
    fs::write(path, bytecode)
}
