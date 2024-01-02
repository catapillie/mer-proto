use std::{fs, io, path::Path};

use super::{
    abt::ProgramAbt, analysis::Analyser, codegen::Codegen, diagnostics::Diagnostics, parser::Parser,
};

pub enum AnalysisStage {
    Ok(ProgramAbt, Diagnostics),
    CannotCompile(Diagnostics),
}

pub fn analyse_program(source: &str) -> AnalysisStage {
    let mut diagnostics = Diagnostics::new();

    let ast = Parser::new(source, &mut diagnostics).parse_program();
    let abt = Analyser::new(&mut diagnostics).analyse_program(&ast);

    if diagnostics.is_fatal() {
        AnalysisStage::CannotCompile(diagnostics)
    } else {
        AnalysisStage::Ok(abt, diagnostics)
    }
}

pub fn compile_to_bytecode(abt: ProgramAbt) -> io::Result<Vec<u8>> {
    Codegen::new().gen(&abt)
}

pub fn write_bytecode(path: impl AsRef<Path>, bytecode: &[u8]) -> io::Result<()> {
    fs::write(path, bytecode)
}
