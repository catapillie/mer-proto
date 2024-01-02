use std::fs;

use colored::Colorize;
use merlib::{
    binaries,
    com::{self, AnalysisStage},
    runtime::VM,
};

#[test]
fn all_samples_work() {
    for file in fs::read_dir("./samples/").unwrap() {
        // read file
        let path_buf = file.unwrap().path();
        let path = &*path_buf.to_string_lossy();
        assert!(
            path.ends_with(".mer"),
            "found non-source-code file in ./samples/"
        );

        // analysis
        let source = fs::read_to_string(path).unwrap();
        let AnalysisStage::Ok(abt, diagnostics) = com::analyse_program(&source) else {
            panic!("failed to analyse sample {}", path.bold().underline());
        };

        // no other diagnostics (warnings, etc...)
        assert!(
            diagnostics.done().is_empty(),
            "sample {} has remaining diagnostics",
            path.bold().underline()
        );

        // compiles to bytecode
        let bytecode = com::compile_to_bytecode(abt).unwrap_or_else(|err| {
            panic!(
                "sample {} failed to compile: {err}",
                path.bold().underline()
            )
        });

        // disassembles properly
        binaries::disassemble(&bytecode).unwrap_or_else(|err| {
            panic!(
                "sample {} failed to disassemble: {err}",
                path.bold().underline()
            )
        });

        // executes properly
        VM::new(&bytecode).run::<()>().unwrap_or_else(|err| {
            panic!(
                "sample {} failed to execute properly: {err}",
                path.bold().underline()
            )
        });

        // all good
    }
}
