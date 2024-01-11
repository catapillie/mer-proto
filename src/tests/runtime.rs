use std::fs;

use colored::Colorize;
use merlib::{
    binaries,
    com::{self, AnalysisStage, TypeAbt},
    runtime::VM,
};

check_program_output!(return_i64 => (i64, TypeAbt::I64) 10);
check_program_output!(return_f64 => (f64, TypeAbt::F64) 3.1415);
check_program_output!(return_unit => ((), TypeAbt::Unit) ());
check_program_output!(return_bool => (bool, TypeAbt::Bool) false);

macro_rules! check_program_output {
    ($name:ident => ($expected_ty:ty, $type_abt:expr) $expected:expr) => {
        #[test]
        fn $name() {
            // analysis
            let path = format!("./samples_test/{}.mer", stringify!($name));
            let source = fs::read_to_string(&path).unwrap();
            let AnalysisStage::Ok(abt, diagnostics) = com::analyse_program_with_type(&source, $type_abt)
            else {
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
            let val = VM::new(&bytecode).run::<$expected_ty>().unwrap_or_else(|err| {
                panic!(
                    "sample {} failed to execute properly: {err}",
                    path.bold().underline()
                )
            });

            // check result
            assert_eq!(val, $expected);
        }
    };
}

use check_program_output;
