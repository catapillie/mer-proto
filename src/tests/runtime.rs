use std::fs;

use colored::Colorize;
use merlib::{
    binaries,
    com::{self, AnalysisStage, TypeAbt},
    runtime::VM,
};

check_program_output!(return_i64 => (i64, TypeAbt::I64) 10);
check_program_output!(return_f64 => (f64, TypeAbt::F64) 6.2584);
check_program_output!(return_unit => ((), TypeAbt::Unit) ());
check_program_output!(return_bool => (bool, TypeAbt::Bool) false);

check_program_output!(arithmetic_00 => (i64, TypeAbt::I64) 1000);
check_program_output!(arithmetic_01 => (f64, TypeAbt::F64) 3.7);
check_program_output!(arithmetic_02 => (i64, TypeAbt::I64) 24);
check_program_output!(arithmetic_03 => (i64, TypeAbt::I64) 17);
check_program_output!(arithmetic_04 => (f64, TypeAbt::F64) -0.5);

check_program_output!(boolean_00 => (bool, TypeAbt::Bool) false);
check_program_output!(boolean_01 => (bool, TypeAbt::Bool) true);
check_program_output!(boolean_02 => (bool, TypeAbt::Bool) false);
check_program_output!(boolean_03 => (bool, TypeAbt::Bool) true);

check_program_output!(variables_decl => (i64, TypeAbt::I64) 7);
check_program_output!(variables_shadow => (bool, TypeAbt::Bool) true);
check_program_output!(variables_assign => (bool, TypeAbt::Bool) true);

check_program_output!(flow_if_then => (bool, TypeAbt::Bool) true);
check_program_output!(flow_if_then_else => (bool, TypeAbt::Bool) true);
check_program_output!(flow_if_then_else_nested => (bool, TypeAbt::Bool) true);
check_program_output!(flow_while_do => (bool, TypeAbt::Bool) true);
check_program_output!(flow_do_while => (bool, TypeAbt::Bool) true);
check_program_output!(flow_while_do_nested => (bool, TypeAbt::Bool) true);

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
