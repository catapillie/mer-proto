use std::fs;

use colored::Colorize;
use merlib::{
    binary,
    com::{self, abt::Type, AnalysisStage},
    diagnostics::Severity,
    runtime::VM,
};

check_program_output!(return_i64 => (i64, Type::I64) 10);
check_program_output!(return_f64 => (f64, Type::F64) 6.2584);
check_program_output!(return_unit => ((), Type::Unit) ());
check_program_output!(return_bool => (bool, Type::Bool) false);

check_program_output!(arithmetic_00 => (i64, Type::I64) 1000);
check_program_output!(arithmetic_01 => (f64, Type::F64) 3.7);
check_program_output!(arithmetic_02 => (i64, Type::I64) 24);
check_program_output!(arithmetic_03 => (i64, Type::I64) 17);
check_program_output!(arithmetic_04 => (f64, Type::F64) -0.5);

check_program_output!(boolean_00 => (bool, Type::Bool) false);
check_program_output!(boolean_01 => (bool, Type::Bool) true);
check_program_output!(boolean_02 => (bool, Type::Bool) false);
check_program_output!(boolean_03 => (bool, Type::Bool) true);

check_program_output!(variables_decl => (i64, Type::I64) 7);
check_program_output!(variables_shadow => (bool, Type::Bool) true);
check_program_output!(variables_assign => (bool, Type::Bool) true);

check_program_output!(flow_if_then => (bool, Type::Bool) true);
check_program_output!(flow_if_then_else => (bool, Type::Bool) true);
check_program_output!(flow_if_then_else_nested => (bool, Type::Bool) true);
check_program_output!(flow_while_do => (bool, Type::Bool) true);
check_program_output!(flow_do_while => (bool, Type::Bool) true);
check_program_output!(flow_while_do_nested => (bool, Type::Bool) true);

check_program_output!(references_one => (bool, Type::Bool) true);
check_program_output!(references_two => (bool, Type::Bool) true);
check_program_output!(references_three => (bool, Type::Bool) true);

check_program_output!(references_immediate_alloc => (bool, Type::Bool) true);
check_program_output!(references_deref => (bool, Type::Bool) true);
check_program_output!(references_of_args => (bool, Type::Bool) true);
check_program_output!(references_assign_one => (bool, Type::Bool) true);
check_program_output!(references_assign_two => (bool, Type::Bool) true);
check_program_output!(references_assign_three => (bool, Type::Bool) true);

check_program_output!(functions_expr => (bool, Type::Bool) true);
check_program_output!(functions_params => (bool, Type::Bool) true);
check_program_output!(functions_body => (bool, Type::Bool) true);
check_program_output!(functions_nested => (bool, Type::Bool) true);
check_program_output!(functions_recursive => (bool, Type::Bool) true);
check_program_output!(functions_side_effects => (bool, Type::Bool) true);
check_program_output!(functions_unit => ((), Type::Unit) ());

check_program_output!(functions_as_values_heap => ((), Type::Unit) ());
check_program_output!(functions_as_values_no_arg => (bool, Type::Bool) true);
check_program_output!(functions_as_values_one_arg => (bool, Type::Bool) true);
check_program_output!(functions_as_values_many_args => (bool, Type::Bool) true);
check_program_output!(functions_as_values_ref_of_args => (bool, Type::Bool) true);

check_program_output!(indirect_call_no_arg => ((), Type::Unit) ());
check_program_output!(indirect_call_one_arg => ((), Type::Unit) ());
check_program_output!(indirect_call_many_args => ((), Type::Unit) ());

check_program_output!(tuple_variables => (bool, Type::Bool) true);
check_program_output!(tuple_functions_returned => ((), Type::Unit) ());
check_program_output!(tuple_functions_one_arg => ((), Type::Unit) ());
check_program_output!(tuple_functions_many_args => ((), Type::Unit) ());
check_program_output!(tuple_references => (bool, Type::Bool) true);
check_program_output!(tuple_immediate_alloc => (bool, Type::Bool) true);
check_program_output!(tuple_assign_value => (bool, Type::Bool) true);
check_program_output!(tuple_assign_index => (bool, Type::Bool) true);
check_program_output!(tuple_assign_index_many => (bool, Type::Bool) true);
check_program_output!(tuple_assign_index_nested => (bool, Type::Bool) true);
check_program_output!(tuple_assign_in_func => (bool, Type::Bool) true);
check_program_output!(tuple_assign_ref_one => (bool, Type::Bool) true);
check_program_output!(tuple_assign_ref_two => (bool, Type::Bool) true);
check_program_output!(tuple_assign_ref_three => (bool, Type::Bool) true);
check_program_output!(tuple_assign_immediate_ref_one => (bool, Type::Bool) true);
check_program_output!(tuple_assign_immediate_ref_two => (bool, Type::Bool) true);
check_program_output!(tuple_assign_immediate_ref_three => (bool, Type::Bool) true);
check_program_output!(tuple_func_arg_realloc => (bool, Type::Bool) true);
check_program_output!(tuple_nested_one => (bool, Type::Bool) true);
check_program_output!(tuple_nested_two => (bool, Type::Bool) true);
check_program_output!(tuple_nested_three => (bool, Type::Bool) true);
check_program_output!(tuple_indexing => (bool, Type::Bool) true);
check_program_output!(tuple_indexing_nested => (bool, Type::Bool) true);

check_program_output!(array_init => (bool, Type::Bool) true);
check_program_output!(array_copy => (bool, Type::Bool) true);
check_program_output!(array_as_func_arg => (bool, Type::Bool) true);
check_program_output!(array_as_func_arg_many => (bool, Type::Bool) true);
check_program_output!(array_as_func_return => (bool, Type::Bool) true);
check_program_output!(array_ref => (bool, Type::Bool) true);
check_program_output!(array_immediate_heap => (bool, Type::Bool) true);
check_program_output!(array_nested => (bool, Type::Bool) true);
check_program_output!(array_indexing => (bool, Type::Bool) true);
check_program_output!(array_nested_indexing => (bool, Type::Bool) true);
check_program_output!(array_assign_index => (bool, Type::Bool) true);
check_program_output!(array_assign_index_many => (bool, Type::Bool) true);
check_program_output!(array_assign_index_nested => (bool, Type::Bool) true);
check_program_output!(array_assign_at_index => (bool, Type::Bool) true);
check_program_output!(array_assign_at_index_many => (bool, Type::Bool) true);
check_program_output!(array_assign_at_index_nested => (bool, Type::Bool) true);
check_program_output!(array_assign_heap_at_index => (bool, Type::Bool) true);
check_program_output!(array_assign_heap_at_index_many => (bool, Type::Bool) true);
check_program_output!(array_assign_heap_at_index_nested => (bool, Type::Bool) true);
check_program_output!(array_assign_immediate_ref_one => (bool, Type::Bool) true);
check_program_output!(array_assign_immediate_ref_two => (bool, Type::Bool) true);
check_program_output!(array_assign_immediate_ref_three => (bool, Type::Bool) true);
check_program_output!(array_assign_in_func => (bool, Type::Bool) true);
check_program_output!(array_assign_ref_one => (bool, Type::Bool) true);
check_program_output!(array_assign_ref_two => (bool, Type::Bool) true);
check_program_output!(array_assign_ref_three => (bool, Type::Bool) true);
check_program_output!(array_assign_value => (bool, Type::Bool) true);
check_program_output!(array_func_arg_realloc => (bool, Type::Bool) true);

check_program_output!(assign_complex_00 => (bool, Type::Bool) true);
check_program_output!(assign_complex_01 => (bool, Type::Bool) true);

check_program_output!(case_zero => (bool, Type::Bool) true);
check_program_output!(case_one => (bool, Type::Bool) true);
check_program_output!(case_two => (bool, Type::Bool) true);
check_program_output!(case_many => (bool, Type::Bool) true);
check_program_output!(case_order => (bool, Type::Bool) true);
check_program_output!(case_nested => (bool, Type::Bool) true);
check_program_output!(case_simple_syntax_eq => (bool, Type::Bool) true);

check_program_output!(data_init => (bool, Type::Bool) true);
check_program_output!(data_init_many => (bool, Type::Bool) true);
check_program_output!(data_init_ref => (bool, Type::Bool) true);
check_program_output!(data_init_ref_many => (bool, Type::Bool) true);
check_program_output!(data_init_heap_allocated => (bool, Type::Bool) true);
check_program_output!(data_init_heap_allocated_many => (bool, Type::Bool) true);

check_program_output!(data_assign => (bool, Type::Bool) true);
check_program_output!(data_assign_many => (bool, Type::Bool) true);
check_program_output!(data_assign_ref => (bool, Type::Bool) true);
check_program_output!(data_assign_ref_many => (bool, Type::Bool) true);
check_program_output!(data_assign_heap_allocated => (bool, Type::Bool) true);
check_program_output!(data_assign_heap_allocated_many => (bool, Type::Bool) true);

check_program_output!(pointer_assign => (bool, Type::Bool) true);
check_program_output!(pointer_tuple_assign => (bool, Type::Bool) true);
check_program_output!(pointer_array_assign => (bool, Type::Bool) true);
check_program_output!(pointer_data_assign => (bool, Type::Bool) true);

macro_rules! check_program_output {
    ($name:ident => ($expected_ty:ty, $type_abt:expr) $expected:expr) => {
        #[test]
        fn $name() {
            // analysis
            let path = format!("./samples_test/{}.mer", stringify!($name));
            let source = fs::read_to_string(&path).unwrap();
            let AnalysisStage::Ok(abt, diagnostics) =
                com::analyse_program_with_type(&source, $type_abt)
            else {
                panic!("failed to analyse sample {}", path.bold().underline());
            };

            // no other diagnostics (warnings, etc...)
            assert!(
                diagnostics.iter().all(|d| d.severity != Severity::Error),
                "sample {} has remaining errors",
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
            binary::disassemble(&bytecode).unwrap_or_else(|err| {
                panic!(
                    "sample {} failed to disassemble: {err}",
                    path.bold().underline()
                )
            });

            // executes properly
            let val = VM::new(&bytecode)
                .run::<$expected_ty>()
                .unwrap_or_else(|err| {
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
