use colored::Colorize;
use merlib::{
    binary,
    com::{self, abt::Type, AnalysisStage},
    diagnostics::Severity,
    runtime::VM,
};
use std::fs;

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
check_program_output!(pointer_heap_assign => (bool, Type::Bool) true);
check_program_output!(pointer_heap_tuple_assign => (bool, Type::Bool) true);
check_program_output!(pointer_heap_array_assign => (bool, Type::Bool) true);
check_program_output!(pointer_heap_data_assign => (bool, Type::Bool) true);
check_program_output!(pointer_ref_assign => (bool, Type::Bool) true);
check_program_output!(pointer_ref_tuple_assign => (bool, Type::Bool) true);
check_program_output!(pointer_ref_array_assign => (bool, Type::Bool) true);
check_program_output!(pointer_ref_data_assign => (bool, Type::Bool) true);

check_program_output!(concat_array => (bool, Type::Bool) true);
check_program_output!(concat_tuple_tuple => (bool, Type::Bool) true);

check_program_output!(data_with_00 => (bool, Type::Bool) true);
check_program_output!(data_with_01 => (bool, Type::Bool) true);
check_program_output!(data_with_02 => (bool, Type::Bool) true);
check_program_output!(data_with_03 => (bool, Type::Bool) true);
check_program_output!(data_with_04 => (bool, Type::Bool) true);

check_program_output!(lvalue_single_var => (bool, Type::Bool) true);
check_program_output!(lvalue_single_var_deref_one => (bool, Type::Bool) true);
check_program_output!(lvalue_single_var_deref_two => (bool, Type::Bool) true);
check_program_output!(lvalue_single_var_deref_three => (bool, Type::Bool) true);
check_program_output!(lvalue_single_tuple => (bool, Type::Bool) true);
check_program_output!(lvalue_single_tuple_deref_one => (bool, Type::Bool) true);
check_program_output!(lvalue_single_tuple_deref_two => (bool, Type::Bool) true);
check_program_output!(lvalue_single_tuple_deref_three => (bool, Type::Bool) true);
check_program_output!(lvalue_single_array => (bool, Type::Bool) true);
check_program_output!(lvalue_single_array_deref_one => (bool, Type::Bool) true);
check_program_output!(lvalue_single_array_deref_two => (bool, Type::Bool) true);
check_program_output!(lvalue_single_array_deref_three => (bool, Type::Bool) true);
check_program_output!(lvalue_single_array_index => (bool, Type::Bool) true);
check_program_output!(lvalue_single_array_index_deref_one => (bool, Type::Bool) true);
check_program_output!(lvalue_single_array_index_deref_two => (bool, Type::Bool) true);
check_program_output!(lvalue_single_array_index_deref_three => (bool, Type::Bool) true);
check_program_output!(lvalue_single_pointer => (bool, Type::Bool) true);
check_program_output!(lvalue_single_pointer_deref_one => (bool, Type::Bool) true);
check_program_output!(lvalue_single_pointer_deref_two => (bool, Type::Bool) true);
check_program_output!(lvalue_single_pointer_deref_three => (bool, Type::Bool) true);
check_program_output!(lvalue_single_data => (bool, Type::Bool) true);
check_program_output!(lvalue_single_data_deref_one => (bool, Type::Bool) true);
check_program_output!(lvalue_single_data_deref_two => (bool, Type::Bool) true);
check_program_output!(lvalue_single_data_deref_three => (bool, Type::Bool) true);

check_program_output!(lvalue_many_var => (bool, Type::Bool) true);
check_program_output!(lvalue_many_var_deref_one => (bool, Type::Bool) true);
check_program_output!(lvalue_many_var_deref_two => (bool, Type::Bool) true);
check_program_output!(lvalue_many_var_deref_three => (bool, Type::Bool) true);
check_program_output!(lvalue_many_tuple => (bool, Type::Bool) true);
check_program_output!(lvalue_many_tuple_deref_one => (bool, Type::Bool) true);
check_program_output!(lvalue_many_tuple_deref_two => (bool, Type::Bool) true);
check_program_output!(lvalue_many_tuple_deref_three => (bool, Type::Bool) true);
check_program_output!(lvalue_many_array => (bool, Type::Bool) true);
check_program_output!(lvalue_many_array_deref_one => (bool, Type::Bool) true);
check_program_output!(lvalue_many_array_deref_two => (bool, Type::Bool) true);
check_program_output!(lvalue_many_array_deref_three => (bool, Type::Bool) true);
check_program_output!(lvalue_many_array_index => (bool, Type::Bool) true);
check_program_output!(lvalue_many_array_index_deref_one => (bool, Type::Bool) true);
check_program_output!(lvalue_many_array_index_deref_two => (bool, Type::Bool) true);
check_program_output!(lvalue_many_array_index_deref_three => (bool, Type::Bool) true);
check_program_output!(lvalue_many_pointer => (bool, Type::Bool) true);
check_program_output!(lvalue_many_pointer_deref_one => (bool, Type::Bool) true);
check_program_output!(lvalue_many_pointer_deref_two => (bool, Type::Bool) true);
check_program_output!(lvalue_many_pointer_deref_three => (bool, Type::Bool) true);
check_program_output!(lvalue_many_data => (bool, Type::Bool) true);
check_program_output!(lvalue_many_data_deref_one => (bool, Type::Bool) true);
check_program_output!(lvalue_many_data_deref_two => (bool, Type::Bool) true);
check_program_output!(lvalue_many_data_deref_three => (bool, Type::Bool) true);

check_program_output!(type_alias_assign => (bool, Type::Bool) true);
check_program_output!(type_alias_binary => (bool, Type::Bool) true);
check_program_output!(type_alias_unary => (bool, Type::Bool) true);
check_program_output!(type_alias_deref => (bool, Type::Bool) true);
check_program_output!(type_alias_array_index => (bool, Type::Bool) true);
check_program_output!(type_alias_array_immediate_index => (bool, Type::Bool) true);
check_program_output!(type_alias_tuple_immediate_index => (bool, Type::Bool) true);
check_program_output!(type_alias_pointer_index => (bool, Type::Bool) true);
check_program_output!(type_alias_field_access => (bool, Type::Bool) true);
check_program_output!(type_alias_data_with => (bool, Type::Bool) true);
check_program_output!(type_alias_immediate_call => (bool, Type::Bool) true);
check_program_output!(type_alias_indirect_call => (bool, Type::Bool) true);
check_program_output!(type_alias_case => (bool, Type::Bool) true);
check_program_output!(type_alias_case_ternary => (bool, Type::Bool) true);
check_program_output!(type_opaque_binary => (bool, Type::Bool) true);
check_program_output!(type_opaque_unary => (bool, Type::Bool) true);
check_program_output!(type_opaque_deref => (bool, Type::Bool) true);
check_program_output!(type_opaque_array_index => (bool, Type::Bool) true);
check_program_output!(type_opaque_array_immediate_index => (bool, Type::Bool) true);
check_program_output!(type_opaque_tuple_immediate_index => (bool, Type::Bool) true);
check_program_output!(type_opaque_pointer_index => (bool, Type::Bool) true);
check_program_output!(type_opaque_field_access => (bool, Type::Bool) true);
check_program_output!(type_opaque_data_with => (bool, Type::Bool) true);
check_program_output!(type_opaque_immediate_call => (bool, Type::Bool) true);
check_program_output!(type_opaque_indirect_call => (bool, Type::Bool) true);
check_program_output!(type_opaque_case => (bool, Type::Bool) true);
check_program_output!(type_opaque_case_ternary => (bool, Type::Bool) true);

check_program_output!(deconstruct_expr_array_discard => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_array_x => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_array_xy => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_array_xyz => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_array_xz => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_array_y => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_array_yz => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_array_z => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_binding_many => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_binding_one => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_discard_many => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_discard_one => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_opaque_array => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_opaque_discard => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_opaque_tuple => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_opaque => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_ref_array => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_ref_one => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_ref_tuple => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_tuple_discard => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_tuple_x => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_tuple_xy => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_tuple_xyz => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_tuple_xz => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_tuple_y => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_tuple_yz => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_tuple_z => (bool, Type::Bool) true);
check_program_output!(deconstruct_expr_unit => (bool, Type::Bool) true);

check_program_output!(deconstruct_type_array_discard => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_array_x => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_array_xy => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_array_xyz => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_array_xz => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_array_y => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_array_yz => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_array_z => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_binding_many => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_binding_one => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_discard_many => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_discard_one => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_opaque_array => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_opaque_discard => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_opaque_tuple => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_opaque => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_ref_array => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_ref_one => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_ref_tuple => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_tuple_discard => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_tuple_x => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_tuple_xy => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_tuple_xyz => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_tuple_xz => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_tuple_y => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_tuple_yz => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_tuple_z => (bool, Type::Bool) true);
check_program_output!(deconstruct_type_unit => (bool, Type::Bool) true);

check_program_output!(capture_imm_one => (bool, Type::Bool) true);
check_program_output!(capture_imm_two => (bool, Type::Bool) true);
check_program_output!(capture_closure_one => (bool, Type::Bool) true);
check_program_output!(capture_closure_two => (bool, Type::Bool) true);
check_program_output!(capture_import_imm => (bool, Type::Bool) true);
check_program_output!(capture_import_closure => (bool, Type::Bool) true);

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
