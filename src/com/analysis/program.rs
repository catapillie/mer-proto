use super::Analyser;
use crate::{
    com::{
        abt::{self, FunctionInfo, Size},
        ast,
    },
    diagnostics::{self, DiagnosticKind, Severity},
    utils::OptSpanned,
};

impl<'d> Analyser<'d> {
    pub fn analyse_program(mut self, ast: &ast::Stmt, expected_type: abt::Type) -> abt::Program {
        let main_fn_id = self.program.main_fn_id;
        self.program.functions.insert(
            main_fn_id,
            FunctionInfo {
                id: main_fn_id,
                name: OptSpanned {
                    value: "<main>".to_string(),
                    span: None,
                },
                depth: 0,
                position: 0,
                args: vec![],
                arg_ids: vec![],
                ty: OptSpanned {
                    value: expected_type,
                    span: None,
                },
                local_variables: Default::default(),
                captured_variables: Default::default(),
                called_functions: Default::default(),
                defined_functions: Default::default(),
                code: None,
                was_analysed: true,
            },
        );

        let abt = self.analyse_statement(ast);
        if !self.analyse_control_flow(&abt) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::TopLevelMustReturn)
                .with_severity(Severity::Error)
                .without_span()
                .done();
            self.diagnostics.push(d);
        }

        let info = self.program.functions.get_mut(&main_fn_id).unwrap();
        info.code = Some(Box::new(abt));

        if let Size::Known(var_count) = self.count_all_variable_sizes(main_fn_id) {
            if var_count > 255 {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::TooManyTopLevelVariables(var_count))
                    .with_severity(Severity::Error)
                    .without_span()
                    .done();
                self.diagnostics.push(d);
            }
        }

        self.analyse_function_variable_usage();

        // correct scope usage
        assert!(self.scope.is_root());

        self.program
    }
}
