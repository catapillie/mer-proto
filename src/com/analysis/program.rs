use crate::com::{
    abt::{ProgramAbt, TypeAbt},
    analysis::FunctionInfo,
    diagnostics::{self, DiagnosticKind, Severity},
    syntax::stmt::{StmtAst, StmtAstKind},
};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn analyse_program(mut self, ast: &StmtAst, expected_type: TypeAbt) -> ProgramAbt {
        let main_fn_id = 0;
        self.functions.insert(
            main_fn_id,
            FunctionInfo {
                id: main_fn_id,
                name: "<main>".to_string(),
                span: None,
                depth: 0,
                args: vec![],
                arg_ids: vec![],
                ty: expected_type,
                ty_span: None,
                used_variables: Default::default(),
                code: None,
            },
        );

        self.scope.depth = 1;
        self.reach_top_level_declarations(ast);
        self.scope.depth = 0;

        let abt = self.analyse_statement(ast);
        if !self.analyse_control_flow(&abt) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::TopLevelMustReturn)
                .with_severity(Severity::Error)
                .without_span()
                .done();
            self.diagnostics.push(d);
        }

        let info = self.functions.get_mut(&main_fn_id).unwrap();
        info.code = Some(Box::new(abt));

        let var_count = self.count_all_variable_sizes(main_fn_id);
        if var_count > 255 {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::TooManyTopLevelVariables(var_count))
                .with_severity(Severity::Error)
                .without_span()
                .done();
            self.diagnostics.push(d);
        }

        self.analyse_function_variable_usage();

        // correct scope usage
        assert!(self.scope.is_root());

        ProgramAbt {
            main_fn_id,
            functions: self.functions,
            variables: self.variables,
        }
    }

    fn reach_top_level_declarations(&mut self, ast: &StmtAst) {
        if let StmtAstKind::Block(stmts) = &ast.kind {
            for stmt in stmts {
                if let StmtAstKind::Func(Some((name, span)), args, _, ty) = &stmt.kind {
                    let bound_args = args
                        .iter()
                        .map(|(name, ty, _)| (name.clone(), self.analyse_type(ty)))
                        .collect();
                    let bound_ty = self.analyse_type(ty);
                    self.declare_function_here(
                        name,
                        Some(*span),
                        bound_args,
                        bound_ty,
                        Some(ty.span),
                    );
                }
            }
        }
    }
}
