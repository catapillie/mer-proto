use crate::{
    com::{
        abt::{self, FunctionInfo},
        ast,
    },
    diagnostics::{self, DiagnosticKind, Severity},
    utils::{OptSpanned, Spanned},
};

use super::Analyser;

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
                args: vec![],
                arg_ids: vec![],
                ty: OptSpanned {
                    value: expected_type,
                    span: None,
                },
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

        let info = self.program.functions.get_mut(&main_fn_id).unwrap();
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

        self.program
    }

    fn reach_top_level_declarations(&mut self, ast: &ast::Stmt) {
        let ast::StmtKind::Block(stmts) = &ast.value else {
            return;
        };

        for stmt in stmts.iter() {
            match &stmt.value {
                ast::StmtKind::Func(Some(name), args, _, ty) => {
                    let bound_args = args
                        .iter()
                        .map(|(name, ty, _)| (name.clone(), self.analyse_type(ty)))
                        .collect();
                    let bound_ty = self.analyse_type(ty);
                    self.declare_function_here(
                        name.clone().into(),
                        bound_args,
                        OptSpanned {
                            value: bound_ty,
                            span: Some(ty.span),
                        },
                    );
                }
                ast::StmtKind::DataDef(name, fields) => {
                    let bound_fields = fields
                        .iter()
                        .map(|(name, ty)| {
                            (
                                name.clone(),
                                Spanned {
                                    value: self.analyse_type(ty),
                                    span: ty.span,
                                },
                            )
                        })
                        .collect::<Vec<_>>();
                    self.declare_data_structure_here(name, bound_fields);
                }
                _ => continue,
            }
        }
    }
}
