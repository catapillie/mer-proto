use crate::com::{
    abt::{ProgramAbt, TypeAbt},
    diagnostics::{self, DiagnosticKind, Severity},
    syntax::stmt::{StmtAst, StmtAstKind},
};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn analyse_program(mut self, ast: &StmtAst) -> ProgramAbt {
        let main_fn_id = self
            .declare_function_here("<main>", None, vec![], TypeAbt::Unit, None)
            .declared;

        self.scope.depth = 1;
        self.reach_top_level_declarations(ast);
        self.scope.depth = 0;

        self.scope.current_func_id = Some(main_fn_id);

        let abt = self.analyse_statement(ast);
        if !self.analyse_control_flow(&abt) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::TopLevelMustReturn)
                .with_severity(Severity::Error)
                .without_span()
                .done();
            self.diagnostics.push(d);
        }

        assert!(self.scope.is_root()); // correct scope usage

        self.functions.get_mut(&main_fn_id).unwrap().code = Some(Box::new(abt));

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
