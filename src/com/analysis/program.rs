use crate::com::{
    diagnostics::{self, DiagnosticKind, Severity},
    syntax::stmt::{StmtAst, StmtAstKind}, abt::ProgramAbt,
};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn analyse_program(mut self, ast: &StmtAst) -> ProgramAbt {
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

        assert!(self.scope.is_root()); // correct scope usage

        ProgramAbt {
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
                    self.declare_function_here(name, *span, bound_args, bound_ty, ty.span);
                }
            }
        }
    }
}
