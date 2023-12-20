use crate::com::syntax::stmt::{StmtAst, StmtAstKind};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn analyse_program(&mut self, ast: &StmtAst) {
        self.reach_top_level_declarations(ast);
        self.analyse_statement(ast);
        assert!(self.scope.is_root()) // correct scope usage
    }

    fn reach_top_level_declarations(&mut self, ast: &StmtAst) {
        if let StmtAstKind::Block(stmts) = &ast.kind {
            for stmt in stmts {
                if let StmtAstKind::Func(Some(name), args, _, ty) = &stmt.kind {
                    let bound_args = args
                        .iter()
                        .map(|(name, ty)| (name.clone(), self.analyse_type(ty)))
                        .collect();
                    let bound_ty = self.analyse_type(ty);
                    self.declare_function_here(name, bound_args, bound_ty);
                }
            }
        }
    }
}
