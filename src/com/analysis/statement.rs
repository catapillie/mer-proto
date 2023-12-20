use crate::com::{
    abt::{StmtAbt, StmtAbtKind},
    syntax::stmt::{StmtAst, StmtAstKind},
};

use super::Analyser;

impl<'d> Analyser<'d> {
    #[rustfmt::skip]
    pub fn analyse_statement(&mut self, stmt: &StmtAst) -> StmtAbt {
        match &stmt.kind {
            StmtAstKind::Empty
                => StmtAbtKind::Empty,
            StmtAstKind::VarDef(name, value)
                => self.analyse_variable_definition(name, value),
            StmtAstKind::Expr(expr)
                => StmtAbtKind::Expr(Box::new(self.analyse_expression(expr))),
            StmtAstKind::Block(stmts)
                => self.analyse_block_statement(stmts),
            StmtAstKind::IfThen(guard, body)
                => self.analyse_if_then_statement(guard, body),
            StmtAstKind::Then(body)
                => self.analyse_then_statement(body),
            StmtAstKind::IfThenElse(guard, body_then, body_else)
                => self.analyse_if_then_else_statement(guard, body_then, body_else),
            StmtAstKind::Else(body)
                => self.analyse_else_statement(body),
            StmtAstKind::WhileDo(guard, body)
                => self.analyse_while_do_statement(guard, body),
            StmtAstKind::DoWhile(body, guard)
                => self.analyse_do_while_statement(body, guard),
            StmtAstKind::Do(body)
                => self.analyse_do_statement(body),
            StmtAstKind::Func(name, args, body, ty)
                => self.analyse_function_definition(name, args, body, ty),
            StmtAstKind::Return
                => self.analyse_return_statement(stmt.span),
            StmtAstKind::ReturnWith(expr)
                => self.analyse_return_with_statement(expr),
        }
        .wrap(stmt.span)
    }

    fn analyse_block_statement(&mut self, stmts: &[StmtAst]) -> StmtAbtKind {
        self.open_scope();
        let bound_stmts = stmts
            .iter()
            .map(|stmt| self.analyse_statement(stmt))
            .filter(|stmt| !matches!(stmt.kind, StmtAbtKind::Empty))
            .collect::<Vec<_>>();
        self.close_scope();

        if bound_stmts.is_empty() {
            StmtAbtKind::Empty
        } else {
            StmtAbtKind::Block(bound_stmts)
        }
    }
}
