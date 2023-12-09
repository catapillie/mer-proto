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
            StmtAstKind::VarDef(_, _)
                => todo!(),
            StmtAstKind::Expr(expr)
                => StmtAbtKind::Expr(Box::new(self.analyse_expression(expr))),
            StmtAstKind::Block(_)
                => todo!(),
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
            StmtAstKind::Func(_, _, _, _)
                => todo!(),
            StmtAstKind::Return
                => todo!(),
            StmtAstKind::ReturnWith(_)
                => todo!(),
        }
        .wrap(stmt.span)
    }
}
