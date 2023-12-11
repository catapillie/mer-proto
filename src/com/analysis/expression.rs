use crate::com::{
    abt::ExprAbt,
    syntax::expr::{ExprAst, ExprAstKind},
};

use super::Analyser;

impl<'d> Analyser<'d> {
    #[rustfmt::skip]
    pub fn analyse_expression(&mut self, expr: &ExprAst) -> ExprAbt {
        match &expr.kind {
            ExprAstKind::Bad
                => ExprAbt::Unknown,
            ExprAstKind::Integer(num)
                => ExprAbt::Integer(*num),
            ExprAstKind::Decimal(num)
                => ExprAbt::Decimal(*num),
            ExprAstKind::Identifier(id)
                => self.analyse_variable_expression(id, expr.span),
            ExprAstKind::Boolean(b)
                => ExprAbt::Boolean(*b),
            ExprAstKind::Parenthesized(inner)
                => self.analyse_expression(inner),
            ExprAstKind::BinaryOp(_, _, _)
                => todo!(),
            ExprAstKind::UnaryOp(_, _)
                => todo!(),
            ExprAstKind::Call(callee, args)
                => self.analyse_call_expression(callee, args, expr.span),
            ExprAstKind::Debug(inner)
                => ExprAbt::Debug(Box::new(self.analyse_expression(inner))),
        }
    }
}
