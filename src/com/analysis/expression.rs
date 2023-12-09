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
                => todo!(),
            ExprAstKind::Boolean(b)
                => ExprAbt::Boolean(*b),
            ExprAstKind::Parenthesized(inner)
                => self.analyse_expression(inner),
            ExprAstKind::BinaryOp(op, left, right)
                => todo!(),
            ExprAstKind::UnaryOp(op, operand)
                => todo!(),
            ExprAstKind::Call(name, params)
                => todo!(),
            ExprAstKind::Debug(inner)
                => ExprAbt::Debug(Box::new(self.analyse_expression(inner))),
        }
    }
}
