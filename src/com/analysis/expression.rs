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
            ExprAstKind::BinaryOp(op, left, right)
                => self.analyse_binary_operation(*op, left, right, expr.span),
            ExprAstKind::UnaryOp(op, operand)
                => self.analyse_unary_operation(*op, operand, expr.span),
            ExprAstKind::Call(callee, args)
                => self.analyse_call_expression(callee, args, expr.span),
            ExprAstKind::Debug(inner)
                => self.analyse_debug_expression(inner),
            ExprAstKind::Ref(expr)
                => self.analyse_reference_expression(expr),
        }
    }

    fn analyse_debug_expression(&mut self, expr: &ExprAst) -> ExprAbt {
        let inner = self.analyse_expression(expr);
        let ty = self.type_of(&inner);
        ExprAbt::Debug(Box::new(inner), ty)
    }
}
