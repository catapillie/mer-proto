use crate::com::{syntax::expr::ExprAst, abt::ExprAbt};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn analyse_reference_expression(&mut self, expr: &ExprAst) -> ExprAbt {
        let bound_expr = self.analyse_expression(expr);

        ExprAbt::Ref(Box::new(bound_expr))
    }
}