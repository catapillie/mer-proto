use crate::com::{syntax::expr::ExprAst, abt::ExprAbt};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn analyse_reference_expression(&mut self, expr: &ExprAst) -> ExprAbt {
        let bound_expr = self.analyse_expression(expr);

        if let ExprAbt::Variable(var_id) = bound_expr {
            // mark variable as heap-allocated
            if let Some(info) = self.variables.get_mut(&var_id) {
                info.is_on_heap = true;
            }
            ExprAbt::VarRef(var_id)
        } else {
            ExprAbt::Ref(Box::new(bound_expr))
        }
    }
}
