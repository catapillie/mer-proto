use crate::{
    com::{abt, ast},
    diagnostics::{self, DiagnosticKind, Note, Severity},
};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn analyse_reference_expression(&mut self, expr: &ast::Expr) -> abt::Expr {
        let bound_expr = self.analyse_expression(expr);

        if let abt::Expr::Variable(var_id) = bound_expr {
            // mark variable as heap-allocated
            if let Some(info) = self.variables.get_mut(&var_id) {
                info.is_on_heap = true;
            }
            abt::Expr::VarRef(var_id)
        } else {
            abt::Expr::Ref(Box::new(bound_expr))
        }
    }

    pub fn analyse_dereference_expression(&mut self, expr: &ast::Expr) -> abt::Expr {
        let bound_expr = self.analyse_expression(expr);
        let ty = self.type_of(&bound_expr);

        if matches!(bound_expr, abt::Expr::Unknown) {
            return abt::Expr::Unknown;
        }

        match ty {
            abt::TypeAbt::Ref(_) => match bound_expr {
                abt::Expr::Variable(var_id) => abt::Expr::VarDeref(var_id),
                _ => abt::Expr::Deref(Box::new(bound_expr)),
            },
            _ => {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::InvalidDereference(ty.clone()))
                    .with_severity(Severity::Error)
                    .with_span(expr.span)
                    .annotate_primary(Note::OfType(ty), expr.span)
                    .done();
                self.diagnostics.push(d);
                abt::Expr::Unknown
            }
        }
    }
}
