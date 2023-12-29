use crate::com::{
    abt::{ExprAbt, TypeAbt},
    diagnostics::{self, DiagnosticKind, Note, Severity},
    syntax::expr::ExprAst,
};

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

    pub fn analyse_dereference_expression(&mut self, expr: &ExprAst) -> ExprAbt {
        let bound_expr = self.analyse_expression(expr);
        let ty = self.type_of(&bound_expr);

        if matches!(bound_expr, ExprAbt::Unknown) {
            return ExprAbt::Unknown;
        }

        match ty {
            TypeAbt::Ref(_) => ExprAbt::Deref(Box::new(bound_expr)),
            _ => {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::InvalidDereference(ty.clone()))
                    .with_severity(Severity::Error)
                    .with_span(expr.span)
                    .annotate_primary(Note::ImpliedType(ty), expr.span)
                    .done();
                self.diagnostics.push(d);
                ExprAbt::Unknown
            }
        }
    }
}
