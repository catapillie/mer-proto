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
            if let Some(info) = self.program.variables.get_mut(&var_id) {
                info.is_on_heap = true;
            }
            return abt::Expr::VarRef(var_id);
        }

        let d = diagnostics::create_diagnostic()
            .with_kind(DiagnosticKind::CannotTakeReference)
            .with_span(expr.span)
            .with_severity(Severity::Error)
            .annotate_primary(Note::NotLValue, expr.span)
            .done();
        self.diagnostics.push(d);
        abt::Expr::Unknown
    }

    pub fn analyse_heap_expression(&mut self, expr: &ast::Expr) -> abt::Expr {
        abt::Expr::Heap(Box::new(self.analyse_expression(expr)))
    }

    pub fn analyse_dereference_expression(&mut self, expr: &ast::Expr) -> abt::Expr {
        let bound_expr = self.analyse_expression(expr);
        let ty = self.program.type_of(&bound_expr);

        if !ty.is_known() || matches!(bound_expr, abt::Expr::Unknown) {
            return abt::Expr::Unknown;
        }

        match ty {
            abt::Type::Ref(_) => abt::Expr::Deref(Box::new(bound_expr)),
            _ => {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::InvalidDereference(
                        self.program.type_repr(&ty),
                    ))
                    .with_severity(Severity::Error)
                    .with_span(expr.span)
                    .annotate_primary(Note::OfType(self.program.type_repr(&ty)), expr.span)
                    .done();
                self.diagnostics.push(d);
                abt::Expr::Unknown
            }
        }
    }
}
