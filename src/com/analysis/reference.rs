use super::Analyser;
use crate::{
    com::{
        abt::{self},
        ast,
    },
    diagnostics::{self, DiagnosticKind, Note, Severity},
};

impl<'d> Analyser<'d> {
    pub fn analyse_reference_expression(&mut self, expr: &ast::Expr) -> abt::Expr {
        let bound_expr = self.analyse_expression(expr);

        match self.to_lvalue(&bound_expr) {
            Some((lvalue, var_id, ty)) => {
                // mark variable as heap-allocated
                self.program.variables.get_mut(&var_id).unwrap().is_on_heap = true;
                abt::Expr::Ref(Box::new(lvalue), var_id, Box::new(ty))
            }
            None => abt::Expr::Heap(Box::new(self.analyse_expression(expr))),
        }
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
