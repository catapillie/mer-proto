use crate::{
    com::{abt, ast},
    diagnostics::{self, DiagnosticKind, Note, Severity},
    utils::Span,
};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn analyse_index_expression(
        &mut self,
        expr: &ast::Expr,
        index_expr: &ast::Expr,
        span: Span,
    ) -> abt::Expr {
        let bound_expr = self.analyse_expression(expr);
        let bound_index = self.analyse_expression(index_expr);

        let expr_ty = self.type_of(&bound_expr);
        let index_ty = self.type_of(&bound_index);

        if !index_ty.is(&abt::Type::I64) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::ArrayIndexMustBeInteger)
                .with_span(index_expr.span)
                .with_severity(Severity::Error)
                .annotate_primary(Note::OfType(index_ty.repr()), index_expr.span)
                .done();
            self.diagnostics.push(d);
            return abt::Expr::Unknown;
        }

        if !expr_ty.is_known() {
            return abt::Expr::Unknown;
        }

        let abt::Type::Array(_, size) = expr_ty else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::InvalidIndex)
                .with_span(span)
                .with_severity(Severity::Error)
                .annotate_primary(Note::OfType(expr_ty.repr()), expr.span)
                .done();
            self.diagnostics.push(d);
            return abt::Expr::Unknown;
        };

        if let abt::Expr::Integer(index) = bound_index {
            let d = if index as usize >= size {
                diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::OutOfRangeConstantIndex {
                        len: size,
                        index: index as usize,
                    })
                    .with_span(index_expr.span)
                    .with_severity(Severity::Error)
                    .annotate_primary(Note::KnownIndexTooLarge, index_expr.span)
                    .done()
            } else {
                diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::CanBeImmediateIndex)
                    .with_span(index_expr.span)
                    .with_severity(Severity::Warning)
                    .annotate_primary(Note::CanBeImmediateIndex(index as usize), index_expr.span)
                    .done()
            };
            self.diagnostics.push(d);
        }

        abt::Expr::ArrayIndex(Box::new(bound_expr), Box::new(bound_index))
    }

    pub fn analyse_immediate_index(
        &mut self,
        expr: &ast::Expr,
        index: u64,
        span: Span,
    ) -> abt::Expr {
        let bound_expr = self.analyse_expression(expr);
        let ty = self.type_of(&bound_expr);

        if !ty.is_known() {
            return abt::Expr::Unknown;
        }

        if let abt::Type::Tuple(_, tail) = ty {
            self.analyse_tuple_immediate_index(expr, bound_expr, &tail, index, span)
        } else if let abt::Type::Array(_, size) = ty {
            self.analyse_array_immediate_index(expr, bound_expr, index, size, span)
        } else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::InvalidImmediateIndex)
                .with_span(span)
                .with_severity(Severity::Error)
                .annotate_primary(Note::OfType(ty.repr()), expr.span)
                .done();
            self.diagnostics.push(d);
            abt::Expr::Unknown
        }
    }
}
