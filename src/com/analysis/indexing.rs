use crate::{
    com::{abt::ExprAbt, ast, TypeAbt},
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
    ) -> ExprAbt {
        let bound_expr = self.analyse_expression(expr);
        let bound_index = self.analyse_expression(index_expr);

        let expr_ty = self.type_of(&bound_expr);
        let index_ty = self.type_of(&bound_index);

        if !index_ty.is(&TypeAbt::I64) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::ArrayIndexMustBeInteger)
                .with_span(index_expr.span)
                .with_severity(Severity::Error)
                .annotate_primary(Note::OfType(index_ty), index_expr.span)
                .done();
            self.diagnostics.push(d);
            return ExprAbt::Unknown;
        }

        if !expr_ty.is_known() {
            return ExprAbt::Unknown;
        }

        let TypeAbt::Array(_, size) = expr_ty else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::InvalidIndex)
                .with_span(span)
                .with_severity(Severity::Error)
                .annotate_primary(Note::OfType(expr_ty), expr.span)
                .done();
            self.diagnostics.push(d);
            return ExprAbt::Unknown;
        };

        if let ExprAbt::Integer(index) = bound_index {
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

        ExprAbt::ArrayIndex(Box::new(bound_expr), Box::new(bound_index))
    }

    pub fn analyse_immediate_index(&mut self, expr: &ast::Expr, index: u64, span: Span) -> ExprAbt {
        let bound_expr = self.analyse_expression(expr);
        let ty = self.type_of(&bound_expr);

        if !ty.is_known() {
            return ExprAbt::Unknown;
        }

        if let TypeAbt::Tuple(_, tail) = ty {
            self.analyse_tuple_immediate_index(expr, bound_expr, &tail, index, span)
        } else if let TypeAbt::Array(_, size) = ty {
            self.analyse_array_immediate_index(expr, bound_expr, index, size, span)
        } else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::InvalidImmediateIndex)
                .with_span(span)
                .with_severity(Severity::Error)
                .annotate_primary(Note::OfType(ty), expr.span)
                .done();
            self.diagnostics.push(d);
            ExprAbt::Unknown
        }
    }
}
