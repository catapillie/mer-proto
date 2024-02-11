use crate::{
    com::{abt, ast},
    diagnostics::{self, DiagnosticKind, Note, Severity},
    utils::Span,
};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn analyse_array_expression(&mut self, exprs: &[ast::Expr], span: Span) -> abt::Expr {
        if exprs.is_empty() {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::EmptyArray)
                .with_span(span)
                .with_severity(Severity::Error)
                .annotate_primary(Note::Here, span)
                .done();
            self.diagnostics.push(d);
            return abt::Expr::Unknown;
        }

        if exprs.len() == 1 {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::SingletonArray)
                .with_span(span)
                .with_severity(Severity::Warning)
                .annotate_primary(Note::Here, span)
                .done();
            self.diagnostics.push(d);
        }

        let bound_exprs = exprs
            .iter()
            .map(|expr| self.analyse_expression(expr))
            .collect::<Box<_>>();

        let tys = bound_exprs
            .iter()
            .map(|expr| self.program.type_of(expr))
            .collect::<Vec<_>>();
        let first_ty = tys
            .iter()
            .find(|ty| !matches!(ty, abt::Type::Never))
            .unwrap_or(tys.first().expect("empty arrays are not handled properly"));

        let all_types_match = tys.iter().all(|ty| ty.is(first_ty));
        if !all_types_match {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::ArrayMismatchingTypes)
                .with_span(span)
                .with_severity(Severity::Error)
                .annotate_primary(Note::Here, span)
                .done();
            self.diagnostics.push(d);
            return abt::Expr::Unknown;
        }

        abt::Expr::Array(bound_exprs)
    }

    pub fn analyse_array_immediate_index(
        &mut self,
        expr: &ast::Expr,
        bound_expr: abt::Expr,
        index: u64,
        size: usize,
        span: Span,
    ) -> abt::Expr {
        if index as usize >= size {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::OutOfRangeArrayIndex {
                    len: size,
                    index: index as usize,
                })
                .with_span(span)
                .with_severity(Severity::Error)
                .annotate_primary(
                    Note::OfType(self.program.type_repr(&self.program.type_of(&bound_expr))),
                    expr.span,
                )
                .done();
            self.diagnostics.push(d);
            return abt::Expr::Unknown;
        }

        abt::Expr::ArrayImmediateIndex(Box::new(bound_expr), index as usize)
    }
}
