use diagnostics::{DiagnosticKind, Note, Severity};

use crate::{
    com::{abt::ExprAbt, diagnostics, syntax::expr::ExprAst, TypeAbt},
    utils::Span,
};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn analyse_array_expression(&mut self, exprs: &[ExprAst], span: Span) -> ExprAbt {
        if exprs.is_empty() {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::EmptyArray)
                .with_span(span)
                .with_severity(Severity::Error)
                .annotate_primary(Note::Here, span)
                .done();
            self.diagnostics.push(d);
            return ExprAbt::Unknown;
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
            .map(|expr| self.type_of(expr))
            .collect::<Vec<_>>();
        let first_ty = tys
            .iter()
            .find(|ty| !matches!(ty, TypeAbt::Never))
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
            return ExprAbt::Unknown;
        }

        ExprAbt::Array(bound_exprs)
    }

    pub fn analyse_array_immediate_index(
        &mut self,
        expr: &ExprAst,
        bound_expr: ExprAbt,
        index: u64,
        size: usize,
        span: Span,
    ) -> ExprAbt {
        if index as usize >= size {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::OutOfRangeArrayIndex {
                    len: size,
                    index: index as usize,
                })
                .with_span(span)
                .with_severity(Severity::Error)
                .annotate_primary(Note::OfType(self.type_of(&bound_expr)), expr.span)
                .done();
            self.diagnostics.push(d);
            return ExprAbt::Unknown;
        }

        ExprAbt::ArrayImmediateIndex(Box::new(bound_expr), index as usize)
    }
}