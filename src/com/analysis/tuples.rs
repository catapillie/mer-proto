use diagnostics::{DiagnosticKind, Note, Severity};

use crate::{
    com::{abt::ExprAbt, diagnostics, syntax::expr::ExprAst, TypeAbt},
    utils::Span,
};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn analyse_tuple_expression(&mut self, head: &ExprAst, tail: &[ExprAst]) -> ExprAbt {
        ExprAbt::Tuple(
            Box::new(self.analyse_expression(head)),
            tail.iter().map(|e| self.analyse_expression(e)).collect(),
        )
    }

    pub fn analyse_tuple_field_access(
        &mut self,
        expr: &ExprAst,
        index: u64,
        span: Span,
    ) -> ExprAbt {
        let bound_expr = self.analyse_expression(expr);
        let ty = self.type_of(&bound_expr);

        if !ty.is_known() {
            return ExprAbt::Unknown;
        }

        let TypeAbt::Tuple(_, tail) = ty else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::InvalidImmediateIndex)
                .with_span(span)
                .with_severity(Severity::Error)
                .annotate_primary(Note::OfType(ty), expr.span)
                .done();
            self.diagnostics.push(d);
            return ExprAbt::Unknown;
        };

        if index == 0 {
            return ExprAbt::TupleIndex(Box::new(bound_expr), 0);
        }

        let len = 1 + tail.len();
        if index as usize >= len {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::InvalidTupleIndex {
                    len,
                    accessed: index as usize,
                })
                .with_span(span)
                .with_severity(Severity::Error)
                .annotate_primary(Note::TupleValueCount(len), expr.span)
                .done();
            self.diagnostics.push(d);
            return ExprAbt::Unknown;
        }

        ExprAbt::TupleIndex(Box::new(bound_expr), index as usize)
    }
}
