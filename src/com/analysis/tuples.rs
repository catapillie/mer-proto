use crate::{
    com::{abt::ExprAbt, syntax::expr::ExprAst, TypeAbt},
    diagnostics::{self, DiagnosticKind, Note, Severity},
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

    pub fn analyse_tuple_immediate_index(
        &mut self,
        expr: &ExprAst,
        bound_expr: ExprAbt,
        tail: &[TypeAbt],
        index: u64,
        span: Span,
    ) -> ExprAbt {
        if index == 0 {
            return ExprAbt::TupleImmediateIndex(Box::new(bound_expr), 0);
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

        ExprAbt::TupleImmediateIndex(Box::new(bound_expr), index as usize)
    }
}
