use crate::{
    com::{abt, ast},
    diagnostics::{self, DiagnosticKind, Note, Severity},
    utils::Span,
};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn analyse_tuple_expression(&mut self, head: &ast::Expr, tail: &[ast::Expr]) -> abt::Expr {
        abt::Expr::Tuple(
            Box::new(self.analyse_expression(head)),
            tail.iter().map(|e| self.analyse_expression(e)).collect(),
        )
    }

    pub fn analyse_tuple_immediate_index(
        &mut self,
        expr: &ast::Expr,
        bound_expr: abt::Expr,
        tail: &[abt::Type],
        index: u64,
        span: Span,
    ) -> abt::Expr {
        if index == 0 {
            return abt::Expr::TupleImmediateIndex(Box::new(bound_expr), 0);
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
            return abt::Expr::Unknown;
        }

        abt::Expr::TupleImmediateIndex(Box::new(bound_expr), index as usize)
    }
}
