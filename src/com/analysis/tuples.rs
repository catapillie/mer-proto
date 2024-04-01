use super::Analyser;
use crate::{
    com::{abt, ast},
    diagnostics::{self, DiagnosticKind, Note, Severity},
    utils::Span,
};

impl<'d> Analyser<'d> {
    pub fn analyse_tuple_expression(&mut self, head: &ast::Expr, tail: &[ast::Expr]) -> abt::TypedExpr {
        let bound_head = Box::new(self.analyse_expression(head));
        let bound_tail = tail
            .iter()
            .map(|e| self.analyse_expression(e))
            .collect::<Box<_>>();

        let head_ty = bound_head.value.ty.clone();
        let tail_ty = bound_tail.iter().map(|e| e.value.ty.clone()).collect();

        abt::TypedExpr {
            kind: abt::ExprKind::Tuple(bound_head, bound_tail),
            ty: abt::Type::Tuple(Box::new(head_ty), tail_ty),
        }
    }

    pub fn analyse_tuple_immediate_index(
        &mut self,
        expr: &ast::Expr,
        bound_expr: abt::Expr,
        head: &abt::Type,
        tail: &[abt::Type],
        index: u64,
        span: Span,
    ) -> abt::TypedExpr {
        if index == 0 {
            return abt::TypedExpr {
                kind: abt::ExprKind::TupleImmediateIndex(Box::new(bound_expr), 0),
                ty: head.clone(),
            };
        }

        let len = 1 + tail.len();
        let Some(ty) = tail.get(index as usize - 1).cloned() else {
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
            return abt::TypedExpr::unknown();
        };

        abt::TypedExpr {
            kind: abt::ExprKind::TupleImmediateIndex(Box::new(bound_expr), index as usize),
            ty,
        }
    }
}
