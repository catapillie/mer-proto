use super::Analyser;
use crate::{
    com::{abt, ast},
    diagnostics::{self, DiagnosticKind, Note, NoteSeverity, Severity},
    utils::Span,
};

impl<'d> Analyser<'d> {
    fn try_coerce_indexable(&self, expr: &mut abt::Expr) -> Option<abt::Type> {
        let mut ty = &expr.ty;
        ty = self.program.dealias_type(ty);

        let mut deref_count = 0;
        let final_ty = loop {
            match ty {
                abt::Type::Array(_, _) => break ty.clone(),
                abt::Type::Pointer(_) => break ty.clone(),
                abt::Type::Ref(inner) => {
                    ty = &**inner;
                    deref_count += 1;
                }
                _ => return None,
            }
        };

        for _ in 0..deref_count {
            let inner = std::mem::replace(expr, abt::Expr::unknown());
            let abt::Type::Ref(ty) = inner.ty.clone() else {
                unreachable!()
            };
            *expr = abt::Expr {
                kind: abt::ExprKind::Deref(Box::new(inner)),
                ty: *ty,
            };
        }

        Some(final_ty)
    }

    fn try_coerce_immediate_indexable(&self, expr: &mut abt::Expr) -> Option<abt::Type> {
        let mut ty = &expr.ty;
        ty = self.program.dealias_type(ty);

        let mut deref_count = 0;
        let final_ty = loop {
            match ty {
                abt::Type::Tuple(_, _) => break ty.clone(),
                abt::Type::Array(_, _) => break ty.clone(),
                abt::Type::Ref(inner) => {
                    ty = &**inner;
                    deref_count += 1;
                }
                _ => return None,
            }
        };

        for _ in 0..deref_count {
            let inner = std::mem::replace(expr, abt::Expr::unknown());
            let abt::Type::Ref(ty) = inner.ty.clone() else {
                unreachable!()
            };
            *expr = abt::Expr {
                kind: abt::ExprKind::Deref(Box::new(inner)),
                ty: *ty,
            };
        }

        Some(final_ty.clone())
    }

    pub fn analyse_index_expression(
        &mut self,
        expr: &ast::Expr,
        index_expr: &ast::Expr,
        span: Span,
    ) -> abt::Expr {
        let mut bound_expr = self.analyse_expression(expr);
        let bound_index = self.analyse_expression(index_expr);

        let expr_ty = bound_expr.ty.clone();
        let index_ty = &bound_index.ty;

        if !self.type_check(index_ty, &abt::Type::I64) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::ArrayIndexMustBeInteger)
                .with_span(index_expr.span)
                .with_severity(Severity::Error)
                .annotate_primary(
                    Note::OfTypeButShouldBe(
                        self.program.type_repr(index_ty),
                        self.program.type_repr(&abt::Type::I64),
                    ),
                    index_expr.span,
                )
                .done();
            self.diagnostics.push(d);
            return abt::Expr::unknown();
        }

        if !expr_ty.is_known() {
            return abt::Expr::unknown();
        }

        match self.try_coerce_indexable(&mut bound_expr) {
            Some(abt::Type::Array(ty, size)) => {
                if let abt::ExprKind::Integer(index) = bound_index.kind {
                    let d = if index as usize >= size {
                        diagnostics::create_diagnostic()
                            .with_kind(DiagnosticKind::OutOfRangeConstantIndex {
                                len: size,
                                index: index as usize,
                            })
                            .with_span(index_expr.span)
                            .with_severity(Severity::Error)
                            .annotate_primary(Note::KnownIndexTooLarge, index_expr.span)
                            .annotate_secondary(
                                Note::ArrayLength(size),
                                expr.span,
                                NoteSeverity::Annotation,
                            )
                            .done()
                    } else {
                        diagnostics::create_diagnostic()
                            .with_kind(DiagnosticKind::CanBeImmediateIndex)
                            .with_span(index_expr.span)
                            .with_severity(Severity::Warning)
                            .annotate_primary(
                                Note::CanBeImmediateIndex(index as usize),
                                index_expr.span,
                            )
                            .done()
                    };
                    self.diagnostics.push(d);
                }

                abt::Expr {
                    kind: abt::ExprKind::ArrayIndex(Box::new(bound_expr), Box::new(bound_index)),
                    ty: *ty.clone(),
                }
            }
            Some(abt::Type::Pointer(ty)) => abt::Expr {
                kind: abt::ExprKind::PointerIndex(Box::new(bound_expr), Box::new(bound_index)),
                ty: *ty.clone(),
            },
            _ => {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::InvalidIndex)
                    .with_span(span)
                    .with_severity(Severity::Error)
                    .annotate_primary(Note::OfType(self.program.type_repr(&expr_ty)), expr.span)
                    .done();
                self.diagnostics.push(d);
                abt::Expr::unknown()
            }
        }
    }

    pub fn analyse_immediate_index(
        &mut self,
        expr: &ast::Expr,
        index: u64,
        span: Span,
    ) -> abt::Expr {
        let mut bound_expr = self.analyse_expression(expr);
        let ty = bound_expr.ty.clone();
        if !ty.is_known() {
            return abt::Expr::unknown();
        }

        match self.try_coerce_immediate_indexable(&mut bound_expr) {
            Some(abt::Type::Tuple(head, tail)) => {
                self.analyse_tuple_immediate_index(expr, bound_expr, &head, &tail, index, span)
            }
            Some(abt::Type::Array(array_ty, size)) => {
                self.analyse_array_immediate_index(expr, bound_expr, index, &array_ty, size, span)
            }
            _ => {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::InvalidImmediateIndex)
                    .with_span(span)
                    .with_severity(Severity::Error)
                    .annotate_primary(Note::OfType(self.program.type_repr(&ty)), expr.span)
                    .done();
                self.diagnostics.push(d);
                abt::Expr::unknown()
            }
        }
    }
}
