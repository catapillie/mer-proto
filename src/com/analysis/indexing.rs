use super::Analyser;
use crate::{
    com::{abt, ast},
    diagnostics::{self, DiagnosticKind, Note, NoteSeverity, Severity},
    utils::Span,
};

impl<'d> Analyser<'d> {
    fn try_coerce_indexable(&self, expr: &mut abt::Expr) -> Option<abt::Type> {
        let mut ty = self.program.type_of(expr);
        let mut deref_count = 0;
        let final_ty = loop {
            match ty {
                abt::Type::Array(_, _) => break ty,
                abt::Type::Pointer(_) => break ty,
                abt::Type::Ref(inner) => {
                    ty = *inner;
                    deref_count += 1;
                }
                _ => return None,
            }
        };

        for _ in 0..deref_count {
            let inner = std::mem::replace(expr, abt::Expr::Unknown);
            *expr = abt::Expr::Deref(Box::new(inner));
        }

        Some(final_ty)
    }

    fn try_coerce_immediate_indexable(&self, expr: &mut abt::Expr) -> Option<abt::Type> {
        let mut ty = self.program.type_of(expr);
        let mut deref_count = 0;
        let final_ty = loop {
            match ty {
                abt::Type::Tuple(_, _) => break ty,
                abt::Type::Array(_, _) => break ty,
                abt::Type::Ref(inner) => {
                    ty = *inner;
                    deref_count += 1;
                }
                _ => return None,
            }
        };

        for _ in 0..deref_count {
            let inner = std::mem::replace(expr, abt::Expr::Unknown);
            *expr = abt::Expr::Deref(Box::new(inner));
        }

        Some(final_ty)
    }

    pub fn analyse_index_expression(
        &mut self,
        expr: &ast::Expr,
        index_expr: &ast::Expr,
        span: Span,
    ) -> abt::Expr {
        let mut bound_expr = self.analyse_expression(expr);
        let bound_index = self.analyse_expression(index_expr);

        let expr_ty = self.program.type_of(&bound_expr);
        let index_ty = self.program.type_of(&bound_index);

        if !index_ty.is(&abt::Type::I64) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::ArrayIndexMustBeInteger)
                .with_span(index_expr.span)
                .with_severity(Severity::Error)
                .annotate_primary(
                    Note::OfTypeButShouldBe(
                        self.program.type_repr(&index_ty),
                        self.program.type_repr(&abt::Type::I64),
                    ),
                    index_expr.span,
                )
                .done();
            self.diagnostics.push(d);
            return abt::Expr::Unknown;
        }

        if !expr_ty.is_known() {
            return abt::Expr::Unknown;
        }

        match self.try_coerce_indexable(&mut bound_expr) {
            Some(abt::Type::Array(_, size)) => {
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

                abt::Expr::ArrayIndex(Box::new(bound_expr), Box::new(bound_index))
            }
            Some(abt::Type::Pointer(_)) => {
                abt::Expr::PointerIndex(Box::new(bound_expr), Box::new(bound_index))
            }
            _ => {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::InvalidIndex)
                    .with_span(span)
                    .with_severity(Severity::Error)
                    .annotate_primary(Note::OfType(self.program.type_repr(&expr_ty)), expr.span)
                    .done();
                self.diagnostics.push(d);
                abt::Expr::Unknown
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
        let ty = self.program.type_of(&bound_expr);
        if !ty.is_known() {
            return abt::Expr::Unknown;
        }

        match self.try_coerce_immediate_indexable(&mut bound_expr) {
            Some(abt::Type::Tuple(_, tail)) => {
                self.analyse_tuple_immediate_index(expr, bound_expr, &tail, index, span)
            }
            Some(abt::Type::Array(_, size)) => {
                self.analyse_array_immediate_index(expr, bound_expr, index, size, span)
            }
            _ => {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::InvalidImmediateIndex)
                    .with_span(span)
                    .with_severity(Severity::Error)
                    .annotate_primary(Note::OfType(self.program.type_repr(&ty)), expr.span)
                    .done();
                self.diagnostics.push(d);
                abt::Expr::Unknown
            }
        }
    }
}
