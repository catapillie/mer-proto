use super::Analyser;
use crate::{
    com::{abt, ast},
    diagnostics::{self, DiagnosticKind, Note, Severity},
    utils::Span,
};

impl<'d> Analyser<'d> {
    pub fn analyse_array_expression(&mut self, exprs: &[ast::Expr], span: Span) -> abt::TypedExpr {
        if exprs.is_empty() {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::EmptyArray)
                .with_span(span)
                .with_severity(Severity::Error)
                .annotate_primary(Note::Here, span)
                .done();
            self.diagnostics.push(d);
            return abt::TypedExpr::unknown();
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
            .map(|expr| expr.value.ty.clone())
            .collect::<Vec<_>>();
        let first_ty = tys
            .iter()
            .find(|ty| !matches!(ty, abt::Type::Never))
            .unwrap_or(tys.first().expect("empty arrays are not handled properly"));

        let all_types_match = tys.iter().all(|ty| self.type_check(ty, first_ty).is_ok());
        if !all_types_match {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::ArrayMismatchingTypes)
                .with_span(span)
                .with_severity(Severity::Error)
                .annotate_primary(Note::Here, span)
                .done();
            self.diagnostics.push(d);
            return abt::TypedExpr::unknown();
        }

        let len = bound_exprs.len();
        abt::TypedExpr {
            kind: abt::ExprKind::Array(bound_exprs),
            ty: abt::Type::Array(Box::new(first_ty.clone()), len),
        }
    }

    pub fn analyse_array_immediate_index(
        &mut self,
        expr: &ast::Expr,
        bound_expr: abt::Expr,
        index: u64,
        array_ty: &abt::Type,
        size: usize,
        span: Span,
    ) -> abt::TypedExpr {
        if index as usize >= size {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::OutOfRangeArrayIndex {
                    len: size,
                    index: index as usize,
                })
                .with_span(span)
                .with_severity(Severity::Error)
                .annotate_primary(Note::ArrayLength(size), expr.span)
                .done();
            self.diagnostics.push(d);
            return abt::TypedExpr::unknown();
        }

        abt::TypedExpr {
            kind: abt::ExprKind::ArrayImmediateIndex(Box::new(bound_expr), index as usize),
            ty: array_ty.clone(),
        }
    }

    pub fn analyse_alloc_expression(&mut self, ty: &ast::Type, size: &ast::Expr) -> abt::TypedExpr {
        let bound_ty = self.analyse_type(ty);
        let bound_size = self.analyse_expression(size);
        let size_ty = &bound_size.value.ty;
        if self.type_check(size_ty, &abt::Type::I64).is_err() {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::NonIntegerSize)
                .with_severity(Severity::Error)
                .with_span(size.span)
                .annotate_primary(
                    Note::OfTypeButShouldBe(
                        self.program.type_repr(size_ty),
                        self.program.type_repr(&abt::Type::I64),
                    ),
                    size.span,
                )
                .done();
            self.diagnostics.push(d);
            return abt::TypedExpr::unknown();
        }

        abt::TypedExpr {
            kind: abt::ExprKind::Alloc(Box::new(bound_ty.clone()), Box::new(bound_size)),
            ty: abt::Type::Pointer(Box::new(bound_ty)),
        }
    }
}
