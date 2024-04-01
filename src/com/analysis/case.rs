use super::Analyser;
use crate::{
    com::{abt, ast},
    diagnostics::{self, DiagnosticKind, Note, NoteSeverity, Severity},
    utils::Span,
};

impl<'d> Analyser<'d> {
    pub fn analyse_ternary_case_expression(
        &mut self,
        guard: &ast::Expr,
        expr: &ast::Expr,
        fallback: &ast::Expr,
        span: Span,
    ) -> abt::TypedExpr {
        let bound_guard = self.analyse_expression(guard);
        let bound_expr = self.analyse_expression(expr);
        let bound_fallback = self.analyse_expression(fallback);

        let guard_ty = &bound_guard.value.ty;
        if !self.type_check(guard_ty, &abt::Type::Bool) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::GuardNotBoolean)
                .with_span(guard.span)
                .with_severity(Severity::Error)
                .annotate_primary(
                    Note::OfTypeButShouldBe(
                        self.program.type_repr(guard_ty),
                        self.program.type_repr(&abt::Type::Bool),
                    ),
                    guard.span,
                )
                .done();
            self.diagnostics.push(d);
            return abt::TypedExpr::unknown();
        }

        let expr_ty = &bound_expr.value.ty;
        let fallback_ty = &bound_fallback.value.ty;

        if !expr_ty.is_known() || !fallback_ty.is_known() {
            return abt::TypedExpr::unknown();
        }

        let ty = if expr_ty == &abt::Type::Never && fallback_ty == &abt::Type::Never {
            abt::Type::Never
        } else if expr_ty == &abt::Type::Never {
            fallback_ty.clone()
        } else if fallback_ty == &abt::Type::Never || self.type_check(expr_ty, fallback_ty) {
            expr_ty.clone()
        } else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::CasePathsTypeMismatch)
                .with_span(span)
                .with_severity(Severity::Error)
                .annotate_primary(Note::Quiet, span)
                .annotate_secondary(
                    Note::Type(self.program.type_repr(expr_ty)),
                    expr.span,
                    NoteSeverity::Annotation,
                )
                .annotate_secondary(
                    Note::Type(self.program.type_repr(fallback_ty)),
                    fallback.span,
                    NoteSeverity::Annotation,
                )
                .done();
            self.diagnostics.push(d);
            return abt::TypedExpr::unknown();
        };

        // TODO: warning for always-matching case
        // TODO: warning for never-matching case

        abt::TypedExpr {
            kind: abt::ExprKind::CaseTernary(
                Box::new(bound_guard),
                Box::new(bound_expr),
                Box::new(bound_fallback),
            ),
            ty,
        }
    }

    pub fn analyse_case_expression(
        &mut self,
        paths: &[(Option<ast::Expr>, ast::Expr)],
        span: Span,
    ) -> abt::TypedExpr {
        let mut bound_paths = paths
            .iter()
            .map(|(guard, expr)| {
                (
                    guard.as_ref().map(|g| self.analyse_expression(g)),
                    self.analyse_expression(expr),
                )
            })
            .collect::<Vec<_>>();

        // check that all guards are booleans
        for ((bound_guard, _), (guard, _)) in bound_paths.iter().zip(paths.iter()) {
            let (Some(bound_guard), Some(guard)) = (bound_guard, guard) else {
                continue;
            };

            let guard_ty = &bound_guard.value.ty;
            if !self.type_check(guard_ty, &abt::Type::Bool) {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::GuardNotBoolean)
                    .with_span(guard.span)
                    .with_severity(Severity::Error)
                    .annotate_primary(
                        Note::OfTypeButShouldBe(
                            self.program.type_repr(guard_ty),
                            self.program.type_repr(&abt::Type::Bool),
                        ),
                        guard.span,
                    )
                    .done();
                self.diagnostics.push(d);
                return abt::TypedExpr::unknown();
            }
        }

        // count 'otherwise' paths -- ensures case is non-empty
        let otherwise_count = bound_paths.iter().filter(|(g, _)| g.is_none()).count();
        if otherwise_count == 0 {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::MissingOtherwisePath)
                .with_span(span)
                .with_severity(Severity::Error)
                .annotate_primary(Note::Quiet, span)
                .done();
            self.diagnostics.push(d);
            return abt::TypedExpr::unknown();
        }
        if otherwise_count > 1 {
            let mut d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::TooManyOtherwisePaths)
                .with_span(span)
                .with_severity(Severity::Error)
                .annotate_primary(Note::MoreThanOneOtherwisePath, span);
            for span in paths
                .iter()
                .filter(|(g, _)| g.is_none())
                .map(|(_, e)| e.span)
            {
                d = d.annotate_secondary(Note::Here, span, NoteSeverity::Annotation);
            }
            self.diagnostics.push(d.done());
            return abt::TypedExpr::unknown();
        }

        // ensure last case is otherwise path
        let last_guard = &bound_paths.last().unwrap().0;
        if last_guard.is_some() {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::LastCasePathIsNotOtherwise)
                .with_span(span)
                .with_severity(Severity::Error)
                .annotate_primary(Note::Quiet, span)
                .done();
            self.diagnostics.push(d);
            return abt::TypedExpr::unknown();
        }

        // ensure all paths give the same type
        let types = bound_paths
            .iter()
            .map(|(_, expr)| expr.value.ty.clone())
            .collect::<Vec<_>>();
        let ty = types
            .iter()
            .find(|ty| !matches!(ty, abt::Type::Never))
            .unwrap_or(
                types
                    .first()
                    .expect("empty case expressions are not handled properly"),
            );
        let all_types_match = types.iter().all(|t| self.type_check(t, ty));
        if !all_types_match {
            let mut d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::CasePathsTypeMismatch)
                .with_span(span)
                .with_severity(Severity::Error)
                .annotate_primary(Note::Quiet, span);
            for ((_, bound_expr), (_, expr)) in bound_paths.iter().zip(paths.iter()) {
                d = d.annotate_secondary(
                    Note::Type(self.program.type_repr(&bound_expr.value.ty)),
                    expr.span,
                    NoteSeverity::Annotation,
                );
            }
            self.diagnostics.push(d.done());
            return abt::TypedExpr::unknown();
        }

        if bound_paths.len() == 1 {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::CaseOtherwiseCanBeSimplified)
                .with_span(span)
                .with_severity(Severity::Warning)
                .annotate_primary(Note::Here, span)
                .done();
            self.diagnostics.push(d);
        }

        if bound_paths.len() == 2 {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::CaseThenOtherwiseCanBeSimplified)
                .with_span(span)
                .with_severity(Severity::Warning)
                .annotate_primary(Note::Here, span)
                .done();
            self.diagnostics.push(d);
        }

        // TODO: warning for always-matching case
        // TODO: warning for never-matching case

        let last_expr = bound_paths.pop().unwrap().1;
        let bound_paths = bound_paths
            .into_iter()
            .map(|(guard, expr)| (guard.unwrap(), expr))
            .collect();
        abt::TypedExpr {
            kind: abt::ExprKind::Case(bound_paths, Box::new(last_expr)),
            ty: ty.clone(),
        }
    }
}
