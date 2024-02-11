use super::Analyser;
use crate::{
    com::{abt, ast, Type},
    diagnostics::{self, DiagnosticKind, Note, NoteSeverity, Severity},
    utils::Span,
};

impl<'d> Analyser<'d> {
    pub fn analyse_case_expression(
        &mut self,
        paths: &[(Option<ast::Expr>, ast::Expr)],
        span: Span,
    ) -> abt::Expr {
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

            let guard_ty = self.type_of(bound_guard);
            if !guard_ty.is(&Type::Bool) {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::GuardNotBoolean)
                    .with_span(guard.span)
                    .with_severity(Severity::Error)
                    .annotate_primary(Note::MustBeOfType(Type::Bool), guard.span)
                    .done();
                self.diagnostics.push(d);
                return abt::Expr::Unknown;
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
            return abt::Expr::Unknown;
        } else if otherwise_count > 1 {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::TooManyOtherwisePaths)
                .with_span(span)
                .with_severity(Severity::Error)
                .annotate_primary(Note::Quiet, span)
                .done();
            self.diagnostics.push(d);
            return abt::Expr::Unknown;
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
            return abt::Expr::Unknown;
        }

        // ensure all paths give the same type
        let types = bound_paths
            .iter()
            .map(|(_, expr)| self.type_of(expr))
            .collect::<Vec<_>>();
        let ty = types
            .iter()
            .find(|ty| !matches!(ty, Type::Never))
            .unwrap_or(
                types
                    .first()
                    .expect("empty case expressions are not handled properly"),
            );
        let all_types_match = types.iter().all(|t| t.is(ty));
        if !all_types_match {
            let mut d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::CasePathsTypeMismatch)
                .with_span(span)
                .with_severity(Severity::Error)
                .annotate_primary(Note::Quiet, span);
            for ((_, bound_expr), (_, expr)) in bound_paths.iter().zip(paths.iter()) {
                d = d.annotate_secondary(
                    Note::Type(self.type_of(bound_expr)),
                    expr.span,
                    NoteSeverity::Annotation,
                );
            }
            self.diagnostics.push(d.done());
            return abt::Expr::Unknown;
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
        // TODO: warning for always-matching case
        // TODO: warning for never-matching case

        let last_expr = bound_paths.pop().unwrap().1;
        let bound_paths = bound_paths
            .into_iter()
            .map(|(guard, expr)| (guard.unwrap(), expr))
            .collect();
        abt::Expr::Case(bound_paths, Box::new(last_expr), ty.clone())
    }
}
