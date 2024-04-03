use super::Analyser;
use crate::{
    com::{abt, ast},
    diagnostics::{self, DiagnosticKind, Note, Severity},
};

impl<'d> Analyser<'d> {
    pub fn analyse_while_do_statement(
        &mut self,
        guard: &ast::Expr,
        body: &ast::Stmt,
    ) -> abt::StmtKind {
        let bound_guard = self.analyse_expression(guard);
        self.open_scope();
        let bound_body = self.analyse_statement(body);
        self.close_scope();

        if matches!(bound_body.value, abt::StmtKind::Empty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::EmptyWhileDoStatement)
                .with_severity(Severity::Warning)
                .with_span(body.span)
                .annotate_primary(Note::CanBeRemoved, body.span)
                .done();
            self.diagnostics.push(d);
        }

        let guard_ty = &bound_guard.value.ty;
        if self.type_check(guard_ty, &abt::Type::Bool).is_err() {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::GuardNotBoolean)
                .with_severity(Severity::Error)
                .with_span(guard.span)
                .annotate_primary(
                    Note::OfTypeButShouldBe(
                        self.program.type_repr(guard_ty),
                        self.program.type_repr(&abt::Type::Bool),
                    ),
                    guard.span,
                )
                .done();
            self.diagnostics.push(d);
        }

        abt::StmtKind::WhileDo(Box::new(bound_guard), Box::new(bound_body))
    }

    pub fn analyse_do_while_statement(
        &mut self,
        body: &ast::Stmt,
        guard: &ast::Expr,
    ) -> abt::StmtKind {
        let bound_guard = self.analyse_expression(guard);

        self.open_scope();
        let bound_body = self.analyse_statement(body);
        self.close_scope();

        if matches!(bound_body.value, abt::StmtKind::Empty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::EmptyDoWhileStatement)
                .with_severity(Severity::Warning)
                .with_span(body.span)
                .annotate_primary(Note::CanBeRemoved, body.span)
                .done();
            self.diagnostics.push(d);
        }

        let guard_ty = &bound_guard.value.ty;
        if self.type_check(guard_ty, &abt::Type::Bool).is_err() {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::GuardNotBoolean)
                .with_severity(Severity::Error)
                .with_span(guard.span)
                .annotate_primary(
                    Note::OfTypeButShouldBe(
                        self.program.type_repr(guard_ty),
                        self.program.type_repr(&abt::Type::Bool),
                    ),
                    guard.span,
                )
                .done();
            self.diagnostics.push(d);
        }

        abt::StmtKind::DoWhile(Box::new(bound_body), Box::new(bound_guard))
    }

    pub fn analyse_do_statement(&mut self, body: &ast::Stmt) -> abt::StmtKind {
        self.open_scope();
        self.analyse_statement(body);
        self.close_scope();

        let d = diagnostics::create_diagnostic()
            .with_kind(DiagnosticKind::DoWithoutWhile)
            .with_severity(Severity::Error)
            .with_span(body.span)
            .annotate_primary(Note::MissingWhile, body.span)
            .done();
        self.diagnostics.push(d);

        abt::StmtKind::Empty
    }
}
