use crate::{
    com::{abt, ast},
    diagnostics::{self, DiagnosticKind, Note, Severity},
};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn analyse_if_then_statement(
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
                .with_kind(DiagnosticKind::EmptyThenStatement)
                .with_severity(Severity::Warning)
                .with_span(body.span)
                .annotate_primary(Note::CanBeRemoved, body.span)
                .done();
            self.diagnostics.push(d);
        }

        if !self.type_of(&bound_guard).is(&abt::Type::Bool) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::GuardNotBoolean)
                .with_severity(Severity::Error)
                .with_span(guard.span)
                .annotate_primary(Note::MustBeOfType(abt::Type::Bool), guard.span)
                .done();
            self.diagnostics.push(d);
        }

        abt::StmtKind::IfThen(Box::new(bound_guard), Box::new(bound_body))
    }

    pub fn analyse_if_then_else_statement(
        &mut self,
        guard: &ast::Expr,
        body_then: &ast::Stmt,
        body_else: &ast::Stmt,
    ) -> abt::StmtKind {
        let bound_guard = self.analyse_expression(guard);

        self.open_scope();
        let bound_body_then = self.analyse_statement(body_then);
        self.close_scope();

        self.open_scope();
        let bound_body_else = self.analyse_statement(body_else);
        self.close_scope();

        if matches!(bound_body_then.value, abt::StmtKind::Empty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::EmptyThenStatement)
                .with_severity(Severity::Warning)
                .with_span(body_then.span)
                .annotate_primary(Note::CanBeRemoved, body_then.span)
                .done();
            self.diagnostics.push(d);
        }

        if matches!(bound_body_else.value, abt::StmtKind::Empty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::EmptyElseStatement)
                .with_severity(Severity::Warning)
                .with_span(body_else.span)
                .annotate_primary(Note::CanBeRemoved, body_else.span)
                .done();
            self.diagnostics.push(d);
        }

        if !self.type_of(&bound_guard).is(&abt::Type::Bool) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::GuardNotBoolean)
                .with_severity(Severity::Error)
                .with_span(guard.span)
                .annotate_primary(Note::MustBeOfType(abt::Type::Bool), guard.span)
                .done();
            self.diagnostics.push(d);
        }

        abt::StmtKind::IfThenElse(
            Box::new(bound_guard),
            Box::new(bound_body_then),
            Box::new(bound_body_else),
        )
    }

    pub fn analyse_then_statement(&mut self, body: &ast::Stmt) -> abt::StmtKind {
        self.open_scope();
        self.analyse_statement(body);
        self.close_scope();

        let d = diagnostics::create_diagnostic()
            .with_kind(DiagnosticKind::ThenWithoutIf)
            .with_severity(Severity::Error)
            .with_span(body.span)
            .annotate_primary(Note::FollowsIf, body.span)
            .done();
        self.diagnostics.push(d);

        abt::StmtKind::Empty
    }

    pub fn analyse_else_statement(&mut self, body: &ast::Stmt) -> abt::StmtKind {
        self.open_scope();
        self.analyse_statement(body);
        self.close_scope();

        let d = diagnostics::create_diagnostic()
            .with_kind(DiagnosticKind::ElseWithoutIfThen)
            .with_severity(Severity::Error)
            .with_span(body.span)
            .annotate_primary(Note::FollowsIfThen, body.span)
            .done();
        self.diagnostics.push(d);

        abt::StmtKind::Empty
    }
}
