use crate::{
    com::{
        abt::{StmtAbtKind, TypeAbt},
        syntax::{expr::ExprAst, stmt::StmtAst},
    },
    diagnostics::{self, DiagnosticKind, Note, Severity},
};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn analyse_if_then_statement(&mut self, guard: &ExprAst, body: &StmtAst) -> StmtAbtKind {
        let bound_guard = self.analyse_expression(guard);

        self.open_scope();
        let bound_body = self.analyse_statement(body);
        self.close_scope();

        if matches!(bound_body.kind, StmtAbtKind::Empty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::EmptyThenStatement)
                .with_severity(Severity::Warning)
                .with_span(body.span)
                .annotate_primary(Note::CanBeRemoved, body.span)
                .done();
            self.diagnostics.push(d);
        }

        if !self.type_of(&bound_guard).is(&TypeAbt::Bool) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::GuardNotBoolean)
                .with_severity(Severity::Error)
                .with_span(guard.span)
                .annotate_primary(Note::MustBeOfType(TypeAbt::Bool), guard.span)
                .done();
            self.diagnostics.push(d);
        }

        StmtAbtKind::IfThen(Box::new(bound_guard), Box::new(bound_body))
    }

    pub fn analyse_if_then_else_statement(
        &mut self,
        guard: &ExprAst,
        body_then: &StmtAst,
        body_else: &StmtAst,
    ) -> StmtAbtKind {
        let bound_guard = self.analyse_expression(guard);

        self.open_scope();
        let bound_body_then = self.analyse_statement(body_then);
        self.close_scope();

        self.open_scope();
        let bound_body_else = self.analyse_statement(body_else);
        self.close_scope();

        if matches!(bound_body_then.kind, StmtAbtKind::Empty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::EmptyThenStatement)
                .with_severity(Severity::Warning)
                .with_span(body_then.span)
                .annotate_primary(Note::CanBeRemoved, body_then.span)
                .done();
            self.diagnostics.push(d);
        }

        if matches!(bound_body_else.kind, StmtAbtKind::Empty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::EmptyElseStatement)
                .with_severity(Severity::Warning)
                .with_span(body_else.span)
                .annotate_primary(Note::CanBeRemoved, body_else.span)
                .done();
            self.diagnostics.push(d);
        }

        if !self.type_of(&bound_guard).is(&TypeAbt::Bool) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::GuardNotBoolean)
                .with_severity(Severity::Error)
                .with_span(guard.span)
                .annotate_primary(Note::MustBeOfType(TypeAbt::Bool), guard.span)
                .done();
            self.diagnostics.push(d);
        }

        StmtAbtKind::IfThenElse(
            Box::new(bound_guard),
            Box::new(bound_body_then),
            Box::new(bound_body_else),
        )
    }

    pub fn analyse_then_statement(&mut self, body: &StmtAst) -> StmtAbtKind {
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

        StmtAbtKind::Empty
    }

    pub fn analyse_else_statement(&mut self, body: &StmtAst) -> StmtAbtKind {
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

        StmtAbtKind::Empty
    }
}
