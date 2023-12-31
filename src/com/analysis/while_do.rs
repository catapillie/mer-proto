use crate::com::{
    abt::{StmtAbtKind, TypeAbt},
    diagnostics::{self, DiagnosticKind, Note, Severity},
    syntax::{expr::ExprAst, stmt::StmtAst},
};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn analyse_while_do_statement(&mut self, guard: &ExprAst, body: &StmtAst) -> StmtAbtKind {
        let bound_guard = self.analyse_expression(guard);
        self.open_scope();
        let bound_body = self.analyse_statement(body);
        self.close_scope();

        if matches!(bound_body.kind, StmtAbtKind::Empty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::EmptyWhileDoStatement)
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

        StmtAbtKind::WhileDo(Box::new(bound_guard), Box::new(bound_body))
    }

    pub fn analyse_do_while_statement(&mut self, body: &StmtAst, guard: &ExprAst) -> StmtAbtKind {
        let bound_guard = self.analyse_expression(guard);

        self.open_scope();
        let bound_body = self.analyse_statement(body);
        self.close_scope();

        if matches!(bound_body.kind, StmtAbtKind::Empty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::EmptyDoWhileStatement)
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

        StmtAbtKind::DoWhile(Box::new(bound_body), Box::new(bound_guard))
    }

    pub fn analyse_do_statement(&mut self, body: &StmtAst) -> StmtAbtKind {
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

        StmtAbtKind::Empty
    }
}
