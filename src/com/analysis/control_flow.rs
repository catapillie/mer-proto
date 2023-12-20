use crate::com::{
    abt::{StmtAbt, StmtAbtKind},
    diagnostics::{self, DiagnosticKind, Severity},
    span::Span,
};

use super::Analyser;

impl<'d> Analyser<'d> {
    // returns whether the provided statement is guaranteed to return
    pub fn analyse_control_flow(&mut self, stmt: &StmtAbt) -> bool {
        match &stmt.kind {
            StmtAbtKind::Block(stmts) => {
                let mut does_return = false;

                let mut iter = stmts.iter();
                for stmt in iter.by_ref() {
                    if self.analyse_control_flow(stmt) {
                        does_return = true;
                        break;
                    }
                }

                let remaining = iter.collect::<Vec<_>>();
                if let (Some(first), Some(last)) = (remaining.first(), remaining.last()) {
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::UnreachableCode)
                        .with_severity(Severity::Warning)
                        .with_span(Span::join(first.span, last.span))
                        .done();
                    self.diagnostics.push(d);
                }

                does_return
            }
            StmtAbtKind::Empty => false,
            StmtAbtKind::Expr(_) => false,
            StmtAbtKind::IfThen(_, _) => false,
            StmtAbtKind::IfThenElse(_, body_then, body_else) => {
                self.analyse_control_flow(body_then.as_ref())
                    && self.analyse_control_flow(body_else.as_ref())
            }
            StmtAbtKind::WhileDo(_, body) => self.analyse_control_flow(body.as_ref()),
            StmtAbtKind::DoWhile(body, _) => self.analyse_control_flow(body.as_ref()),
            StmtAbtKind::Return(_) => true,
        }
    }
}
