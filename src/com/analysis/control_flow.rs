use crate::com::{
    abt::{ExprAbt, StmtAbt, StmtAbtKind},
    diagnostics::{self, DiagnosticKind, Note, Severity},
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
                    let span = Span::join(first.span, last.span);
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::UnreachableCode)
                        .with_severity(Severity::Warning)
                        .with_span(span)
                        .annotate_primary(Note::CanBeRemoved, span)
                        .done();
                    self.diagnostics.push(d);
                }

                does_return
            }
            StmtAbtKind::Empty => false,
            StmtAbtKind::Expr(expr) => self.expression_terminates(expr),
            StmtAbtKind::VarInit(_, expr) => self.expression_terminates(expr),
            StmtAbtKind::IfThen(guard, _) => self.expression_terminates(guard),
            StmtAbtKind::IfThenElse(guard, body_then, body_else) => {
                (self.analyse_control_flow(body_then.as_ref())
                    & self.analyse_control_flow(body_else.as_ref()))
                    | self.expression_terminates(guard)
            }
            StmtAbtKind::WhileDo(guard, body) => {
                self.analyse_control_flow(body.as_ref()) | self.expression_terminates(guard)
            }
            StmtAbtKind::DoWhile(body, guard) => {
                self.analyse_control_flow(body.as_ref()) | self.expression_terminates(guard)
            }
            StmtAbtKind::Return(_) => true,
        }
    }

    fn expression_terminates(&mut self, expr: &ExprAbt) -> bool {
        match expr {
            ExprAbt::Unknown => false,
            ExprAbt::Unit => false,
            ExprAbt::Integer(_) => false,
            ExprAbt::Decimal(_) => false,
            ExprAbt::Boolean(_) => false,
            ExprAbt::Variable(_) => false,
            ExprAbt::Assignment {
                var_id: _,
                deref_count: _,
                expr,
            } => self.expression_terminates(expr),
            ExprAbt::Binary(_, left, right) => {
                self.expression_terminates(left) || self.expression_terminates(right)
            }
            ExprAbt::Unary(_, expr) => self.expression_terminates(expr),
            ExprAbt::Call(_, args, _) => args.iter().any(|expr| self.expression_terminates(expr)),
            ExprAbt::Debug(expr, _) => self.expression_terminates(expr),
            ExprAbt::Ref(expr) => self.expression_terminates(expr),
            ExprAbt::VarRef(_) => todo!(),
            ExprAbt::Deref(expr) => self.expression_terminates(expr),
            ExprAbt::Todo => true,
            ExprAbt::Unreachable => true,
        }
    }
}
