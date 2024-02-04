use crate::{
    com::abt::{ExprAbt, StmtAbt, StmtAbtKind},
    diagnostics::{self, DiagnosticKind, Note, Severity},
    utils::Span,
};

use super::Analyser;

impl<'d> Analyser<'d> {
    // returns whether the provided statement is guaranteed to return
    pub fn analyse_control_flow(&mut self, stmt: &StmtAbt) -> bool {
        use StmtAbtKind as S;
        match &stmt.kind {
            S::Block(stmts) => {
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
            S::Empty => false,
            S::Expr(expr) => Self::is_never(expr),
            S::VarInit(_, expr) => Self::is_never(expr),
            S::IfThen(guard, body) => {
                self.analyse_control_flow(body); // analyse but discard
                Self::is_never(guard) // only the guard determines the termination
            }
            S::IfThenElse(guard, body_then, body_else) => {
                (self.analyse_control_flow(body_then.as_ref())
                    & self.analyse_control_flow(body_else.as_ref()))
                    | Self::is_never(guard)
            }
            S::WhileDo(guard, body) => {
                self.analyse_control_flow(body.as_ref()) | Self::is_never(guard)
            }
            S::DoWhile(body, guard) => {
                self.analyse_control_flow(body.as_ref()) | Self::is_never(guard)
            }
            S::Return(_) => true,
        }
    }

    fn is_never(expr: &ExprAbt) -> bool {
        use ExprAbt as E;
        match expr {
            E::Unknown => false,
            E::Unit => false,
            E::Integer(_) => false,
            E::Decimal(_) => false,
            E::Boolean(_) => false,
            E::Variable(_) => false,
            E::Function(_) => false,
            E::Tuple(head, tail) => Self::is_never(head) || tail.iter().any(Self::is_never),
            E::TupleImmediateIndex(tuple, _) => Self::is_never(tuple),
            E::Array(exprs) => exprs.iter().any(Self::is_never),
            E::ArrayImmediateIndex(array, _) => Self::is_never(array),
            E::ArrayIndex(array, index) => Self::is_never(array) || Self::is_never(index),
            E::Assignment {
                assignee: _,
                var_id: _,
                expr,
            } => Self::is_never(expr),
            E::Binary(_, left, right) => Self::is_never(left) || Self::is_never(right),
            E::Unary(_, expr) => Self::is_never(expr),
            E::Call(_, args, _) => args.iter().any(Self::is_never),
            E::IndirectCall(callee, args, _) => {
                Self::is_never(callee) && args.iter().any(Self::is_never)
            }
            E::Debug(expr, _) => Self::is_never(expr),
            E::Ref(expr) => Self::is_never(expr),
            E::VarRef(_) => false,
            E::Deref(expr) => Self::is_never(expr),
            E::VarDeref(_) => false,
            E::Todo => true,
            E::Unreachable => true,
        }
    }
}
