use crate::com::{
    abt::{ExprAbt, TypeAbt},
    diagnostics::{self, DiagnosticKind, Note, Severity},
    syntax::expr::{ExprAst, ExprAstKind},
};

use super::Analyser;

impl<'d> Analyser<'d> {
    #[rustfmt::skip]
    pub fn analyse_expression(&mut self, expr: &ExprAst) -> ExprAbt {
        match &expr.kind {
            ExprAstKind::Bad
                => ExprAbt::Unknown,
            ExprAstKind::Integer(num)
                => ExprAbt::Integer(*num),
            ExprAstKind::Decimal(num)
                => ExprAbt::Decimal(*num),
            ExprAstKind::Identifier(id)
                => self.analyse_variable_expression(id, expr.span),
            ExprAstKind::Boolean(b)
                => ExprAbt::Boolean(*b),
            ExprAstKind::Parenthesized(inner)
                => self.analyse_expression(inner),
            ExprAstKind::BinaryOp(op, left, right)
                => self.analyse_binary_operation(*op, left, right, expr.span),
            ExprAstKind::UnaryOp(op, operand)
                => self.analyse_unary_operation(*op, operand, expr.span),
            ExprAstKind::Call(callee, args)
                => self.analyse_call_expression(callee, args, expr.span),
            ExprAstKind::Debug(inner)
                => self.analyse_debug_expression(inner),
            ExprAstKind::Ref(expr)
                => self.analyse_reference_expression(expr),
            ExprAstKind::Deref(expr)
                => self.analyse_dereference_expression(expr),
            ExprAstKind::Todo
                => ExprAbt::Todo,
            ExprAstKind::Unreachable
                => ExprAbt::Unreachable,
        }
    }

    fn analyse_debug_expression(&mut self, expr: &ExprAst) -> ExprAbt {
        let inner = self.analyse_expression(expr);
        let ty = self.type_of(&inner);

        match ty {
            TypeAbt::U8
            | TypeAbt::U16
            | TypeAbt::U32
            | TypeAbt::U64
            | TypeAbt::I8
            | TypeAbt::I16
            | TypeAbt::I32
            | TypeAbt::I64
            | TypeAbt::F32
            | TypeAbt::F64
            | TypeAbt::Bool
            | TypeAbt::Unit => ExprAbt::Debug(Box::new(inner), ty),
            TypeAbt::Unknown => ExprAbt::Unknown,
            _ => {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::InvalidDebugExpression(ty.clone()))
                    .with_severity(Severity::Error)
                    .with_span(expr.span)
                    .annotate_primary(Note::OfType(ty), expr.span)
                    .done();
                self.diagnostics.push(d);
                ExprAbt::Unknown
            }
        }
    }
}
