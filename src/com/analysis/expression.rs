use crate::{
    com::{
        abt::{ExprAbt, TypeAbt},
        ast,
    },
    diagnostics::{self, DiagnosticKind, Note, Severity},
};

use super::Analyser;

impl<'d> Analyser<'d> {
    #[rustfmt::skip]
    pub fn analyse_expression(&mut self, expr: &ast::Expr) -> ExprAbt {
        use ast::ExprKind as K;
        match &expr.value {
            K::Bad
                => ExprAbt::Unknown,
            K::Unit
                => ExprAbt::Unit,
            K::Integer(num)
                => ExprAbt::Integer(*num),
            K::Decimal(num)
                => ExprAbt::Decimal(*num),
            K::Identifier(id)
                => self.analyse_variable_expression(id, expr.span),
            K::Boolean(b)
                => ExprAbt::Boolean(*b),
            K::Parenthesized(inner)
                => self.analyse_expression(inner),
            K::Tuple(head, tail)
                => self.analyse_tuple_expression(head, tail),
            K::Array(exprs)
                => self.analyse_array_expression(exprs, expr.span),
            K::ImmediateIndex(inner, index)
                => self.analyse_immediate_index(inner, *index, expr.span),
            K::Index(inner, index_expr)
                => self.analyse_index_expression(inner, index_expr, expr.span),
            K::BinaryOp(op, left, right)
                => self.analyse_binary_operation(*op, left, right, expr.span),
            K::UnaryOp(op, operand)
                => self.analyse_unary_operation(*op, operand, expr.span),
            K::Call(callee, args)
                => self.analyse_call_expression(callee, args, expr.span),
            K::Debug(inner)
                => self.analyse_debug_expression(inner),
            K::Ref(expr)
                => self.analyse_reference_expression(expr),
            K::Deref(expr)
                => self.analyse_dereference_expression(expr),
            K::Todo
                => ExprAbt::Todo,
            K::Unreachable
                => ExprAbt::Unreachable,
            K::Case(paths, span)
                => self.analyse_case_expression(paths, *span),
        }
    }

    fn analyse_debug_expression(&mut self, expr: &ast::Expr) -> ExprAbt {
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
