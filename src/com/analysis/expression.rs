use crate::{
    com::{abt, ast},
    diagnostics::{self, DiagnosticKind, Note, Severity},
};

use super::Analyser;

impl<'d> Analyser<'d> {
    #[rustfmt::skip]
    pub fn analyse_expression(&mut self, expr: &ast::Expr) -> abt::Expr {
        use ast::ExprKind as K;
        match &expr.value {
            K::Bad
                => abt::Expr::Unknown,
            K::Unit
                => abt::Expr::Unit,
            K::Integer(num)
                => abt::Expr::Integer(*num),
            K::Decimal(num)
                => abt::Expr::Decimal(*num),
            K::Identifier(id)
                => self.analyse_variable_expression(id, expr.span),
            K::Boolean(b)
                => abt::Expr::Boolean(*b),
            K::StringLiteral(s)
                => abt::Expr::StringLiteral(s.clone()),
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
                => abt::Expr::Todo,
            K::Unreachable
                => abt::Expr::Unreachable,
            K::TernaryCase(guard, expr, fallback, span)
                => self.analyse_ternary_case_expression(guard, expr, fallback, *span),
            K::Case(paths, span)
                => self.analyse_case_expression(paths, *span),
            K::DataInit(name, fields)
                => self.analyse_data_init_expression(name, fields),
            K::FieldAccess(expr, name)
                => self.analyse_data_field_access(expr, name),
            K::Alloc(ty, size)
                => self.analyse_alloc_expression(ty, size),
        }
    }

    fn analyse_debug_expression(&mut self, expr: &ast::Expr) -> abt::Expr {
        let inner = self.analyse_expression(expr);
        let ty = self.program.type_of(&inner);

        use abt::Type as Ty;
        match ty {
            Ty::U8
            | Ty::U16
            | Ty::U32
            | Ty::U64
            | Ty::I8
            | Ty::I16
            | Ty::I32
            | Ty::I64
            | Ty::F32
            | Ty::F64
            | Ty::Bool
            | Ty::Unit => abt::Expr::Debug(Box::new(inner), ty),
            Ty::Unknown => abt::Expr::Unknown,
            _ => {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::InvalidDebugExpression(
                        self.program.type_repr(&ty),
                    ))
                    .with_severity(Severity::Error)
                    .with_span(expr.span)
                    .annotate_primary(Note::OfType(self.program.type_repr(&ty)), expr.span)
                    .done();
                self.diagnostics.push(d);
                abt::Expr::Unknown
            }
        }
    }
}
