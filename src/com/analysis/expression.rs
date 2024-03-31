use super::Analyser;
use crate::{
    com::{abt, ast},
    diagnostics::{self, DiagnosticKind, Note, Severity},
};

impl<'d> Analyser<'d> {
    pub fn analyse_expression(&mut self, expr: &ast::Expr) -> abt::Expr {
        use ast::ExprKind as K;
        match &expr.value {
            K::Bad => abt::Expr::unknown(),
            K::Unit => abt::Expr {
                kind: abt::ExprKind::Unit,
                ty: abt::Type::Unit,
            },
            K::Integer(num) => abt::Expr {
                kind: abt::ExprKind::Integer(*num),
                ty: abt::Type::I64,
            },
            K::Decimal(num) => abt::Expr {
                kind: abt::ExprKind::Decimal(*num),
                ty: abt::Type::F64,
            },
            K::Identifier(id) => self.analyse_variable_expression(id, expr.span),
            K::Boolean(b) => abt::Expr {
                kind: abt::ExprKind::Boolean(*b),
                ty: abt::Type::Bool,
            },
            K::StringLiteral(s) => abt::Expr {
                kind: abt::ExprKind::StringLiteral(s.clone()),
                ty: abt::Type::Array(Box::new(abt::Type::U8), s.len()),
            },
            K::Parenthesized(inner) => self.analyse_expression(inner),
            K::Tuple(head, tail) => self.analyse_tuple_expression(head, tail),
            K::Array(exprs) => self.analyse_array_expression(exprs, expr.span),
            K::ImmediateIndex(inner, index) => {
                self.analyse_immediate_index(inner, *index, expr.span)
            }
            K::Index(inner, index_expr) => {
                self.analyse_index_expression(inner, index_expr, expr.span)
            }
            K::BinaryOp(op, left, right) => {
                self.analyse_binary_operation(*op, left, right, expr.span)
            }
            K::UnaryOp(op, operand) => self.analyse_unary_operation(*op, operand, expr.span),
            K::Call(callee, args) => self.analyse_call_expression(callee, args, expr.span),
            K::Debug(inner) => self.analyse_debug_expression(inner),
            K::Ref(expr) => self.analyse_reference_expression(expr),
            K::Deref(expr) => self.analyse_dereference_expression(expr),
            K::Todo => abt::Expr {
                kind: abt::ExprKind::Todo,
                ty: abt::Type::Never,
            },
            K::Unreachable => abt::Expr {
                kind: abt::ExprKind::Unreachable,
                ty: abt::Type::Never,
            },
            K::TernaryCase(guard, expr, fallback, span) => {
                self.analyse_ternary_case_expression(guard, expr, fallback, *span)
            }
            K::Case(paths, span) => self.analyse_case_expression(paths, *span),
            K::DataInit(name, fields) => self.analyse_data_init_expression(name, fields),
            K::DataWith(inner, fields) => {
                self.analyse_data_with_expression(inner, fields, expr.span)
            }
            K::FieldAccess(expr, name) => self.analyse_data_field_access(expr, name),
            K::Alloc(ty, size) => self.analyse_alloc_expression(ty, size),
        }
    }

    fn analyse_debug_expression(&mut self, expr: &ast::Expr) -> abt::Expr {
        let inner = self.analyse_expression(expr);
        let ty = inner.ty.clone();

        use abt::Type as Ty;
        match self.program.dealias_type(&ty) {
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
            | Ty::Unit => abt::Expr {
                kind: abt::ExprKind::Debug(Box::new(inner)),
                ty,
            },
            Ty::Unknown => abt::Expr::unknown(),
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
                abt::Expr::unknown()
            }
        }
    }
}
