use super::Analyser;
use crate::{
    com::{
        abt::{self},
        ast,
    },
    diagnostics::{self, DiagnosticKind, Note, NoteSeverity, Severity},
    utils::Span,
};

impl<'d> Analyser<'d> {
    pub fn analyse_binary_operation(
        &mut self,
        op: ast::BinOp,
        left: &ast::Expr,
        right: &ast::Expr,
        span: Span,
    ) -> abt::TypedExpr {
        if matches!(op, ast::BinOp::Assign) {
            return self.analyse_assignment(left, right);
        }

        let bound_left = self.analyse_expression(left);
        let bound_right = self.analyse_expression(right);

        let ty_left = &bound_left.value.ty;
        let ty_right = &bound_right.value.ty;

        if !ty_left.is_known() || !ty_right.is_known() {
            return abt::TypedExpr::unknown();
        }

        if matches!(op, ast::BinOp::Concat) {
            use abt::Type::Array as Arr;
            use abt::Type::Tuple as Tup;
            if let (Arr(arr_left, size_left), Arr(arr_right, size_right)) = (&ty_left, &ty_right) {
                if arr_left != arr_right {
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::InvalidArrayConcatenation {
                            inner_left: self.program.type_repr(arr_left),
                            inner_right: self.program.type_repr(arr_right),
                        })
                        .with_severity(Severity::Error)
                        .with_span(span)
                        .annotate_primary(
                            Note::InnerTypesMismatch {
                                inner_left: self.program.type_repr(arr_left),
                                inner_right: self.program.type_repr(arr_right),
                            },
                            span,
                        )
                        .done();
                    self.diagnostics.push(d);
                    return abt::TypedExpr::unknown();
                }

                let out_ty = abt::Type::Array(arr_left.clone(), size_left + size_right);
                let bound_op = abt::BinOpKind::Concat.wrap_extern(
                    ty_left.clone(),
                    ty_right.clone(),
                    out_ty.clone(),
                );
                return abt::TypedExpr {
                    kind: abt::ExprKind::Binary(
                        bound_op,
                        Box::new(bound_left),
                        Box::new(bound_right),
                    ),
                    ty: out_ty,
                };
            }

            if let (Tup(hd_left, tl_left), Tup(hd_right, tl_right)) = (&ty_left, &ty_right) {
                let head = (*hd_left).clone();
                let tail = [&**tl_left, &[(**hd_right).clone()], &**tl_right]
                    .concat()
                    .into_boxed_slice();
                let out_ty = abt::Type::Tuple(head, tail);
                let bound_op = abt::BinOpKind::Concat.wrap_extern(
                    ty_left.clone(),
                    ty_right.clone(),
                    out_ty.clone(),
                );
                return abt::TypedExpr {
                    kind: abt::ExprKind::Binary(
                        bound_op,
                        Box::new(bound_left),
                        Box::new(bound_right),
                    ),
                    ty: out_ty,
                };
            }
        }

        if self.type_check(ty_left, ty_right) && self.type_check(ty_right, ty_left) {
            use abt::Type as Ty;
            let ty = self.program.dealias_type(ty_left).clone();
            let bound_op = match ty {
                Ty::U8 => Self::integer_binary_operation(op, ty),
                Ty::U16 => Self::integer_binary_operation(op, ty),
                Ty::U32 => Self::integer_binary_operation(op, ty),
                Ty::U64 => Self::integer_binary_operation(op, ty),
                Ty::I8 => Self::integer_binary_operation(op, ty),
                Ty::I16 => Self::integer_binary_operation(op, ty),
                Ty::I32 => Self::integer_binary_operation(op, ty),
                Ty::I64 => Self::integer_binary_operation(op, ty),
                Ty::F32 => Self::decimal_binary_operation(op, ty),
                Ty::F64 => Self::decimal_binary_operation(op, ty),
                Ty::Bool => Self::boolean_binary_operation(op),
                _ => None,
            };

            if let Some(op) = bound_op {
                let ty = op.out_ty.clone();
                return abt::TypedExpr {
                    kind: abt::ExprKind::Binary(op, Box::new(bound_left), Box::new(bound_right)),
                    ty,
                };
            }
        }

        let d = diagnostics::create_diagnostic()
            .with_kind(DiagnosticKind::InvalidBinaryOperation {
                op,
                left: self.program.type_repr(ty_left),
                right: self.program.type_repr(ty_right),
            })
            .with_severity(Severity::Error)
            .with_span(span)
            .annotate_primary(Note::Quiet, span)
            .done();
        self.diagnostics.push(d);
        abt::TypedExpr::unknown()
    }

    fn analyse_assignment(&mut self, left: &ast::Expr, right: &ast::Expr) -> abt::TypedExpr {
        let bound_left = self.analyse_expression(left);
        let mut bound_right = self.analyse_expression(right);

        if matches!(bound_left.value.kind, abt::ExprKind::Unknown) {
            return abt::TypedExpr::unknown();
        }

        let Some((assignee, var_id, expected_type)) = self.to_lvalue(&bound_left.value) else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::AssigneeMustBeVariable)
                .with_severity(Severity::Error)
                .with_span(left.span)
                .annotate_primary(Note::CannotAssign, left.span)
                .done();
            self.diagnostics.push(d);
            return abt::TypedExpr::unknown();
        };

        let right_ty = bound_right.value.ty.clone();
        if !self.type_check_coerce(&mut bound_right, &expected_type) {
            let info = self.program.variables.get(&var_id).unwrap();
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::TypeMismatch {
                    found: self.program.type_repr(&right_ty),
                    expected: self.program.type_repr(&expected_type),
                })
                .with_severity(Severity::Error)
                .with_span(right.span)
                .annotate_primary(
                    Note::OfType(self.program.type_repr(&right_ty))
                        .but()
                        .dddot_front()
                        .num(2),
                    right.span,
                )
                .annotate_secondary(
                    Note::VariableType(info.name.value.clone(), self.program.type_repr(&info.ty))
                        .dddot_back()
                        .num(1),
                    info.name.span,
                    NoteSeverity::Annotation,
                )
                .done();
            self.diagnostics.push(d);
            return abt::TypedExpr::unknown();
        }

        abt::TypedExpr {
            kind: abt::ExprKind::Assignment {
                assignee,
                var_id,
                expr: Box::new(bound_right),
            },
            ty: right_ty,
        }
    }

    fn integer_binary_operation(op: ast::BinOp, ty: abt::Type) -> Option<abt::BinOp> {
        use abt::BinOpKind as Abt;
        use abt::Type as Ty;
        use ast::BinOp as Ast;
        match op {
            Ast::Add => Some(Abt::Add.wrap_intern(ty.clone(), ty)),
            Ast::Sub => Some(Abt::Sub.wrap_intern(ty.clone(), ty)),
            Ast::Mul => Some(Abt::Mul.wrap_intern(ty.clone(), ty)),
            Ast::Div => Some(Abt::Div.wrap_intern(ty.clone(), ty)),
            Ast::Rem => Some(Abt::Rem.wrap_intern(ty.clone(), ty)),
            Ast::Eq => Some(Abt::Eq.wrap_intern(ty, Ty::Bool)),
            Ast::Ne => Some(Abt::Ne.wrap_intern(ty, Ty::Bool)),
            Ast::Le => Some(Abt::Le.wrap_intern(ty, Ty::Bool)),
            Ast::Lt => Some(Abt::Lt.wrap_intern(ty, Ty::Bool)),
            Ast::Ge => Some(Abt::Ge.wrap_intern(ty, Ty::Bool)),
            Ast::Gt => Some(Abt::Gt.wrap_intern(ty, Ty::Bool)),
            Ast::BitAnd => Some(Abt::BitAnd.wrap_intern(ty.clone(), ty)),
            Ast::BitXor => Some(Abt::BitXor.wrap_intern(ty.clone(), ty)),
            Ast::BitOr => Some(Abt::BitOr.wrap_intern(ty.clone(), ty)),
            _ => None,
        }
    }

    fn decimal_binary_operation(op: ast::BinOp, ty: abt::Type) -> Option<abt::BinOp> {
        use abt::BinOpKind as Abt;
        use abt::Type as Ty;
        use ast::BinOp as Ast;
        match op {
            Ast::Add => Some(Abt::Add.wrap_intern(ty.clone(), ty)),
            Ast::Sub => Some(Abt::Sub.wrap_intern(ty.clone(), ty)),
            Ast::Mul => Some(Abt::Mul.wrap_intern(ty.clone(), ty)),
            Ast::Div => Some(Abt::Div.wrap_intern(ty.clone(), ty)),
            Ast::Rem => Some(Abt::Rem.wrap_intern(ty.clone(), ty)),
            Ast::Eq => Some(Abt::Eq.wrap_intern(ty, Ty::Bool)),
            Ast::Ne => Some(Abt::Ne.wrap_intern(ty, Ty::Bool)),
            Ast::Le => Some(Abt::Le.wrap_intern(ty, Ty::Bool)),
            Ast::Lt => Some(Abt::Lt.wrap_intern(ty, Ty::Bool)),
            Ast::Ge => Some(Abt::Ge.wrap_intern(ty, Ty::Bool)),
            Ast::Gt => Some(Abt::Gt.wrap_intern(ty, Ty::Bool)),
            _ => None,
        }
    }

    fn boolean_binary_operation(op: ast::BinOp) -> Option<abt::BinOp> {
        use abt::BinOpKind as Abt;
        use abt::Type as Ty;
        use ast::BinOp as Ast;
        match op {
            Ast::Eq => Some(Abt::Eq.wrap_intern(Ty::Bool, Ty::Bool)),
            Ast::Ne => Some(Abt::Ne.wrap_intern(Ty::Bool, Ty::Bool)),
            Ast::BitAnd => Some(Abt::BitAnd.wrap_intern(Ty::Bool, Ty::Bool)),
            Ast::BitXor => Some(Abt::BitXor.wrap_intern(Ty::Bool, Ty::Bool)),
            Ast::BitOr => Some(Abt::BitOr.wrap_intern(Ty::Bool, Ty::Bool)),
            Ast::And => Some(Abt::And.wrap_intern(Ty::Bool, Ty::Bool)),
            Ast::Or => Some(Abt::Or.wrap_intern(Ty::Bool, Ty::Bool)),
            Ast::Xor => Some(Abt::Xor.wrap_intern(Ty::Bool, Ty::Bool)),
            _ => None,
        }
    }

    pub fn analyse_unary_operation(
        &mut self,
        op: ast::UnOp,
        operand: &ast::Expr,
        span: Span,
    ) -> abt::TypedExpr {
        let bound_operand = self.analyse_expression(operand);
        let ty = &bound_operand.value.ty;

        if !ty.is_known() {
            return abt::TypedExpr::unknown();
        }

        let bound_op = match self.program.dealias_type(ty) {
            abt::Type::U8 | abt::Type::U16 | abt::Type::U32 | abt::Type::U64 => {
                Self::number_unary_operation(false, op, ty.clone())
            }
            abt::Type::I8
            | abt::Type::I16
            | abt::Type::I32
            | abt::Type::I64
            | abt::Type::F32
            | abt::Type::F64 => Self::number_unary_operation(true, op, ty.clone()),
            abt::Type::Bool => Self::boolean_unary_operation(op),
            _ => None,
        };

        if let Some(op) = bound_op {
            let ty = op.ty.clone();
            return abt::TypedExpr {
                kind: abt::ExprKind::Unary(op, Box::new(bound_operand)),
                ty,
            };
        }

        let d = diagnostics::create_diagnostic()
            .with_kind(DiagnosticKind::InvalidUnaryOperation {
                op,
                ty: self.program.type_repr(ty),
            })
            .with_severity(Severity::Error)
            .with_span(span)
            .annotate_primary(Note::Quiet, span)
            .done();
        self.diagnostics.push(d);
        abt::TypedExpr::unknown()
    }

    fn number_unary_operation(signed: bool, op: ast::UnOp, ty: abt::Type) -> Option<abt::UnOp> {
        match op {
            ast::UnOp::Pos => Some(abt::UnOpKind::Pos.wrap(ty)),
            ast::UnOp::Neg if signed => Some(abt::UnOpKind::Neg.wrap(ty)),
            _ => None,
        }
    }

    fn boolean_unary_operation(op: ast::UnOp) -> Option<abt::UnOp> {
        match op {
            ast::UnOp::Not => Some(abt::UnOpKind::Not.wrap(abt::Type::Bool)),
            _ => None,
        }
    }
}
