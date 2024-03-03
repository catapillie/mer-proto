use crate::{
    com::{
        abt::{self, LValue},
        ast,
    },
    diagnostics::{self, DiagnosticKind, Note, NoteSeverity, Severity},
    utils::Span,
};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn analyse_binary_operation(
        &mut self,
        op: ast::BinOp,
        left: &ast::Expr,
        right: &ast::Expr,
        span: Span,
    ) -> abt::Expr {
        if matches!(op, ast::BinOp::Assign) {
            return self.analyse_assignment(left, right);
        }

        let bound_left = self.analyse_expression(left);
        let bound_right = self.analyse_expression(right);

        let ty_left = self.program.type_of(&bound_left);
        let ty_right = self.program.type_of(&bound_right);

        if !ty_left.is_known() || !ty_right.is_known() {
            return abt::Expr::Unknown;
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
                    return abt::Expr::Unknown;
                }

                let out_ty = abt::Type::Array(arr_left.clone(), size_left + size_right);
                let bound_op =
                    abt::BinOpKind::Concat.wrap_extern(ty_left.clone(), ty_right.clone(), out_ty);
                return abt::Expr::Binary(bound_op, Box::new(bound_left), Box::new(bound_right));
            }

            if let (Tup(hd_left, tl_left), Tup(hd_right, tl_right)) = (&ty_left, &ty_right) {
                let head = (*hd_left).clone();
                let tail = [&**tl_left, &[(**hd_right).clone()], &**tl_right]
                    .concat()
                    .into_boxed_slice();
                let out_ty = abt::Type::Tuple(head, tail);
                let bound_op =
                    abt::BinOpKind::Concat.wrap_extern(ty_left.clone(), ty_right.clone(), out_ty);
                return abt::Expr::Binary(bound_op, Box::new(bound_left), Box::new(bound_right));
            }
        }

        if ty_left == ty_right {
            use abt::Type as Ty;
            let ty = ty_left.clone();
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
                return abt::Expr::Binary(op, Box::new(bound_left), Box::new(bound_right));
            }
        }

        let d = diagnostics::create_diagnostic()
            .with_kind(DiagnosticKind::InvalidBinaryOperation {
                op,
                left: self.program.type_repr(&ty_left),
                right: self.program.type_repr(&ty_right),
            })
            .with_severity(Severity::Error)
            .with_span(span)
            .annotate_primary(Note::Quiet, span)
            .done();
        self.diagnostics.push(d);
        abt::Expr::Unknown
    }

    fn to_l_value(&self, expr: &abt::Expr) -> Option<(LValue, u64, abt::Type)> {
        match expr {
            abt::Expr::Variable(var_id) => {
                let ty = self.program.variables.get(var_id).unwrap().ty.clone();
                Some((LValue::Variable, *var_id, ty))
            }
            abt::Expr::Deref(inner) => {
                let (assignee, var_id, ty) = self.to_l_value(inner)?;
                let ty = match ty {
                    abt::Type::Ref(inner) => *inner.to_owned(),
                    _ => unreachable!(),
                };
                Some((LValue::Deref(Box::new(assignee)), var_id, ty))
            }
            abt::Expr::TupleImmediateIndex(expr, index) => {
                let (assignee, var_id, tuple_ty) = self.to_l_value(expr)?;
                let abt::Type::Tuple(head, tail) = tuple_ty.clone() else {
                    unreachable!()
                };
                let ty = if *index == 0 {
                    *head
                } else {
                    tail.get(*index - 1).unwrap().clone()
                };
                Some((
                    LValue::TupleImmediateIndex(Box::new(assignee), tuple_ty, *index),
                    var_id,
                    ty,
                ))
            }
            abt::Expr::ArrayImmediateIndex(expr, index) => {
                let (assignee, var_id, tuple_ty) = self.to_l_value(expr)?;
                let abt::Type::Array(inner_ty, _) = tuple_ty.clone() else {
                    unreachable!()
                };
                Some((
                    LValue::ArrayImmediateIndex(Box::new(assignee), tuple_ty, *index),
                    var_id,
                    *inner_ty,
                ))
            }
            abt::Expr::ArrayIndex(expr, index_expr) => {
                let (assignee, var_id, tuple_ty) = self.to_l_value(expr)?;
                let abt::Type::Array(inner_ty, _) = tuple_ty.clone() else {
                    unreachable!()
                };
                Some((
                    LValue::ArrayIndex(Box::new(assignee), tuple_ty, index_expr.clone()),
                    var_id,
                    *inner_ty,
                ))
            }
            abt::Expr::PointerIndex(pointer, index) => {
                let (assignee, var_id, pointer_ty) = self.to_l_value(pointer)?;
                let abt::Type::Pointer(inner_ty) = pointer_ty.clone() else {
                    unreachable!()
                };
                Some((
                    LValue::PointerIndex(Box::new(assignee), pointer_ty, index.clone()),
                    var_id,
                    *inner_ty,
                ))
            }
            abt::Expr::FieldAccess {
                expr,
                data_id,
                field_id,
            } => {
                let (assignee, var_id, _) = self.to_l_value(expr)?;
                let info = self.program.datas.get(data_id).unwrap();
                let field_ty = info.fields[*field_id].1.value.clone();

                Some((
                    LValue::FieldAccess(Box::new(assignee), *data_id, *field_id),
                    var_id,
                    field_ty,
                ))
            }
            _ => None,
        }
    }

    fn analyse_assignment(&mut self, left: &ast::Expr, right: &ast::Expr) -> abt::Expr {
        let bound_left = self.analyse_expression(left);
        let mut bound_right = self.analyse_expression(right);

        if matches!(bound_left, abt::Expr::Unknown) {
            return abt::Expr::Unknown;
        }

        let Some((assignee, var_id, expected_type)) = self.to_l_value(&bound_left) else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::AssigneeMustBeVariable)
                .with_severity(Severity::Error)
                .with_span(left.span)
                .annotate_primary(Note::CannotAssign, left.span)
                .done();
            self.diagnostics.push(d);
            return abt::Expr::Unknown;
        };

        if !self.type_check_coerce(&mut bound_right, &expected_type) {
            let right_ty = self.program.type_of(&bound_right);
            let info = self.program.variables.get(&var_id).unwrap();
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::TypeMismatch {
                    found: self.program.type_repr(&right_ty),
                    expected: self.program.type_repr(&expected_type),
                })
                .with_severity(Severity::Error)
                .with_span(right.span)
                .annotate_primary(
                    Note::MustBeOfType(self.program.type_repr(&expected_type))
                        .so()
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
            return abt::Expr::Unknown;
        }

        abt::Expr::Assignment {
            assignee,
            var_id,
            expr: Box::new(bound_right),
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
    ) -> abt::Expr {
        let bound_operand = self.analyse_expression(operand);
        let ty = self.program.type_of(&bound_operand);

        if !ty.is_known() {
            return abt::Expr::Unknown;
        }

        let bound_op = match ty {
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
            return abt::Expr::Unary(op, Box::new(bound_operand));
        }

        let d = diagnostics::create_diagnostic()
            .with_kind(DiagnosticKind::InvalidUnaryOperation {
                op,
                ty: self.program.type_repr(&ty),
            })
            .with_severity(Severity::Error)
            .with_span(span)
            .annotate_primary(Note::Quiet, span)
            .done();
        self.diagnostics.push(d);
        abt::Expr::Unknown
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
