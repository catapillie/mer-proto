use crate::{
    com::{
        abt::{self, Assignee},
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

    fn to_assignee(&self, expr: &abt::Expr) -> Option<(Assignee, u64, abt::Type)> {
        match expr {
            abt::Expr::Variable(var_id) => {
                let ty = self.program.variables.get(var_id).unwrap().ty.clone();
                Some((Assignee::Variable, *var_id, ty))
            }
            abt::Expr::VarDeref(var_id) => {
                let ty = match &self.program.variables.get(var_id).unwrap().ty {
                    abt::Type::Ref(inner) => *inner.to_owned(),
                    _ => unreachable!(),
                };
                Some((Assignee::VarDeref, *var_id, ty))
            }
            abt::Expr::Deref(inner) => {
                let (assignee, var_id, ty) = self.to_assignee(inner)?;
                let ty = match ty {
                    abt::Type::Ref(inner) => *inner.to_owned(),
                    _ => unreachable!(),
                };
                Some((Assignee::Deref(Box::new(assignee)), var_id, ty))
            }
            abt::Expr::TupleImmediateIndex(expr, index) => {
                let (assignee, var_id, tuple_ty) = self.to_assignee(expr)?;
                let abt::Type::Tuple(head, tail) = tuple_ty.clone() else {
                    unreachable!()
                };
                let ty = if *index == 0 {
                    *head
                } else {
                    tail.get(*index - 1).unwrap().clone()
                };
                Some((
                    Assignee::TupleImmediateIndex(Box::new(assignee), tuple_ty, *index),
                    var_id,
                    ty,
                ))
            }
            abt::Expr::ArrayImmediateIndex(expr, index) => {
                let (assignee, var_id, tuple_ty) = self.to_assignee(expr)?;
                let abt::Type::Array(inner_ty, _) = tuple_ty.clone() else {
                    unreachable!()
                };
                Some((
                    Assignee::ArrayImmediateIndex(Box::new(assignee), tuple_ty, *index),
                    var_id,
                    *inner_ty,
                ))
            }
            abt::Expr::ArrayIndex(expr, index_expr) => {
                let (assignee, var_id, tuple_ty) = self.to_assignee(expr)?;
                let abt::Type::Array(inner_ty, _) = tuple_ty.clone() else {
                    unreachable!()
                };
                Some((
                    Assignee::ArrayIndex(Box::new(assignee), tuple_ty, index_expr.clone()),
                    var_id,
                    *inner_ty,
                ))
            }
            abt::Expr::PointerIndex(pointer, index) => {
                let (assignee, var_id, pointer_ty) = self.to_assignee(pointer)?;
                let abt::Type::Pointer(inner_ty) = pointer_ty.clone() else {
                    unreachable!()
                };
                Some((
                    Assignee::PointerIndex(Box::new(assignee), pointer_ty, index.clone()),
                    var_id,
                    *inner_ty,
                ))
            }
            abt::Expr::FieldAccess {
                expr,
                data_id,
                field_id,
            } => {
                let (assignee, var_id, _) = self.to_assignee(expr)?;
                let info = self.program.datas.get(data_id).unwrap();
                let field_ty = info.fields[*field_id].1.value.clone();

                Some((
                    Assignee::FieldAccess(Box::new(assignee), *data_id, *field_id),
                    var_id,
                    field_ty,
                ))
            }
            _ => None,
        }
    }

    fn analyse_assignment(&mut self, left: &ast::Expr, right: &ast::Expr) -> abt::Expr {
        let bound_left = self.analyse_expression(left);
        let bound_right = self.analyse_expression(right);

        if matches!(bound_left, abt::Expr::Unknown) {
            return abt::Expr::Unknown;
        }

        let Some((assignee, var_id, expected_type)) = self.to_assignee(&bound_left) else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::AssigneeMustBeVariable)
                .with_severity(Severity::Error)
                .with_span(left.span)
                .annotate_primary(Note::CannotAssign, left.span)
                .done();
            self.diagnostics.push(d);
            return abt::Expr::Unknown;
        };

        let right_ty = self.program.type_of(&bound_right);
        let info = self.program.variables.get(&var_id).unwrap();

        if !right_ty.is(&expected_type) {
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
                    Note::VariableType(info.name.clone(), self.program.type_repr(&info.ty))
                        .dddot_back()
                        .num(1),
                    info.declaration_span,
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
            Ast::Add => Some(Abt::Add.wrap(ty.clone(), ty)),
            Ast::Sub => Some(Abt::Sub.wrap(ty.clone(), ty)),
            Ast::Mul => Some(Abt::Mul.wrap(ty.clone(), ty)),
            Ast::Div => Some(Abt::Div.wrap(ty.clone(), ty)),
            Ast::Rem => Some(Abt::Rem.wrap(ty.clone(), ty)),
            Ast::Eq => Some(Abt::Eq.wrap(ty, Ty::Bool)),
            Ast::Ne => Some(Abt::Ne.wrap(ty, Ty::Bool)),
            Ast::Le => Some(Abt::Le.wrap(ty, Ty::Bool)),
            Ast::Lt => Some(Abt::Lt.wrap(ty, Ty::Bool)),
            Ast::Ge => Some(Abt::Ge.wrap(ty, Ty::Bool)),
            Ast::Gt => Some(Abt::Gt.wrap(ty, Ty::Bool)),
            Ast::BitAnd => Some(Abt::BitAnd.wrap(ty.clone(), ty)),
            Ast::BitXor => Some(Abt::BitXor.wrap(ty.clone(), ty)),
            Ast::BitOr => Some(Abt::BitOr.wrap(ty.clone(), ty)),
            _ => None,
        }
    }

    fn decimal_binary_operation(op: ast::BinOp, ty: abt::Type) -> Option<abt::BinOp> {
        use abt::BinOpKind as Abt;
        use abt::Type as Ty;
        use ast::BinOp as Ast;
        match op {
            Ast::Add => Some(Abt::Add.wrap(ty.clone(), ty)),
            Ast::Sub => Some(Abt::Sub.wrap(ty.clone(), ty)),
            Ast::Mul => Some(Abt::Mul.wrap(ty.clone(), ty)),
            Ast::Div => Some(Abt::Div.wrap(ty.clone(), ty)),
            Ast::Rem => Some(Abt::Rem.wrap(ty.clone(), ty)),
            Ast::Eq => Some(Abt::Eq.wrap(ty, Ty::Bool)),
            Ast::Ne => Some(Abt::Ne.wrap(ty, Ty::Bool)),
            Ast::Le => Some(Abt::Le.wrap(ty, Ty::Bool)),
            Ast::Lt => Some(Abt::Lt.wrap(ty, Ty::Bool)),
            Ast::Ge => Some(Abt::Ge.wrap(ty, Ty::Bool)),
            Ast::Gt => Some(Abt::Gt.wrap(ty, Ty::Bool)),
            _ => None,
        }
    }

    fn boolean_binary_operation(op: ast::BinOp) -> Option<abt::BinOp> {
        use abt::BinOpKind as Abt;
        use abt::Type as Ty;
        use ast::BinOp as Ast;
        match op {
            Ast::Eq => Some(Abt::Eq.wrap(Ty::Bool, Ty::Bool)),
            Ast::Ne => Some(Abt::Ne.wrap(Ty::Bool, Ty::Bool)),
            Ast::BitAnd => Some(Abt::BitAnd.wrap(Ty::Bool, Ty::Bool)),
            Ast::BitXor => Some(Abt::BitXor.wrap(Ty::Bool, Ty::Bool)),
            Ast::BitOr => Some(Abt::BitOr.wrap(Ty::Bool, Ty::Bool)),
            Ast::And => Some(Abt::And.wrap(Ty::Bool, Ty::Bool)),
            Ast::Or => Some(Abt::Or.wrap(Ty::Bool, Ty::Bool)),
            Ast::Xor => Some(Abt::Xor.wrap(Ty::Bool, Ty::Bool)),
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
