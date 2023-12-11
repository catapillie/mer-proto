use crate::com::{
    abt::{BinOpAbt, BinOpAbtKind, ExprAbt, TypeAbt},
    diagnostics::{self, DiagnosticKind, Severity},
    syntax::{bin_op::BinOpAst, expr::ExprAst},
};

use super::Analyser;

impl<'d> Analyser<'d> {
    fn analyse_binary_operation(
        &mut self,
        op: BinOpAst,
        left: &ExprAst,
        right: &ExprAst,
    ) -> ExprAbt {
        if matches!(op, BinOpAst::Assign) {
            return self.analyse_assignment(left, right);
        }

        let bound_left = self.analyse_expression(left);
        let bound_right = self.analyse_expression(right);

        let ty_left = self.type_of(&bound_left);
        let ty_right = self.type_of(&bound_right);

        if !ty_left.is_known() || !ty_right.is_known() {
            return ExprAbt::Unknown;
        }

        if ty_left == ty_right {
            use TypeAbt as Ty;
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
                return ExprAbt::Binary(op, Box::new(bound_left), Box::new(bound_right));
            }
        }

        let d = diagnostics::create_diagnostic()
            .with_kind(DiagnosticKind::InvalidBinaryOperation {
                op,
                left: ty_left,
                right: ty_right,
            })
            .with_severity(Severity::Error)
            .with_span(right.span.join(left.span))
            .done();
        self.diagnostics.push(d);
        ExprAbt::Unknown
    }

    fn integer_binary_operation(op: BinOpAst, ty: TypeAbt) -> Option<BinOpAbt> {
        use BinOpAbtKind as Abt;
        use BinOpAst as Ast;
        use TypeAbt as Ty;
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

    fn decimal_binary_operation(op: BinOpAst, ty: TypeAbt) -> Option<BinOpAbt> {
        use BinOpAbtKind as Abt;
        use BinOpAst as Ast;
        use TypeAbt as Ty;
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

    fn boolean_binary_operation(op: BinOpAst) -> Option<BinOpAbt> {
        use BinOpAbtKind as Abt;
        use BinOpAst as Ast;
        use TypeAbt as Ty;
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

    fn analyse_assignment(&mut self, left: &ExprAst, right: &ExprAst) -> ExprAbt {
        let bound_left = self.analyse_expression(left);
        let bound_right = self.analyse_expression(right);

        let ExprAbt::Variable(var_id) = bound_left else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::AssigneeMustBeVariable)
                .with_severity(Severity::Error)
                .with_span(left.span)
                .done();
            self.diagnostics.push(d);
            return ExprAbt::Unknown;
        };

        let right_ty = self.type_of(&bound_right);
        let var_ty = self.get_variable_by_id(var_id).unwrap();

        if !right_ty.is(&var_ty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::TypeMismatch {
                    found: right_ty,
                    expected: var_ty.clone(),
                })
                .with_severity(Severity::Error)
                .with_span(right.span)
                .done();
            self.diagnostics.push(d);
            return ExprAbt::Unknown;
        }

        ExprAbt::Assignment(var_id, Box::new(bound_right))
    }
}
