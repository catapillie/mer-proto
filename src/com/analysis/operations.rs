use crate::com::{
    abt::{BinOpAbt, BinOpAbtKind, ExprAbt, TypeAbt, UnOpAbt, UnOpAbtKind},
    diagnostics::{self, DiagnosticKind, Note, NoteSeverity, Severity},
    span::Span,
    syntax::{bin_op::BinOpAst, expr::ExprAst, un_op::UnOpAst},
};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn analyse_binary_operation(
        &mut self,
        op: BinOpAst,
        left: &ExprAst,
        right: &ExprAst,
        span: Span,
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

        if self.check_type(ty_left, ty_right) {
            use TypeAbt as Ty;
            let ty = ty_left;
            let bound_op = match ty {
                Ty::Var(_) => self.integer_binary_operation(op, ty),
                Ty::U8 => self.integer_binary_operation(op, ty),
                Ty::U16 => self.integer_binary_operation(op, ty),
                Ty::U32 => self.integer_binary_operation(op, ty),
                Ty::U64 => self.integer_binary_operation(op, ty),
                Ty::I8 => self.integer_binary_operation(op, ty),
                Ty::I16 => self.integer_binary_operation(op, ty),
                Ty::I32 => self.integer_binary_operation(op, ty),
                Ty::I64 => self.integer_binary_operation(op, ty),
                Ty::F32 => self.decimal_binary_operation(op, ty),
                Ty::F64 => self.decimal_binary_operation(op, ty),
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
            .with_span(span)
            .annotate_primary(Note::Quiet, span)
            .done();
        self.diagnostics.push(d);
        ExprAbt::Unknown
    }

    fn analyse_assignment(&mut self, left: &ExprAst, right: &ExprAst) -> ExprAbt {
        let bound_left = self.analyse_expression(left);
        let bound_right = self.analyse_expression(right);

        let ExprAbt::Variable(var_id) = bound_left else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::AssigneeMustBeVariable)
                .with_severity(Severity::Error)
                .with_span(left.span)
                .annotate_primary(Note::CannotAssign, left.span)
                .done();
            self.diagnostics.push(d);
            return ExprAbt::Unknown;
        };

        let right_ty = self.type_of(&bound_right);
        let info = self.variables.get(&var_id).unwrap();
        let left_ty = info.ty;
        let name = info.name.clone();
        let decl_span = info.declaration_span;

        if !self.check_type(right_ty, left_ty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::TypeMismatch {
                    found: right_ty,
                    expected: left_ty,
                })
                .with_severity(Severity::Error)
                .with_span(right.span)
                .annotate_primary(
                    Note::MustBeOfType(left_ty).so().dddot_front().num(2),
                    right.span,
                )
                .annotate_secondary(
                    Note::VariableType(name, left_ty).dddot_back().num(1),
                    decl_span,
                    NoteSeverity::Annotation,
                )
                .done();
            self.diagnostics.push(d);
            return ExprAbt::Unknown;
        }

        ExprAbt::Assignment(var_id, Box::new(bound_right))
    }

    fn integer_binary_operation(&self, op: BinOpAst, ty: TypeAbt) -> Option<BinOpAbt> {
        if !self.check_integer_type(ty) {
            return None;
        }

        use BinOpAbtKind as Abt;
        use BinOpAst as Ast;
        use TypeAbt as Ty;
        match op {
            Ast::Add => Some(Abt::Add.wrap(ty, ty)),
            Ast::Sub => Some(Abt::Sub.wrap(ty, ty)),
            Ast::Mul => Some(Abt::Mul.wrap(ty, ty)),
            Ast::Div => Some(Abt::Div.wrap(ty, ty)),
            Ast::Rem => Some(Abt::Rem.wrap(ty, ty)),
            Ast::Eq => Some(Abt::Eq.wrap(ty, Ty::Bool)),
            Ast::Ne => Some(Abt::Ne.wrap(ty, Ty::Bool)),
            Ast::Le => Some(Abt::Le.wrap(ty, Ty::Bool)),
            Ast::Lt => Some(Abt::Lt.wrap(ty, Ty::Bool)),
            Ast::Ge => Some(Abt::Ge.wrap(ty, Ty::Bool)),
            Ast::Gt => Some(Abt::Gt.wrap(ty, Ty::Bool)),
            Ast::BitAnd => Some(Abt::BitAnd.wrap(ty, ty)),
            Ast::BitXor => Some(Abt::BitXor.wrap(ty, ty)),
            Ast::BitOr => Some(Abt::BitOr.wrap(ty, ty)),
            _ => None,
        }
    }

    fn decimal_binary_operation(&self, op: BinOpAst, ty: TypeAbt) -> Option<BinOpAbt> {
        if !self.check_float_type(ty) {
            return None
        }
        
        use BinOpAbtKind as Abt;
        use BinOpAst as Ast;
        use TypeAbt as Ty;
        match op {
            Ast::Add => Some(Abt::Add.wrap(ty, ty)),
            Ast::Sub => Some(Abt::Sub.wrap(ty, ty)),
            Ast::Mul => Some(Abt::Mul.wrap(ty, ty)),
            Ast::Div => Some(Abt::Div.wrap(ty, ty)),
            Ast::Rem => Some(Abt::Rem.wrap(ty, ty)),
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

    pub fn analyse_unary_operation(
        &mut self,
        op: UnOpAst,
        operand: &ExprAst,
        span: Span,
    ) -> ExprAbt {
        let bound_operand = self.analyse_expression(operand);
        let ty = self.type_of(&bound_operand);

        if !ty.is_known() {
            return ExprAbt::Unknown;
        }

        todo!();

        let bound_op = match ty {
            TypeAbt::U8 | TypeAbt::U16 | TypeAbt::U32 | TypeAbt::U64 => {
                self.number_unary_operation(false, op, ty)
            }
            TypeAbt::I8
            | TypeAbt::I16
            | TypeAbt::I32
            | TypeAbt::I64
            | TypeAbt::F32
            | TypeAbt::F64 => self.number_unary_operation(true, op, ty),
            TypeAbt::Bool => Self::boolean_unary_operation(op),
            _ => None,
        };

        if let Some(op) = bound_op {
            return ExprAbt::Unary(op, Box::new(bound_operand));
        }

        let d = diagnostics::create_diagnostic()
            .with_kind(DiagnosticKind::InvalidUnaryOperation { op, ty })
            .with_severity(Severity::Error)
            .with_span(span)
            .annotate_primary(Note::Quiet, span)
            .done();
        self.diagnostics.push(d);
        ExprAbt::Unknown
    }

    fn number_unary_operation(&self, signed: bool, op: UnOpAst, ty: TypeAbt) -> Option<UnOpAbt> {
        match op {
            UnOpAst::Pos => Some(UnOpAbtKind::Pos.wrap(ty)),
            UnOpAst::Neg if signed => Some(UnOpAbtKind::Neg.wrap(ty)),
            _ => None,
        }
    }

    fn boolean_unary_operation(op: UnOpAst) -> Option<UnOpAbt> {
        match op {
            UnOpAst::Not => Some(UnOpAbtKind::Not.wrap(TypeAbt::Bool)),
            _ => None,
        }
    }
}
