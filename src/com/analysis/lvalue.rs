use super::Analyser;
use crate::com::abt::{self, LValue};

impl<'d> Analyser<'d> {
    pub fn to_lvalue(&self, expr: &abt::Expr) -> Option<(LValue, u64, abt::Type)> {
        match &expr.kind {
            abt::ExprKind::Variable(var_id) => {
                let ty = self.program.variables.get(var_id).unwrap().ty.clone();
                Some((LValue::Variable, *var_id, ty))
            }
            abt::ExprKind::Deref(inner) => {
                let (assignee, var_id, ty) = self.to_lvalue(inner)?;
                let ty = match ty {
                    abt::Type::Ref(inner) => *inner.to_owned(),
                    _ => unreachable!(),
                };
                Some((LValue::Deref(Box::new(assignee)), var_id, ty))
            }
            abt::ExprKind::TupleImmediateIndex(expr, index) => {
                let (assignee, var_id, tuple_ty) = self.to_lvalue(expr)?;
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
            abt::ExprKind::ArrayImmediateIndex(expr, index) => {
                let (assignee, var_id, tuple_ty) = self.to_lvalue(expr)?;
                let abt::Type::Array(inner_ty, _) = tuple_ty.clone() else {
                    unreachable!()
                };
                Some((
                    LValue::ArrayImmediateIndex(Box::new(assignee), tuple_ty, *index),
                    var_id,
                    *inner_ty,
                ))
            }
            abt::ExprKind::ArrayIndex(expr, index_expr) => {
                let (assignee, var_id, tuple_ty) = self.to_lvalue(expr)?;
                let abt::Type::Array(inner_ty, _) = tuple_ty.clone() else {
                    unreachable!()
                };
                Some((
                    LValue::ArrayIndex(Box::new(assignee), tuple_ty, index_expr.clone()),
                    var_id,
                    *inner_ty,
                ))
            }
            abt::ExprKind::PointerIndex(pointer, index) => {
                let (assignee, var_id, pointer_ty) = self.to_lvalue(pointer)?;
                let abt::Type::Pointer(inner_ty) = pointer_ty.clone() else {
                    unreachable!()
                };
                Some((
                    LValue::PointerIndex(Box::new(assignee), pointer_ty, index.clone()),
                    var_id,
                    *inner_ty,
                ))
            }
            abt::ExprKind::FieldAccess {
                expr,
                data_id,
                field_id,
            } => {
                let (assignee, var_id, _) = self.to_lvalue(expr)?;
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
}
