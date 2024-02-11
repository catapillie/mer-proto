use crate::{
    com::{abt, ast},
    diagnostics::{self, DiagnosticKind, Note, Severity},
};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn analyse_type(&mut self, ty: &ast::Type) -> abt::Type {
        match &ty.value {
            ast::TypeKind::Bad => abt::Type::Unknown,
            ast::TypeKind::Unit => abt::Type::Unit,
            ast::TypeKind::Tuple(head, tail) => abt::Type::Tuple(
                Box::new(self.analyse_type(head)),
                tail.iter().map(|ty| self.analyse_type(ty)).collect(),
            ),
            ast::TypeKind::Array(inner, size) => {
                abt::Type::Array(Box::new(self.analyse_type(inner)), *size)
            }
            ast::TypeKind::Ref(ty) => abt::Type::Ref(Box::new(self.analyse_type(ty))),
            ast::TypeKind::Declared(id) => {
                match id.as_str() {
                    "u8" => return abt::Type::U8,
                    "u16" => return abt::Type::U16,
                    "u32" => return abt::Type::U32,
                    "u64" => return abt::Type::U64,
                    "i8" => return abt::Type::I8,
                    "i16" => return abt::Type::I16,
                    "i32" => return abt::Type::I32,
                    "i64" => return abt::Type::I64,
                    "f32" => return abt::Type::F32,
                    "f64" => return abt::Type::F64,
                    "bool" => return abt::Type::Bool,
                    _ => {}
                };

                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::UnknownType(id.clone()))
                    .with_severity(Severity::Error)
                    .with_span(ty.span)
                    .annotate_primary(Note::Unknown, ty.span)
                    .done();
                self.diagnostics.push(d);

                abt::Type::Unknown
            }
            ast::TypeKind::Func(arg_tys, ret_ty) => {
                let bound_arg_tys = arg_tys.iter().map(|ty| self.analyse_type(ty)).collect();
                let bound_ret_ty = self.analyse_type(ret_ty);
                abt::Type::Func(bound_arg_tys, Box::new(bound_ret_ty))
            }
        }
    }

    pub fn type_of(&self, expr: &abt::Expr) -> abt::Type {
        use abt::Expr as E;
        use abt::Type as Ty;
        match expr {
            E::Unknown => Ty::Unknown,
            E::Unit => Ty::Unit,
            E::Integer(_) => Ty::I64,
            E::Decimal(_) => Ty::F64,
            E::Boolean(_) => Ty::Bool,

            E::Tuple(head, tail) => Ty::Tuple(
                Box::new(self.type_of(head)),
                tail.iter().map(|e| self.type_of(e)).collect(),
            ),
            E::TupleImmediateIndex(tuple, index) => {
                let ty = self.type_of(tuple);
                let Ty::Tuple(head, tail) = ty else {
                    unreachable!()
                };

                if *index == 0 {
                    *head
                } else {
                    tail[*index - 1].clone()
                }
            }

            E::Array(exprs) => {
                Ty::Array(Box::new(self.type_of(exprs.first().unwrap())), exprs.len())
            }
            E::ArrayImmediateIndex(array, _) => {
                let ty = self.type_of(array);
                let Ty::Array(inner_ty, _) = ty else {
                    unreachable!()
                };
                *inner_ty
            }
            E::ArrayIndex(array, _) => {
                let ty = self.type_of(array);
                let Ty::Array(inner_ty, _) = ty else {
                    unreachable!()
                };
                *inner_ty
            }

            E::Variable(var_id) => self.variables.get(var_id).unwrap().ty.clone(),
            E::Function(func_id) => {
                let info = self.functions.get(func_id).unwrap();
                let args = info
                    .args
                    .iter()
                    .map(|(_, ty)| ty.clone())
                    .collect::<Box<_>>();
                Ty::Func(args, Box::new(info.ty.clone()))
            }

            E::Call(func_id, _, _) => self.functions.get(func_id).unwrap().ty.clone(),
            E::IndirectCall(_, _, ty) => ty.clone(),

            E::Assignment {
                assignee: _,
                var_id: _,
                expr,
            } => self.type_of(expr),

            E::Binary(op, _, _) => op.out_ty.clone(),
            E::Unary(op, _) => op.ty.clone(),
            E::Debug(_, ty) => ty.clone(),

            E::Ref(inner) => Ty::Ref(Box::new(self.type_of(inner))),
            E::VarRef(var_id) => Ty::Ref(Box::new(self.type_of(&E::Variable(*var_id)))),
            E::Deref(inner) => match self.type_of(inner) {
                Ty::Ref(ty) => *ty,
                _ => unreachable!(),
            },
            E::VarDeref(var_id) => match self.type_of(&E::Variable(*var_id)) {
                Ty::Ref(ty) => *ty,
                _ => unreachable!(),
            },

            E::Todo => Ty::Never,
            E::Unreachable => Ty::Never,

            E::Case(_, _, ty) => ty.clone(),
        }
    }

    pub fn size_of(ty: &abt::Type) -> usize {
        match ty {
            abt::Type::Unknown => 1,
            abt::Type::Never => 1,
            abt::Type::Unit => 1,
            abt::Type::U8 => 1,
            abt::Type::U16 => 1,
            abt::Type::U32 => 1,
            abt::Type::U64 => 1,
            abt::Type::I8 => 1,
            abt::Type::I16 => 1,
            abt::Type::I32 => 1,
            abt::Type::I64 => 1,
            abt::Type::F32 => 1,
            abt::Type::F64 => 1,
            abt::Type::Bool => 1,
            abt::Type::Tuple(head, tail) => {
                Self::size_of(head) + tail.iter().map(Self::size_of).sum::<usize>()
            }
            abt::Type::Array(ty, size) => Self::size_of(ty) * size,
            abt::Type::Ref(_) => 1,
            abt::Type::Func(_, _) => 1,
        }
    }
}
