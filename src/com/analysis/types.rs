use crate::{
    com::{
        abt::{ExprAbt, TypeAbt},
        ast,
    },
    diagnostics::{self, DiagnosticKind, Note, Severity},
};

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn analyse_type(&mut self, ty: &ast::Type) -> TypeAbt {
        match &ty.kind {
            ast::TypeKind::Bad => TypeAbt::Unknown,
            ast::TypeKind::Unit => TypeAbt::Unit,
            ast::TypeKind::Tuple(head, tail) => TypeAbt::Tuple(
                Box::new(self.analyse_type(head)),
                tail.iter().map(|ty| self.analyse_type(ty)).collect(),
            ),
            ast::TypeKind::Array(inner, size) => {
                TypeAbt::Array(Box::new(self.analyse_type(inner)), *size)
            }
            ast::TypeKind::Ref(ty) => TypeAbt::Ref(Box::new(self.analyse_type(ty))),
            ast::TypeKind::Declared(id) => {
                match id.as_str() {
                    "u8" => return TypeAbt::U8,
                    "u16" => return TypeAbt::U16,
                    "u32" => return TypeAbt::U32,
                    "u64" => return TypeAbt::U64,
                    "i8" => return TypeAbt::I8,
                    "i16" => return TypeAbt::I16,
                    "i32" => return TypeAbt::I32,
                    "i64" => return TypeAbt::I64,
                    "f32" => return TypeAbt::F32,
                    "f64" => return TypeAbt::F64,
                    "bool" => return TypeAbt::Bool,
                    _ => {}
                };

                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::UnknownType(id.clone()))
                    .with_severity(Severity::Error)
                    .with_span(ty.span)
                    .annotate_primary(Note::Unknown, ty.span)
                    .done();
                self.diagnostics.push(d);

                TypeAbt::Unknown
            }
            ast::TypeKind::Func(arg_tys, ret_ty) => {
                let bound_arg_tys = arg_tys.iter().map(|ty| self.analyse_type(ty)).collect();
                let bound_ret_ty = self.analyse_type(ret_ty);
                TypeAbt::Func(bound_arg_tys, Box::new(bound_ret_ty))
            }
        }
    }

    pub fn type_of(&self, expr: &ExprAbt) -> TypeAbt {
        use ExprAbt as E;
        use TypeAbt as Ty;
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

    pub fn size_of(ty: &TypeAbt) -> usize {
        match ty {
            TypeAbt::Unknown => 1,
            TypeAbt::Never => 1,
            TypeAbt::Unit => 1,
            TypeAbt::U8 => 1,
            TypeAbt::U16 => 1,
            TypeAbt::U32 => 1,
            TypeAbt::U64 => 1,
            TypeAbt::I8 => 1,
            TypeAbt::I16 => 1,
            TypeAbt::I32 => 1,
            TypeAbt::I64 => 1,
            TypeAbt::F32 => 1,
            TypeAbt::F64 => 1,
            TypeAbt::Bool => 1,
            TypeAbt::Tuple(head, tail) => {
                Self::size_of(head) + tail.iter().map(Self::size_of).sum::<usize>()
            }
            TypeAbt::Array(ty, size) => Self::size_of(ty) * size,
            TypeAbt::Ref(_) => 1,
            TypeAbt::Func(_, _) => 1,
        }
    }
}
