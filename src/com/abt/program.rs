use std::collections::HashMap;

use super::{AliasInfo, DataInfo, Expr, FunctionInfo, PatternKind, Size, Type, VariableInfo};
use crate::diagnostics::{PatRepr, TypeRepr};

pub struct Program {
    pub main_fn_id: u64,
    pub functions: HashMap<u64, FunctionInfo>,
    pub variables: HashMap<u64, VariableInfo>,
    pub datas: HashMap<u64, DataInfo>,
    pub aliases: HashMap<u64, AliasInfo>,
}

impl Program {
    pub fn dealias_type<'a>(&'a self, mut ty: &'a Type) -> &'a Type {
        while let Type::Alias(id) = ty {
            let info = self.aliases.get(id).unwrap();
            if info.is_opaque {
                break;
            }
            ty = &info.ty;
        }
        ty
    }

    pub fn type_of(&self, expr: &Expr) -> Type {
        use Expr as E;
        use Type as Ty;
        match expr {
            E::Unknown => Ty::Unknown,
            E::Unit => Ty::Unit,
            E::Integer(_) => Ty::I64,
            E::Decimal(_) => Ty::F64,
            E::Boolean(_) => Ty::Bool,
            E::StringLiteral(s) => Ty::Array(Box::new(Ty::U8), s.len()),

            E::OpaqueConstructor {
                ctor_id,
                alias_id: _,
            } => self.type_of(&Expr::Function(*ctor_id)),

            E::Tuple(head, tail) => Ty::Tuple(
                Box::new(self.type_of(head)),
                tail.iter().map(|e| self.type_of(e)).collect(),
            ),
            E::TupleImmediateIndex(tuple, index) => {
                let ty = self.type_of(tuple);
                let Ty::Tuple(head, tail) = self.dealias_type(&ty) else {
                    unreachable!()
                };

                if *index == 0 {
                    *head.clone()
                } else {
                    tail[*index - 1].clone()
                }
            }

            E::Array(exprs) => {
                Ty::Array(Box::new(self.type_of(exprs.first().unwrap())), exprs.len())
            }
            E::ArrayImmediateIndex(array, _) => {
                let ty = self.type_of(array);
                let Ty::Array(inner_ty, _) = self.dealias_type(&ty) else {
                    unreachable!()
                };
                *inner_ty.clone()
            }
            E::ArrayIndex(array, _) => {
                let ty = self.type_of(array);
                let Ty::Array(inner_ty, _) = self.dealias_type(&ty) else {
                    unreachable!()
                };
                *inner_ty.clone()
            }

            E::PointerIndex(pointer, _) => {
                let ty = self.type_of(pointer);
                let Ty::Pointer(inner_ty) = self.dealias_type(&ty) else {
                    unreachable!()
                };
                *inner_ty.clone()
            }

            E::Variable(var_id) => self.variables.get(var_id).unwrap().ty.clone(),
            E::Function(func_id) => {
                let info = self.functions.get(func_id).unwrap();
                let args = info
                    .args
                    .iter()
                    .map(|(_, ty)| ty.clone())
                    .collect::<Box<_>>();
                Ty::Func(args, Box::new(info.ty.value.clone()))
            }

            E::Call(func_id, _, _) => self.functions.get(func_id).unwrap().ty.value.clone(),
            E::IndirectCall(_, _, ty) => ty.clone(),

            E::Assignment {
                assignee: _,
                var_id: _,
                expr,
            } => self.type_of(expr),

            E::Binary(op, _, _) => op.out_ty.clone(),
            E::Unary(op, _) => op.ty.clone(),
            E::Debug(_, ty) => ty.clone(),

            E::Heap(inner) => Ty::Ref(Box::new(self.type_of(inner))),
            E::Ref(_, _, ty) => Ty::Ref(ty.clone()),
            E::Deref(inner) => {
                let ty = self.type_of(inner);
                match self.dealias_type(&ty) {
                    Ty::Ref(ty) => *ty.clone(),
                    _ => unreachable!(),
                }
            }

            E::Todo => Ty::Never,
            E::Unreachable => Ty::Never,

            E::Case(_, _, ty) => ty.clone(),
            E::CaseTernary(_, _, _, ty) => ty.clone(),

            E::Data(id, _) => Ty::Data(*id),
            E::DataWith(id, _, _) => Ty::Data(*id),
            E::FieldAccess {
                expr: _,
                data_id,
                field_id,
            } => self
                .datas
                .get(data_id)
                .unwrap()
                .fields
                .get(*field_id)
                .unwrap()
                .1
                .value
                .clone(),

            E::Alloc(ty, _) => Ty::Pointer(ty.clone()),
            E::ToPointer(expr) => {
                let Ty::Ref(inner) = self.type_of(expr) else {
                    unreachable!()
                };
                let Ty::Array(inner, _) = *inner else {
                    unreachable!()
                };
                Ty::Pointer(inner)
            }
        }
    }

    #[rustfmt::skip]
    pub fn size_of(&self, ty: &Type) -> Size {
        use Type as Ty;
        match ty {
            Ty::Unknown => Size::Known(1),
            Ty::Never => Size::Known(1),
            Ty::Unit => Size::Known(1),
            Ty::U8 => Size::Known(1),
            Ty::U16 => Size::Known(1),
            Ty::U32 => Size::Known(1),
            Ty::U64 => Size::Known(1),
            Ty::I8 => Size::Known(1),
            Ty::I16 => Size::Known(1),
            Ty::I32 => Size::Known(1),
            Ty::I64 => Size::Known(1),
            Ty::F32 => Size::Known(1),
            Ty::F64 => Size::Known(1),
            Ty::Bool => Size::Known(1),
            Ty::Ref(_) => Size::Known(1),
            Ty::Func(_, _) => Size::Known(1),
            Ty::Pointer(_) => Size::Known(2),
            Ty::Tuple(head, tail)
                => self.size_of(head) + tail.iter().map(|ty| self.size_of(ty)).sum::<Size>(),
            Ty::Array(ty, size)
                => self.size_of(ty) * *size,
            Ty::Data(id)
                => self.datas.get(id).unwrap().size,
            Ty::Alias(id)
                => self.size_of(&self.aliases.get(id).unwrap().ty),
        }
    }

    #[rustfmt::skip]
    pub fn type_repr(&self, ty: &Type) -> TypeRepr {
        use Type as Ty;
        use TypeRepr as Re;
        match ty {
            Ty::Unknown => Re::Unknown,
            Ty::Never => Re::Never,
            Ty::Unit => Re::Unit,
            Ty::U8 => Re::U8,
            Ty::U16 => Re::U16,
            Ty::U32 => Re::U32,
            Ty::U64 => Re::U64,
            Ty::I8 => Re::I8,
            Ty::I16 => Re::I16,
            Ty::I32 => Re::I32,
            Ty::I64 => Re::I64,
            Ty::F32 => Re::F32,
            Ty::F64 => Re::F64,
            Ty::Bool => Re::Bool,
            Ty::Data(id)
                => Re::Data(self.datas.get(id).unwrap().name.value.clone()),
            Ty::Alias(id)
                => Re::Data(self.aliases.get(id).unwrap().name.value.clone()),
            Ty::Tuple(head, tail)
                => Re::Tuple(
                    Box::new(self.type_repr(head)),
                    tail.iter().map(|ty| self.type_repr(ty)).collect()
                ),
            Ty::Array(ty, size)
                => Re::Array(Box::new(self.type_repr(ty)), *size),
            Ty::Pointer(inner) => Re::Pointer(Box::new(self.type_repr(inner))),
            Ty::Ref(inner)
                => Re::Ref(Box::new(self.type_repr(inner))),
            Ty::Func(args, ty)
                => Re::Func(
                    args.iter().map(|ty| self.type_repr(ty)).collect(),
                    Box::new(self.type_repr(ty))
                ),
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    pub fn pat_repr(&self, pat: &PatternKind) -> PatRepr {
        match pat {
            PatternKind::Discard => PatRepr::Discard,
            PatternKind::Binding(name) => PatRepr::Binding(name.clone()),
            PatternKind::Unit => PatRepr::Unit,
            PatternKind::Tuple(head, tail) => PatRepr::Tuple(
                Box::new(self.pat_repr(&head.value)),
                tail.iter().map(|p| self.pat_repr(&p.value)).collect(),
            ),
            PatternKind::Array(pats) => {
                PatRepr::Array(pats.iter().map(|p| self.pat_repr(&p.value)).collect())
            }
            PatternKind::Ref(pat) => PatRepr::Ref(Box::new(self.pat_repr(&pat.value))),
            PatternKind::OpaqueTypeConstructor(id, pats) => {
                let name = self.aliases.get(id).unwrap().name.value.clone();
                PatRepr::OpaqueTypeConstructor(
                    name,
                    pats.iter().map(|p| self.pat_repr(&p.value)).collect(),
                )
            }
        }
    }
}
