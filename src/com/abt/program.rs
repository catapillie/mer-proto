use std::collections::HashMap;

use super::{AliasInfo, DataInfo, FunctionInfo, PatternKind, Size, Type, VariableInfo};
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
            Ty::Func(_, _) => Size::Known(2),
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
