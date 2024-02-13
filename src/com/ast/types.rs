use crate::utils::{Span, Spanned};

pub type Type = Spanned<TypeKind>;

#[derive(Debug)]
pub enum TypeKind {
    Bad,
    Unit,
    Declared(String),
    Tuple(Box<Type>, Box<[Type]>), // non-empty
    Array(Box<Type>, usize),
    Pointer(Box<Type>),
    Ref(Box<Type>),
    Func(Box<[Type]>, Box<Type>),
}

impl TypeKind {
    pub fn wrap(self, span: Span) -> Type {
        Spanned { value: self, span }
    }
}
