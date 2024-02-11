use crate::utils::Span;

#[derive(Debug)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum TypeKind {
    Bad,
    Unit,
    Declared(String),
    Tuple(Box<Type>, Box<[Type]>), // non-empty
    Array(Box<Type>, usize),
    Ref(Box<Type>),
    Func(Box<[Type]>, Box<Type>),
}

impl TypeKind {
    pub fn wrap(self, span: Span) -> Type {
        Type { kind: self, span }
    }
}
