use crate::com::span::Span;

#[derive(Debug)]
pub struct TypeAst {
    pub kind: TypeAstKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum TypeAstKind {
    Bad,
    Unit,
    Declared(String),
    Ref(Box<TypeAst>),
}

impl TypeAstKind {
    pub fn wrap(self, span: Span) -> TypeAst {
        TypeAst { kind: self, span }
    }
}
