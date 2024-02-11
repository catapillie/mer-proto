use super::Type;

#[derive(Debug, Clone)]
pub struct UnOp {
    pub kind: UnOpKind,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub enum UnOpKind {
    Pos,
    Neg,
    Not,
}

impl UnOpKind {
    pub fn wrap(self, ty: Type) -> UnOp {
        UnOp { kind: self, ty }
    }
}
