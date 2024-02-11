use super::TypeAbt;

#[derive(Debug)]
pub struct UnOp {
    pub kind: UnOpKind,
    pub ty: TypeAbt,
}

#[derive(Debug, Clone)]
pub enum UnOpKind {
    Pos,
    Neg,
    Not,
}

impl UnOpKind {
    pub fn wrap(self, ty: TypeAbt) -> UnOp {
        UnOp { kind: self, ty }
    }
}
