use super::Type;

#[derive(Debug, Clone)]
pub struct BinOp {
    pub kind: BinOpKind,
    pub in_ty: Type,
    pub out_ty: Type,
}

#[derive(Debug, Clone)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    Ne,
    Le,
    Lt,
    Ge,
    Gt,
    BitAnd,
    BitXor,
    BitOr,
    And,
    Or,
    Xor,
}

impl BinOpKind {
    pub fn wrap(self, in_ty: Type, out_ty: Type) -> BinOp {
        BinOp {
            kind: self,
            in_ty,
            out_ty,
        }
    }
}
