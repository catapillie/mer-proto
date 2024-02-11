use super::TypeAbt;

#[derive(Debug)]
pub struct BinOp {
    pub kind: BinOpKind,
    pub in_ty: TypeAbt,
    pub out_ty: TypeAbt,
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
    pub fn wrap(self, in_ty: TypeAbt, out_ty: TypeAbt) -> BinOp {
        BinOp {
            kind: self,
            in_ty,
            out_ty,
        }
    }
}
