use super::Type;

#[derive(Debug, Clone)]
pub struct BinOp {
    pub kind: BinOpKind,
    pub in_left: Type,
    pub in_right: Type,
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
    Concat,
}

impl BinOpKind {
    pub fn wrap_intern(self, in_ty: Type, out_ty: Type) -> BinOp {
        BinOp {
            kind: self,
            in_left: in_ty.clone(),
            in_right: in_ty,
            out_ty,
        }
    }

    pub fn wrap_extern(self, in_left: Type, in_right: Type, out_ty: Type) -> BinOp {
        BinOp {
            kind: self,
            in_left,
            in_right,
            out_ty,
        }
    }
}
