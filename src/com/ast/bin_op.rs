use std::fmt::Display;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOpAst {
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
    Assign,
}

impl Display for BinOpAst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOpAst::Add => write!(f, "+"),
            BinOpAst::Sub => write!(f, "-"),
            BinOpAst::Mul => write!(f, "*"),
            BinOpAst::Div => write!(f, "/"),
            BinOpAst::Rem => write!(f, "%"),
            BinOpAst::Eq => write!(f, "=="),
            BinOpAst::Ne => write!(f, "!="),
            BinOpAst::Le => write!(f, "<="),
            BinOpAst::Lt => write!(f, "<"),
            BinOpAst::Ge => write!(f, ">="),
            BinOpAst::Gt => write!(f, ">"),
            BinOpAst::BitAnd => write!(f, "&"),
            BinOpAst::BitXor => write!(f, "^"),
            BinOpAst::BitOr => write!(f, "|"),
            BinOpAst::And => write!(f, "and"),
            BinOpAst::Or => write!(f, "or"),
            BinOpAst::Xor => write!(f, "xor"),
            BinOpAst::Assign => write!(f, "="),
        }
    }
}
