use std::fmt::Display;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum UnOp {
    Pos,
    Neg,
    Not,
}

impl Display for UnOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnOp::Pos => write!(f, "+"),
            UnOp::Neg => write!(f, "-"),
            UnOp::Not => write!(f, "not"),
        }
    }
}
