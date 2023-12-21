use std::fmt::Display;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum UnOpAst {
    Pos,
    Neg,
    Not,
}

impl Display for UnOpAst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnOpAst::Pos => write!(f, "+"),
            UnOpAst::Neg => write!(f, "-"),
            UnOpAst::Not => write!(f, "not"),
        }
    }
}
