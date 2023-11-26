use std::fmt::Display;

#[derive(Debug)]
pub enum StmtAbt {
    Empty,
    VarDef(String, ExprAbt),
    Block(Vec<StmtAbt>),
    Expr(Box<ExprAbt>),
    IfThen(Box<ExprAbt>, Box<StmtAbt>),
    IfThenElse(Box<ExprAbt>, Box<StmtAbt>, Box<StmtAbt>),
    WhileDo(Box<ExprAbt>, Box<StmtAbt>),
    DoWhile(Box<StmtAbt>, Box<ExprAbt>),
}

#[derive(Debug)]
pub enum ExprAbt {
    Unknown,
    Number(f64),
    Boolean(bool),
    Variable(String, TypeAbt),
    Assignment(String, TypeAbt, Box<ExprAbt>),
    Binary((BinaryOp, TypeAbt), Box<ExprAbt>, Box<ExprAbt>)
}

impl ExprAbt {
    pub fn ty(&self) -> TypeAbt {
        match self {
            ExprAbt::Unknown => TypeAbt::Unknown,
            ExprAbt::Number(_) => TypeAbt::Number,
            ExprAbt::Boolean(_) => TypeAbt::Boolean,
            ExprAbt::Variable(_, ty) => ty.clone(),
            ExprAbt::Assignment(_, ty, _) => ty.clone(),
            ExprAbt::Binary((_, ty), _, _) => ty.clone(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    EqualEqual,
    NotEqual,
    LessEqual,
    LessThan,
    GreaterEqual,
    GreaterThan,
    Ampersand,
    Caret,
    Bar,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeAbt {
    Unknown,
    Number,
    Boolean,
}

impl TypeAbt {
    /// Determines whether [`self`] is of the specified type.
    /// If [`self`] is [`TypeAbt::Unknown`], then the check is true.
    pub fn is(&self, ty: &Self) -> bool {
        match self {
            TypeAbt::Unknown => true, // ignored
            TypeAbt::Number => matches!(ty, TypeAbt::Number),
            TypeAbt::Boolean => matches!(ty, TypeAbt::Boolean),
        }
    }

    pub fn is_known(&self) -> bool {
        !matches!(self, TypeAbt::Unknown)
    }
}

impl Display for TypeAbt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeAbt::Unknown => write!(f, "unknown"),
            TypeAbt::Number => write!(f, "number"),
            TypeAbt::Boolean => write!(f, "boolean"),
        }
    }
}
