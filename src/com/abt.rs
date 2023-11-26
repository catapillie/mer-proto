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
    Return(Box<ExprAbt>),
}

#[derive(Debug)]
pub enum ExprAbt {
    Unknown,
    Unit,
    Number(f64),
    Boolean(bool),
    Variable(String, TypeAbt),
    Assignment(String, TypeAbt, Box<ExprAbt>),
    Unary((UnaryOp, TypeAbt), Box<ExprAbt>),
    Binary((BinaryOp, TypeAbt), Box<ExprAbt>, Box<ExprAbt>),
    Call(String, Vec<ExprAbt>, TypeAbt),
}

impl ExprAbt {
    pub fn ty(&self) -> TypeAbt {
        match self {
            ExprAbt::Unknown => TypeAbt::Unknown,
            ExprAbt::Unit => TypeAbt::Unit,
            ExprAbt::Number(_) => TypeAbt::Number,
            ExprAbt::Boolean(_) => TypeAbt::Boolean,
            ExprAbt::Variable(_, ty) => ty.clone(),
            ExprAbt::Assignment(_, ty, _) => ty.clone(),
            ExprAbt::Unary((_, ty), _) => ty.clone(),
            ExprAbt::Binary((_, ty), _, _) => ty.clone(),
            ExprAbt::Call(_, _, ty) => ty.clone(),
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Pos,
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeAbt {
    Unknown,
    Unit,
    Number,
    Boolean,
}

impl TypeAbt {
    /// Determines whether [`self`] is of the specified type.
    /// If [`self`] is [`TypeAbt::Unknown`], then the check is true.
    pub fn is(&self, ty: &Self) -> bool {
        if !ty.is_known() {
            return true; // ignored
        }

        match self {
            Self::Unknown => true, // ignored too
            Self::Unit => matches!(ty, TypeAbt::Unit),
            Self::Number => matches!(ty, TypeAbt::Number),
            Self::Boolean => matches!(ty, TypeAbt::Boolean),
        }
    }

    pub fn is_known(&self) -> bool {
        !matches!(self, Self::Unknown)
    }
}

impl Display for TypeAbt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeAbt::Unknown => write!(f, "unknown"),
            TypeAbt::Unit => write!(f, "()"),
            TypeAbt::Number => write!(f, "number"),
            TypeAbt::Boolean => write!(f, "boolean"),
        }
    }
}
