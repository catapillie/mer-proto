use std::fmt::Display;

use super::span::Span;

#[derive(Debug)]
pub struct StmtAbt {
    pub kind: StmtAbtKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum StmtAbtKind {
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

impl StmtAbtKind {
    pub fn wrap(self, span: Span) -> StmtAbt {
        StmtAbt { kind: self, span }
    }
}

#[derive(Debug)]
pub enum ExprAbt {
    Unknown,
    Unit,
    Number(f64),
    Boolean(bool),
    Variable(Variable),
    Assignment(Variable, Box<ExprAbt>),
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
            ExprAbt::Variable(var) => var.ty.clone(),
            ExprAbt::Assignment(var, _) => var.ty.clone(),
            ExprAbt::Unary((_, ty), _) => ty.clone(),
            ExprAbt::Binary((_, ty), _, _) => ty.clone(),
            ExprAbt::Call(_, _, ty) => ty.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub id: u8,
    pub ty: TypeAbt,
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
