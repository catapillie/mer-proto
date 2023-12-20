use std::{collections::BTreeMap, fmt::Display};

use super::{analysis::FunctionInfo, span::Span};

pub struct ProgramAbt {
    pub functions_by_id: BTreeMap<u32, FunctionInfo>,
}

#[derive(Debug)]
pub struct StmtAbt {
    pub kind: StmtAbtKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum StmtAbtKind {
    Empty,
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
    Integer(i64),
    Decimal(f64),
    Boolean(bool),
    Variable(u64),
    Assignment(u64, Box<ExprAbt>),
    Binary(BinOpAbt, Box<ExprAbt>, Box<ExprAbt>),
    Unary(UnOpAbt, Box<ExprAbt>),
    Call(u64, Vec<ExprAbt>, TypeAbt),
    Debug(Box<ExprAbt>),
}

#[derive(Debug)]
pub struct BinOpAbt {
    pub kind: BinOpAbtKind,
    pub in_ty: TypeAbt,
    pub out_ty: TypeAbt,
}

#[derive(Debug, Clone)]
pub enum BinOpAbtKind {
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

impl BinOpAbtKind {
    pub fn wrap(self, in_ty: TypeAbt, out_ty: TypeAbt) -> BinOpAbt {
        BinOpAbt {
            kind: self,
            in_ty,
            out_ty,
        }
    }
}

#[derive(Debug)]
pub struct UnOpAbt {
    pub kind: UnOpAbtKind,
    pub ty: TypeAbt,
}

#[derive(Debug, Clone)]
pub enum UnOpAbtKind {
    Pos,
    Neg,
    Not,
}

impl UnOpAbtKind {
    pub fn wrap(self, ty: TypeAbt) -> UnOpAbt {
        UnOpAbt { kind: self, ty }
    }
}

#[rustfmt::skip]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeAbt {
    Unknown,
    Unit,
    U8, U16, U32, U64,
    I8, I16, I32, I64,
    F32, F64,
    Bool,
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
            Self::Unit => matches!(ty, Self::Unit),
            Self::U8 => matches!(ty, Self::U8),
            Self::U16 => matches!(ty, Self::U16),
            Self::U32 => matches!(ty, Self::U32),
            Self::U64 => matches!(ty, Self::U64),
            Self::I8 => matches!(ty, Self::I8),
            Self::I16 => matches!(ty, Self::I16),
            Self::I32 => matches!(ty, Self::I32),
            Self::I64 => matches!(ty, Self::I64),
            Self::F32 => matches!(ty, Self::F32),
            Self::F64 => matches!(ty, Self::F64),
            Self::Bool => matches!(ty, Self::Bool),
        }
    }

    pub fn is_known(&self) -> bool {
        !matches!(self, Self::Unknown)
    }
}

impl Display for TypeAbt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unknown => write!(f, "unknown"),
            Self::Unit => write!(f, "()"),
            Self::U8 => write!(f, "u8"),
            Self::U16 => write!(f, "u16"),
            Self::U32 => write!(f, "u32"),
            Self::U64 => write!(f, "u64"),
            Self::I8 => write!(f, "i8"),
            Self::I16 => write!(f, "i16"),
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::F32 => write!(f, "f32"),
            Self::F64 => write!(f, "f64"),
            Self::Bool => write!(f, "bool"),
        }
    }
}
