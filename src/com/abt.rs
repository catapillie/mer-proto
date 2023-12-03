use std::{collections::BTreeMap, fmt::Display};

use super::span::Span;

#[derive(Debug)]
pub struct ProgramAbt {
    // functions appear in the order that they were declared
    // main function (@) is last
    pub functions_by_id: BTreeMap<u32, (String, Function)>,
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
    Number(f64),
    Boolean(bool),
    Variable(Variable),
    Assignment(Variable, Box<ExprAbt>),
    Unary(UnOpAbt, Box<ExprAbt>),
    Binary(BinOpAbt, Box<ExprAbt>, Box<ExprAbt>),
    Call(u32, Vec<ExprAbt>, TypeAbt),
}

impl ExprAbt {
    pub fn ty(&self) -> TypeAbt {
        match self {
            ExprAbt::Unknown => TypeAbt::Unknown,
            ExprAbt::Unit => TypeAbt::Unit,
            ExprAbt::Number(_) => TypeAbt::F64,
            ExprAbt::Boolean(_) => TypeAbt::Bool,
            ExprAbt::Variable(var) => var.ty.clone(),
            ExprAbt::Assignment(var, _) => var.ty.clone(),
            ExprAbt::Unary(op, _) => op.ty.clone(),
            ExprAbt::Binary(op, _, _) => op.out_ty.clone(),
            ExprAbt::Call(_, _, ty) => ty.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub id: u8,
    pub ty: TypeAbt,
}

#[derive(Debug)]
pub struct Function {
    pub id: u32,
    pub param_types: Vec<TypeAbt>,
    pub return_type: TypeAbt,
    pub code: StmtAbt,
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
            TypeAbt::Unknown => write!(f, "unknown"),
            TypeAbt::Unit => write!(f, "()"),
            TypeAbt::U8 => write!(f, "u8"),
            TypeAbt::U16 => write!(f, "u16"),
            TypeAbt::U32 => write!(f, "u32"),
            TypeAbt::U64 => write!(f, "u64"),
            TypeAbt::I8 => write!(f, "i8"),
            TypeAbt::I16 => write!(f, "i16"),
            TypeAbt::I32 => write!(f, "i32"),
            TypeAbt::I64 => write!(f, "i64"),
            TypeAbt::F32 => write!(f, "f32"),
            TypeAbt::F64 => write!(f, "f64"),
            TypeAbt::Bool => write!(f, "bool"),
        }
    }
}
