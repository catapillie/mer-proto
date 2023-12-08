use std::fmt::Display;

use super::span::Span;

#[derive(Debug)]
pub struct StmtAst {
    pub kind: StmtAstKind,
    pub span: Span,
}

#[derive(Debug)]
pub struct ExprAst {
    pub kind: ExprAstKind,
    pub span: Span,
}

#[derive(Debug)]
pub struct TypeAst {
    pub kind: TypeAstKind,
    pub span: Span,
}

impl StmtAstKind {
    pub fn wrap(self, span: Span) -> StmtAst {
        StmtAst { kind: self, span }
    }
}

#[derive(Debug)]
pub enum StmtAstKind {
    Empty,
    VarDef(Option<(String, Span)>, Box<ExprAst>),
    Expr(Box<ExprAst>),
    Block(Vec<StmtAst>),
    IfThen(Box<ExprAst>, Box<StmtAst>),
    Then(Box<StmtAst>),
    IfThenElse(Box<ExprAst>, Box<StmtAst>, Box<StmtAst>),
    Else(Box<StmtAst>),
    WhileDo(Box<ExprAst>, Box<StmtAst>),
    DoWhile(Box<StmtAst>, Box<ExprAst>),
    Do(Box<StmtAst>),
    Func(
        Option<String>,
        Vec<(String, TypeAst)>,
        Box<StmtAst>,
        Box<TypeAst>,
    ),
    Return,
    ReturnWith(Box<ExprAst>),
}

impl ExprAstKind {
    pub fn wrap(self, span: Span) -> ExprAst {
        ExprAst { kind: self, span }
    }
}

#[derive(Debug)]
pub enum ExprAstKind {
    Bad,
    Integer(i64),
    Decimal(f64),
    Identifier(String),
    Boolean(bool),
    Parenthesized(Box<ExprAst>),
    BinaryOp(BinOpAst, Box<ExprAst>, Box<ExprAst>),
    UnaryOp(UnOpAst, Box<ExprAst>),
    Call(String, Vec<ExprAst>),
    Debug(Box<ExprAst>),
}

impl TypeAstKind {
    pub fn wrap(self, span: Span) -> TypeAst {
        TypeAst { kind: self, span }
    }
}

#[derive(Debug)]
pub enum TypeAstKind {
    Bad,
    Unit,
    Declared(String),
}

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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
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

pub type Precedence = u8;

#[derive(Debug)]
pub enum Associativity {
    Left,
    Right,
}
