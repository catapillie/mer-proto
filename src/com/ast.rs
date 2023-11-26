use std::fmt::Display;

use super::span::Span;

pub type ProgramAst = Vec<StmtAst>;

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
    VarDef(Option<String>, Box<ExprAst>),
    Expr(Box<ExprAst>),
    Block(Vec<StmtAst>),
    IfThen(Box<ExprAst>, Box<StmtAst>),
    Then(Box<StmtAst>),
    IfThenElse(Box<ExprAst>, Box<StmtAst>, Box<StmtAst>),
    Else(Box<StmtAst>),
    WhileDo(Box<ExprAst>, Box<StmtAst>),
    DoWhile(Box<StmtAst>, Box<ExprAst>),
    Do(Box<StmtAst>),
    Func(Option<String>, Vec<String>, Box<StmtAst>, Box<TypeAst>),
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
    Number(f64),
    Identifier(String),
    Boolean(bool),
    Parenthesized(Box<ExprAst>),
    BinaryOp(BinaryOperator, Box<ExprAst>, Box<ExprAst>),
    UnaryOp(UnaryOperator, Box<ExprAst>),
    Call(String, Vec<ExprAst>),
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
pub enum UnaryOperator {
    Pos,
    Neg,
    Not,
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::Pos => write!(f, "+"),
            UnaryOperator::Neg => write!(f, "-"),
            UnaryOperator::Not => write!(f, "not"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
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
    Equal,
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Plus => write!(f, "+"),
            BinaryOperator::Minus => write!(f, "-"),
            BinaryOperator::Star => write!(f, "*"),
            BinaryOperator::Slash => write!(f, "/"),
            BinaryOperator::Percent => write!(f, "%"),
            BinaryOperator::EqualEqual => write!(f, "=="),
            BinaryOperator::NotEqual => write!(f, "!="),
            BinaryOperator::LessEqual => write!(f, "<="),
            BinaryOperator::LessThan => write!(f, "<"),
            BinaryOperator::GreaterEqual => write!(f, ">="),
            BinaryOperator::GreaterThan => write!(f, ">"),
            BinaryOperator::Ampersand => write!(f, "&"),
            BinaryOperator::Caret => write!(f, "^"),
            BinaryOperator::Bar => write!(f, "|"),
            BinaryOperator::And => write!(f, "and"),
            BinaryOperator::Or => write!(f, "or"),
            BinaryOperator::Equal => write!(f, "="),
        }
    }
}

pub type Precedence = u8;

#[derive(Debug)]
pub enum Associativity {
    Left,
    Right,
}
