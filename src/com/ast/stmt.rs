use super::{expr::Expr, types::Type};
use crate::utils::Span;

#[derive(Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum StmtKind {
    Empty,
    VarDef(Option<(String, Span)>, Box<Expr>),
    Expr(Box<Expr>),
    Block(Box<[Stmt]>),
    IfThen(Box<Expr>, Box<Stmt>),
    Then(Box<Stmt>),
    IfThenElse(Box<Expr>, Box<Stmt>, Box<Stmt>),
    Else(Box<Stmt>),
    WhileDo(Box<Expr>, Box<Stmt>),
    DoWhile(Box<Stmt>, Box<Expr>),
    Do(Box<Stmt>),
    Func(
        Option<(String, Span)>,
        Box<[(String, Type, Span)]>,
        Box<Stmt>,
        Box<Type>,
    ),
    Return,
    ReturnWith(Box<Expr>),
}

impl StmtKind {
    pub fn wrap(self, span: Span) -> Stmt {
        Stmt { kind: self, span }
    }
}
