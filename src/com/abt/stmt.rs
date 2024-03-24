use super::{BoundPattern, Expr};
use crate::utils::{Span, Spanned};

pub type Stmt = Spanned<StmtKind>;

#[derive(Debug)]
pub enum StmtKind {
    Empty,
    Block(Box<[Stmt]>),
    Expr(Box<Expr>),
    VarInit(u64, Box<Expr>),
    Deconstruct(Box<BoundPattern>, Box<Expr>),
    IfThen(Box<Expr>, Box<Stmt>),
    IfThenElse(Box<Expr>, Box<Stmt>, Box<Stmt>),
    WhileDo(Box<Expr>, Box<Stmt>),
    DoWhile(Box<Stmt>, Box<Expr>),
    Return(Box<Expr>),
    Print(Box<Expr>),
}

impl StmtKind {
    pub fn wrap(self, span: Span) -> Stmt {
        Spanned { value: self, span }
    }
}
