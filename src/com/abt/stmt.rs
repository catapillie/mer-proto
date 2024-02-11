use crate::utils::{Span, Spanned};

use super::Expr;

pub type Stmt = Spanned<StmtKind>;

#[derive(Debug)]
pub enum StmtKind {
    Empty,
    Block(Box<[Stmt]>),
    Expr(Box<Expr>),
    VarInit(u64, Box<Expr>),
    IfThen(Box<Expr>, Box<Stmt>),
    IfThenElse(Box<Expr>, Box<Stmt>, Box<Stmt>),
    WhileDo(Box<Expr>, Box<Stmt>),
    DoWhile(Box<Stmt>, Box<Expr>),
    Return(Box<Expr>),
}

impl StmtKind {
    pub fn wrap(self, span: Span) -> Stmt {
        Spanned { value: self, span }
    }
}
