use super::{Expr, Type};
use crate::utils::{Span, Spanned};

pub type Stmt = Spanned<StmtKind>;

#[derive(Debug)]
pub enum StmtKind {
    Empty,
    DataDef(DataDef),
    VarDef(Option<Spanned<String>>, Box<Expr>),
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
        Option<Spanned<String>>,
        Box<[(String, Type, Span)]>,
        Box<Stmt>,
        Box<Type>,
    ),
    Return,
    ReturnWith(Box<Expr>),
    Print(Box<Expr>),
}

impl StmtKind {
    pub fn wrap(self, span: Span) -> Stmt {
        Spanned { value: self, span }
    }
}

#[derive(Debug)]
pub struct DataDef {
    pub name: Spanned<String>,
    pub fields: Box<[(Spanned<String>, Type)]>,
}
