use super::{Expr, Pattern, Type};
use crate::utils::{Span, Spanned};

pub type Stmt = Spanned<StmtKind>;

#[derive(Debug)]
pub enum StmtKind {
    Empty,
    DataDef(DataDef),
    AliasDef(AliasDef),
    FuncDef(FuncDef),
    VarDef(VarDef),
    Expr(Box<Expr>),
    Block(Box<[Stmt]>),
    IfThen(Box<Expr>, Box<Stmt>),
    Then(Box<Stmt>),
    IfThenElse(Box<Expr>, Box<Stmt>, Box<Stmt>),
    Else(Box<Stmt>),
    WhileDo(Box<Expr>, Box<Stmt>),
    DoWhile(Box<Stmt>, Box<Expr>),
    Do(Box<Stmt>),
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
    pub is_opaque: bool,
}

#[derive(Debug)]
pub struct AliasDef {
    pub name: Spanned<String>,
    pub ty: Box<Type>,
    pub is_opaque: bool,
}

#[derive(Debug)]
pub struct FuncDef {
    pub name: Option<Spanned<String>>,
    pub args: Box<[(String, Type, Span)]>,
    pub ty: Box<Type>,
    pub body: Box<Stmt>,
}

#[derive(Debug)]
pub struct VarDef {
    pub pattern: Box<Pattern>,
    pub expr: Box<Expr>,
}
