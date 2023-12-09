use crate::com::span::Span;

use super::{expr::ExprAst, types::TypeAst};

#[derive(Debug)]
pub struct StmtAst {
    pub kind: StmtAstKind,
    pub span: Span,
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

impl StmtAstKind {
    pub fn wrap(self, span: Span) -> StmtAst {
        StmtAst { kind: self, span }
    }
}
