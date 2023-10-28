use super::ops::BinOp;

#[derive(Debug, Clone)]
pub enum ExprAst {
    Missing,
    True,
    False,
    Number(f64),
    Variable(String),
    Binary(BinOp, Box<ExprAst>, Box<ExprAst>),
    Call(String, Vec<ExprAst>),
}

#[derive(Debug, Clone)]
pub enum StmtAst {
    Missing,
    Block(Vec<StmtAst>),
    IfThen(ExprAst, Box<StmtAst>),
    Expr(ExprAst),
    Return(ExprAst),
    FuncDef(String, Vec<String>, Box<StmtAst>),
}

impl Default for ExprAst {
    fn default() -> Self {
        Self::Missing
    }
}

impl Default for StmtAst {
    fn default() -> Self {
        Self::Missing
    }
}
