pub type ProgramAst = Vec<StmtAst>;

#[derive(Debug)]
pub enum StmtAst {
    Empty,
    Expr(ExprAst),
    Block(Vec<StmtAst>),
    IfThen(ExprAst, Box<StmtAst>),
    Then(Box<StmtAst>),
    IfThenElse(ExprAst, Box<StmtAst>, Box<StmtAst>),
    Else(Box<StmtAst>),
    Return,
    ReturnWith(ExprAst),
}

#[derive(Debug)]
pub enum ExprAst {
    Bad,
    Num(f64),
    Ident(String),
    Boolean(bool),
}
