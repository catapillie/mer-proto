pub type ProgramAst = Vec<StmtAst>;

#[derive(Debug)]
pub enum StmtAst {
    Empty,
    VarDef(Option<String>, ExprAst),
    Expr(ExprAst),
    Block(Vec<StmtAst>),
    IfThen(ExprAst, Box<StmtAst>),
    Then(Box<StmtAst>),
    IfThenElse(ExprAst, Box<StmtAst>, Box<StmtAst>),
    Else(Box<StmtAst>),
    WhileDo(ExprAst, Box<StmtAst>),
    DoWhile(Box<StmtAst>, ExprAst),
    Do(Box<StmtAst>),
    Func(Option<String>, Vec<String>, Box<StmtAst>, TypeAst),
    Return,
    ReturnWith(ExprAst),
}

#[derive(Debug)]
pub enum ExprAst {
    Bad,
    Number(f64),
    Identifier(String),
    Boolean(bool),
    BinaryOp(BinaryOperator, Box<ExprAst>, Box<ExprAst>),
    UnaryOp(UnaryOperator, Box<ExprAst>),
    Call(String, Vec<ExprAst>),
}

#[derive(Debug)]
pub enum TypeAst {
    Bad,
    Unit,
    Declared(String),
}

#[derive(Debug)]
pub enum UnaryOperator {
    Pos,
    Neg,
    Not,
}

#[derive(Debug, Copy, Clone)]
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

pub type Precedence = u8;

#[derive(Debug)]
pub enum Associativity {
    Left,
    Right,
}
