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
