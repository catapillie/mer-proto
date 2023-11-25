#[derive(Debug)]
pub enum StmtAbt {
    Empty,
    Expr(Box<ExprAbt>),
    IfThen(Box<ExprAbt>, Box<StmtAbt>),
}

#[derive(Debug)]
pub enum ExprAbt {
    Unknown,
    Unit,
    Number(f64),
    Boolean(bool),
}

impl ExprAbt {
    pub fn ty(&self) -> TypeAbt {
        match self {
            ExprAbt::Unknown => TypeAbt::Unknown,
            ExprAbt::Unit => TypeAbt::Unit,
            ExprAbt::Number(_) => TypeAbt::Number,
            ExprAbt::Boolean(_) => TypeAbt::Boolean,
        }
    }
}

#[derive(Debug)]
pub enum TypeAbt {
    Unknown,
    Unit,
    Number,
    Boolean,
}
