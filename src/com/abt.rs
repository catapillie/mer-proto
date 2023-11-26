#[derive(Debug)]
pub enum StmtAbt {
    Empty,
    VarDef(String, ExprAbt),
    Block(Vec<StmtAbt>),
    Expr(Box<ExprAbt>),
    IfThen(Box<ExprAbt>, Box<StmtAbt>),
    IfThenElse(Box<ExprAbt>, Box<StmtAbt>, Box<StmtAbt>),
    WhileDo(Box<ExprAbt>, Box<StmtAbt>),
    DoWhile(Box<StmtAbt>, Box<ExprAbt>),
}

#[derive(Debug)]
pub enum ExprAbt {
    Unknown,
    Number(f64),
    Boolean(bool),
    Variable(TypeAbt),
}

impl ExprAbt {
    pub fn ty(&self) -> TypeAbt {
        match self {
            ExprAbt::Unknown => TypeAbt::Unknown,
            ExprAbt::Number(_) => TypeAbt::Number,
            ExprAbt::Boolean(_) => TypeAbt::Boolean,
            ExprAbt::Variable(ty) => ty.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeAbt {
    Unknown,
    Number,
    Boolean,
}

impl TypeAbt {
    /// Determines whether [`self`] is of the specified type.
    /// If [`self`] is [`TypeAbt::Unknown`], then the check is true.
    pub fn is(&self, ty: &Self) -> bool {
        match self {
            TypeAbt::Unknown => true, // ignored
            TypeAbt::Number => matches!(ty, TypeAbt::Number),
            TypeAbt::Boolean => matches!(ty, TypeAbt::Boolean),
        }
    }
}
