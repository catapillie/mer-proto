use super::{BinOp, LValue, Type, UnOp};

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Type,
}

impl Expr {
    pub fn unknown() -> Self {
        Self {
            kind: ExprKind::Unknown,
            ty: Type::Unknown,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Unknown,
    Unit,
    Integer(i64),
    Decimal(f64),
    Boolean(bool),
    StringLiteral(String),
    Variable(u64),
    Function(u64),
    OpaqueConstructor {
        ctor_id: u64,
        alias_id: u64,
    },
    Tuple(Box<Expr>, Box<[Expr]>),
    TupleImmediateIndex(Box<Expr>, usize),
    Array(Box<[Expr]>),
    ArrayImmediateIndex(Box<Expr>, usize),
    ArrayIndex(Box<Expr>, Box<Expr>),
    PointerIndex(Box<Expr>, Box<Expr>),
    Assignment {
        assignee: LValue,
        var_id: u64,
        expr: Box<Expr>,
    },
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnOp, Box<Expr>),
    Call(u64, Box<[Expr]>, Type),
    IndirectCall(Box<Expr>, Box<[Expr]>, Type),
    Debug(Box<Expr>, Type),
    Heap(Box<Expr>),
    Ref(Box<LValue>, u64, Box<Type>),
    Deref(Box<Expr>),
    Todo,
    Unreachable,
    Case(Box<[(Expr, Expr)]>, Box<Expr>, Type),
    CaseTernary(Box<Expr>, Box<Expr>, Box<Expr>, Type),
    Data(u64, Box<[Expr]>),
    DataWith(u64, Box<Expr>, Box<[(usize, Expr)]>),
    FieldAccess {
        expr: Box<Expr>,
        data_id: u64,
        field_id: usize,
    },
    Alloc(Box<Type>, Box<Expr>),
    ToPointer(Box<Expr>),
}
