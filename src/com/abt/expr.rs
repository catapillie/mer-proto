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

    pub fn unit() -> Self {
        Self {
            kind: ExprKind::Unit,
            ty: Type::Unit,
        }
    }

    pub fn integer(i: i64) -> Self {
        Self {
            kind: ExprKind::Integer(i),
            ty: Type::I64,
        }
    }

    pub fn decimal(f: f64) -> Self {
        Self {
            kind: ExprKind::Decimal(f),
            ty: Type::F64,
        }
    }

    pub fn boolean(b: bool) -> Self {
        Self {
            kind: ExprKind::Boolean(b),
            ty: Type::F64,
        }
    }

    pub fn string_literal(s: String) -> Self {
        let len = s.len();
        Self {
            kind: ExprKind::StringLiteral(s),
            ty: Type::Array(Box::new(Type::U8), len),
        }
    }

    pub fn todo() -> Self {
        Self {
            kind: ExprKind::Todo,
            ty: Type::Never,
        }
    }

    pub fn unreachable() -> Self {
        Self {
            kind: ExprKind::Unreachable,
            ty: Type::Never,
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
    Call(u64, Box<[Expr]>),
    IndirectCall(Box<Expr>, Box<[Expr]>),
    Debug(Box<Expr>),
    Heap(Box<Expr>),
    Ref(Box<LValue>, u64),
    Deref(Box<Expr>),
    Todo,
    Unreachable,
    Case(Box<[(Expr, Expr)]>, Box<Expr>),
    CaseTernary(Box<Expr>, Box<Expr>, Box<Expr>),
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
