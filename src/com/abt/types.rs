#[rustfmt::skip]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Unknown,
    Never, // bottom type
    Unit,
    U8, U16, U32, U64,
    I8, I16, I32, I64,
    F32, F64,
    Bool,
    Data(u64),
    Alias(u64),
    Tuple(Box<Type>, Box<[Type]>), // non-empty
    Array(Box<Type>, usize),
    Pointer(Box<Type>),
    Ref(Box<Type>),
    Func(Box<[Type]>, Box<Type>),
}

impl Type {
    pub fn is_known(&self) -> bool {
        match self {
            Type::Unknown => false,
            Type::Never => true,
            Type::Unit => true,
            Type::U8 => true,
            Type::U16 => true,
            Type::U32 => true,
            Type::U64 => true,
            Type::I8 => true,
            Type::I16 => true,
            Type::I32 => true,
            Type::I64 => true,
            Type::F32 => true,
            Type::F64 => true,
            Type::Bool => true,
            Type::Data(_) => true,
            Type::Alias(_) => true,
            Type::Tuple(head, tail) => head.is_known() && tail.iter().all(Self::is_known),
            Type::Array(ty, _) => ty.is_known(),
            Type::Pointer(ty) => ty.is_known(),
            Type::Ref(ty) => ty.is_known(),
            Type::Func(args, ty) => args.iter().all(Self::is_known) || ty.is_known(),
        }
    }
}
