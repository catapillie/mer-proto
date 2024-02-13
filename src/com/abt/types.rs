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
    Tuple(Box<Type>, Box<[Type]>), // non-empty
    Array(Box<Type>, usize),
    Pointer(Box<Type>),
    Ref(Box<Type>),
    Func(Box<[Type]>, Box<Type>),
}

impl Type {
    /// Determines whether [`self`] is of the specified type.
    /// If [`self`] is [`TypeAbt::Unknown`], then the check is true.
    pub fn is(&self, ty: &Self) -> bool {
        if !self.is_known() || !ty.is_known() {
            true
        } else {
            matches!(self, Self::Never) || self == ty
        }
    }

    pub fn is_known(&self) -> bool {
        match self {
            Type::Unknown => false,
            Type::Ref(inner) => inner.is_known(),
            Type::Func(args, ty) => args.iter().all(Self::is_known) && ty.is_known(),
            _ => true,
        }
    }
}
