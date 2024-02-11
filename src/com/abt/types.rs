use crate::diagnostics::TypeRepr;

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
    Tuple(Box<Type>, Box<[Type]>), // non-empty
    Array(Box<Type>, usize),
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

    #[rustfmt::skip]
    pub fn repr(&self) -> TypeRepr {
        use TypeRepr as Ty;
        match self {
            Self::Unknown => Ty::Unknown,
            Self::Never => Ty::Never,
            Self::Unit => Ty::Unit,
            Self::U8 => Ty::U8,
            Self::U16 => Ty::U16,
            Self::U32 => Ty::U32,
            Self::U64 => Ty::U64,
            Self::I8 => Ty::I8,
            Self::I16 => Ty::I16,
            Self::I32 => Ty::I32,
            Self::I64 => Ty::I64,
            Self::F32 => Ty::F32,
            Self::F64 => Ty::F64,
            Self::Bool => Ty::Bool,
            Self::Tuple(head, tail)
                => Ty::Tuple(
                    Box::new(head.repr()),
                    tail.iter().map(Self::repr).collect()
                ),
            Self::Array(ty, size)
                => Ty::Array(Box::new(ty.repr()), *size),
            Self::Ref(inner)
                => Ty::Ref(Box::new(inner.repr())),
            Self::Func(args, ty)
                => Ty::Func(
                    args.iter().map(Self::repr).collect(),
                    Box::new(ty.repr())
                ),
        }
    }
}
