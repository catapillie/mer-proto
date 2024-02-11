use std::fmt::Display;

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

    fn fmt_paren(&self, f: &mut std::fmt::Formatter<'_>, paren: bool) -> std::fmt::Result {
        match self {
            Self::Unknown => write!(f, "unknown"),
            Self::Never => write!(f, "!"),
            Self::Unit => write!(f, "()"),
            Self::U8 => write!(f, "u8"),
            Self::U16 => write!(f, "u16"),
            Self::U32 => write!(f, "u32"),
            Self::U64 => write!(f, "u64"),
            Self::I8 => write!(f, "i8"),
            Self::I16 => write!(f, "i16"),
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::F32 => write!(f, "f32"),
            Self::F64 => write!(f, "f64"),
            Self::Bool => write!(f, "bool"),
            Self::Tuple(head, tail) => {
                write!(f, "({head}")?;
                for ty in tail.iter() {
                    write!(f, ", ")?;
                    ty.fmt_paren(f, false)?;
                }
                write!(f, ")")?;
                Ok(())
            }
            Self::Array(ty, size) => {
                write!(f, "[{size}]")?;
                ty.fmt_paren(f, true)?;
                Ok(())
            }
            Self::Ref(inner) => {
                write!(f, "&")?;
                inner.fmt_paren(f, true)
            }
            Self::Func(args, to) => {
                if paren {
                    write!(f, "(")?;
                }
                if let Some((head, tail)) = args.split_first() {
                    head.fmt_paren(f, true)?;
                    for arg in tail {
                        write!(f, ", ")?;
                        arg.fmt_paren(f, true)?;
                    }
                    write!(f, " -> {to}")?;
                } else {
                    write!(f, "-> {to}")?;
                }
                if paren {
                    write!(f, ")")?;
                }
                Ok(())
            }
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_paren(f, false)
    }
}
