use std::fmt::Display;

#[rustfmt::skip]
#[derive(Debug, Clone)]
pub enum TypeRepr {
    Unknown,
    Never, // bottom type
    Unit,
    U8, U16, U32, U64,
    I8, I16, I32, I64,
    F32, F64,
    Bool,
    Tuple(Box<TypeRepr>, Box<[TypeRepr]>),
    Array(Box<TypeRepr>, usize),
    Ref(Box<TypeRepr>),
    Func(Box<[TypeRepr]>, Box<TypeRepr>),
}

impl Display for TypeRepr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn fmt_paren(
            ty: &TypeRepr,
            f: &mut std::fmt::Formatter<'_>,
            paren: bool,
        ) -> std::fmt::Result {
            use TypeRepr as Ty;
            match ty {
                Ty::Unknown => write!(f, "unknown"),
                Ty::Never => write!(f, "!"),
                Ty::Unit => write!(f, "()"),
                Ty::U8 => write!(f, "u8"),
                Ty::U16 => write!(f, "u16"),
                Ty::U32 => write!(f, "u32"),
                Ty::U64 => write!(f, "u64"),
                Ty::I8 => write!(f, "i8"),
                Ty::I16 => write!(f, "i16"),
                Ty::I32 => write!(f, "i32"),
                Ty::I64 => write!(f, "i64"),
                Ty::F32 => write!(f, "f32"),
                Ty::F64 => write!(f, "f64"),
                Ty::Bool => write!(f, "bool"),
                Ty::Tuple(head, tail) => {
                    write!(f, "({head}")?;
                    for ty in tail.iter() {
                        write!(f, ", ")?;
                        fmt_paren(ty, f, false)?;
                    }
                    write!(f, ")")?;
                    Ok(())
                }
                Ty::Array(ty, size) => {
                    write!(f, "[{size}]")?;
                    fmt_paren(ty, f, true)?;
                    Ok(())
                }
                Ty::Ref(inner) => {
                    write!(f, "&")?;
                    fmt_paren(inner, f, true)
                }
                Ty::Func(args, to) => {
                    if paren {
                        write!(f, "(")?;
                    }
                    if let Some((head, tail)) = args.split_first() {
                        fmt_paren(head, f, true)?;
                        for arg in tail {
                            write!(f, ", ")?;
                            fmt_paren(arg, f, true)?;
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

        fmt_paren(self, f, false)
    }
}
