use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum PatRepr {
    Discard,
    Binding(String),
    Unit,
    Tuple(Box<PatRepr>, Box<[PatRepr]>),
}

impl Display for PatRepr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PatRepr::Discard => write!(f, "_"),
            PatRepr::Binding(name) => write!(f, "{}", name),
            PatRepr::Unit => write!(f, "()"),
            PatRepr::Tuple(head, tail) => {
                write!(f, "({head}")?;
                for ty in tail.iter() {
                    write!(f, ", ")?;
                    ty.fmt(f)?;
                }
                write!(f, ")")?;
                Ok(())
            }
        }
    }
}
