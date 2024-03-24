use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum PatRepr {
    Discard,
    Binding(String),
    OpaqueTypeConstructor(String, Box<[PatRepr]>),
    Unit,
    Tuple(Box<PatRepr>, Box<[PatRepr]>),
    Array(Box<[PatRepr]>),
    Ref(Box<PatRepr>),
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
                    write!(f, ", {ty}")?;
                }
                write!(f, ")")?;
                Ok(())
            }
            PatRepr::Array(pats) => match pats.split_first() {
                Some((head, tail)) => {
                    write!(f, "[{head}")?;
                    for ty in tail.iter() {
                        write!(f, ", {ty}")?;
                    }
                    write!(f, "]")?;
                    Ok(())
                }
                None => write!(f, "[]"),
            },
            PatRepr::Ref(pat) => write!(f, "&{pat}"),
            PatRepr::OpaqueTypeConstructor(name, pats) => match pats.split_first() {
                Some((head, tail)) => {
                    write!(f, "{name}({head}")?;
                    for ty in tail.iter() {
                        write!(f, ", {ty}")?;
                    }
                    write!(f, ")")?;
                    Ok(())
                }
                None => write!(f, "{name}()"),
            },
        }
    }
}
