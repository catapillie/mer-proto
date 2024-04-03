use crate::com::abt::Type;

#[derive(Debug)]
pub enum Tc {
    Ok,
    Seq(Box<[Tc]>),
    Mismatch { left: Type, right: Type },
    TupleSizeMismatch { left: usize, right: usize },
    ArraySizeMismatch { left: usize, right: usize },
    ArgCountMismatch { left: usize, right: usize },
}

impl Tc {
    pub fn is_ok(&self) -> bool {
        match self {
            Tc::Ok => true,
            Tc::Seq(tcs) => tcs.iter().all(Self::is_ok),
            _ => false,
        }
    }
    pub fn is_err(&self) -> bool {
        !self.is_ok()
    }
}
