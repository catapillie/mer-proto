use super::Size;
use crate::utils::{Span, Spanned};

pub type Pattern = Spanned<PatternKind>;

pub enum PatternKind {
    Discard,
    Binding(String),
    OpaqueTypeConstructor(u64, Box<[Pattern]>),
    Unit,
    Tuple(Box<Pattern>, Box<[Pattern]>),
    Array(Box<[Pattern]>),
    Ref(Box<Pattern>),
}

impl PatternKind {
    pub fn wrap(self, span: Span) -> Pattern {
        Spanned { span, value: self }
    }
}

#[derive(Debug)]
pub enum BoundPattern {
    Bad,
    Discard { len: Size },
    Loc { id: u64 },
    Seq(Box<[BoundPattern]>),
    Ref { pat: Box<BoundPattern>, len: Size },
}
