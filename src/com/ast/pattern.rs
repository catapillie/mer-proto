use crate::utils::{Span, Spanned};

pub type Pattern = Spanned<PatternKind>;

#[derive(Debug)]
pub enum PatternKind {
    Bad,
    Discard,
    Binding(String),
    Constructor(Spanned<String>, Box<[Pattern]>),
    Unit,
    Parenthesized(Box<Pattern>),
    Tuple(Box<Pattern>, Box<[Pattern]>),
    Array(Box<[Pattern]>),
    Ref(Box<Pattern>),
}

impl PatternKind {
    pub fn wrap(self, span: Span) -> Pattern {
        Spanned { span, value: self }
    }
}
