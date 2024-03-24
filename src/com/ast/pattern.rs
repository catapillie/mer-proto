use crate::utils::{Span, Spanned};

pub type Pattern = Spanned<PatternKind>;

#[derive(Debug)]
pub enum PatternKind {
    Bad,
    Discard,
    Binding(String),
    Unit,
    Parenthesized(Box<Pattern>),
    Tuple(Box<Pattern>, Box<[Pattern]>),
}

impl PatternKind {
    pub fn wrap(self, span: Span) -> Pattern {
        Spanned { span, value: self }
    }
}
