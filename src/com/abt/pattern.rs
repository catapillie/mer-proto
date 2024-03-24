use crate::utils::{Span, Spanned};

pub type Pattern = Spanned<PatternKind>;

pub enum PatternKind {
    Discard,
    Binding(String),
}

impl PatternKind {
    pub fn wrap(self, span: Span) -> Pattern {
        Spanned { span, value: self }
    }
}
