use super::Span;

#[derive(Debug, Default, Clone, Copy)]
pub struct Spanned<T> {
    pub span: Span,
    pub value: T,
}

#[derive(Debug, Default, Clone, Copy)]
pub struct OptSpanned<T> {
    pub span: Option<Span>,
    pub value: T,
}

impl<T> From<Spanned<T>> for OptSpanned<T> {
    fn from(spanned: Spanned<T>) -> Self {
        Self {
            value: spanned.value,
            span: Some(spanned.span),
        }
    }
}
