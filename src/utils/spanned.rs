use super::Span;

#[derive(Debug, Default, Clone, Copy)]
pub struct Spanned<T> {
    pub span: Span,
    pub value: T,
}
