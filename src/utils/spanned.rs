use super::Span;

#[derive(Debug, Clone, Copy)]
pub struct Spanned<T> {
    pub span: Span,
    pub value: T,
}
