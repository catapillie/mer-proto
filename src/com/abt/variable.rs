use super::Type;
use crate::utils::Span;

pub struct VariableInfo {
    pub id: u64,
    pub name: String,
    pub depth: u16,
    pub ty: Type,
    pub declaration_span: Span,
    pub is_on_heap: bool,
}

pub struct VariableUsage {
    pub captured: bool,
    pub used: bool,
}
