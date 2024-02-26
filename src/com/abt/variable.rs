use super::Type;
use crate::utils::Spanned;

pub struct VariableInfo {
    pub id: u64,
    pub name: Spanned<String>,
    pub depth: u16,
    pub ty: Type,
    pub is_on_heap: bool,
}

pub struct VariableUsage {
    pub captured: bool,
    pub used: bool,
}
