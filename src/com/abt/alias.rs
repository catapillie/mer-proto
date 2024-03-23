use super::Type;
use crate::utils::Spanned;

pub struct AliasInfo {
    pub name: Spanned<String>,
    pub ty: Type,
    pub is_opaque: bool,
    pub constructor: Option<u64>,
}
