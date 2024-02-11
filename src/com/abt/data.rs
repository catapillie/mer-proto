use super::Type;
use crate::utils::Spanned;

pub struct DataInfo {
    pub name: Spanned<String>,
    pub id: u64,
    pub fields: Vec<(Spanned<String>, Spanned<Type>)>,
}
