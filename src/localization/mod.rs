use crate::{
    com::tokens::{Token, TokenKind},
    diagnostics::{DiagnosticKind, Note, Severity},
};

pub trait Lang {
    fn token_kind_str(&self, kind: &TokenKind) -> &str;
    fn token_str(&self, token: &Token) -> String;
    fn severity_msg(&self, severity: &Severity) -> &str;
    fn diagnostic_msg(&self, kind: &DiagnosticKind) -> String;
    fn note_msg(&self, note: &Note) -> String;
}

pub mod english;
pub use english::English;
pub mod french;
pub use french::French;
