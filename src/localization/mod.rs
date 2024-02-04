use crate::diagnostics::{DiagnosticKind, Note, Severity};

pub trait Lang {
    fn severity_msg(&self, severity: &Severity) -> &str;
    fn diagnostic_msg(&self, kind: &DiagnosticKind) -> String;
    fn note_msg(&self, note: &Note) -> String;
}

pub mod english;
pub use english::English;
