use super::{DiagnosticKind, Note, NoteSeverity, Severity};
use crate::utils::Span;

/// Alias for a vector of diagnotics.
pub type DiagnosticList = Vec<Diagnostic>;

#[derive(Debug)]
pub struct Diagnostic {
    pub kind: DiagnosticKind,
    pub severity: Severity,
    pub span: Option<Span>,
    pub annotations: Vec<(Span, Note, NoteSeverity)>,
    pub highlights: Vec<Span>,
}
