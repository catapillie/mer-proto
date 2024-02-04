#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}

#[derive(Debug, Copy, Clone)]
pub enum NoteSeverity {
    Default,
    Annotation,
}
