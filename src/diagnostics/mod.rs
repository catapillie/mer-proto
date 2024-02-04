pub mod builder;
pub mod diagnostic;
pub mod kind;
pub mod note;
pub mod printer;
pub mod severity;

pub use builder::create_diagnostic;
pub use diagnostic::{Diagnostic, DiagnosticList};
pub use kind::DiagnosticKind;
pub use note::Note;
pub use printer::print_diagnostic;
pub use severity::{NoteSeverity, Severity};
