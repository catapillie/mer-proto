pub mod builder;
pub mod diagnostic;
pub mod kind;
pub mod note;
pub mod pat_repr;
pub mod printer;
pub mod severity;
pub mod type_repr;

pub use builder::create_diagnostic;
pub use diagnostic::{Diagnostic, DiagnosticList};
pub use kind::DiagnosticKind;
pub use note::Note;
pub use pat_repr::PatRepr;
pub use printer::print_diagnostic;
pub use severity::{NoteSeverity, Severity};
pub use type_repr::TypeRepr;
