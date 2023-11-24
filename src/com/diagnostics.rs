use super::{
    pos::Pos,
    tokens::{Token, TokenKind},
};

pub struct DiagnosticBuilder {
    diagnostics: Vec<Diagnostic>,
}

impl DiagnosticBuilder {
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
        }
    }

    pub fn push(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    pub fn done(self) -> Vec<Diagnostic> {
        self.diagnostics
    }
}

#[derive(Debug, Clone)]
pub enum Diagnostic {
    IllegalCharacter { ill: char, at: Pos },
    ExpectedToken { found: Token, expected: TokenKind },
    ExpectedExpression { at: Pos },
    ExpectedStatement { at: Pos },
}

#[rustfmt::skip]
impl Diagnostic {
    pub fn msg(&self) -> String {
        match self {
            Diagnostic::IllegalCharacter { ill, at: _ }
                => format!("encountered illegal character {ill:?}"),
            Diagnostic::ExpectedToken { found, expected }
                => format!("expected {expected}, but found {found}"),
            Diagnostic::ExpectedExpression { at: _ }
                => "expected an expression".to_string(),
            Diagnostic::ExpectedStatement { at: _ }
                => "expected a statement".to_string(),
        }
    }
}
