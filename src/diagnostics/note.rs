use crate::com::{tokens::TokenKind, TypeAbt};

#[derive(Debug, Clone)]
pub enum Note {
    Numbered(usize, Box<Note>),
    Then(Box<Note>),
    But(Box<Note>),
    So(Box<Note>),
    DDDotFront(Box<Note>),
    DDDotBack(Box<Note>),
    Quiet,
    Here,
    Unknown,
    ExpectedToken(TokenKind),
    CanBeRemoved,
    CanRemoveParentheses,
    FollowsIf,
    FollowsIfThen,
    MissingWhile,
    CannotAssign,
    MustBeOfType(TypeAbt),
    OfType(TypeAbt),
    Type(TypeAbt),
    VariableDeclaration(String),
    VariableType(String, TypeAbt),
    ArgumentType(String, TypeAbt),
    NotFunction(TypeAbt),
    FunctionArgs(String, usize),
    FunctionReturnType(String, TypeAbt),
    FunctionVariableCount(usize),
    ProvidedArgs(usize),
    VariableCapturedBy(String, String),
    TupleValueCount(usize),
    KnownIndexTooLarge,
    CanBeImmediateIndex(usize),
}

#[rustfmt::skip]
impl Note {
    pub fn num(self, num: usize) -> Self {
        Note::Numbered(num, Box::new(self))
    }

    pub fn then(self) -> Self {
        Note::Then(Box::new(self))
    }

    pub fn but(self) -> Self {
        Note::But(Box::new(self))
    }

    pub fn so(self) -> Self {
        Note::So(Box::new(self))
    }

    pub fn dddot_front(self) -> Self {
        Note::DDDotFront(Box::new(self))
    }

    pub fn dddot_back(self) -> Self {
        Note::DDDotBack(Box::new(self))
    }
}
