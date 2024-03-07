use super::TypeRepr;
use crate::com::tokens::TokenKind;

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
    MustBeOfType(TypeRepr),
    OfType(TypeRepr),
    OfTypeButShouldBe(TypeRepr, TypeRepr),
    Type(TypeRepr),
    ReturnsUnit,
    VariableDeclaration(String),
    VariableType(String, TypeRepr),
    ArgumentType(String, TypeRepr),
    NotFunction(TypeRepr),
    FunctionArgs(String, usize),
    FunctionReturnType(String, TypeRepr),
    FunctionVariableCount(usize),
    ProvidedArgs(usize),
    VariableCapturedBy(String, String),
    TupleValueCount(usize),
    KnownIndexTooLarge,
    CanBeImmediateIndex(usize),
    ArrayLength(usize),
    FieldSet(String),
    FieldSetAgain(String),
    FieldType(String, TypeRepr),
    MissingFields(Box<[String]>, String),
    NotDataStructure(TypeRepr),
    DiscardedDataStructure,
    UnmodifiedDataStructure,
    InnerTypesMismatch {
        inner_left: TypeRepr,
        inner_right: TypeRepr,
    },
    MoreThanOneOtherwisePath,
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
