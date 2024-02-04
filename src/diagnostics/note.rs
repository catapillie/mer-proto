use colored::Colorize;

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

    pub fn msg(&self) -> String {
        match self {
            Self::Quiet
                => String::new(),
            Self::Numbered(num, note)
                => format!("{} {}", format!("[{num}]").bold(), note.msg()),
            Self::Then(note)
                => format!("then {}", note.msg()),
            Self::But(note)
                => format!("but {}", note.msg()),
            Self::So(note)
                => format!("so {}", note.msg()),
            Self::DDDotFront(note)
                => format!("...{}", note.msg()),
            Self::DDDotBack(note)
                => format!("{}...", note.msg()),
            Self::Here
                => "here".to_string(),
            Self::Unknown
                => "???".to_string(),
            Self::ExpectedToken(expected)
                => format!("expected {}", expected.to_string().bold()),
            Self::CanBeRemoved
                => "this can be removed".to_string(),
            Self::CanRemoveParentheses
                => "parentheses can be removed".to_string(),
            Self::FollowsIf
                => "this should follow an if statement".to_string(),
            Self::FollowsIfThen
                => "this should follow an if-then statement".to_string(),
            Self::MissingWhile
                => "this must be followed by a while guard".to_string(),
            Self::CannotAssign
                => "this cannot be assigned to".to_string(),
            Self::MustBeOfType(ty)
                => format!("this must be of type '{}'",
                    ty.to_string().bold()
                ),
            Self::OfType(ty)
                => format!("this is of type '{}'",
                    ty.to_string().bold(),
                ),
            Self::VariableDeclaration(name)
                => format!("variable '{}' is declared here", name.bold()),
            Self::VariableType(name, ty)
                => format!("variable '{}' has type '{}'",
                    name.bold(),
                    ty.to_string().bold(),
                ),
            Self::ArgumentType(name, ty)
                => format!("argument '{}' has type '{}'",
                    name.bold(),
                    ty.to_string().bold(),
                ),
            Self::NotFunction(ty)
                => format!("this is not a function, and is of type '{}'",
                    ty.to_string().bold(),
                ),
            Self::FunctionArgs(name, count)
                => format!("function '{}' takes in {} arguments",
                    name.bold(),
                    count.to_string().bold(),
                ),
            Self::FunctionReturnType(name, ty)
                => format!("function '{}' returns '{}'",
                    name.bold(),
                    ty.to_string().bold(),
                ),
            Self::FunctionVariableCount(count)
                => format!("uses {} variables",
                    count.to_string().bold(),
                ),
            Self::ProvidedArgs(count)
                => format!("{} were provided", count.to_string().bold()),
            Self::VariableCapturedBy(var_name, func_name)
                => format!("'{}' gets captured by '{}' here",
                    var_name.bold(),
                    func_name.bold(),
                ),
            Self::TupleValueCount(len)
                => format!("this tuple contains {} values",
                    len.to_string().bold(),
                ),
        }
    }
}
