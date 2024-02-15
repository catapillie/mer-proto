use crate::utils::Span;

pub trait TokenValue: Clone + Default {
    fn wrap(self, span: Span) -> Token;
    fn is_inside(token: &Token) -> bool;
    fn extract_from(token: &Token) -> Option<(Self, Span)>;
    fn kind() -> TokenKind;
}

gen_tokens! {
    Eof
    Newline

    LeftParen
    RightParen
    LeftBracket
    RightBracket
    LeftBrace
    RightBrace

    Dot
    Comma
    Colon

    Equal
    Plus
    PlusPlus
    Minus
    Star
    Slash
    Percent
    Ampersand
    Bar
    Caret
    EqualEqual
    NotEqual
    LessEqual
    LessThan
    GreaterEqual
    GreaterThan
    At

    RightArrow

    IfKw
    ThenKw
    ElseKw
    WhileKw
    DoKw
    VarKw
    FuncKw
    ReturnKw
    AndKw
    OrKw
    XorKw
    NotKw
    CaseKw
    DataKw
    OtherwiseKw
    AllocKw
    TodoKw
    UnreachableKw
    DebugKw
    PrintKw

    TrueKw
    FalseKw
    Identifier(String)
    Integer(i64)
    MalformedNumeral
    StringLit(String)
}

macro_rules! gen_tokens {
    (
      $(
        $name:ident $( ( $($type:ty),* ) )?
      )*
    ) => {
        #[derive(Debug, Clone)]
        pub enum Token {
            $(
                $name($name, Span)
            ),*
        }

        impl Token {
            pub fn span(&self) -> Span {
                match self {
                    $(
                        Self::$name(_, span) => *span
                    ),*
                }
            }
        }

        #[derive(Debug, Copy, Clone)]
        pub enum TokenKind {
            $(
                $name
            ),*
        }

        $(
            #[derive(Debug, Default, Clone)]
            pub struct $name $( ( $(pub $type),* ) )?;

            impl TokenValue for $name {
                fn wrap(self, span: Span) -> Token {
                    Token::$name(self, span)
                }

                fn is_inside(token: &Token) -> bool {
                    matches!(token, Token::$name(_, _))
                }

                fn extract_from(token: &Token) -> Option<(Self, Span)> {
                    if let Token::$name(value, span) = token {
                        Some((value.clone(), span.clone()))
                    } else {
                        None
                    }
                }

                fn kind() -> TokenKind {
                    TokenKind::$name
                }
            }

            impl $name {
                pub fn kind(&self) -> TokenKind {
                    TokenKind::$name
                }
            }
        )*
    };
}

use gen_tokens;
