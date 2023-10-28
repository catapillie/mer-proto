use std::fmt::Display;

use super::span::Span;

pub trait TokenValue: Clone + Default + Display {
    fn wrap(self, span: Span) -> Token;
    fn extract(token: &Token) -> Option<(Self, Span)>;
    fn kind() -> TokenKind;
    fn display() -> &'static str;
}

gen_tokens! {
    Eof "end-of-file"
        self => ("end-of-file"),
    Illegal(char) "illegal character"
        self => ("illegal character {:?}", self.0),

    Newline "newline"
        self => ("newline"),

    LeftParen "("
        self => ("("),
    RightParen ")"
        self => (")"),
    LeftBrace "{{"
        self => ("{{"),
    RightBrace "}}"
        self => ("}}"),

    Comma ","
        self => (","),

    Equal "="
        self => ("="),
    Plus "+"
        self => ("+"),
    Minus "-"
        self => ("-"),
    Star "*"
        self => ("*"),
    Slash "/"
        self => ("/"),
    Mod "%"
        self => ("%"),
    Amper "&"
        self => ("&"),
    Bar "|"
        self => ("|"),
    Caret "^"
        self => ("^"),
    Eq "=="
        self => ("=="),
    Neq "!="
        self => ("!="),
    LtEq "<="
        self => ("<="),
    Lt "<"
        self => ("<"),
    GtEq ">="
        self => (">="),
    Gt ">"
        self => ("<"),

    IfKeyword "'if' keyword"
        self => ("'if' keyword"),
    ThenKeyword "'then' keyword"
        self => ("'then' keyword"),
    FuncKeyword "'func' keyword"
        self => ("'func' keyword"),
    ReturnKeyword "'return' keyword"
        self => ("'return' keyword"),
    AndKeyword "'and' keyword"
        self => ("'and' keyword"),
    OrKeyword "'or' keyword"
        self => ("'or' keyword"),
    NotKeyword "'not' keyword"
        self => ("'not' keyword"),

    TrueLiteral "'true' literal"
        self => ("'true' literal"),
    FalseLiteral "'false' literal"
        self => ("'false' literal"),

    Identifier(String) "identifier"
        self => ("identifier '{}'", self.0),
    Number(f64) "number"
        self => ("{}", self.0),
}

macro_rules! gen_tokens {
    (
      $(
        $name:ident $( ( $($type:ty),* ) )?
        $display:literal $self:ident => ($( $format_arg:expr ),+)
      ),* $(,)?
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

        impl Display for Token {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        Self::$name(value, _) => value.fmt(f),
                    )*
                }
            }
        }

        #[derive(Debug)]
        pub enum TokenKind {
            $(
                $name
            ),*
        }

        impl Display for TokenKind {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        Self::$name => write!(f, $display),
                    )*
                }
            }
        }

        $(
            #[derive(Debug, Default, Clone)]
            pub struct $name $( ( $(pub $type),* ) )?;

            impl TokenValue for $name {
                fn wrap(self, span: Span) -> Token {
                    Token::$name(self, span)
                }

                fn extract(token: &Token) -> Option<(Self, Span)> {
                    if let Token::$name(value, span) = token {
                        Some((value.clone(), span.clone()))
                    } else {
                        None
                    }
                }

                fn kind() -> TokenKind {
                    TokenKind::$name
                }

                fn display() -> &'static str { $display }
            }

            impl Display for $name {
                fn fmt(&$self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{}", format!{$($format_arg),+})
                }
            }
        )*
    };
}

use gen_tokens;
