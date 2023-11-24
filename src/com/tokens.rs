use std::fmt::Display;

use super::span::Span;

pub trait TokenValue: Clone + Default + Display {
    fn wrap(self, span: Span) -> Token;
    fn is_inside(token: &Token) -> bool;
    fn extract_from(token: &Token) -> Option<(Self, Span)>;
    fn kind() -> TokenKind;
    fn display() -> &'static str;
}

gen_tokens! {
    Eof "end-of-file"
        self => ("end-of-file"),

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
    Percent "%"
        self => ("%"),
    Ampersand "&"
        self => ("&"),
    Bar "|"
        self => ("|"),
    Caret "^"
        self => ("^"),
    EqualEqual "=="
        self => ("=="),
    NotEqual "!="
        self => ("!="),
    LessEqual "<="
        self => ("<="),
    LessThan "<"
        self => ("<"),
    GreaterEqual ">="
        self => (">="),
    GreaterThan ">"
        self => ("<"),

    IfKw "'if' keyword"
        self => ("'if' keyword"),
    ThenKw "'then' keyword"
        self => ("'then' keyword"),
    ElseKw "'else' keyword"
        self => ("'else' keyword"),
    WhileKw "'whild' keyword"
        self => ("'whild' keyword"),
    DoKw "'do' keyword"
        self => ("'do' keyword"),
    VarKw "'var' keyword"
        self => ("'var' keyword"),
    FuncKw "'func' keyword"
        self => ("'func' keyword"),
    ReturnKw "'return' keyword"
        self => ("'return' keyword"),
    AndKw "'and' keyword"
        self => ("'and' keyword"),
    OrKw "'or' keyword"
        self => ("'or' keyword"),
    NotKw "'not' keyword"
        self => ("'not' keyword"),
    TrueKw "'true' literal"
        self => ("'true' literal"),
    FalseKw "'false' literal"
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

        #[derive(Debug, Copy, Clone)]
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
