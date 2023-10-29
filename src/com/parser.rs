use super::{cursor::Cursor, pos::Pos, span::Span, tokens::*};

pub struct Parser<'src> {
    cursor: Cursor<'src>,
    lookahead: Token,
}

impl<'src> Parser<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            cursor: Cursor::new(source),
            lookahead: Token::Eof(Eof, Span::at(Pos::default())),
        }
    }

    fn consume_token(&mut self) -> Token {
        let next = self.lex();
        let seen = std::mem::replace(&mut self.lookahead, next);
        seen
    }

    fn try_consume_string(&mut self, string: &str) -> Option<Span> {
        let mut clone = self.cursor.clone();
        let start_pos = self.cursor.pos();

        if clone.by_ref().take(string.len()).eq(string.chars()) {
            let span = Span::new(start_pos, clone.pos());
            self.cursor = clone;
            Some(span)
        } else {
            None
        }
    }

    fn is_identifier_head(c: char) -> bool {
        c.is_alphabetic()
    }

    fn is_identifier_tail(c: char) -> bool {
        c.is_alphanumeric() || c == '_'
    }

    fn is_newline_character(c: char) -> bool {
        c == '\n' || c == '\r'
    }

    fn try_consume_identifier(&mut self) -> Option<(String, Span)> {
        let start_pos = self.cursor.pos();

        let Some(c) = self.cursor.peek() else {
            return None;
        };

        if !Self::is_identifier_head(c) {
            return None;
        }

        let mut identifier = c.to_string();
        self.cursor.next();

        while let Some(c) = self.cursor.peek() {
            if !Self::is_identifier_tail(c) {
                break;
            }

            identifier.push(c);
            self.cursor.next();
        }

        let end_pos = self.cursor.pos();

        Some((identifier, Span::new(start_pos, end_pos)))
    }

    fn try_consume_number(&mut self) -> Option<(f64, Span)> {
        let mut number = String::new();
        let start_pos = self.cursor.pos();

        while let Some(c) = self.cursor.peek() {
            if !c.is_ascii_digit() {
                break;
            }

            number.push(c);
            self.cursor.next();
        }

        if let Some('.') = self.cursor.peek() {
            self.cursor.next();
            number.push('.');
        }

        while let Some(c) = self.cursor.peek() {
            if !c.is_ascii_digit() {
                break;
            }

            number.push(c);
            self.cursor.next();
        }

        if number.is_empty() {
            return None;
        }

        let end_pos = self.cursor.pos();
        let span = Span::new(start_pos, end_pos);

        Some(match number.parse() {
            Ok(num) => (num, span),
            Err(_) => (f64::NAN, span),
        })
    }

    fn try_consume_newlines(&mut self) -> bool {
        if let Some(c) = self.cursor.peek() {
            if !Self::is_newline_character(c) {
                return false;
            }
        }

        while let Some(c) = self.cursor.peek() {
            if c.is_whitespace() {
                self.cursor.next();
                continue;
            }

            break;
        }

        true
    }

    fn lex(&mut self) -> Token {
        loop {
            loop {
                let Some(c) = self.cursor.peek() else {
                    return Eof.wrap(Span::EOF);
                };

                if c.is_whitespace() && !Self::is_newline_character(c) {
                    self.cursor.next();
                    continue;
                }

                break;
            }

            if self.cursor.peek().is_none() {
                return Eof.wrap(Span::EOF);
            }

            let start_pos = self.cursor.pos();

            if self.try_consume_newlines() {
                return Newline.wrap(Span::at(start_pos));
            }

            match_by_string!(self, "==" => Eq);
            match_by_string!(self, "!=" => Neq);
            match_by_string!(self, "<=" => Le);
            match_by_string!(self, "<" => Lt);
            match_by_string!(self, ">=" => Ge);
            match_by_string!(self, ">" => Gt);
            match_by_string!(self, "(" => LeftParen);
            match_by_string!(self, ")" => RightParen);
            match_by_string!(self, "{" => LeftBrace);
            match_by_string!(self, "}" => RightBrace);
            match_by_string!(self, "," => Comma);
            match_by_string!(self, "=" => Equal);
            match_by_string!(self, "+" => Plus);
            match_by_string!(self, "-" => Minus);
            match_by_string!(self, "*" => Star);
            match_by_string!(self, "/" => Slash);
            match_by_string!(self, "%" => Mod);
            match_by_string!(self, "&" => Amper);
            match_by_string!(self, "|" => Bar);
            match_by_string!(self, "^" => Caret);

            if let Some((num, span)) = self.try_consume_number() {
                return Num(num).wrap(span);
            }

            if let Some((id, span)) = self.try_consume_identifier() {
                return match id.as_str() {
                    "if" => IfKw.wrap(span),
                    "then" => ThenKw.wrap(span),
                    "func" => FuncKw.wrap(span),
                    "return" => ReturnKw.wrap(span),
                    "true" => TrueLit.wrap(span),
                    "false" => FalseLit.wrap(span),
                    "and" => AndKw.wrap(span),
                    "or" => OrKw.wrap(span),
                    "not" => NotKw.wrap(span),
                    _ => Ident(id).wrap(span),
                };
            }

            match self.cursor.peek() {
                Some(u) => {
                    self.cursor.next();
                    continue;
                }
                None => return Eof.wrap(Span::EOF),
            }
        }
    }
}

macro_rules! match_by_string {
    ($self:ident, $string:literal => $token:ident) => {
        if let Some(span) = $self.try_consume_string($string) {
            return $token.wrap(span);
        }
    };
}

use match_by_string;
