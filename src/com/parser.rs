use super::{
    ast::{ExprAst, ProgramAst, StmtAst},
    cursor::Cursor,
    errors::ParseError,
    span::Span,
    tokens::*,
};

pub struct Parser<'src> {
    cursor: Cursor<'src>,
    look_ahead: Token,
    last_token: Token,
    errors: Vec<ParseError>,
}

impl<'src> Parser<'src> {
    pub fn init(source: &'src str) -> Self {
        let mut parser = Self {
            cursor: Cursor::new(source),
            look_ahead: Token::Eof(Eof, Span::EOF),
            last_token: Token::Eof(Eof, Span::EOF),
            errors: vec![],
        };
        parser.look_ahead = parser.lex();
        parser
    }

    pub fn parse_program(mut self) -> (ProgramAst, Vec<ParseError>) {
        let mut stmts = Vec::new();

        loop {
            while let Some(stmt) = self.parse_statement() {
                stmts.push(stmt);
                if self.match_token_or_eof::<Newline>().is_none() {
                    self.recover_to_next_statement();
                }
            }

            match self.try_match_token::<Eof>() {
                Some(_) => break,
                None => {
                    self.errors.push(ParseError::ExpectedStatement);
                    self.recover_to_next_statement();
                }
            }
        }

        (stmts, self.errors)
    }

    fn recover_to_next_statement(&mut self) {
        loop {
            if self.found_eof()
                || self.try_match_token::<Newline>().is_some()
                || self.is_start_of_statement()
            {
                return;
            }

            self.consume_token();
        }
    }

    fn is_start_of_statement(&self) -> bool {
        match self.look_ahead {
            Token::ReturnKw(_, _) => true,
            Token::LeftBrace(_, _) => true,
            _ if self.is_start_of_expression() => true,
            _ => false,
        }
    }

    pub fn parse_statement(&mut self) -> Option<StmtAst> {
        self.try_match_token::<Newline>();

        if let Some(expr) = self.parse_expression() {
            return Some(StmtAst::Expr(expr));
        }

        if let Some(stmts) = self.parse_block_statement() {
            return Some(StmtAst::Block(stmts));
        }

        if self.try_match_token::<IfKw>().is_some() {
            let expr = match self.parse_expression() {
                Some(expr) => expr,
                None => {
                    self.errors.push(ParseError::ExpectedExpression);
                    ExprAst::Bad
                }
            };

            self.skip_newlines();
            self.match_token::<ThenKw>();
            self.skip_newlines();
            if self.try_match_token::<ElseKw>().is_some() {
                self.skip_newlines();
                let stmt_else = self.parse_statement().unwrap_or(StmtAst::Empty);
                return Some(StmtAst::IfThenElse(
                    expr,
                    Box::new(StmtAst::Empty),
                    Box::new(stmt_else),
                ));
            }

            let stmt_if = self.parse_statement().unwrap_or(StmtAst::Empty);

            self.skip_newlines();
            if self.try_match_token::<ElseKw>().is_some() {
                self.skip_newlines();
                let stmt_else = self.parse_statement().unwrap_or(StmtAst::Empty);
                return Some(StmtAst::IfThenElse(
                    expr,
                    Box::new(stmt_if),
                    Box::new(stmt_else),
                ));
            }

            return Some(StmtAst::IfThen(expr, Box::new(stmt_if)));
        }

        if self.try_match_token::<ThenKw>().is_some() {
            self.skip_newlines();
            let stmt = self.parse_statement().unwrap_or(StmtAst::Empty);
            return Some(StmtAst::Then(Box::new(stmt)));
        }

        if self.try_match_token::<ElseKw>().is_some() {
            self.skip_newlines();
            let stmt = self.parse_statement().unwrap_or(StmtAst::Empty);
            return Some(StmtAst::Else(Box::new(stmt)));
        }

        if self.try_match_token::<WhileKw>().is_some() {
            self.skip_newlines();
            let expr = match self.parse_expression() {
                Some(expr) => expr,
                None => {
                    self.errors.push(ParseError::ExpectedExpression);
                    ExprAst::Bad
                }
            };

            self.skip_newlines();
            self.match_token::<DoKw>();
            self.skip_newlines();

            let stmt_do = self.parse_statement().unwrap_or(StmtAst::Empty);
            return Some(StmtAst::WhileDo(expr, Box::new(stmt_do)));
        }

        if self.try_match_token::<DoKw>().is_some() {
            self.skip_newlines();
            let stmt_do = self.parse_statement().unwrap_or(StmtAst::Empty);
            self.skip_newlines();

            if self.try_match_token::<WhileKw>().is_some() {
                self.skip_newlines();
                let expr = match self.parse_expression() {
                    Some(expr) => expr,
                    None => {
                        self.errors.push(ParseError::ExpectedExpression);
                        ExprAst::Bad
                    }
                };

                return Some(StmtAst::DoWhile(Box::new(stmt_do), expr));
            }

            return Some(StmtAst::Do(Box::new(stmt_do)));
        }

        if self.try_match_token::<ReturnKw>().is_some() {
            if let Some(expr) = self.parse_expression() {
                return Some(StmtAst::ReturnWith(expr));
            } else {
                return Some(StmtAst::Return);
            }
        }

        None
    }

    fn parse_block_statement(&mut self) -> Option<Vec<StmtAst>> {
        self.try_match_token::<LeftBrace>()?;
        self.skip_newlines();

        let mut stmts = Vec::new();
        loop {
            while let Some(stmt) = self.parse_statement() {
                stmts.push(stmt);
                if self.peek_token::<RightBrace>() {
                    break;
                }
                if self.match_token_or_eof::<Newline>().is_none() {
                    self.recover_to_next_statement();
                }
            }

            match self.try_match_token::<RightBrace>() {
                Some(_) => break,
                None => {
                    self.errors.push(ParseError::ExpectedStatement);
                    self.recover_to_next_statement();
                }
            }

            if self.try_match_token::<Eof>().is_some() {
                self.errors.push(ParseError::ExpectedToken(
                    self.last_token.clone(),
                    RightBrace::kind(),
                ));
                break;
            }
        }

        Some(stmts)
    }

    pub fn is_start_of_expression(&self) -> bool {
        matches!(
            self.look_ahead,
            Token::Num(_, _)
                | Token::Ident(_, _)
                | Token::TrueLit(_, _)
                | Token::FalseLit(_, _)
                | Token::LeftParen(_, _)
        )
    }

    pub fn parse_expression(&mut self) -> Option<ExprAst> {
        self.parse_primary_expression()
    }

    fn parse_primary_expression(&mut self) -> Option<ExprAst> {
        if let Some(num) = self.try_match_token::<Num>() {
            return Some(ExprAst::Num(num.0));
        }

        if let Some(id) = self.try_match_token::<Ident>() {
            return Some(ExprAst::Ident(id.0));
        }

        if self.try_match_token::<TrueLit>().is_some() {
            return Some(ExprAst::Boolean(true));
        }

        if self.try_match_token::<FalseLit>().is_some() {
            return Some(ExprAst::Boolean(false));
        }

        if self.try_match_token::<LeftParen>().is_some() {
            self.skip_newlines();
            let expr = match self.parse_expression() {
                Some(expr) => expr,
                None => {
                    self.errors.push(ParseError::ExpectedExpression);
                    ExprAst::Bad
                }
            };
            self.skip_newlines();
            self.match_token::<RightParen>();
            return Some(expr);
        }

        None
    }

    fn skip_newlines(&mut self) {
        self.try_match_token::<Newline>();
    }

    fn consume_token(&mut self) {
        let next = self.lex();
        self.last_token = std::mem::replace(&mut self.look_ahead, next);
    }

    fn peek_token<T: TokenValue>(&mut self) -> bool {
        T::is_inside(&self.look_ahead)
    }

    fn match_token<T: TokenValue>(&mut self) -> Option<T> {
        match T::extract_from(&self.look_ahead) {
            Some((value, _)) => {
                self.consume_token();
                Some(value)
            }
            None => {
                self.errors.push(ParseError::ExpectedToken(
                    self.look_ahead.clone(),
                    T::kind(),
                ));
                None
            }
        }
    }

    fn match_token_or_eof<T: TokenValue>(&mut self) -> Option<T> {
        match T::extract_from(&self.look_ahead) {
            Some((value, _)) => {
                self.consume_token();
                Some(value)
            }
            None => match self.try_match_token::<Eof>() {
                Some(_) => Some(T::default()),
                None => {
                    self.errors.push(ParseError::ExpectedToken(
                        self.look_ahead.clone(),
                        T::kind(),
                    ));
                    None
                }
            },
        }
    }

    fn try_match_token<T: TokenValue>(&mut self) -> Option<T> {
        match T::extract_from(&self.look_ahead) {
            Some((value, _)) => {
                self.consume_token();
                Some(value)
            }
            None => None,
        }
    }

    fn found_eof(&self) -> bool {
        matches!(self.look_ahead, Token::Eof(_, _))
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
                    "else" => ElseKw.wrap(span),
                    "while" => WhileKw.wrap(span),
                    "do" => DoKw.wrap(span),
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
                    self.errors.push(ParseError::IllegalCharacter(u));
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
