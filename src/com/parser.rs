use std::fmt::Display;

use super::{asts::*, cursor::Cursor, ops::*, tokens::*};

#[derive(Debug)]
pub enum ParserError {
    InvalidNumber(String),
    Unexpected(Token, TokenKind),
    ExpectedExpression(Token),
    ExpectedStatement(Token),
    ExpectedBlockStatement(Token),
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::InvalidNumber(repr) => {
                write!(f, "invalid number literal '{repr}'")
            }
            ParserError::Unexpected(found, wanted) => {
                write!(f, "expected {wanted}, but found {found}")
            }
            ParserError::ExpectedExpression(found) => {
                write!(f, "expected an expression, but found {found}")
            }
            ParserError::ExpectedStatement(found) => {
                write!(f, "expected a statement, but found {found}")
            }
            ParserError::ExpectedBlockStatement(found) => {
                write!(f, "expected a block statement, but found {found}")
            }
        }
    }
}

pub struct Parser<'src> {
    cursor: Cursor<'src>,
    lookahead: Token,
    seen_tokens: Vec<Token>,
    errors: Vec<ParserError>,
}

impl<'src> Parser<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            cursor: Cursor::new(source),
            lookahead: Token::Eof(Eof),
            seen_tokens: Vec::new(),
            errors: Vec::new(),
        }
    }

    fn consume_token(&mut self) -> Token {
        let next = self.lex();
        let seen = std::mem::replace(&mut self.lookahead, next);
        self.seen_tokens.push(seen.clone());
        seen
    }

    fn try_expect_token<T: TokenValue>(&mut self) -> Result<T, ParserError> {
        match T::extract(&self.lookahead) {
            Some(token) => {
                self.consume_token();
                Ok(token)
            }
            None => Err(ParserError::Unexpected(self.lookahead.clone(), T::kind())),
        }
    }

    fn expect_token<T: TokenValue>(&mut self) -> T {
        self.try_expect_token().accept_error(self)
    }

    fn match_token<T: TokenValue>(&mut self) -> Option<T> {
        match T::extract(&self.lookahead) {
            Some(token) => {
                self.consume_token();
                Some(token)
            }
            None => None,
        }
    }

    pub fn parse(mut self) -> (StmtAst, Vec<ParserError>) {
        self.consume_token(); // consume initial dummy eof token

        self.skip_newlines();
        let stmt = self.parse_statement().accept_error(&mut self);
        self.skip_newlines();

        self.expect_token::<Eof>();

        (stmt, self.errors)
    }

    fn skip_newlines(&mut self) {
        self.match_token::<Newline>();
    }

    // looks at the previous token in case the newlines were skipped
    fn expect_newline(&mut self) {
        if let Some(Token::Newline(_)) = self.seen_tokens.last() {
            return;
        }
        self.expect_token::<Newline>();
    }

    fn parse_statement(&mut self) -> Result<StmtAst, ParserError> {
        if self.match_token::<FuncKeyword>().is_some() {
            let name = self.expect_token::<Identifier>().0;
            let mut args = Vec::new();

            self.expect_token::<LeftParen>();

            if let Some(first_arg) = self.match_token::<Identifier>() {
                args.push(first_arg.0);
                while self.match_token::<Comma>().is_some() {
                    let arg = self.expect_token::<Identifier>().0;
                    args.push(arg);
                }
            }

            self.expect_token::<RightParen>();
            self.skip_newlines();

            let stmt = if self.match_token::<Equal>().is_some() {
                let expr = self.parse_expression().accept_error(self);
                StmtAst::Return(expr)
            } else {
                self.parse_block_statement().accept_error(self)
            };

            return Ok(StmtAst::FuncDef(name, args, Box::new(stmt)));
        }

        if self.match_token::<IfKeyword>().is_some() {
            let expr = self.parse_expression().accept_error(self);
            self.expect_token::<ThenKeyword>();
            self.skip_newlines();
            let stmt = self.parse_statement().accept_error(self);
            return Ok(StmtAst::IfThen(expr, Box::new(stmt)));
        }

        if self.match_token::<ReturnKeyword>().is_some() {
            let expr = self.parse_expression().accept_error(self);
            return Ok(StmtAst::Return(expr));
        }

        if self.match_token::<LogKeyword>().is_some() {
            let expr = self.parse_expression().accept_error(self);
            return Ok(StmtAst::Log(expr));
        }

        if let Ok(block) = self.parse_block_statement() {
            return Ok(block);
        }

        if let Ok(expr) = self.parse_expression() {
            return Ok(StmtAst::Expr(expr));
        }

        Err(ParserError::ExpectedStatement(self.lookahead.clone()))
    }

    fn parse_block_statement(&mut self) -> Result<StmtAst, ParserError> {
        if self.match_token::<LeftBrace>().is_some() {
            self.skip_newlines();

            let mut stmts = Vec::new();
            while let Ok(stmt) = self.parse_statement() {
                stmts.push(stmt);
                self.expect_newline();
            }

            self.expect_token::<RightBrace>();
            self.skip_newlines();

            Ok(StmtAst::Block(stmts))
        } else {
            Err(ParserError::ExpectedBlockStatement(self.lookahead.clone()))
        }
    }

    fn match_binary_operator(&mut self) -> Option<BinOp> {
        match self.lookahead {
            Token::Equal(_) => Some(BinOp::Equal),
            Token::Plus(_) => Some(BinOp::Plus),
            Token::Minus(_) => Some(BinOp::Minus),
            Token::Star(_) => Some(BinOp::Star),
            Token::Slash(_) => Some(BinOp::Slash),
            Token::Mod(_) => Some(BinOp::Mod),
            Token::Amper(_) => Some(BinOp::Amper),
            Token::Bar(_) => Some(BinOp::Bar),
            Token::Caret(_) => Some(BinOp::Caret),
            Token::Eq(_) => Some(BinOp::Eq),
            Token::Neq(_) => Some(BinOp::Neq),
            Token::LtEq(_) => Some(BinOp::LtEq),
            Token::Lt(_) => Some(BinOp::Lt),
            Token::GtEq(_) => Some(BinOp::GtEq),
            Token::Gt(_) => Some(BinOp::Gt),
            Token::AndKeyword(_) => Some(BinOp::And),
            Token::OrKeyword(_) => Some(BinOp::Or),
            _ => None,
        }
    }

    fn parse_expression(&mut self) -> Result<ExprAst, ParserError> {
        self.parse_operation_expression(0, Associativity::Left)
    }

    fn parse_operation_expression(
        &mut self,
        prec: u8,
        assoc: Associativity,
    ) -> Result<ExprAst, ParserError> {
        let mut expr = self.parse_primary_expression()?;

        while let Some(op) = self.match_binary_operator() {
            let prec_ahead = op.precedence();
            let assoc_ahead = op.associativity();

            if prec_ahead > prec || (prec_ahead == prec && assoc.is_left()) {
                self.consume_token();
                let rhs = self
                    .parse_operation_expression(prec_ahead + 1, assoc_ahead)
                    .accept_error(self);
                expr = ExprAst::Binary(op, Box::new(expr), Box::new(rhs));
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_primary_expression(&mut self) -> Result<ExprAst, ParserError> {
        if let Some(number) = self.match_token::<Number>() {
            return Ok(ExprAst::Number(number.0));
        };

        if self.match_token::<TrueLiteral>().is_some() {
            return Ok(ExprAst::True);
        }

        if self.match_token::<FalseLiteral>().is_some() {
            return Ok(ExprAst::False);
        }

        if self.match_token::<TimeLiteral>().is_some() {
            return Ok(ExprAst::Time);
        }

        if self.match_token::<LeftParen>().is_some() {
            let expr = self.parse_expression()?;
            self.try_expect_token::<RightParen>()?;
            return Ok(expr);
        }

        if let Some(id) = self.match_token::<Identifier>() {
            if self.match_token::<LeftParen>().is_none() {
                return Ok(ExprAst::Variable(id.0));
            }

            let callee = id.0;
            let mut args = Vec::new();
            loop {
                let expr = self.parse_expression();
                let Ok(expr) = expr else {
                    break;
                };

                args.push(expr);

                if self.match_token::<Comma>().is_none() {
                    break;
                }
            }

            self.try_expect_token::<RightParen>()?;
            return Ok(ExprAst::Call(callee, args));
        }

        Err(ParserError::ExpectedExpression(self.lookahead.clone()))
    }

    fn try_consume_string(&mut self, string: &str) -> bool {
        let mut clone = self.cursor.clone();

        if clone.by_ref().take(string.len()).eq(string.chars()) {
            self.cursor = clone;
            true
        } else {
            false
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

    fn try_consume_identifier(&mut self) -> Option<String> {
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

        Some(identifier)
    }

    fn try_consume_number(&mut self) -> Option<f64> {
        let mut number = String::new();

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

        Some(match number.parse() {
            Ok(num) => num,
            Err(_) => {
                self.errors.push(ParserError::InvalidNumber(number));
                f64::NAN
            }
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
            let Some(c) = self.cursor.peek() else {
                return Eof.wrap();
            };

            if c.is_whitespace() && !Self::is_newline_character(c) {
                self.cursor.next();
                continue;
            }

            break;
        }

        if self.cursor.peek().is_none() {
            return Eof.wrap();
        }

        if self.try_consume_newlines() {
            return Newline.wrap();
        }

        match_by_string!(self, "==" => Eq);
        match_by_string!(self, "!=" => Neq);
        match_by_string!(self, "<=" => LtEq);
        match_by_string!(self, "<" => Lt);
        match_by_string!(self, ">=" => GtEq);
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

        if let Some(num) = self.try_consume_number() {
            return Number(num).wrap();
        }

        if let Some(id) = self.try_consume_identifier() {
            return match id.as_str() {
                "if" => IfKeyword.wrap(),
                "then" => ThenKeyword.wrap(),
                "func" => FuncKeyword.wrap(),
                "return" => ReturnKeyword.wrap(),
                "log" => LogKeyword.wrap(),
                "true" => TrueLiteral.wrap(),
                "false" => FalseLiteral.wrap(),
                "time" => TimeLiteral.wrap(),
                "and" => AndKeyword.wrap(),
                "or" => OrKeyword.wrap(),
                "not" => NotKeyword.wrap(),
                _ => Identifier(id).wrap(),
            };
        }

        match self.cursor.peek() {
            Some(u) => {
                self.cursor.next();
                Illegal(u).wrap()
            }
            None => Eof.wrap(),
        }
    }
}

// so that we can gracefully accept an error after trying to parse something
trait ParserResult {
    type Inner;
    fn accept_error(self, parser: &mut Parser) -> Self::Inner;
}

impl<T: Default> ParserResult for Result<T, ParserError> {
    type Inner = T;

    fn accept_error(self, parser: &mut Parser) -> Self::Inner {
        match self {
            Ok(inner) => inner,
            Err(e) => {
                parser.errors.push(e);
                T::default()
            }
        }
    }
}

macro_rules! match_by_string {
    ($self:ident, $string:literal => $token:ident) => {
        if $self.try_consume_string($string) {
            return $token.wrap();
        }
    };
}

use match_by_string;
