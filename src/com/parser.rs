use super::{
    ast::{Associativity, BinaryOperator, ExprAst, Precedence, ProgramAst, StmtAst, UnaryOperator},
    cursor::Cursor,
    diagnostics::{self, DiagnosticKind, Diagnostics},
    pos::Pos,
    span::Span,
    tokens::*,
};

pub struct Parser<'a> {
    cursor: Cursor<'a>,
    look_ahead: Token,
    last_token: Token,
    diagnostics: &'a mut Diagnostics,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, diagnostics: &'a mut Diagnostics) -> Self {
        let mut parser = Self {
            cursor: Cursor::new(source),
            look_ahead: Token::Eof(Eof, Span::EOF),
            last_token: Token::Eof(Eof, Span::EOF),
            diagnostics,
        };
        parser.look_ahead = parser.lex();
        parser
    }

    pub fn parse_program(mut self) -> ProgramAst {
        let mut program = Vec::new();

        loop {
            while let Some(stmt) = self.parse_statement() {
                program.push(stmt);
                if !self.expect_newlines_or_eof() {
                    self.recover_to_next_statement();
                }
            }

            match self.try_match_token::<Eof>() {
                Some(_) => break,
                None => {
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::ExpectedStatement)
                        .with_pos(self.pos())
                        .done();
                    self.diagnostics.push(d);
                    self.recover_to_next_statement();
                }
            }
        }

        program
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
        self.skip_newlines();

        if self.try_match_token::<VarKw>().is_some() {
            let id = self.match_token::<Identifier>().map(|tok| tok.0);
            self.match_token::<Equal>();
            let expr = self.expect_expression();
            return Some(StmtAst::VarDef(id, expr));
        }

        if let Some(expr) = self.parse_expression() {
            return Some(StmtAst::Expr(expr));
        }

        if let Some(stmts) = self.parse_block_statement() {
            return Some(StmtAst::Block(stmts));
        }

        if self.try_match_token::<IfKw>().is_some() {
            let expr = self.expect_expression();

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
            let expr = self.expect_expression();
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
                let expr = self.expect_expression();
                return Some(StmtAst::DoWhile(Box::new(stmt_do), expr));
            }

            return Some(StmtAst::Do(Box::new(stmt_do)));
        }

        if self.try_match_token::<FuncKw>().is_some() {
            let name = self.match_token::<Identifier>().map(|id| id.0);
            let mut params = Vec::new();

            self.match_token::<LeftParen>();
            loop {
                let Some(id) = self.try_match_token::<Identifier>() else {
                    break;
                };

                params.push(id.0);

                if self.try_match_token::<Comma>().is_none() {
                    break;
                }
            }
            self.match_token::<RightParen>();

            self.skip_newlines();
            let stmt = match self.parse_block_statement() {
                Some(stmt) => StmtAst::Block(stmt),
                None => {
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::ExpectedStatement)
                        .with_pos(self.pos())
                        .done();
                    self.diagnostics.push(d);
                    StmtAst::Empty
                }
            };

            return Some(StmtAst::Func(name, params, Box::new(stmt)));
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
                if !self.expect_newlines_or_eof() {
                    self.recover_to_next_statement();
                }
            }

            match self.try_match_token::<RightBrace>() {
                Some(_) => break,
                None => {
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::ExpectedStatement)
                        .with_pos(self.pos())
                        .done();
                    self.diagnostics.push(d);
                    self.recover_to_next_statement();
                }
            }

            if self.try_match_token::<Eof>().is_some() {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::ExpectedToken {
                        found: self.last_token.clone(),
                        expected: TokenKind::RightBrace,
                    })
                    .with_span(self.last_token.span())
                    .done();
                self.diagnostics.push(d);
                break;
            }
        }

        Some(stmts)
    }

    pub fn is_start_of_expression(&self) -> bool {
        matches!(
            self.look_ahead,
            Token::Number(_, _)
                | Token::Identifier(_, _)
                | Token::TrueKw(_, _)
                | Token::FalseKw(_, _)
                | Token::LeftParen(_, _)
        )
    }

    /*

    */

    #[rustfmt::skip]
    fn is_binary_operator(&mut self) -> Option<(BinaryOperator, Precedence, Associativity)> {
        match self.look_ahead {
            Token::Star(_, _) => Some((BinaryOperator::Star, 90, Associativity::Left)),
            Token::Slash(_, _) => Some((BinaryOperator::Slash, 90, Associativity::Left)),
            Token::Percent(_, _) => Some((BinaryOperator::Percent, 90, Associativity::Left)),
            Token::Plus(_, _) => Some((BinaryOperator::Plus, 80, Associativity::Left)),
            Token::Minus(_, _) => Some((BinaryOperator::Minus, 80, Associativity::Left)),
            Token::Ampersand(_, _) => Some((BinaryOperator::Ampersand, 70, Associativity::Left)),
            Token::Caret(_, _) => Some((BinaryOperator::Caret, 60, Associativity::Left)),
            Token::Bar(_, _) => Some((BinaryOperator::Bar, 50, Associativity::Left)),
            Token::EqualEqual(_, _) => Some((BinaryOperator::EqualEqual, 40, Associativity::Left)),
            Token::NotEqual(_, _) => Some((BinaryOperator::NotEqual, 40, Associativity::Left)),
            Token::LessEqual(_, _) => Some((BinaryOperator::LessEqual, 40, Associativity::Left)),
            Token::LessThan(_, _) => Some((BinaryOperator::LessThan, 40, Associativity::Left)),
            Token::GreaterEqual(_, _) => Some((BinaryOperator::GreaterEqual, 40, Associativity::Left)),
            Token::GreaterThan(_, _) => Some((BinaryOperator::GreaterThan, 40, Associativity::Left)),
            Token::AndKw(_, _) => Some((BinaryOperator::And, 30, Associativity::Left)),
            Token::OrKw(_, _) => Some((BinaryOperator::Or, 20, Associativity::Left)),
            Token::Equal(_, _) => Some((BinaryOperator::Equal, 10, Associativity::Right)),
            _ => None,
        }
    }

    fn is_unary_operator(&mut self) -> Option<UnaryOperator> {
        match self.look_ahead {
            Token::Plus(_, _) => Some(UnaryOperator::Pos),
            Token::Minus(_, _) => Some(UnaryOperator::Neg),
            Token::NotKw(_, _) => Some(UnaryOperator::Not),
            _ => None,
        }
    }

    fn expect_expression(&mut self) -> ExprAst {
        match self.parse_expression() {
            Some(expr) => expr,
            None => {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::ExpectedExpression)
                    .with_pos(self.pos())
                    .done();
                self.diagnostics.push(d);
                ExprAst::Bad
            }
        }
    }

    pub fn parse_expression(&mut self) -> Option<ExprAst> {
        self.parse_operation_expression(Precedence::MIN)
    }

    pub fn parse_operation_expression(&mut self, prec: Precedence) -> Option<ExprAst> {
        let mut expr = if let Some(op) = self.is_unary_operator() {
            self.consume_token();
            let inner = match self.parse_operation_expression(Precedence::MAX) {
                Some(inner) => inner,
                None => {
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::ExpectedExpression)
                        .with_pos(self.pos())
                        .done();
                    self.diagnostics.push(d);
                    ExprAst::Bad
                }
            };
            ExprAst::UnaryOp(op, Box::new(inner))
        } else {
            self.parse_primary_expression()?
        };

        loop {
            let Some((op, prec_ahead, assoc_ahead)) = self.is_binary_operator() else {
                break;
            };

            if prec_ahead < prec
                || ((prec_ahead == prec) && matches!(assoc_ahead, Associativity::Left))
            {
                break;
            }

            self.consume_token();
            let expr_right = match self.parse_operation_expression(prec_ahead) {
                Some(inner) => inner,
                None => {
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::ExpectedExpression)
                        .with_pos(self.pos())
                        .done();
                    self.diagnostics.push(d);
                    ExprAst::Bad
                }
            };

            expr = ExprAst::BinaryOp(op, Box::new(expr), Box::new(expr_right));
        }

        Some(expr)
    }

    fn parse_primary_expression(&mut self) -> Option<ExprAst> {
        if let Some(num) = self.try_match_token::<Number>() {
            return Some(ExprAst::Number(num.0));
        }

        if let Some(id) = self.try_match_token::<Identifier>() {
            if self.try_match_token::<LeftParen>().is_none() {
                return Some(ExprAst::Identifier(id.0));
            }

            let mut params = Vec::new();
            loop {
                let Some(expr) = self.parse_expression() else {
                    break;
                };

                params.push(expr);

                if self.try_match_token::<Comma>().is_none() {
                    break;
                }
            }
            self.match_token::<RightParen>();

            return Some(ExprAst::Call(id.0, params));
        }

        if self.try_match_token::<TrueKw>().is_some() {
            return Some(ExprAst::Boolean(true));
        }

        if self.try_match_token::<FalseKw>().is_some() {
            return Some(ExprAst::Boolean(false));
        }

        if self.try_match_token::<LeftParen>().is_some() {
            self.skip_newlines();
            let expr = self.expect_expression();
            self.skip_newlines();
            self.match_token::<RightParen>();
            return Some(expr);
        }

        None
    }

    fn expect_newlines_or_eof(&mut self) -> bool {
        matches!(self.last_token, Token::Newline(_, _))
            || self.match_token_or_eof::<Newline>().is_some()
    }

    fn skip_newlines(&mut self) {
        self.try_match_token::<Newline>();
    }

    fn pos(&self) -> Pos {
        self.cursor.pos()
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
                let tok = self.look_ahead.clone();
                let span = tok.span();
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::ExpectedToken {
                        found: tok,
                        expected: T::kind(),
                    })
                    .with_span(span)
                    .done();
                self.diagnostics.push(d);
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
                    let tok = self.look_ahead.clone();
                    let span = tok.span();
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::ExpectedToken {
                            found: tok,
                            expected: T::kind(),
                        })
                        .with_span(span)
                        .done();
                    self.diagnostics.push(d);
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

            match_by_string!(self, "==" => EqualEqual);
            match_by_string!(self, "!=" => NotEqual);
            match_by_string!(self, "<=" => LessEqual);
            match_by_string!(self, "<" => LessThan);
            match_by_string!(self, ">=" => GreaterEqual);
            match_by_string!(self, ">" => GreaterThan);
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
            match_by_string!(self, "%" => Percent);
            match_by_string!(self, "&" => Ampersand);
            match_by_string!(self, "|" => Bar);
            match_by_string!(self, "^" => Caret);

            if let Some((num, span)) = self.try_consume_number() {
                return Number(num).wrap(span);
            }

            if let Some((id, span)) = self.try_consume_identifier() {
                return match id.as_str() {
                    "if" => IfKw.wrap(span),
                    "then" => ThenKw.wrap(span),
                    "else" => ElseKw.wrap(span),
                    "while" => WhileKw.wrap(span),
                    "do" => DoKw.wrap(span),
                    "var" => VarKw.wrap(span),
                    "func" => FuncKw.wrap(span),
                    "return" => ReturnKw.wrap(span),
                    "true" => TrueKw.wrap(span),
                    "false" => FalseKw.wrap(span),
                    "and" => AndKw.wrap(span),
                    "or" => OrKw.wrap(span),
                    "not" => NotKw.wrap(span),
                    _ => Identifier(id).wrap(span),
                };
            }

            match self.cursor.peek() {
                Some(u) => {
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::IllegalCharacter(u))
                        .with_pos(self.pos())
                        .done();
                    self.diagnostics.push(d);
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
