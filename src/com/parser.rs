use crate::com::diagnostics::Severity;

use super::{
    ast::{
        Associativity, BinaryOperator, ExprAst, ExprAstKind, Precedence, ProgramAst, StmtAst,
        StmtAstKind, TypeAst, TypeAstKind, UnaryOperator,
    },
    cursor::Cursor,
    diagnostics::{self, DiagnosticKind, Diagnostics},
    pos::Pos,
    span::Span,
    tokens::*,
};

pub struct Parser<'a> {
    cursor: Cursor<'a>,
    last_boundary: Pos,
    look_ahead: Token,
    last_token: Token,
    diagnostics: &'a mut Diagnostics,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, diagnostics: &'a mut Diagnostics) -> Self {
        let mut parser = Self {
            cursor: Cursor::new(source),
            last_boundary: Pos::MIN,
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
                        .with_severity(Severity::Error)
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
            Token::ReturnKw(_, _)
            | Token::LeftBrace(_, _)
            | Token::IfKw(_, _)
            | Token::ThenKw(_, _)
            | Token::ElseKw(_, _)
            | Token::WhileKw(_, _)
            | Token::DoKw(_, _)
            | Token::VarKw(_, _)
            | Token::FuncKw(_, _) => true,
            _ if self.is_start_of_expression() => true,
            _ => false,
        }
    }

    fn empty_statement_here(&self) -> StmtAst {
        StmtAstKind::Empty.wrap(Span::at(self.last_boundary))
    }

    pub fn parse_statement(&mut self) -> Option<StmtAst> {
        self.skip_newlines();

        try_return_some!(self.parse_variable_definition());

        if let Some(expr) = self.parse_expression() {
            let span = expr.span;
            return Some(StmtAstKind::Expr(Box::new(expr)).wrap(span));
        }

        try_return_some!(self.parse_block_statement());

        try_return_some!(self.parse_if_then_statement());
        try_return_some!(self.parse_then_statement());
        try_return_some!(self.parse_else_statement());

        try_return_some!(self.parse_while_do_statement());
        try_return_some!(self.parse_do_while_statement());

        try_return_some!(self.parse_function_statement());
        try_return_some!(self.parse_return_statement());

        None
    }

    fn parse_variable_definition(&mut self) -> Option<StmtAst> {
        let (stmt, span) = take_span!(self => {
            self.try_match_token::<VarKw>()?;

            let id = self.match_token::<Identifier>().map(|tok| tok.0);
            self.match_token::<Equal>();
            let expr = self.expect_expression();
            Some(StmtAstKind::VarDef(id, Box::new(expr)))
        });

        stmt.map(|s| s.wrap(span))
    }

    fn parse_if_then_statement(&mut self) -> Option<StmtAst> {
        let (stmt, span) = take_span!(self => {
            self.try_match_token::<IfKw>()?;

            let guard = self.expect_expression();

            self.skip_newlines();
            self.match_token::<ThenKw>();
            self.skip_newlines();
            if self.try_match_token::<ElseKw>().is_some() {
                self.skip_newlines();
                let stmt_then = self.empty_statement_here();
                let stmt_else = self.parse_statement().unwrap_or(self.empty_statement_here());
                return Some(StmtAstKind::IfThenElse(
                    Box::new(guard),
                    Box::new(stmt_then),
                    Box::new(stmt_else),
                ));
            }

            let stmt_then = self.parse_statement().unwrap_or(self.empty_statement_here());

            self.skip_newlines();
            if self.try_match_token::<ElseKw>().is_some() {
                self.skip_newlines();
                let stmt_else = self.parse_statement().unwrap_or(self.empty_statement_here());
                return Some(StmtAstKind::IfThenElse(
                    Box::new(guard),
                    Box::new(stmt_then),
                    Box::new(stmt_else),
                ));
            }

            Some(StmtAstKind::IfThen(Box::new(guard), Box::new(stmt_then)))
        });

        stmt.map(|s| s.wrap(span))
    }

    fn parse_then_statement(&mut self) -> Option<StmtAst> {
        let (stmt, span) = take_span!(self => {
            self.try_match_token::<ThenKw>()?;

            self.skip_newlines();
            let stmt = self.parse_statement().unwrap_or(self.empty_statement_here());
            Some(StmtAstKind::Then(Box::new(stmt)))
        });

        stmt.map(|s| s.wrap(span))
    }

    fn parse_else_statement(&mut self) -> Option<StmtAst> {
        let (stmt, span) = take_span!(self => {
            self.try_match_token::<ElseKw>()?;

            self.skip_newlines();
            let stmt = self.parse_statement().unwrap_or(self.empty_statement_here());
            Some(StmtAstKind::Else(Box::new(stmt)))
        });

        stmt.map(|s| s.wrap(span))
    }

    fn parse_while_do_statement(&mut self) -> Option<StmtAst> {
        let (stmt, span) = take_span!(self => {
            self.try_match_token::<WhileKw>()?;

            self.skip_newlines();
            let expr = self.expect_expression();
            self.skip_newlines();

            self.match_token::<DoKw>();
            self.skip_newlines();
            let stmt_do = self.parse_statement().unwrap_or(self.empty_statement_here());
            return Some(StmtAstKind::WhileDo(Box::new(expr), Box::new(stmt_do)));
        });

        stmt.map(|s| s.wrap(span))
    }

    fn parse_do_while_statement(&mut self) -> Option<StmtAst> {
        let (stmt, span) = take_span!(self => {
            self.try_match_token::<DoKw>()?;

            self.skip_newlines();
            let stmt_do = self.parse_statement().unwrap_or(self.empty_statement_here());
            self.skip_newlines();

            if self.try_match_token::<WhileKw>().is_some() {
                self.skip_newlines();
                let expr = self.expect_expression();
                return Some(StmtAstKind::DoWhile(Box::new(stmt_do), Box::new(expr)));
            }

            Some(StmtAstKind::Do(Box::new(stmt_do)))
        });

        stmt.map(|s| s.wrap(span))
    }

    fn parse_function_statement(&mut self) -> Option<StmtAst> {
        let (stmt, span) = take_span!(self => {
            self.try_match_token::<FuncKw>()?;

            let name = self.match_token::<Identifier>().map(|id| id.0);
            let mut params = Vec::new();

            self.match_token::<LeftParen>();
            loop {
                let Some(id) = self.try_match_token::<Identifier>() else {
                    break;
                };

                self.match_token::<Colon>();
                let ty = self.expect_type_expression();

                params.push((id.0, ty));

                if self.try_match_token::<Comma>().is_none() {
                    break;
                }
            }
            self.match_token::<RightParen>();

            self.match_token::<RightArrow>();
            let ty = self.expect_type_expression();

            self.skip_newlines();

            if self.try_match_token::<Equal>().is_some() {
                let expr = self.expect_expression();
                let span = expr.span;
                let body = StmtAstKind::ReturnWith(Box::new(expr)).wrap(span);

                return Some(StmtAstKind::Func(name, params, Box::new(body), Box::new(ty)));
            }

            let stmt = match self.parse_block_statement() {
                Some(stmt) => stmt,
                None => {
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::ExpectedStatement)
                        .with_severity(Severity::Error)
                        .with_pos(self.pos())
                        .done();
                    self.diagnostics.push(d);
                    self.empty_statement_here()
                }
            };

            Some(StmtAstKind::Func(name, params, Box::new(stmt), Box::new(ty)))
        });

        stmt.map(|s| s.wrap(span))
    }

    fn parse_return_statement(&mut self) -> Option<StmtAst> {
        let (stmt, span) = take_span!(self => {
            self.try_match_token::<ReturnKw>()?;

            if let Some(expr) = self.parse_expression() {
                Some(StmtAstKind::ReturnWith(Box::new(expr)))
            } else {
                Some(StmtAstKind::Return)
            }
        });

        stmt.map(|s| s.wrap(span))
    }

    fn parse_block_statement(&mut self) -> Option<StmtAst> {
        let (stmts, span) = take_span!(self => {
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
                            .with_severity(Severity::Error)
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
                        .with_severity(Severity::Error)
                        .with_span(self.last_token.span())
                        .done();
                    self.diagnostics.push(d);
                    break;
                }
            }

            Some(stmts)
        });

        stmts.map(|s| StmtAstKind::Block(s).wrap(span))
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
                    .with_severity(Severity::Error)
                    .with_pos(self.pos())
                    .done();
                self.diagnostics.push(d);
                ExprAstKind::Bad.wrap(Span::at(self.pos()))
            }
        }
    }

    pub fn parse_expression(&mut self) -> Option<ExprAst> {
        self.parse_operation_expression(Precedence::MIN)
    }

    pub fn parse_operation_expression(&mut self, prec: Precedence) -> Option<ExprAst> {
        let mut expr = if let Some(op) = self.is_unary_operator() {
            let (inner, span) = take_span!(self => {
                self.consume_token();
                match self.parse_operation_expression(Precedence::MAX) {
                    Some(inner) => inner,
                    None => {
                        let d = diagnostics::create_diagnostic()
                            .with_kind(DiagnosticKind::ExpectedExpression)
                            .with_severity(Severity::Error)
                            .with_pos(self.pos())
                            .done();
                        self.diagnostics.push(d);
                        ExprAstKind::Bad.wrap(Span::at(self.pos()))
                    }
                }
            });
            ExprAstKind::UnaryOp(op, Box::new(inner)).wrap(span)
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
                        .with_severity(Severity::Error)
                        .with_pos(self.pos())
                        .done();
                    self.diagnostics.push(d);
                    ExprAstKind::Bad.wrap(Span::at(self.pos()))
                }
            };

            let span = expr.span.join(expr_right.span);
            expr = ExprAstKind::BinaryOp(op, Box::new(expr), Box::new(expr_right)).wrap(span);
        }

        Some(expr)
    }

    fn parse_primary_expression(&mut self) -> Option<ExprAst> {
        let (expr, span) = take_span!(self => {
            if let Some(num) = self.try_match_token::<Number>() {
                return Some(ExprAstKind::Number(num.0));
            }

            if let Some(id) = self.try_match_token::<Identifier>() {
                if self.try_match_token::<LeftParen>().is_none() {
                    return Some(ExprAstKind::Identifier(id.0));
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

                return Some(ExprAstKind::Call(id.0, params));
            }

            if self.try_match_token::<TrueKw>().is_some() {
                return Some(ExprAstKind::Boolean(true));
            }

            if self.try_match_token::<FalseKw>().is_some() {
                return Some(ExprAstKind::Boolean(false));
            }

            if self.try_match_token::<LeftParen>().is_some() {
                self.skip_newlines();
                let expr = self.expect_expression();
                self.skip_newlines();
                self.match_token::<RightParen>();
                return Some(ExprAstKind::Parenthesized(Box::new(expr)));
            }

            None
        });

        expr.map(|e| e.wrap(span))
    }

    fn expect_type_expression(&mut self) -> TypeAst {
        match self.parse_type_expression() {
            Some(ty) => ty,
            None => {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::ExpectedType)
                    .with_severity(Severity::Error)
                    .with_pos(self.last_span().from)
                    .done();
                self.diagnostics.push(d);
                TypeAstKind::Bad.wrap(Span::at(self.last_span().from))
            }
        }
    }

    fn parse_unit_type_expression(&mut self) -> Option<TypeAst> {
        let (ty, span) = take_span!(self => {
            if self.try_match_token::<LeftParen>().is_some() {
                self.match_token::<RightParen>();
                return Some(TypeAstKind::Unit);
            }

            None
        });

        ty.map(|ty| ty.wrap(span))
    }

    fn parse_type_expression(&mut self) -> Option<TypeAst> {
        if let Some(id) = self.try_match_token::<Identifier>() {
            return Some(TypeAstKind::Declared(id.0).wrap(self.last_span()));
        }

        try_return_some!(self.parse_unit_type_expression());

        None
    }

    fn expect_newlines_or_eof(&mut self) -> bool {
        matches!(self.last_token, Token::Newline(_, _))
            || self.match_token_or_eof::<Newline>().is_some()
    }

    fn skip_newlines(&mut self) {
        self.try_match_token::<Newline>();
    }

    fn last_span(&self) -> Span {
        self.last_token.span()
    }

    fn pos(&self) -> Pos {
        self.cursor.pos()
    }

    fn consume_token(&mut self) {
        let next = self.lex();
        self.last_token = std::mem::replace(&mut self.look_ahead, next);
        match self.last_token {
            Token::Eof(_, _) | Token::Newline(_, _) => (),
            _ => {
                self.last_boundary = self.last_token.span().to;
            }
        }
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
                    .with_severity(Severity::Error)
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
                        .with_severity(Severity::Error)
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
        c.is_alphabetic() || c == '_'
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

            match_by_string!(self, "->" => RightArrow);
            match_by_string!(self, "==" => EqualEqual);
            match_by_string!(self, "!=" => NotEqual);
            match_by_string!(self, "<=" => LessEqual);
            match_by_string!(self, ">=" => GreaterEqual);
            match_by_string!(self, "<" => LessThan);
            match_by_string!(self, ">" => GreaterThan);
            match_by_string!(self, "(" => LeftParen);
            match_by_string!(self, ")" => RightParen);
            match_by_string!(self, "{" => LeftBrace);
            match_by_string!(self, "}" => RightBrace);
            match_by_string!(self, "," => Comma);
            match_by_string!(self, ":" => Colon);
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
                    let pos_from = self.pos();
                    self.cursor.next();
                    let pos_to = self.pos();

                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::IllegalCharacter(u))
                        .with_severity(Severity::Error)
                        .with_span(Span::new(pos_from, pos_to))
                        .done();
                    self.diagnostics.push(d);
                    continue;
                }
                None => return Eof.wrap(Span::EOF),
            }
        }
    }
}

macro_rules! take_span {
    ($self:ident => $e:expr) => {{
        let from = $self.look_ahead.span().from;
        #[allow(clippy::redundant_closure_call)]
        let res = (|| $e)();
        let to = $self.last_boundary;
        (res, Span::new(from, to))
    }};
}

macro_rules! try_return_some {
    ($e:expr) => {
        let e = $e;
        if e.is_some() {
            return e;
        }
    };
}

macro_rules! match_by_string {
    ($self:ident, $string:literal => $token:ident) => {
        if let Some(span) = $self.try_consume_string($string) {
            return $token.wrap(span);
        }
    };
}

use {match_by_string, take_span, try_return_some};
