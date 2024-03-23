use self::stmt::{AliasDef, DataDef, FuncDef, VarDef};

use super::{ast::*, tokens::*};
use crate::{
    com::ast::types::TypeKind,
    diagnostics::{self, DiagnosticKind, DiagnosticList, Note, Severity},
    utils::{Cursor, Pos, Span, Spanned},
};

pub struct Parser<'a> {
    cursor: Cursor<'a>,
    tokens: Vec<Token>,
    token_index: usize,
    diagnostics: &'a mut DiagnosticList,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, diagnostics: &'a mut DiagnosticList) -> Self {
        let mut parser = Self {
            cursor: Cursor::new(source),
            tokens: Vec::new(),
            token_index: 0,
            diagnostics,
        };

        let first_token = parser.lex();
        parser.tokens.push(first_token);

        parser
    }

    pub fn parse_program(mut self) -> Stmt {
        let (stmts, span) = take_span!(self => {
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
                            .with_pos(self.last_boundary())
                            .annotate_primary(Note::Here, Span::at(self.last_boundary()))
                            .done();
                        self.diagnostics.push(d);
                        self.recover_to_next_statement();
                    }
                }
            }

            program
        });

        StmtKind::Block(stmts.into()).wrap(span)
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
        match self.look_ahead() {
            Token::TypeKw(_, _)
            | Token::VarKw(_, _)
            | Token::LeftBrace(_, _)
            | Token::IfKw(_, _)
            | Token::ThenKw(_, _)
            | Token::ElseKw(_, _)
            | Token::WhileKw(_, _)
            | Token::DoKw(_, _)
            | Token::FuncKw(_, _)
            | Token::ReturnKw(_, _) => true,
            _ if self.is_start_of_expression() => true,
            _ => false,
        }
    }

    fn empty_statement_here(&self) -> Stmt {
        StmtKind::Empty.wrap(Span::at(self.last_boundary()))
    }

    pub fn parse_statement(&mut self) -> Option<Stmt> {
        self.skip_newlines();

        try_return_some!(self.parse_data_definition());
        try_return_some!(self.parse_variable_definition());
        try_return_some!(self.parse_block_statement());
        try_return_some!(self.parse_if_then_statement());
        try_return_some!(self.parse_then_statement());
        try_return_some!(self.parse_else_statement());
        try_return_some!(self.parse_while_do_statement());
        try_return_some!(self.parse_do_while_statement());
        try_return_some!(self.parse_function_statement());
        try_return_some!(self.parse_return_statement());
        try_return_some!(self.parse_print_statement());

        if let Some(expr) = self.parse_expression() {
            let span = expr.span;
            return Some(StmtKind::Expr(Box::new(expr)).wrap(span));
        }

        None
    }

    fn parse_data_definition(&mut self) -> Option<Stmt> {
        let (stmt, span) = take_span!(self => {
            let opaque_kw = self.try_match_token::<OpaqueKw>();
            match opaque_kw {
                Some(_) => self.match_token::<TypeKw>(),
                None => Some(self.try_match_token::<TypeKw>()?),
            };

            let name = self.match_token::<Identifier>().map(|id| Spanned {
                value: id.0,
                span: self.last_span(),
            }).unwrap_or_default();

            self.skip_newlines();
            if self.try_match_token::<Equal>().is_some() {
                self.skip_newlines();
                let ty = self.expect_type_expression();
                return Some(StmtKind::AliasDef(AliasDef {
                    name,
                    ty: Box::new(ty),
                    is_opaque: opaque_kw.is_some(),
                }))
            }

            self.match_token::<LeftBrace>();
            self.skip_newlines();

            let mut fields = Vec::new();
            while let Some(id) = self.try_match_token::<Identifier>() {
                let field_name = Spanned {
                    value: id.0,
                    span: self.last_span()
                };

                self.match_token::<Colon>();
                let field_type = self.expect_type_expression();
                fields.push((field_name, field_type));

                self.expect_newlines_or_eof();
            }

            self.skip_newlines();
            self.match_token::<RightBrace>();
            self.skip_newlines();

            Some(StmtKind::DataDef(DataDef {
                name,
                fields: fields.into(),
                is_opaque: opaque_kw.is_some(),
            }))
        });

        Some(stmt?.wrap(span))
    }

    fn parse_variable_definition(&mut self) -> Option<Stmt> {
        let (stmt, span) = take_span!(self => {
            self.try_match_token::<VarKw>()?;

            let id = self.match_token::<Identifier>().map(|tok| Spanned {
                value: tok.0,
                span: self.last_span()
            });
            self.match_token::<Equal>();
            let expr = self.expect_expression();
            Some(StmtKind::VarDef(VarDef {
                name: id,
                expr: Box::new(expr),
            }))
        });

        stmt.map(|s| s.wrap(span))
    }

    fn parse_if_then_statement(&mut self) -> Option<Stmt> {
        let (stmt, span) = take_span!(self => {
            self.try_match_token::<IfKw>()?;

            let guard = self.expect_expression();

            self.skip_newlines();
            self.match_token::<ThenKw>();
            self.skip_newlines();

            // placeholder at this position
            let stmt_then = self.empty_statement_here();

            if self.try_match_token::<ElseKw>().is_some() {
                self.skip_newlines();
                let stmt_else = self.parse_statement().unwrap_or(self.empty_statement_here());
                return Some(StmtKind::IfThenElse(
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
                return Some(StmtKind::IfThenElse(
                    Box::new(guard),
                    Box::new(stmt_then),
                    Box::new(stmt_else),
                ));
            }

            Some(StmtKind::IfThen(Box::new(guard), Box::new(stmt_then)))
        });

        stmt.map(|s| s.wrap(span))
    }

    fn parse_then_statement(&mut self) -> Option<Stmt> {
        let (stmt, span) = take_span!(self => {
            self.try_match_token::<ThenKw>()?;

            self.skip_newlines();
            let stmt = self.parse_statement().unwrap_or(self.empty_statement_here());
            Some(StmtKind::Then(Box::new(stmt)))
        });

        stmt.map(|s| s.wrap(span))
    }

    fn parse_else_statement(&mut self) -> Option<Stmt> {
        let (stmt, span) = take_span!(self => {
            self.try_match_token::<ElseKw>()?;

            self.skip_newlines();
            let stmt = self.parse_statement().unwrap_or(self.empty_statement_here());
            Some(StmtKind::Else(Box::new(stmt)))
        });

        stmt.map(|s| s.wrap(span))
    }

    fn parse_while_do_statement(&mut self) -> Option<Stmt> {
        let (stmt, span) = take_span!(self => {
            self.try_match_token::<WhileKw>()?;

            self.skip_newlines();
            let expr = self.expect_expression();
            self.skip_newlines();

            self.match_token::<DoKw>();
            self.skip_newlines();
            let stmt_do = self.parse_statement().unwrap_or(self.empty_statement_here());
            return Some(StmtKind::WhileDo(Box::new(expr), Box::new(stmt_do)));
        });

        stmt.map(|s| s.wrap(span))
    }

    fn parse_do_while_statement(&mut self) -> Option<Stmt> {
        let (stmt, span) = take_span!(self => {
            self.try_match_token::<DoKw>()?;

            self.skip_newlines();
            let stmt_do = self.empty_statement_here();

            if self.try_match_token::<WhileKw>().is_some() {
                self.skip_newlines();
                let expr = self.expect_expression();
                return Some(StmtKind::DoWhile(Box::new(stmt_do), Box::new(expr)));
            }

            let stmt_do = self.parse_statement().unwrap_or(self.empty_statement_here());
            self.skip_newlines();

            if self.try_match_token::<WhileKw>().is_some() {
                self.skip_newlines();
                let expr = self.expect_expression();
                return Some(StmtKind::DoWhile(Box::new(stmt_do), Box::new(expr)));
            }

            Some(StmtKind::Do(Box::new(stmt_do)))
        });

        stmt.map(|s| s.wrap(span))
    }

    fn parse_function_statement(&mut self) -> Option<Stmt> {
        let (stmt, span) = take_span!(self => {
            self.try_match_token::<FuncKw>()?;

            let name = self.match_token::<Identifier>().map(|id| Spanned {
                value: id.0,
                span: self.last_span(),
            });
            let mut params = Vec::new();

            self.match_token::<LeftParen>();
            loop {
                let Some(id) = self.try_match_token::<Identifier>() else {
                    break;
                };
                let span = self.last_span();

                self.match_token::<Colon>();
                let ty = self.expect_type_expression();

                let span = span.join(ty.span);
                params.push((id.0, ty, span));

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
                let body = StmtKind::ReturnWith(Box::new(expr)).wrap(span);

                return Some(StmtKind::FuncDef(FuncDef {
                    name,
                    args: params.into(),
                    ty: Box::new(ty),
                    body: Box::new(body),
                }));
            }

            let stmt = match self.parse_block_statement() {
                Some(stmt) => stmt,
                None => {
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::ExpectedStatement)
                        .with_severity(Severity::Error)
                        .with_pos(self.last_boundary())
                        .annotate_primary(Note::Here, Span::at(self.last_boundary()))
                        .done();
                    self.diagnostics.push(d);
                    self.empty_statement_here()
                }
            };

            Some(StmtKind::FuncDef(FuncDef {
                name,
                args: params.into(),
                ty: Box::new(ty),
                body: Box::new(stmt),
            }))
        });

        stmt.map(|s| s.wrap(span))
    }

    fn parse_return_statement(&mut self) -> Option<Stmt> {
        let (stmt, span) = take_span!(self => {
            self.try_match_token::<ReturnKw>()?;

            if let Some(expr) = self.parse_expression() {
                Some(StmtKind::ReturnWith(Box::new(expr)))
            } else {
                Some(StmtKind::Return)
            }
        });

        stmt.map(|s| s.wrap(span))
    }

    fn parse_print_statement(&mut self) -> Option<Stmt> {
        let (stmt, span) = take_span!(self => {
            self.try_match_token::<PrintKw>()?;
            let expr = self.expect_expression();
            Some(StmtKind::Print(Box::new(expr)))
        });

        Some(stmt?.wrap(span))
    }

    fn parse_block_statement(&mut self) -> Option<Stmt> {
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
                            .with_pos(self.last_boundary())
                            .annotate_primary(Note::Here, Span::at(self.last_boundary()))
                            .done();
                        self.diagnostics.push(d);
                        self.recover_to_next_statement();
                    }
                }

                if self.try_match_token::<Eof>().is_some() {
                    let span = self.last_token().span();
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::ExpectedToken {
                            found: self.last_token().clone(),
                            expected: TokenKind::RightBrace,
                        })
                        .with_severity(Severity::Error)
                        .with_span(span)
                        .annotate_primary(Note::ExpectedToken(<Eof as TokenValue>::kind()), span)
                        .done();
                    self.diagnostics.push(d);
                    break;
                }
            }

            Some(stmts)
        });

        stmts.map(|s| StmtKind::Block(s.into()).wrap(span))
    }

    pub fn is_start_of_expression(&self) -> bool {
        self.is_unary_operator().is_some()
            || self.is_unary_operator().is_some()
            || matches!(
                self.look_ahead(),
                Token::Integer(_, _)
                    | Token::MalformedNumeral(_, _)
                    | Token::Identifier(_, _)
                    | Token::TrueKw(_, _)
                    | Token::FalseKw(_, _)
                    | Token::StringLit(_, _)
                    | Token::LeftParen(_, _)
                    | Token::LeftBracket(_, _)
                    | Token::DebugKw(_, _)
                    | Token::Ampersand(_, _)
                    | Token::TodoKw(_, _)
                    | Token::UnreachableKw(_, _)
                    | Token::CaseKw(_, _)
            )
    }

    #[rustfmt::skip]
    fn is_binary_operator(&self) -> Option<(BinOp, Precedence, Associativity)> {
        match self.look_ahead() {
            Token::Star(_, _) => Some((BinOp::Mul, 90, Associativity::Left)),
            Token::Slash(_, _) => Some((BinOp::Div, 90, Associativity::Left)),
            Token::Percent(_, _) => Some((BinOp::Rem, 90, Associativity::Left)),
            Token::PlusPlus(_, _) => Some((BinOp::Concat, 80, Associativity::Left)),
            Token::Plus(_, _) => Some((BinOp::Add, 80, Associativity::Left)),
            Token::Minus(_, _) => Some((BinOp::Sub, 80, Associativity::Left)),
            Token::Ampersand(_, _) => Some((BinOp::BitAnd, 70, Associativity::Left)),
            Token::Caret(_, _) => Some((BinOp::BitXor, 60, Associativity::Left)),
            Token::Bar(_, _) => Some((BinOp::BitOr, 50, Associativity::Left)),
            Token::EqualEqual(_, _) => Some((BinOp::Eq, 40, Associativity::Left)),
            Token::NotEqual(_, _) => Some((BinOp::Ne, 40, Associativity::Left)),
            Token::LessEqual(_, _) => Some((BinOp::Le, 40, Associativity::Left)),
            Token::LessThan(_, _) => Some((BinOp::Lt, 40, Associativity::Left)),
            Token::GreaterEqual(_, _) => Some((BinOp::Ge, 40, Associativity::Left)),
            Token::GreaterThan(_, _) => Some((BinOp::Gt, 40, Associativity::Left)),
            Token::AndKw(_, _) => Some((BinOp::And, 30, Associativity::Left)),
            Token::XorKw(_, _) => Some((BinOp::Xor, 25, Associativity::Left)),
            Token::OrKw(_, _) => Some((BinOp::Or, 20, Associativity::Left)),
            Token::Equal(_, _) => Some((BinOp::Assign, 10, Associativity::Right)),
            _ => None,
        }
    }

    fn is_unary_operator(&self) -> Option<UnOp> {
        match self.look_ahead() {
            Token::Plus(_, _) => Some(UnOp::Pos),
            Token::Minus(_, _) => Some(UnOp::Neg),
            Token::NotKw(_, _) => Some(UnOp::Not),
            _ => None,
        }
    }

    fn expect_expression(&mut self) -> Expr {
        match self.parse_expression() {
            Some(expr) => expr,
            None => {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::ExpectedExpression)
                    .with_severity(Severity::Error)
                    .with_pos(self.last_boundary())
                    .annotate_primary(Note::Here, Span::at(self.last_boundary()))
                    .done();
                self.diagnostics.push(d);
                ExprKind::Bad.wrap(Span::at(self.last_boundary()))
            }
        }
    }

    pub fn parse_expression(&mut self) -> Option<Expr> {
        self.parse_operation_expression(Precedence::MIN)
    }

    pub fn parse_operation_expression(&mut self, prec: Precedence) -> Option<Expr> {
        let mut expr = if let Some(op) = self.is_unary_operator() {
            let (inner, span) = take_span!(self => {
                self.consume_token();
                match self.parse_operation_expression(Precedence::MAX) {
                    Some(inner) => inner,
                    None => {
                        let d = diagnostics::create_diagnostic()
                            .with_kind(DiagnosticKind::ExpectedExpression)
                            .with_severity(Severity::Error)
                            .with_pos(self.last_boundary())
                            .annotate_primary(Note::Here, Span::at(self.last_boundary()))
                            .done();
                        self.diagnostics.push(d);
                        ExprKind::Bad.wrap(Span::at(self.last_boundary()))
                    }
                }
            });
            ExprKind::UnaryOp(op, Box::new(inner)).wrap(span)
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
                        .with_pos(self.last_boundary())
                        .annotate_primary(Note::Here, Span::at(self.last_boundary()))
                        .done();
                    self.diagnostics.push(d);
                    ExprKind::Bad.wrap(Span::at(self.last_boundary()))
                }
            };

            let span = expr.span.join(expr_right.span);
            expr = ExprKind::BinaryOp(op, Box::new(expr), Box::new(expr_right)).wrap(span);
        }

        Some(expr)
    }

    fn parse_primary_expression(&mut self) -> Option<Expr> {
        let (expr, mut span) = take_span!(self => {
            if self.try_match_token::<Ampersand>().is_some() {
                let expr = match self.parse_primary_expression() {
                    Some(expr) => expr,
                    None => {
                        let d = diagnostics::create_diagnostic()
                            .with_kind(DiagnosticKind::ExpectedExpression)
                            .with_severity(Severity::Error)
                            .with_pos(self.last_boundary())
                            .annotate_primary(Note::Here, Span::at(self.last_boundary()))
                            .done();
                        self.diagnostics.push(d);
                        ExprKind::Bad.wrap(Span::at(self.last_boundary()))
                    },
                };

                return Some(ExprKind::Ref(Box::new(expr)));
            }

            if self.try_match_token::<DebugKw>().is_some() {
                return Some(ExprKind::Debug(Box::new(self.expect_expression())));
            }

            if self.try_match_token::<AllocKw>().is_some() {
                self.match_token::<LessThan>();
                let ty = self.expect_type_expression();
                self.match_token::<GreaterThan>();
                self.match_token::<LeftParen>();
                let size = self.expect_expression();
                self.match_token::<RightParen>();
                return Some(ExprKind::Alloc(Box::new(ty), Box::new(size)))
            }

            if let Some(expr) = self.try_parse_data_struct_init() {
                return Some(expr);
            }

            // not sure about this, but instead of lexing floating-point numbers
            // we can parse them like so, and concatenate the digits to parse a float instead
            // this allows other syntax "x.0.2.1" to work instead of having to
            // insert parentheses everywhere, for instance "((x.0).2).1"
            if let Some(left) = self.try_match_token::<Integer>() {
                if self.try_match_token::<Dot>().is_none() {
                    return Some(ExprKind::Integer(left.0));
                }
                let num_left = left.0;
                let num_right = self.try_match_token::<Integer>().unwrap_or_default().0;
                let decimal = format!("{num_left}.{num_right}").parse::<f64>().unwrap();
                return Some(ExprKind::Decimal(decimal));
            }

            if self.try_match_token::<TodoKw>().is_some() {
                return Some(ExprKind::Todo);
            }

            if self.try_match_token::<UnreachableKw>().is_some() {
                return Some(ExprKind::Unreachable);
            }

            if self.try_match_token::<MalformedNumeral>().is_some() {
                return Some(ExprKind::Bad);
            }

            if let Some(id) = self.try_match_token::<Identifier>() {
                return Some(ExprKind::Identifier(id.0));
            }

            if self.try_match_token::<TrueKw>().is_some() {
                return Some(ExprKind::Boolean(true));
            }

            if self.try_match_token::<FalseKw>().is_some() {
                return Some(ExprKind::Boolean(false));
            }

            if let Some(s) = self.try_match_token::<StringLit>() {
                return Some(ExprKind::StringLiteral(s.0));
            }

            if let Some(expr) = self.parse_case_expression() {
                return Some(expr);
            }

            let delimited = self.parse_delimited_sequence::<LeftParen, RightParen, Comma, _, _>(Self::parse_expression);
            if let Some(mut exprs) = delimited {
                if exprs.is_empty() {
                    return Some(ExprKind::Unit);
                }

                let head = exprs.remove(0);
                return if exprs.is_empty() {
                    Some(ExprKind::Parenthesized(Box::new(head)))
                } else {
                    Some(ExprKind::Tuple(Box::new(head), exprs.into()))
                };
            }

            let delimited = self.parse_delimited_sequence::<LeftBracket, RightBracket, Comma, _, _>(Self::parse_expression);
            if let Some(exprs) = delimited {
                return Some(ExprKind::Array(exprs.into_boxed_slice()));
            }

            None
        });

        let mut expr = expr?.wrap(span);
        loop {
            let is_newline = self.did_skip_newlines();

            if self.try_match_token::<LeftBracket>().is_some() {
                let index_expr = self.expect_expression();
                self.match_token::<RightBracket>();
                span = span.join(self.last_span());
                expr = ExprKind::Index(Box::new(expr), Box::new(index_expr)).wrap(span);
                continue;
            }

            if !is_newline && self.try_match_token::<LeftParen>().is_some() {
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

                span = span.join(self.last_span());
                expr = ExprKind::Call(Box::new(expr), params.into()).wrap(span);
                continue;
            }

            if self.try_match_token::<Dot>().is_some() {
                if let Some(num) = self.try_match_token::<Integer>() {
                    let index = num.0 as u64;
                    span = span.join(self.last_span());
                    expr = ExprKind::ImmediateIndex(Box::new(expr), index).wrap(span);
                    continue;
                }

                if let Some(id) = self.try_match_token::<Identifier>() {
                    let name = Spanned {
                        value: id.0,
                        span: self.last_span(),
                    };
                    span = span.join(self.last_span());
                    expr = ExprKind::FieldAccess(Box::new(expr), name).wrap(span);
                    continue;
                }

                if self.try_match_token::<Star>().is_some() {
                    span = span.join(self.last_span());
                    expr = ExprKind::Deref(Box::new(expr)).wrap(span);
                    continue;
                }

                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::ExpectedAccess)
                    .with_pos(self.last_boundary())
                    .with_severity(Severity::Error)
                    .annotate_primary(Note::Here, Span::at(self.last_boundary()))
                    .done();
                self.diagnostics.push(d);

                expr = ExprKind::Bad.wrap(span);
                continue;
            }

            if self.try_match_token::<WithKw>().is_some() {
                self.skip_newlines();
                self.match_token::<LeftBrace>();
                self.skip_newlines();

                let mut fields = Vec::new();
                while let Some(id) = self.try_match_token::<Identifier>() {
                    let field_name = Spanned {
                        value: id.0,
                        span: self.last_span(),
                    };

                    self.match_token::<Equal>();
                    let field_value = self.expect_expression();
                    fields.push((field_name, field_value));

                    if matches!(self.look_ahead(), Token::RightBrace(_, _)) {
                        break;
                    }

                    if self.try_match_token::<Comma>().is_some() {
                        self.skip_newlines()
                    } else {
                        self.expect_newlines_or_eof();
                    }
                }

                self.skip_newlines();
                self.match_token::<RightBrace>();
                self.skip_newlines();

                span = span.join(self.last_span());
                expr = ExprKind::DataWith(Box::new(expr), fields.into()).wrap(span);
                continue;
            }

            break;
        }

        Some(expr)
    }

    fn parse_case_expression(&mut self) -> Option<ExprKind> {
        self.try_match_token::<CaseKw>()?;
        let case_kw_span = self.last_span();

        self.skip_newlines();
        if self.try_match_token::<LeftBrace>().is_none() {
            // short-form case-otherwise
            let guard = self.expect_expression();
            self.match_token::<ThenKw>();
            let expr = self.expect_expression();

            self.skip_newlines();
            self.match_token::<OtherwiseKw>();
            let fallback = self.expect_expression();

            return Some(ExprKind::TernaryCase(
                Box::new(guard),
                Box::new(expr),
                Box::new(fallback),
                case_kw_span,
            ));
        }

        self.skip_newlines();

        let mut paths = Vec::new();
        loop {
            self.skip_newlines();
            if let Some(guard) = self.parse_expression() {
                self.match_token::<ThenKw>();
                let expr = self.expect_expression();
                paths.push((Some(guard), expr));
                self.expect_newlines_or_eof();
                continue;
            }

            if self.try_match_token::<OtherwiseKw>().is_some() {
                let expr = self.expect_expression();
                paths.push((None, expr));
                self.expect_newlines_or_eof();
                continue;
            }

            break;
        }

        self.skip_newlines();
        self.match_token::<RightBrace>();
        self.skip_newlines();

        Some(ExprKind::Case(paths.into_boxed_slice(), case_kw_span))
    }

    fn try_parse_data_struct_init(&mut self) -> Option<ExprKind> {
        let backup = self.save_backup();
        let id = Spanned {
            value: self.try_match_token::<Identifier>()?.0,
            span: self.last_span(),
        };

        self.skip_newlines();
        let Some(_) = self.try_match_token::<LeftBrace>() else {
            self.rewind_backup(backup);
            return None;
        };
        self.skip_newlines();

        let mut fields = Vec::new();
        while let Some(id) = self.try_match_token::<Identifier>() {
            let field_name = Spanned {
                value: id.0,
                span: self.last_span(),
            };

            self.match_token::<Equal>();
            let field_value = self.expect_expression();
            fields.push((field_name, field_value));

            if matches!(self.look_ahead(), Token::RightBrace(_, _)) {
                break;
            }

            if self.try_match_token::<Comma>().is_some() {
                self.skip_newlines()
            } else {
                self.expect_newlines_or_eof();
            }
        }

        self.skip_newlines();
        self.match_token::<RightBrace>();
        self.skip_newlines();

        Some(ExprKind::DataInit(id, fields.into()))
    }

    fn expect_type_expression(&mut self) -> Type {
        match self.parse_type_expression() {
            Some(ty) => ty,
            None => {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::ExpectedType)
                    .with_severity(Severity::Error)
                    .with_pos(self.last_boundary())
                    .annotate_primary(Note::Here, Span::at(self.last_boundary()))
                    .done();
                self.diagnostics.push(d);
                TypeKind::Bad.wrap(Span::at(self.last_span().from))
            }
        }
    }

    fn parse_type_expression(&mut self) -> Option<Type> {
        let (ty, span) = take_span!(self => {
            let mut types = Vec::new();
            while let Some(ty) = self.parse_primary_type_expression() {
                types.push(ty);
                if matches!(self.look_ahead(), Token::RightArrow(_, _)) {
                    break;
                }
            }

            if types.is_empty() {
                return None;
            }

            if self.try_match_token::<RightArrow>().is_some() {
                let ret_ty = self.expect_type_expression();
                return Some(TypeKind::Func(types.into(), Box::new(ret_ty)));
            }

            if types.len() > 1 {
                self.match_token::<RightArrow>();
                let ret_ty = self.expect_type_expression();
                Some(TypeKind::Func(types.into(), Box::new(ret_ty)))
            } else {
                Some(types.swap_remove(0).value)
            }
        });

        Some(ty?.wrap(span))
    }

    fn parse_primary_type_expression(&mut self) -> Option<Type> {
        let delimited = take_span!(
            self => self.parse_delimited_sequence::<LeftParen, RightParen, Comma, _, _>(Self::parse_type_expression)
        );
        if let (Some(mut tys), span) = delimited {
            if tys.is_empty() {
                return Some(TypeKind::Unit.wrap(span));
            }

            let head = tys.remove(0);
            return if tys.is_empty() {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::SingletonTypeSyntax)
                    .with_span(span)
                    .with_severity(Severity::Warning)
                    .annotate_primary(Note::CanRemoveParentheses, span)
                    .done();
                self.diagnostics.push(d);
                Some(head)
            } else {
                Some(TypeKind::Tuple(Box::new(head), tys.into()).wrap(span))
            };
        }

        let (ty, span) = take_span!(self => {
            if self.try_match_token::<Ampersand>().is_some() {
                let ty = self.expect_type_expression();
                return Some(TypeKind::Ref(Box::new(ty)));
            }

            if let Some(id) = self.try_match_token::<Identifier>() {
                return Some(TypeKind::Declared(id.0));
            }

            if self.try_match_token::<RightArrow>().is_some() {
                let ty = self.expect_type_expression();
                return Some(TypeKind::Func(Box::new([]), Box::new(ty)));
            }

            if self.try_match_token::<LeftBracket>().is_some() {
                let size = if let Some(size) = self.try_match_token::<Integer>() {
                    Some(size.0 as usize)
                } else if self.try_match_token::<Ampersand>().is_some() {
                    None
                } else {
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::ExpectedArraySizeOrAmpersand)
                        .with_severity(Severity::Error)
                        .with_pos(self.last_boundary())
                        .annotate_primary(Note::Here, Span::at(self.last_boundary()))
                        .done();
                    self.diagnostics.push(d);
                    return None;
                };

                self.match_token::<RightBracket>();
                let ty = self.expect_type_expression();

                return match size {
                    Some(size) => Some(TypeKind::Array(Box::new(ty), size)),
                    None => Some(TypeKind::Pointer(Box::new(ty))),
                };
            }

            None
        });

        Some(ty?.wrap(span))
    }

    fn expect_pattern(&mut self) -> Pattern {
        match self.parse_pattern() {
            Some(p) => p,
            None => {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::ExpectedPattern)
                    .with_severity(Severity::Error)
                    .with_pos(self.last_boundary())
                    .annotate_primary(Note::Here, Span::at(self.last_boundary()))
                    .done();
                self.diagnostics.push(d);
                PatternKind::Bad.wrap(Span::at(self.last_boundary()))
            }
        }
    }

    fn parse_pattern(&mut self) -> Option<Pattern> {
        if let Some(id) = self.try_match_token::<Identifier>() {
            return Some(PatternKind::Binding(id.0).wrap(self.last_span()));
        }

        if self.try_match_token::<Underscore>().is_some() {
            return Some(PatternKind::Discard.wrap(self.last_span()));
        }

        None
    }

    fn parse_delimited_sequence<Left, Right, Sep, T, F>(&mut self, parser: F) -> Option<Vec<T>>
    where
        Left: TokenValue,
        Right: TokenValue,
        Sep: TokenValue,
        F: Fn(&mut Self) -> Option<T>,
    {
        self.try_match_token::<Left>()?;
        let mut elements = Vec::new();
        while let Some(e) = parser(self) {
            elements.push(e);
            if self.try_match_token::<Sep>().is_none() {
                break;
            }
        }
        self.match_token::<Right>();
        Some(elements)
    }

    fn did_skip_newlines(&mut self) -> bool {
        matches!(self.last_token(), Token::Newline(_, _))
    }

    fn expect_newlines_or_eof(&mut self) -> bool {
        matches!(self.last_token(), Token::Newline(_, _))
            || self.match_token_or_eof::<Newline>().is_some()
    }

    fn skip_newlines(&mut self) {
        self.try_match_token::<Newline>();
    }

    fn last_span(&self) -> Span {
        self.last_token().span()
    }

    fn look_ahead(&self) -> &Token {
        &self.tokens[self.token_index]
    }

    fn last_token(&self) -> &Token {
        let fallback = &Token::Eof(Eof, Span::EOF);
        if self.token_index == 0 {
            self.tokens.first().unwrap()
        } else {
            match self.tokens.get(self.token_index - 1) {
                Some(tok) => tok,
                None => fallback,
            }
        }
    }

    fn last_boundary(&self) -> Pos {
        self.tokens
            .iter()
            .take(self.token_index)
            .rev()
            .find(|tok| !matches!(tok, Token::Eof(_, _) | Token::Newline(_, _)))
            .map(|tok| tok.span().to)
            .unwrap_or(Pos::MIN)
    }

    fn save_backup(&self) -> usize {
        self.token_index
    }

    fn rewind_backup(&mut self, backup: usize) {
        self.token_index = backup;
    }

    fn consume_token(&mut self) {
        self.token_index += 1;
        if self.token_index != self.tokens.len() {
            return;
        }

        let next = self.lex();
        self.tokens.push(next);
    }

    fn peek_token<T: TokenValue>(&mut self) -> bool {
        T::is_inside(self.look_ahead())
    }

    fn match_token<T: TokenValue>(&mut self) -> Option<T> {
        match T::extract_from(self.look_ahead()) {
            Some((value, _)) => {
                self.consume_token();
                Some(value)
            }
            None => {
                let tok = self.look_ahead().clone();
                let span = tok.span();
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::ExpectedToken {
                        found: tok,
                        expected: T::kind(),
                    })
                    .with_severity(Severity::Error)
                    .with_span(span)
                    .annotate_primary(Note::ExpectedToken(T::kind()), span)
                    .done();
                self.diagnostics.push(d);
                None
            }
        }
    }

    fn match_token_or_eof<T: TokenValue>(&mut self) -> Option<T> {
        match T::extract_from(self.look_ahead()) {
            Some((value, _)) => {
                self.consume_token();
                Some(value)
            }
            None => match self.try_match_token::<Eof>() {
                Some(_) => Some(T::default()),
                None => {
                    let tok = self.look_ahead().clone();
                    let span = tok.span();
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::ExpectedToken {
                            found: tok,
                            expected: T::kind(),
                        })
                        .with_severity(Severity::Error)
                        .with_span(span)
                        .annotate_primary(Note::ExpectedToken(T::kind()), span)
                        .done();
                    self.diagnostics.push(d);
                    None
                }
            },
        }
    }

    fn try_match_token<T: TokenValue>(&mut self) -> Option<T> {
        match T::extract_from(self.look_ahead()) {
            Some((value, _)) => {
                self.consume_token();
                Some(value)
            }
            None => None,
        }
    }

    fn found_eof(&self) -> bool {
        matches!(self.look_ahead(), Token::Eof(_, _))
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

    fn try_consume_number(&mut self) -> Option<(Option<i64>, Span)> {
        let mut decimals = String::new();
        let start_pos = self.cursor.pos();

        while let Some(c) = self.cursor.peek() {
            if !c.is_ascii_digit() {
                break;
            }

            decimals.push(c);
            self.cursor.next();
        }

        if decimals.is_empty() {
            return None;
        }

        let end_pos = self.cursor.pos();
        let span = Span::new(start_pos, end_pos);
        match decimals.parse() {
            Ok(num) => Some((Some(num), span)),
            Err(e) => {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::InvalidInteger(e))
                    .with_severity(Severity::Error)
                    .with_span(span)
                    .annotate_primary(Note::Quiet, span)
                    .done();
                self.diagnostics.push(d);
                Some((None, span))
            }
        }
    }

    fn try_consume_string_literal(&mut self) -> Option<(String, Span)> {
        let start_pos = self.cursor.pos();
        let Some('"') = self.cursor.peek() else {
            return None;
        };

        self.cursor.next();
        let mut end_pos = self.cursor.pos();
        let mut boundary = self.cursor.pos();
        let mut chars = String::new();
        let mut closed = false;
        while let Some(c) = self.cursor.peek() {
            let prev_pos = end_pos;

            self.cursor.next();
            end_pos = self.cursor.pos();
            if !c.is_whitespace() {
                boundary = end_pos;
            }

            if c == '"' {
                closed = true;
                break;
            }

            if c == '\\' {
                if let Some(c) = self.cursor.next() {
                    end_pos = self.cursor.pos();
                    boundary = end_pos;
                    let span = Span::new(prev_pos, self.cursor.pos());
                    match c {
                        'n' => chars.push('\n'),
                        'r' => chars.push('\r'),
                        '\\' => chars.push('\\'),
                        '0' => chars.push('\0'),
                        '"' => chars.push('"'),
                        _ => {
                            let d = diagnostics::create_diagnostic()
                                .with_kind(DiagnosticKind::InvalidEscapeSequence)
                                .with_severity(Severity::Error)
                                .with_span(span)
                                .annotate_primary(Note::Here, span)
                                .done();
                            self.diagnostics.push(d);
                        }
                    }
                }
            } else {
                chars.push(c);
            }
        }

        if !closed {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::MissingQuote)
                .with_severity(Severity::Error)
                .with_pos(boundary)
                .annotate_primary(Note::Here, Span::at(boundary))
                .done();
            self.diagnostics.push(d);
            end_pos = boundary
        }

        let span = Span::new(start_pos, end_pos);
        Some((chars, span))
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
                    return Eof.wrap(Span::at(self.last_boundary()));
                };

                if c.is_whitespace() && !Self::is_newline_character(c) {
                    self.cursor.next();
                    continue;
                }

                break;
            }

            if self.cursor.peek().is_none() {
                return Eof.wrap(Span::at(self.last_boundary()));
            }

            let start_pos = self.cursor.pos();

            if self.try_consume_newlines() {
                return Newline.wrap(Span::at(start_pos));
            }

            match_by_string!(self, "->" => RightArrow);
            match_by_string!(self, "++" => PlusPlus);
            match_by_string!(self, "==" => EqualEqual);
            match_by_string!(self, "!=" => NotEqual);
            match_by_string!(self, "<=" => LessEqual);
            match_by_string!(self, ">=" => GreaterEqual);
            match_by_string!(self, "<" => LessThan);
            match_by_string!(self, ">" => GreaterThan);
            match_by_string!(self, "(" => LeftParen);
            match_by_string!(self, ")" => RightParen);
            match_by_string!(self, "[" => LeftBracket);
            match_by_string!(self, "]" => RightBracket);
            match_by_string!(self, "{" => LeftBrace);
            match_by_string!(self, "}" => RightBrace);
            match_by_string!(self, "." => Dot);
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

            if let Some((string, span)) = self.try_consume_string_literal() {
                return StringLit(string).wrap(span);
            }

            if let Some((result, span)) = self.try_consume_number() {
                return match result {
                    Some(num) => Integer(num).wrap(span),
                    None => MalformedNumeral.wrap(span),
                };
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
                    "xor" => XorKw.wrap(span),
                    "not" => NotKw.wrap(span),
                    "case" => CaseKw.wrap(span),
                    "otherwise" => OtherwiseKw.wrap(span),
                    "type" => TypeKw.wrap(span),
                    "opaque" => OpaqueKw.wrap(span),
                    "with" => WithKw.wrap(span),
                    "alloc" => AllocKw.wrap(span),
                    "todo" => TodoKw.wrap(span),
                    "unreachable" => UnreachableKw.wrap(span),
                    "debug" => DebugKw.wrap(span),
                    "print" => PrintKw.wrap(span),
                    "_" => Underscore.wrap(span),
                    _ => Identifier(id).wrap(span),
                };
            }

            match self.cursor.peek() {
                Some(u) => {
                    let pos_from = self.cursor.pos();
                    self.cursor.next();
                    let pos_to = self.cursor.pos();

                    let span = Span::new(pos_from, pos_to);
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::IllegalCharacter(u))
                        .with_severity(Severity::Error)
                        .with_span(Span::new(pos_from, pos_to))
                        .annotate_primary(Note::Here, span)
                        .done();
                    self.diagnostics.push(d);
                    continue;
                }
                None => return Eof.wrap(Span::at(self.last_boundary())),
            }
        }
    }
}

macro_rules! take_span {
    ($self:ident => $e:expr) => {{
        let from = $self.look_ahead().span().from;
        #[allow(clippy::redundant_closure_call)]
        let res = (|| $e)();
        let to = $self.last_boundary();
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
