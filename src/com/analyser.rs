use std::{
    collections::{BTreeMap, HashMap},
    mem,
};

use super::{
    abt::*,
    ast::*,
    diagnostics::{self, *},
    pos::Pos,
    span::*,
};

struct Scope {
    name: String,
    quiet: bool,
    parent: Option<Box<Scope>>,
    blocking: bool,

    variables: HashMap<String, Variable>,
    variable_counter: u16,
    local_count: u16,

    functions: HashMap<String, Function>,
    function_count: u32,

    return_type: TypeAbt,
}

impl Default for Scope {
    fn default() -> Self {
        Self {
            name: Default::default(),
            quiet: false,
            parent: None,
            blocking: false,

            variables: Default::default(),
            variable_counter: 0,
            local_count: 0,

            functions: Default::default(),
            function_count: 0,

            return_type: TypeAbt::Unknown,
        }
    }
}

impl Scope {
    pub fn declare_variable(&mut self, name: String, ty: TypeAbt) -> bool {
        let id = self
            .variables
            .get(name.as_str())
            .map(|v| v.id as u16)
            .unwrap_or(self.variable_counter);

        self.variables.insert(name, Variable { id: id as u8, ty });
        if id >= self.variable_counter {
            self.variable_counter += 1;
        }

        self.variable_counter <= 256
    }

    pub fn get_variable(&self, name: &str) -> Option<&Variable> {
        match self.variables.get(name) {
            Some(var) => Some(var),
            None if !self.blocking => match self.parent {
                Some(ref parent) => parent.get_variable(name),
                None => None,
            },
            None => None,
        }
    }

    pub fn declare_function(
        &mut self,
        name: String,
        signature: (Vec<TypeAbt>, TypeAbt),
        local_count: u8,
        code: StmtAbt,
    ) {
        let id = self.function_count;
        self.function_count += 1;
        self.functions.insert(
            name,
            Function {
                id,
                param_types: signature.0,
                return_type: signature.1,
                local_count,
                code,
            },
        );
    }

    pub fn update_function_declaration(&mut self, name: String, code: StmtAbt, local_count: u8) {
        self.functions.entry(name).and_modify(|s| {
            s.code = code;
            s.local_count = local_count;
        });
    }

    pub fn get_function(&self, name: &str) -> Option<&Function> {
        match self.functions.get(name) {
            Some(signature) => Some(signature),
            None => match self.parent {
                Some(ref parent) => parent.get_function(name),
                None => None,
            },
        }
    }
}

pub struct Analyser<'a> {
    diagnostics: &'a mut Diagnostics,
    scope: Scope,
}

impl<'a> Analyser<'a> {
    pub fn new(diagnostics: &'a mut Diagnostics) -> Self {
        Self {
            diagnostics,
            scope: Scope::default(),
        }
    }

    fn open_scope(&mut self) {
        let parent = mem::take(&mut self.scope);
        let return_type = parent.return_type.clone();
        let variable_count = parent.variable_counter;
        let function_count = parent.function_count;
        self.scope.parent = Some(Box::new(parent));
        self.scope.return_type = return_type;
        self.scope.variable_counter = variable_count;
        self.scope.function_count = function_count;
    }

    fn close_scope(&mut self) -> u16 {
        let mut parent =
            mem::take(&mut self.scope.parent).expect("attempted to close non-existing scope");

        let final_local_count =
            self.scope.local_count + self.scope.variable_counter - parent.variable_counter;
        if !self.scope.blocking {
            parent.local_count = final_local_count;
        }

        let functions = mem::take(&mut self.scope.functions);
        parent.function_count = self.scope.function_count;

        for (name, func) in functions.into_iter() {
            let new_name = if self.scope.quiet {
                name
            } else {
                format!("{}.{name}", parent.name)
            };
            parent.functions.insert(new_name, func);
        }

        self.scope = *parent;
        final_local_count
    }

    pub fn analyse_program(mut self, ast: &StmtAst) -> ProgramAbt {
        self.scope.return_type = TypeAbt::Unit;
        self.scope.name = "@".to_string();
        self.scope.blocking = true;

        let bound_program = self.analyse_statement(ast);

        if !self.analyse_control_flow(&bound_program) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::TopLevelMustReturn)
                .with_severity(Severity::Error)
                .without_span()
                .done();
            self.diagnostics.push(d);
        }

        let signature = (vec![], TypeAbt::Unit);
        let local_count = self.scope.local_count as u8;
        self.scope
            .declare_function("@".to_string(), signature, local_count, bound_program);

        let mut functions_by_id = BTreeMap::new();
        for (name, func) in self.scope.functions.into_iter() {
            functions_by_id.insert(func.id, (name, func));
        }

        ProgramAbt { functions_by_id }
    }

    // returns whether the provided statement is guaranteed to return
    fn analyse_control_flow(&mut self, stmt: &StmtAbt) -> bool {
        match &stmt.kind {
            StmtAbtKind::Block(stmts) => {
                let mut does_return = false;

                let mut iter = stmts.iter();
                for stmt in iter.by_ref() {
                    if self.analyse_control_flow(stmt) {
                        does_return = true;
                        break;
                    }
                }

                let remaining = iter.collect::<Vec<_>>();
                if let (Some(first), Some(last)) = (remaining.first(), remaining.last()) {
                    let d = diagnostics::create_diagnostic()
                        .with_kind(DiagnosticKind::UnreachableCode)
                        .with_severity(Severity::Warning)
                        .with_span(Span::join(first.span, last.span))
                        .done();
                    self.diagnostics.push(d);
                }

                does_return
            }
            StmtAbtKind::Empty => false,
            StmtAbtKind::Expr(_) => false,
            StmtAbtKind::IfThen(_, _) => false,
            StmtAbtKind::IfThenElse(_, body_then, body_else) => {
                self.analyse_control_flow(body_then.as_ref())
                    && self.analyse_control_flow(body_else.as_ref())
            }
            StmtAbtKind::WhileDo(_, body) => self.analyse_control_flow(body.as_ref()),
            StmtAbtKind::DoWhile(body, _) => self.analyse_control_flow(body.as_ref()),
            StmtAbtKind::Return(_) => true,
        }
    }

    #[rustfmt::skip]
    fn analyse_statement(&mut self, stmt: &StmtAst) -> StmtAbt {
        match &stmt.kind {
            StmtAstKind::Empty => StmtAbtKind::Empty,
            StmtAstKind::VarDef(id, expr)
                => self.analyse_variable_definition(id, expr),
            StmtAstKind::Expr(expr)
                => StmtAbtKind::Expr(Box::new(self.analyse_expression(expr))),
            StmtAstKind::Block(stmts)
                => self.analyse_block_statement(stmts),
            StmtAstKind::IfThen(guard, body_then)
                => self.analyse_if_then_statement(guard, body_then),
            StmtAstKind::Then(body_then)
                => self.analyse_then_statement(body_then),
            StmtAstKind::IfThenElse(guard, body_then, body_else)
                => self.analyse_if_then_else_statement(guard, body_then, body_else),
            StmtAstKind::Else(body_else)
                => self.analyse_else_statement(body_else),
            StmtAstKind::WhileDo(guard, body)
                => self.analyse_while_do_statement(guard, body),
            StmtAstKind::DoWhile(body, guard)
                => self.analyse_do_while_statement(body, guard),
            StmtAstKind::Do(body)
                => self.analyse_do_statement(body),
            StmtAstKind::Func(name, params, body, ty)
                => self.analyse_function_definition(name, params, body, ty),
            StmtAstKind::Return
                => self.analyse_return_statement(stmt.span),
            StmtAstKind::ReturnWith(expr)
                => self.analyse_return_with_statement(expr),
        }.wrap(stmt.span)
    }

    fn analyse_variable_definition(
        &mut self,
        id: &Option<(String, Span)>,
        expr: &ExprAst,
    ) -> StmtAbtKind {
        let bound_expr = self.analyse_expression(expr);

        let Some((name, span)) = id else {
            return StmtAbtKind::Empty;
        };

        let ok = self.scope.declare_variable(name.clone(), bound_expr.ty());
        if !ok {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::TooManyVariables)
                .with_severity(Severity::Error)
                .with_span(*span)
                .done();
            self.diagnostics.push(d);
        }

        let var = self.scope.get_variable(name).unwrap().clone();

        // variable definitions are just (the first) assignment
        StmtAbtKind::Expr(Box::new(ExprAbt::Assignment(var, Box::new(bound_expr))))
    }

    fn analyse_block_statement(&mut self, stmts: &[StmtAst]) -> StmtAbtKind {
        self.open_scope();
        let bound_stmts = stmts
            .iter()
            .map(|s| self.analyse_statement(s))
            .collect::<Vec<_>>();
        self.close_scope();

        match bound_stmts.first() {
            None => StmtAbtKind::Empty,
            Some(StmtAbt {
                kind: StmtAbtKind::Empty,
                span: _,
            }) if bound_stmts.len() == 1 => StmtAbtKind::Empty,
            _ => StmtAbtKind::Block(bound_stmts),
        }
    }

    fn analyse_if_then_statement(&mut self, guard: &ExprAst, body: &StmtAst) -> StmtAbtKind {
        let bound_guard = self.analyse_expression(guard);
        let bound_body = self.analyse_statement(body);

        if matches!(bound_body.kind, StmtAbtKind::Empty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::EmptyThenStatement)
                .with_severity(Severity::Warning)
                .with_span(body.span)
                .done();
            self.diagnostics.push(d);
        }

        if !bound_guard.ty().is(&TypeAbt::Bool) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::GuardNotBoolean)
                .with_severity(Severity::Error)
                .with_span(guard.span)
                .done();
            self.diagnostics.push(d);
        }

        StmtAbtKind::IfThen(Box::new(bound_guard), Box::new(bound_body))
    }

    fn analyse_if_then_else_statement(
        &mut self,
        guard: &ExprAst,
        body_then: &StmtAst,
        body_else: &StmtAst,
    ) -> StmtAbtKind {
        let bound_guard = self.analyse_expression(guard);
        let bound_body_then = self.analyse_statement(body_then);
        let bound_body_else = self.analyse_statement(body_else);

        if matches!(bound_body_then.kind, StmtAbtKind::Empty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::EmptyThenStatement)
                .with_severity(Severity::Warning)
                .with_span(body_then.span)
                .done();
            self.diagnostics.push(d);
        }

        if matches!(bound_body_else.kind, StmtAbtKind::Empty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::EmptyElseStatement)
                .with_severity(Severity::Warning)
                .with_span(body_else.span)
                .done();
            self.diagnostics.push(d);
        }

        if !bound_guard.ty().is(&TypeAbt::Bool) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::GuardNotBoolean)
                .with_severity(Severity::Error)
                .with_span(guard.span)
                .done();
            self.diagnostics.push(d);
        }

        StmtAbtKind::IfThenElse(
            Box::new(bound_guard),
            Box::new(bound_body_then),
            Box::new(bound_body_else),
        )
    }

    fn analyse_then_statement(&mut self, body_then: &StmtAst) -> StmtAbtKind {
        let d = diagnostics::create_diagnostic()
            .with_kind(DiagnosticKind::ThenWithoutIf)
            .with_severity(Severity::Error)
            .with_span(body_then.span)
            .done();
        self.diagnostics.push(d);
        StmtAbtKind::Empty
    }

    fn analyse_else_statement(&mut self, body_else: &StmtAst) -> StmtAbtKind {
        let d = diagnostics::create_diagnostic()
            .with_kind(DiagnosticKind::ElseWithoutIfThen)
            .with_severity(Severity::Error)
            .with_span(body_else.span)
            .done();
        self.diagnostics.push(d);
        StmtAbtKind::Empty
    }

    fn analyse_while_do_statement(&mut self, guard: &ExprAst, body: &StmtAst) -> StmtAbtKind {
        let bound_guard = self.analyse_expression(guard);
        let bound_body = self.analyse_statement(body);

        if matches!(bound_body.kind, StmtAbtKind::Empty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::EmptyWhileDoStatement)
                .with_severity(Severity::Warning)
                .with_span(body.span)
                .done();
            self.diagnostics.push(d);
        }

        if !bound_guard.ty().is(&TypeAbt::Bool) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::GuardNotBoolean)
                .with_severity(Severity::Error)
                .with_span(guard.span)
                .done();
            self.diagnostics.push(d);
        }

        StmtAbtKind::WhileDo(Box::new(bound_guard), Box::new(bound_body))
    }

    fn analyse_do_while_statement(&mut self, body: &StmtAst, guard: &ExprAst) -> StmtAbtKind {
        let bound_body = self.analyse_statement(body);
        let bound_guard = self.analyse_expression(guard);

        if matches!(bound_body.kind, StmtAbtKind::Empty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::EmptyDoWhileStatement)
                .with_severity(Severity::Warning)
                .with_span(body.span)
                .done();
            self.diagnostics.push(d);
        }

        if !bound_guard.ty().is(&TypeAbt::Bool) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::GuardNotBoolean)
                .with_severity(Severity::Error)
                .with_span(guard.span)
                .done();
            self.diagnostics.push(d);
        }

        StmtAbtKind::DoWhile(Box::new(bound_body), Box::new(bound_guard))
    }

    fn analyse_do_statement(&mut self, body: &StmtAst) -> StmtAbtKind {
        let d = diagnostics::create_diagnostic()
            .with_kind(DiagnosticKind::DoWithoutWhile)
            .with_severity(Severity::Error)
            .with_span(body.span)
            .done();
        self.diagnostics.push(d);
        StmtAbtKind::Empty
    }

    fn analyse_function_definition(
        &mut self,
        name: &Option<String>,
        params: &[(String, TypeAst)],
        body: &StmtAst,
        ty: &TypeAst,
    ) -> StmtAbtKind {
        let Some(name) = name else {
            return StmtAbtKind::Empty;
        };

        let bound_params = params
            .iter()
            .map(|(name, ty)| (name, self.analyse_type(ty)))
            .collect::<Vec<_>>();
        let bound_ty = self.analyse_type(ty);

        let bound_params_ty = bound_params
            .iter()
            .map(|(_, ty)| ty.clone())
            .collect::<Vec<_>>();

        let signature = (bound_params_ty, bound_ty.clone());
        self.scope.declare_function(
            name.clone(),
            signature,
            0,
            StmtAbtKind::Empty.wrap(Span::at(Pos::MIN)),
        );

        self.open_scope();
        self.scope.name = name.clone();
        self.scope.quiet = true; // don't nest name
        self.scope.return_type = bound_ty;
        self.scope.variable_counter = 0; // reset local counter
        self.scope.blocking = true; // prevent from accessing variables from outside the function

        // this will have to be changed at some point so that it is possible for functions to capture variables

        for (param_name, param_ty) in bound_params {
            self.scope.declare_variable(param_name.clone(), param_ty);
        }

        let bound_body = self.analyse_statement(body);
        if !self.analyse_control_flow(&bound_body) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::NotAllPathsReturn)
                .with_severity(Severity::Error)
                .with_span(ty.span)
                .done();
            self.diagnostics.push(d);
        }

        let local_count = self.close_scope() as u8;
        self.scope
            .update_function_declaration(name.clone(), bound_body, local_count);

        StmtAbtKind::Empty
    }

    fn analyse_return_statement(&mut self, span: Span) -> StmtAbtKind {
        let ty = TypeAbt::Unit;

        if !ty.is(&self.scope.return_type) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::MustReturnValue {
                    expected: self.scope.return_type.clone(),
                })
                .with_severity(Severity::Error)
                .with_span(span)
                .done();
            self.diagnostics.push(d);
            return StmtAbtKind::Return(Box::new(ExprAbt::Unknown));
        }

        StmtAbtKind::Return(Box::new(ExprAbt::Unit))
    }

    fn analyse_return_with_statement(&mut self, expr: &ExprAst) -> StmtAbtKind {
        let bound_expr = self.analyse_expression(expr);
        let ty_expr = bound_expr.ty();

        if !ty_expr.is(&self.scope.return_type) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::TypeMismatch {
                    found: ty_expr,
                    expected: self.scope.return_type.clone(),
                })
                .with_severity(Severity::Error)
                .with_span(expr.span)
                .done();
            self.diagnostics.push(d);
            return StmtAbtKind::Return(Box::new(ExprAbt::Unknown));
        }

        StmtAbtKind::Return(Box::new(bound_expr))
    }

    #[rustfmt::skip]
    fn analyse_expression(&mut self, expr: &ExprAst) -> ExprAbt {
        match &expr.kind {
            ExprAstKind::Bad
                => ExprAbt::Unknown,
            ExprAstKind::Number(num)
                => ExprAbt::Number(*num),
            ExprAstKind::Identifier(id)
                => self.analyse_variable_expression(id, expr.span),
            ExprAstKind::Boolean(b)
                => ExprAbt::Boolean(*b),
            ExprAstKind::Parenthesized(inner)
                => self.analyse_expression(inner),
            ExprAstKind::BinaryOp(op, left, right)
                => self.analyse_binary_operation(*op, left, right),
            ExprAstKind::UnaryOp(op, operand)
                => self.analyse_unary_operation(*op, operand, expr.span),
            ExprAstKind::Call(name, params)
                => self.analyse_call(name, params, expr.span),
        }
    }

    fn analyse_variable_expression(&mut self, id: &str, span: Span) -> ExprAbt {
        match self.scope.get_variable(id) {
            Some(var) => ExprAbt::Variable(var.clone()),
            None => {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::UnknownVariable(id.to_string()))
                    .with_severity(Severity::Error)
                    .with_span(span)
                    .done();
                self.diagnostics.push(d);
                ExprAbt::Unknown
            }
        }
    }

    fn analyse_binary_operation(
        &mut self,
        op: BinOpAst,
        left: &ExprAst,
        right: &ExprAst,
    ) -> ExprAbt {
        if matches!(op, BinOpAst::Assign) {
            return self.analyse_assignment(left, right);
        }

        let bound_left = self.analyse_expression(left);
        let bound_right = self.analyse_expression(right);

        let ty_left = bound_left.ty();
        let ty_right = bound_right.ty();

        if !ty_left.is_known() || !ty_right.is_known() {
            return ExprAbt::Unknown;
        }

        if ty_left == ty_right {
            use TypeAbt as Ty;
            let ty = ty_left.clone();
            let bound_op = match ty {
                Ty::U8 => Self::integer_binary_operation(op, ty),
                Ty::U16 => Self::integer_binary_operation(op, ty),
                Ty::U32 => Self::integer_binary_operation(op, ty),
                Ty::U64 => Self::integer_binary_operation(op, ty),
                Ty::I8 => Self::integer_binary_operation(op, ty),
                Ty::I16 => Self::integer_binary_operation(op, ty),
                Ty::I32 => Self::integer_binary_operation(op, ty),
                Ty::I64 => Self::integer_binary_operation(op, ty),
                Ty::F32 => Self::decimal_binary_operation(op, ty),
                Ty::F64 => Self::decimal_binary_operation(op, ty),
                Ty::Bool => Self::boolean_binary_operation(op),
                _ => None,
            };

            if let Some(op) = bound_op {
                return ExprAbt::Binary(op, Box::new(bound_left), Box::new(bound_right));
            }
        }

        let d = diagnostics::create_diagnostic()
            .with_kind(DiagnosticKind::InvalidBinaryOperation {
                op,
                left: ty_left,
                right: ty_right,
            })
            .with_severity(Severity::Error)
            .with_span(right.span.join(left.span))
            .done();
        self.diagnostics.push(d);
        ExprAbt::Unknown
    }

    fn integer_binary_operation(op: BinOpAst, ty: TypeAbt) -> Option<BinOpAbt> {
        match op {
            BinOpAst::Add => Some(BinOpAbtKind::Add.wrap(ty.clone(), ty)),
            BinOpAst::Sub => Some(BinOpAbtKind::Sub.wrap(ty.clone(), ty)),
            BinOpAst::Mul => Some(BinOpAbtKind::Mul.wrap(ty.clone(), ty)),
            BinOpAst::Div => Some(BinOpAbtKind::Div.wrap(ty.clone(), ty)),
            BinOpAst::Rem => Some(BinOpAbtKind::Rem.wrap(ty.clone(), ty)),
            BinOpAst::Eq => Some(BinOpAbtKind::Eq.wrap(ty, TypeAbt::Bool)),
            BinOpAst::Ne => Some(BinOpAbtKind::Ne.wrap(ty, TypeAbt::Bool)),
            BinOpAst::Le => Some(BinOpAbtKind::Le.wrap(ty, TypeAbt::Bool)),
            BinOpAst::Lt => Some(BinOpAbtKind::Lt.wrap(ty, TypeAbt::Bool)),
            BinOpAst::Ge => Some(BinOpAbtKind::Ge.wrap(ty, TypeAbt::Bool)),
            BinOpAst::Gt => Some(BinOpAbtKind::Gt.wrap(ty, TypeAbt::Bool)),
            BinOpAst::BitAnd => Some(BinOpAbtKind::BitAnd.wrap(ty.clone(), ty)),
            BinOpAst::BitXor => Some(BinOpAbtKind::BitXor.wrap(ty.clone(), ty)),
            BinOpAst::BitOr => Some(BinOpAbtKind::BitOr.wrap(ty.clone(), ty)),
            _ => None,
        }
    }

    fn decimal_binary_operation(op: BinOpAst, ty: TypeAbt) -> Option<BinOpAbt> {
        match op {
            BinOpAst::Add => Some(BinOpAbtKind::Add.wrap(ty.clone(), ty)),
            BinOpAst::Sub => Some(BinOpAbtKind::Sub.wrap(ty.clone(), ty)),
            BinOpAst::Mul => Some(BinOpAbtKind::Mul.wrap(ty.clone(), ty)),
            BinOpAst::Div => Some(BinOpAbtKind::Div.wrap(ty.clone(), ty)),
            BinOpAst::Rem => Some(BinOpAbtKind::Rem.wrap(ty.clone(), ty)),
            BinOpAst::Eq => Some(BinOpAbtKind::Eq.wrap(ty, TypeAbt::Bool)),
            BinOpAst::Ne => Some(BinOpAbtKind::Ne.wrap(ty, TypeAbt::Bool)),
            BinOpAst::Le => Some(BinOpAbtKind::Le.wrap(ty, TypeAbt::Bool)),
            BinOpAst::Lt => Some(BinOpAbtKind::Lt.wrap(ty, TypeAbt::Bool)),
            BinOpAst::Ge => Some(BinOpAbtKind::Ge.wrap(ty, TypeAbt::Bool)),
            BinOpAst::Gt => Some(BinOpAbtKind::Gt.wrap(ty, TypeAbt::Bool)),
            _ => None,
        }
    }

    fn boolean_binary_operation(op: BinOpAst) -> Option<BinOpAbt> {
        match op {
            BinOpAst::Eq => Some(BinOpAbtKind::Eq.wrap(TypeAbt::Bool, TypeAbt::Bool)),
            BinOpAst::Ne => Some(BinOpAbtKind::Ne.wrap(TypeAbt::Bool, TypeAbt::Bool)),
            BinOpAst::BitAnd => Some(BinOpAbtKind::BitAnd.wrap(TypeAbt::Bool, TypeAbt::Bool)),
            BinOpAst::BitXor => Some(BinOpAbtKind::BitXor.wrap(TypeAbt::Bool, TypeAbt::Bool)),
            BinOpAst::BitOr => Some(BinOpAbtKind::BitOr.wrap(TypeAbt::Bool, TypeAbt::Bool)),
            BinOpAst::And => Some(BinOpAbtKind::And.wrap(TypeAbt::Bool, TypeAbt::Bool)),
            BinOpAst::Or => Some(BinOpAbtKind::Or.wrap(TypeAbt::Bool, TypeAbt::Bool)),
            BinOpAst::Xor => Some(BinOpAbtKind::Xor.wrap(TypeAbt::Bool, TypeAbt::Bool)),
            _ => None,
        }
    }

    fn analyse_assignment(&mut self, left: &ExprAst, right: &ExprAst) -> ExprAbt {
        let bound_left = self.analyse_expression(left);
        let bound_right = self.analyse_expression(right);

        let ExprAbt::Variable(var) = bound_left else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::AssigneeMustBeVariable)
                .with_severity(Severity::Error)
                .with_span(left.span)
                .done();
            self.diagnostics.push(d);
            return ExprAbt::Unknown;
        };

        if !bound_right.ty().is(&var.ty) {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::TypeMismatch {
                    found: bound_right.ty(),
                    expected: var.ty,
                })
                .with_severity(Severity::Error)
                .with_span(right.span)
                .done();
            self.diagnostics.push(d);
            return ExprAbt::Unknown;
        }

        ExprAbt::Assignment(var, Box::new(bound_right))
    }

    fn analyse_unary_operation(&mut self, op: UnOpAst, operand: &ExprAst, span: Span) -> ExprAbt {
        let bound_operand = self.analyse_expression(operand);
        let ty = bound_operand.ty();

        if !ty.is_known() {
            return ExprAbt::Unknown;
        }

        let bound_op = match ty {
            TypeAbt::U8 | TypeAbt::U16 | TypeAbt::U32 | TypeAbt::U64 => {
                Self::number_unary_operation(false, op, ty.clone())
            }
            TypeAbt::I8
            | TypeAbt::I16
            | TypeAbt::I32
            | TypeAbt::I64
            | TypeAbt::F32
            | TypeAbt::F64 => Self::number_unary_operation(true, op, ty.clone()),
            TypeAbt::Bool => Self::boolean_unary_operation(op),
            _ => None,
        };

        if let Some(op) = bound_op {
            return ExprAbt::Unary(op, Box::new(bound_operand));
        }

        let d = diagnostics::create_diagnostic()
            .with_kind(DiagnosticKind::InvalidUnaryOperation { op, ty })
            .with_severity(Severity::Error)
            .with_span(span)
            .done();
        self.diagnostics.push(d);
        ExprAbt::Unknown
    }

    fn number_unary_operation(signed: bool, op: UnOpAst, ty: TypeAbt) -> Option<UnOpAbt> {
        match op {
            UnOpAst::Pos => Some(UnOpAbtKind::Pos.wrap(ty)),
            UnOpAst::Neg if signed => Some(UnOpAbtKind::Neg.wrap(ty)),
            _ => None,
        }
    }

    fn boolean_unary_operation(op: UnOpAst) -> Option<UnOpAbt> {
        match op {
            UnOpAst::Not => Some(UnOpAbtKind::Not.wrap(TypeAbt::Bool)),
            _ => None,
        }
    }

    fn analyse_call(&mut self, name: &str, params: &[ExprAst], span: Span) -> ExprAbt {
        let bound_params = params
            .iter()
            .map(|param| self.analyse_expression(param))
            .collect::<Vec<_>>();

        let Some(func) = self.scope.get_function(name) else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::UnknownFunction(name.to_string()))
                .with_severity(Severity::Error)
                .with_span(span)
                .done();
            self.diagnostics.push(d);
            return ExprAbt::Unknown;
        };

        let mut invalid = false;
        for ((bound_param, param), expected_ty) in
            bound_params.iter().zip(params).zip(&func.param_types)
        {
            let ty_param = bound_param.ty();
            if !ty_param.is(expected_ty) {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::TypeMismatch {
                        found: ty_param.clone(),
                        expected: expected_ty.clone(),
                    })
                    .with_severity(Severity::Error)
                    .with_span(param.span)
                    .done();
                self.diagnostics.push(d);
                invalid = true;
            }
        }

        if bound_params.len() != func.param_types.len() {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::InvalidParameterCount {
                    got: bound_params.len(),
                    expected: bound_params.len(),
                })
                .with_severity(Severity::Error)
                .with_span(span)
                .done();
            self.diagnostics.push(d);
            invalid = true;
        }

        if invalid {
            ExprAbt::Unknown
        } else {
            ExprAbt::Call(func.id, bound_params, func.return_type.clone())
        }
    }

    fn analyse_type(&mut self, ty: &TypeAst) -> TypeAbt {
        match &ty.kind {
            TypeAstKind::Bad => TypeAbt::Unknown,
            TypeAstKind::Unit => TypeAbt::Unit,
            TypeAstKind::Declared(id) => {
                match id.as_str() {
                    "u8" => return TypeAbt::U8,
                    "u16" => return TypeAbt::U16,
                    "u32" => return TypeAbt::U32,
                    "u64" => return TypeAbt::U64,
                    "i8" => return TypeAbt::I8,
                    "i16" => return TypeAbt::I16,
                    "i32" => return TypeAbt::I32,
                    "i64" => return TypeAbt::I64,
                    "f32" => return TypeAbt::F32,
                    "f64" => return TypeAbt::F64,
                    "bool" => return TypeAbt::Bool,
                    _ => {}
                };

                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::UnknownType(id.clone()))
                    .with_severity(Severity::Error)
                    .with_span(ty.span)
                    .done();
                self.diagnostics.push(d);

                TypeAbt::Unknown
            }
        }
    }
}
