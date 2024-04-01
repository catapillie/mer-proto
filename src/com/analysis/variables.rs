use super::{Analyser, Declaration};
use crate::{
    com::{
        abt::{self, VariableInfo},
        ast::stmt::VarDef,
    },
    diagnostics::{self, DiagnosticKind, Note, NoteSeverity, Severity},
    utils::{Span, Spanned},
};

impl<'d> Analyser<'d> {
    pub fn declare_variable_here(&mut self, name: Spanned<String>, ty: abt::Type) -> Declaration {
        let declared = self.make_unique_id();
        let shadowed = self.scope.bindings.insert(name.value.clone(), declared);

        let info = VariableInfo {
            id: declared,
            name,
            depth: self.scope.depth,
            position: self.scope.position,
            ty,
            is_on_heap: false,
        };
        let prev = self.program.variables.insert(declared, info);
        assert!(prev.is_none(), "ids must be unique");

        let func_id = self.scope.current_func_id;
        let func_info = self.program.functions.get_mut(&func_id).unwrap();
        func_info.local_variables.insert(declared);

        Declaration { declared, shadowed }
    }

    pub fn analyse_variable_definition(&mut self, ast: &VarDef) -> abt::StmtKind {
        let bound_expr = self.analyse_expression(&ast.expr);
        let pat = self.analyse_pattern(&ast.pattern);
        let bound_pat = self.declare_pattern_bindings_by_expr(&pat, &bound_expr);
        abt::StmtKind::Deconstruct(Box::new(bound_pat), Box::new(bound_expr))
    }

    pub fn analyse_variable_expression(&mut self, name: &str, span: Span) -> abt::TypedExpr {
        let Some(id) = self.scope.search_id(name) else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::UnknownVariable(name.to_string()))
                .with_severity(Severity::Error)
                .with_span(span)
                .annotate_primary(Note::Unknown, span)
                .done();
            self.diagnostics.push(d);
            return abt::TypedExpr::unknown();
        };

        if let Some(info) = self.program.aliases.get(&id) {
            if !info.is_opaque {
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::NonOpaqueTypeConstructor(name.to_string()))
                    .with_severity(Severity::Error)
                    .with_span(span)
                    .annotate_secondary(
                        Note::MarkedAsOpaque(name.to_string()).dddot_back().num(1),
                        info.name.span,
                        NoteSeverity::Annotation,
                    )
                    .annotate_primary(
                        Note::DoesNotHaveConstructor(name.to_string())
                            .so()
                            .dddot_front()
                            .num(2),
                        span,
                    )
                    .done();
                self.diagnostics.push(d);
                return abt::TypedExpr::unknown();
            }
            let ctor_id = self.get_opaque_constructor_func_id(id);
            return abt::TypedExpr {
                kind: abt::ExprKind::OpaqueConstructor {
                    ctor_id,
                    alias_id: id,
                },
                ty: abt::Type::Data(id),
            };
        }

        if self.program.functions.contains_key(&id) {
            let func_info_depth = self.program.functions.get(&id).unwrap().depth;
            let current_func_info = self
                .program
                .functions
                .get_mut(&self.scope.current_func_id)
                .unwrap();
            if current_func_info.depth >= func_info_depth {
                current_func_info.imported_functions.insert(id);
            }

            let ty = self.program.functions.get(&id).unwrap().function_type();
            return abt::TypedExpr {
                kind: abt::ExprKind::Function(id),
                ty,
            };
        }

        let Some(info) = self.program.variables.get_mut(&id) else {
            let d = diagnostics::create_diagnostic()
                .with_kind(DiagnosticKind::NotVariable(name.to_string()))
                .with_severity(Severity::Error)
                .with_span(span)
                .annotate_primary(Note::Unknown, span)
                .done();
            self.diagnostics.push(d);
            return abt::TypedExpr::unknown();
        };

        let depth = info.depth;
        let func_id = self.scope.current_func_id;
        let func_info = self.program.functions.get_mut(&func_id).unwrap();
        let captured = depth <= func_info.depth;

        if captured {
            func_info.captured_variables.insert(id);
            info.is_on_heap = true;
        }

        abt::TypedExpr {
            kind: abt::ExprKind::Variable(id),
            ty: info.ty.clone(),
        }
    }
}
