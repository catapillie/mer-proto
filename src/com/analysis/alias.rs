use super::Analyser;
use crate::{
    com::{
        abt::{self, AliasInfo, VariableInfo, VariableUsage},
        ast::stmt::AliasDef,
    },
    diagnostics::{self, DiagnosticKind, Note, NoteSeverity, Severity},
    utils::{OptSpanned, Span, Spanned},
};

impl<'d> Analyser<'d> {
    pub fn get_type_alias(&self, name: &str) -> Option<&AliasInfo> {
        self.scope.search(|scope| match scope.bindings.get(name) {
            Some(id) => self.program.aliases.get(id),
            None => None,
        })
    }

    pub fn analyse_alias_header(&mut self, ast: &AliasDef) -> Option<u64> {
        if let Some(shadowed) = self.scope.bindings.get(&ast.name.value) {
            if let Some(info) = self.program.aliases.get(shadowed) {
                let shadowed_span = info.name.span;
                let d = diagnostics::create_diagnostic()
                    .with_kind(DiagnosticKind::AliasRedefinition(ast.name.value.clone()))
                    .with_span(ast.name.span)
                    .with_severity(Severity::Error)
                    .annotate_secondary(
                        Note::ShadowedAlias(ast.name.value.clone())
                            .dddot_back()
                            .num(1),
                        shadowed_span,
                        NoteSeverity::Annotation,
                    )
                    .annotate_primary(
                        Note::RedefinedAlias.and().dddot_front().num(2),
                        ast.name.span,
                    )
                    .done();
                self.diagnostics.push(d);
                return None;
            }
        }

        let id = self.make_unique_id();
        self.scope.bindings.insert(ast.name.value.clone(), id);
        self.program.aliases.insert(
            id,
            AliasInfo {
                id,
                name: ast.name.clone(),
                ty: abt::Type::Unknown,
                is_opaque: ast.is_opaque,
                constructor: None,
            },
        );

        Some(id)
    }

    pub fn analyse_alias_definition(&mut self, ast: &AliasDef, id: u64) {
        let ty = self.analyse_type(&ast.ty);
        self.program.aliases.get_mut(&id).unwrap().ty = ty;
    }

    pub fn get_opaque_constructor_func_id(&mut self, alias_id: u64) -> u64 {
        let info = self.program.aliases.get(&alias_id).unwrap();
        if let Some(id) = info.constructor {
            return id;
        }

        // here, we manually build the identity function
        //     a -> A
        // where
        //     a: inner type of the alias
        //     A: the opaque type alias itself

        // function
        let ctor_name = format!("{}.ctor", &info.name.value);
        let out_ty = abt::Type::Alias(alias_id);
        let func_span = info.name.span;

        // argument
        let arg_name = "_".to_string();
        let arg_ty = info.ty.clone();
        let arg_id = self.make_unique_id();
        let arg_usage = VariableUsage {
            captured: false,
            used: true,
        };
        self.program.variables.insert(
            arg_id,
            VariableInfo {
                id: arg_id,
                name: Spanned {
                    value: arg_name.clone(),
                    span: func_span,
                },
                depth: 0,
                ty: arg_ty.clone(),
                is_on_heap: false,
            },
        );

        // function body
        let body = abt::StmtKind::Return(Box::new(abt::Expr::Variable(arg_id)));
        let used_variables = [(arg_id, arg_usage)].into_iter().collect();

        let id = self.make_unique_id();
        self.program.aliases.get_mut(&alias_id).unwrap().constructor = Some(id);
        self.program.functions.insert(
            id,
            abt::FunctionInfo {
                id,
                name: OptSpanned {
                    value: ctor_name,
                    span: Some(func_span),
                },
                depth: 0,
                args: vec![(arg_name.clone(), arg_ty.clone())],
                arg_ids: vec![arg_id],
                ty: OptSpanned {
                    value: out_ty,
                    span: Some(func_span),
                },
                used_variables,
                code: Some(Box::new(Spanned {
                    value: body,
                    span: Span::EOF,
                })),
                was_analysed: true,
            },
        );

        id
    }
}
