use super::Analyser;
use crate::{
    com::{
        abt::{self, DataInfo},
        ast,
    },
    utils::Spanned,
};

impl<'d> Analyser<'d> {
    pub fn analyse_data_structure_definition(
        &mut self,
        name: &Spanned<String>,
        fields: &[(Spanned<String>, ast::Type)],
    ) -> abt::StmtKind {
        let bound_fields = fields
            .iter()
            .map(|(name, ty)| {
                (
                    name.clone(),
                    Spanned {
                        value: self.analyse_type(ty),
                        span: ty.span,
                    },
                )
            })
            .collect::<Vec<_>>();

        let size = bound_fields
            .iter()
            .map(|(_, ty)| self.program.size_of(&ty.value))
            .sum();

        let declared = self.make_unique_id();
        self.scope.bindings.insert(name.value.clone(), declared);

        let info = DataInfo {
            name: name.clone(),
            id: declared,
            fields: bound_fields,
            size,
        };
        let previous = self.program.datas.insert(declared, info);
        assert!(previous.is_none(), "id must be unique");

        abt::StmtKind::Empty
    }
}
