use crate::com::abt::TypeAbt;

use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn open_scope(&mut self) {
        self.current_depth += 1;
        self.current_offsets.push(0);
        self.current_return_ty.push(self.get_return_type())
    }

    pub fn close_scope(&mut self) {
        assert_ne!(self.current_depth, 0, "scope underflow");
        self.current_offsets.pop().unwrap();
        self.current_depth -= 1;
    }

    pub fn set_block_offset(&mut self, offset: u64) {
        let o = self.current_offsets.last_mut().expect("not in scope");
        *o = offset;
    }

    pub fn get_block_offset(&self) -> u64 {
        *self.current_offsets.last().expect("not in scope")
    }

    pub fn get_return_type(&self) -> TypeAbt {
        self.current_return_ty
            .last()
            .cloned()
            .expect("not in scope")
    }

    pub fn set_return_type(&mut self, ty: TypeAbt) {
        let t = self.current_return_ty.last_mut().expect("not in scope");
        *t = ty;
    }
}
