use super::Analyser;

impl<'d> Analyser<'d> {
    pub fn open_scope(&mut self) {
        self.current_depth += 1;
        self.current_offsets.push(0);
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

    pub fn get_block_offset(&mut self) -> u64 {
        *self.current_offsets.last().expect("not in scope")
    }
}
