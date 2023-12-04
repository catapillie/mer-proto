use crate::run::opcode::Opcode;
use std::collections::HashSet;

#[test]
fn all_opcodes_are_unique() {
    let mut unique = HashSet::new();
    let ok = Opcode::all_opcodes()
        .iter()
        .all(move |byte| unique.insert(byte));

    assert!(ok, "some opcodes are represented by the same byte")
}
