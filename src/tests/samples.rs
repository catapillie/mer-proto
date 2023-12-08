use std::fs;

use crate::com;

#[test]
fn all_samples_compile() {
    for file in fs::read_dir("./samples/").unwrap() {
        let path_buf =file.unwrap().path();
        let path = &*path_buf.to_string_lossy();
        let source = fs::read_to_string(path).unwrap();
        com::compile_to_bytecode(path, source);
    }
}
