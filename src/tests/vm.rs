use core::fmt;

use crate::{
    com,
    run::{value::Value, vm::VM},
};

matches_output! {
    return_unit<()>(())
    "
    return
    "
}

fn check_output<T>(src: &str, expected: T)
where
    T: From<Value> + PartialEq + fmt::Debug,
{
    let program = com::compile_to_bytecode("<test>", src.to_string());
    let mut vm = VM::new(&program);
    let result: T = vm.run().into();
    assert_eq!(result, expected)
}

macro_rules! matches_output {
    (
        $name:ident<$ty:ty>($expect:expr)
        $src:literal
    ) => {
        #[test]
        fn $name() {
            check_output::<$ty>($src, $expect);
        }
    };
}

use matches_output;
