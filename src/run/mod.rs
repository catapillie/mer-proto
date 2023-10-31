use self::vm::VM;

mod opcode;
mod vm;

pub fn run(program: Vec<u8>) {
    let mut vm = VM::new(program);
    vm.run();
}
