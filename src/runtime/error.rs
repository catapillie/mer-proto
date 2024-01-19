use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("no entry point defined")]
    NoEntryPoint,
    #[error("encountered illegal opcode")]
    IllegalOpcode,

    #[error("stack overflow")]
    StackOverflow,
    #[error("stack underflow")]
    StackUnderflow,
    #[error("program halted with non-empty stack")]
    HaltWithNonEmptyStack,

    #[error("read from unallocated memory in heap")]
    InvalidMemoryAccess,
    #[error("wrote to unallocated memory in heap")]
    InvalidMemoryWrite,
    #[error("popped address from stack, expected value")]
    UnexpectedAddress,
    #[error("popped value from stack, expected address")]
    UnexpectedValue,

    #[error("invalid unary operation")]
    InvalidUnaryOperation,
    #[error("invalid binary operation")]
    InvalidBinaryOperation,
    #[error("invalid native type")]
    InvalidNativeType,

    #[error("invalid function call (jump to invalid address)")]
    InvalidFunctionCall,

    #[error("reached 'todo' opcode")]
    Todo,
    #[error("reached 'unreachable' opcode")]
    Unreachable,
}
