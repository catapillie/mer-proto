use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("no entry point defined")]
    NoEntryPoint,
    #[error("encountered illegal opcode")]
    IllegalOpcode,

    #[error("stack underflow")]
    StackUnderflow,
    #[error("program halted with non-empty stack")]
    HaltWithNonEmptyStack,

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
