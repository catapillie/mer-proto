use std::{io, string::FromUtf8Error};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum DisassemblyError {
    #[error("illegal opcode at offset {offset}: {error}")]
    IllegalOpcode { offset: u64, error: OpcodeError },
    #[error("io: {0}")]
    IO(#[from] io::Error),
}

#[derive(Error, Debug)]
pub enum OpcodeError {
    #[error("illegal opcode")]
    IllegalOpcode,
    #[error("{0}")]
    IllegalNativeType(#[from] NativeTypeError),
    #[error("io: {0}")]
    IO(#[from] io::Error),
    #[error("{0}")]
    InvalidUtf8(#[from] FromUtf8Error),
}

#[derive(Error, Debug)]
pub enum NativeTypeError {
    #[error("illegal native type")]
    IllegalNativeType,
    #[error("io: {0}")]
    IO(#[from] io::Error),
}
