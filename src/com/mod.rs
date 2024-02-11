pub mod abt;
pub mod ast;
pub mod tokens;

mod analysis;
mod codegen;
mod compilation;
mod parser;

pub use compilation::AnalysisStage;
pub use compilation::{
    analyse_program, analyse_program_with_type, compile_to_bytecode, write_bytecode,
};
