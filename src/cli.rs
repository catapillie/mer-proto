use std::path::PathBuf;

use clap::{Args, Parser, Subcommand};

#[derive(Parser)]
#[command(version)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Command,
}

#[derive(Subcommand)]
pub enum Command {
    /// Compile source code to bytecode
    Com(Com),
    /// Analyse source code and show compiler diagnostics
    Check(Check),
    /// Execute a bytecode program
    Run(Run),
    /// Disassemble a bytecode program
    Dis(Dis),
}

#[derive(Args)]
pub struct Com {
    /// The path to the file containing the source code
    pub path: PathBuf,

    /// Whether the program should be ran after compilation
    #[arg(short, long)]
    pub run: bool,

    /// Hide compiler status messages
    #[arg(short, long)]
    pub quiet: bool,
}

#[derive(Args)]
pub struct Check {
    /// The path to the file containing the source code
    pub path: PathBuf,
}

#[derive(Args)]
pub struct Run {
    /// The path to the bytecode file
    pub path: PathBuf,
}

#[derive(Args)]
pub struct Dis {
    /// The path to the bytecode file
    pub path: PathBuf,
}
