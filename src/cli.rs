use std::path::PathBuf;

use clap::{Args, Parser, Subcommand, ValueEnum};

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

    /// The path to the output file
    #[arg(short, long)]
    pub out: Option<PathBuf>,

    /// Whether the program should be ran after compilation
    #[arg(short, long)]
    pub run: bool,

    /// Hide compiler status messages
    #[arg(short, long)]
    pub quiet: bool,

    /// Language for compiler diagnostics
    #[arg(short, long)]
    pub lang: Option<LangValue>,
}

#[derive(Args)]
pub struct Check {
    /// The path to the file containing the source code
    pub path: PathBuf,

    /// Language for compiler diagnostics
    #[arg(short, long)]
    pub lang: Option<LangValue>,
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

#[derive(Clone, ValueEnum)]
pub enum LangValue {
    En,
    Fr,
}
