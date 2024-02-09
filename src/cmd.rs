#[derive(Debug)]
pub enum Command {
    Compile(CompileCommand),
    Check(CheckCommand),
    Run(RunCommand),
    Disassemble(DisassembleCommand),
    Version,
    Help(Option<String>),
    Unknown(String),
    Error,
}

#[derive(Debug)]
pub enum CompileCommand {
    Go(String),
    NoPath,
}

#[derive(Debug)]
pub enum CheckCommand {
    Go(String),
    NoPath,
}

#[derive(Debug)]
pub enum RunCommand {
    Go(String),
    NoPath,
}

#[derive(Debug)]
pub enum DisassembleCommand {
    Go(String),
    NoPath,
}
