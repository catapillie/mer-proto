#[derive(Debug)]
pub enum Command {
    Compile(CompileCommand),
    Run(RunCommand),
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
pub enum RunCommand {
    Go(String),
    NoPath,
}
