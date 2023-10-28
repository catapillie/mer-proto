#[derive(Debug)]
pub enum Command {
    Compile(CompileCommand),
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