#[derive(Debug)]
pub enum Command {
    Version,
    Help,
    Unknown(String),
    Error,
}