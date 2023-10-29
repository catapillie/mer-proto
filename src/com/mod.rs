use std::process;

use self::parser::Parser;

mod cursor;
mod parser;
mod pos;
mod span;
mod tokens;

pub fn compile(source: String) {
    let parser = Parser::new(source.as_str());
    process::exit(0);
}
