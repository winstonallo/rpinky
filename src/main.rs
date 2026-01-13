use crate::{lexer::Lexer, parser::Parser};

mod lexer;
mod model;
mod parser;
mod tokens;

#[derive(Debug, thiserror::Error)]
pub enum CiflError {
    #[error("Usage: cifl <filename>")]
    InvalidArgument,
    #[error("IO Error: {0}")]
    Io(#[from] std::io::Error),
}

fn main() -> Result<(), CiflError> {
    if std::env::args().len() != 2 {
        return Err(CiflError::InvalidArgument);
    }

    let filename = &std::env::args().collect::<Vec<String>>()[1];

    let source = std::fs::read_to_string(filename)?;

    let mut lexer = Lexer::new(source.as_bytes());
    let tokens = lexer.tokenize();

    for tok in tokens {
        println!("{tok:?}")
    }

    println!();

    let mut parser = Parser::new(tokens);
    let ast = parser.parse();
    println!("{ast:?}");

    Ok(())
}
