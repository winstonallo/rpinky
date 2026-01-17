use clap::Parser as _;
use colored::Colorize;

use cpinky::{interpreter, lexer::Lexer, parser::Parser};

fn main() {
    let args = cpinky::cli::Arguments::parse();

    let source = match std::fs::read_to_string(args.input_file()) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{} couldn't read '{}': {e}", "error:".red().bold(), args.input_file());
            return;
        }
    };

    let mut lexer = Lexer::new(source.as_bytes());
    let tokens = match lexer.tokenize() {
        Ok(t) => t,
        Err(e) => {
            eprintln!("{e}");
            return;
        }
    };

    for tok in tokens {
        println!("{tok:?}")
    }

    println!();

    let mut parser = Parser::new(tokens);

    let ast = match parser.parse() {
        Ok(a) => a,
        Err(e) => {
            eprintln!("{e}");
            return;
        }
    };
    println!("{ast:?}\n");

    match interpreter::interpret(&ast) {
        Ok(..) => (),
        Err(e) => {
            eprintln!("{e}");
            return;
        }
    };
}
