#![allow(unused)]

use clap::Parser as _;
use colored::Colorize;

use crate::{lexer::Lexer, parser::Parser};

mod cli;
mod errors;
mod interpreter;
mod lexer;
mod model;
mod parser;
mod tokens;

fn main() {
    let args = cli::Arguments::parse();

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
    println!("{ast:?}");

    let val = match interpreter::interpret(&ast) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("{e}");
            return;
        }
    };

    println!("{val:?}");
}
