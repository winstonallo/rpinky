#![feature(try_trait_v2)]
#![feature(try_trait_v2_residual)]
#![deny(clippy::unwrap_used)]
#![deny(clippy::expect_used)]

use std::{cell::RefCell, io::Write, rc::Rc};

use crate::{cli::Arguments, lexer::Lexer, parser::Parser};

pub mod cli;
pub mod debug;
pub mod errors;
pub mod interpreter;
pub mod lexer;
pub mod model;
pub mod parser;
pub mod span;
pub mod state;
pub mod tokens;
pub mod visitor;

pub fn run(source: &str, args: Option<&Arguments>, out: Rc<RefCell<dyn Write>>) {
    let mut lexer = Lexer::new(source.as_bytes());
    let tokens = match lexer.tokenize() {
        Ok(t) => t,
        Err(e) => return eprintln!("{}", crate::errors::report(source, e.span(), e.message())),
    };

    if args.is_some_and(|a| a.token_dump()) {
        for tok in tokens {
            println!("{tok:?}")
        }
        println!();
    }

    let mut parser = Parser::new(tokens.to_vec());

    let ast = match parser.parse() {
        Ok(a) => a,
        Err(e) => return eprintln!("{}", crate::errors::report(source, e.span(), e.message())),
    };

    if args.is_some_and(|a| a.ast_dump()) {
        println!("{ast:?}\n");
    }

    match interpreter::interpret(&ast, out) {
        Ok(..) => (),
        Err(e) => eprintln!("{}", crate::errors::report(source, e.span(), e.message())),
    };
}
