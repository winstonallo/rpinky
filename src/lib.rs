#![feature(try_trait_v2)]
#![feature(try_trait_v2_residual)]
#![deny(clippy::unwrap_used)]
#![deny(clippy::expect_used)]

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
