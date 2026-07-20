use std::{cell::RefCell, io::stdout, rc::Rc};

use clap::Parser as _;
use colored::Colorize;

fn main() {
    let args = rpinky::cli::Arguments::parse();

    let source = match std::fs::read_to_string(args.input_file()) {
        Ok(s) => s,
        Err(e) => return eprintln!("{} couldn't read '{}': {e}", "error:".red().bold(), args.input_file()),
    };

    rpinky::run(&source, Some(&args), Rc::new(RefCell::new(stdout())));
}
