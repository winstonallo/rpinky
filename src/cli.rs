#[derive(clap::Parser)]
pub struct Arguments {
    input_file: String,

    #[arg(long, help = "Dump AST to stdout after parsing")]
    ast_dump: bool,

    #[arg(long, help = "Dump tokens to stdout after lexing")]
    token_dump: bool,
}

impl Arguments {
    pub fn input_file(&self) -> &str {
        &self.input_file
    }

    pub fn ast_dump(&self) -> bool {
        self.ast_dump
    }

    pub fn token_dump(&self) -> bool {
        self.token_dump
    }
}
