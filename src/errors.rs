use colored::Colorize;

#[derive(Debug, thiserror::Error)]
pub struct ParseError {
    message: String,
    line: usize,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} line {}: {}", "error:".red().bold(), self.line, self.message)
    }
}

impl ParseError {
    pub fn new(message: String, line: usize) -> Self {
        Self { message, line }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum CompileError {
    #[error("Parse Error")]
    Parse {
        #[from]
        source: ParseError,
    },
}
