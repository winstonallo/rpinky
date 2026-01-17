use colored::Colorize;

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
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

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub struct TokenizationError {
    message: String,
    line: usize,
}

impl std::fmt::Display for TokenizationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} line {}: {}", "error:".red().bold(), self.line, self.message)
    }
}

impl TokenizationError {
    pub fn new(message: String, line: usize) -> Self {
        Self { message, line }
    }
}

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub struct RuntimeError {
    message: String,
    line: usize,
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} line {}: {}", "error:".red().bold(), self.line, self.message)
    }
}

impl RuntimeError {
    pub fn new(message: String, line: usize) -> Self {
        Self { message, line }
    }
}

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum CompileError {
    #[error("Parse Error")]
    Parse {
        #[from]
        source: ParseError,
    },
    #[error("Lex Error")]
    Lex {
        #[from]
        source: TokenizationError,
    },
    #[error("Runtime Error")]
    Runtime {
        #[from]
        source: RuntimeError,
    },
}
