use colored::Colorize;

use crate::span::Span;

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub struct ParseError {
    message: String,
    span: Span,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} {}", "error:".red().bold(), self.message)
    }
}

impl ParseError {
    pub fn new(message: String, span: Span) -> Self {
        Self { message, span }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub struct TokenizationError {
    message: String,
    span: Span,
}

impl std::fmt::Display for TokenizationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} {}", "error:".red().bold(), self.message)
    }
}

impl TokenizationError {
    pub fn new(message: String, span: Span) -> Self {
        Self { message, span }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub struct RuntimeError {
    message: String,
    span: Span,
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} {}", "error:".red().bold(), self.message)
    }
}

impl RuntimeError {
    pub fn new(message: String, span: Span) -> Self {
        Self { message, span }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn message(&self) -> &str {
        &self.message
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

pub fn report(source: &str, span: Span, message: &str) -> String {
    let start = span.start.min(source.len());
    let line_start = source[..start].rfind('\n').map(|i| i + 1).unwrap_or(0);
    let line_end = source[start..].find('\n').map(|i| start + i).unwrap_or(source.len());
    let line_no = source[..start].bytes().filter(|&b| b == b'\n').count() + 1;
    let line_text = &source[line_start..line_end];

    let col = start - line_start;
    let len = (span.end.min(line_end).saturating_sub(span.start)).max(1);
    let underline = format!("{}{}", " ".repeat(col), "^".repeat(len)).red().bold();

    let gutter = line_no.to_string();
    let pad = " ".repeat(gutter.len());

    format!(
        "{} line {line_no}: {message}\n{pad} |\n{} | {line_text}\n{pad} | {underline} here",
        "error:".red().bold(),
        gutter.blue().bold(),
    )
}
