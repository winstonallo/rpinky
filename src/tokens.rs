use std::rc::Rc;

use crate::lexer::unescape;

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Lexeme {
    value: Rc<[u8]>,
}

impl Lexeme {
    pub fn new(value: Vec<u8>) -> Self {
        let value = unescape(&value);

        Self { value: value.into() }
    }

    pub fn value(&self) -> &[u8] {
        &self.value
    }
}

// impl Display for Lexeme so the bytes are displayed as chars.
impl std::fmt::Display for Lexeme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for ch in self.value.iter() {
            match *ch {
                b'\n' => write!(f, "\\n")?,
                b'\r' => write!(f, "\\r")?,
                b'\t' => write!(f, "\\t")?,
                b'\\' => write!(f, "\\")?,
                _ => write!(f, "{}", *ch as char)?,
            }
        }
        Ok(())
    }
}

impl std::fmt::Debug for Lexeme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    kind: TokenKind,
    line: usize,
}

impl Token {
    pub fn new(kind: TokenKind, line: usize) -> Self {
        Self { kind, line }
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum TokenKind {
    LParen,    // (
    RParen,    // )
    LCurly,    // {
    RCurly,    // }
    LSquare,   // ]
    RSquare,   // [
    Comma,     // ,
    Dot,       // .
    Plus,      // +
    Minus,     // -
    Star,      // *
    Slash,     // /
    Caret,     // ^
    Mod,       // %
    Colon,     // :
    Semicolon, // ;
    Question,  // ?
    Not,       // ~
    Greater,   // >
    Less,      // <
    Equal,     // =
    // Two-char tokens
    GreaterEqual,   // >=
    LessEqual,      // <=
    NotEqual,       // ~=
    EqualEqual,     // ==
    Assign,         // :=
    GreaterGreater, // >>
    LessLess,       // <<
    // Literals
    Identifier { lexeme: Lexeme },
    StringLiteral { lexeme: Lexeme },
    IntegerLiteral { lexeme: Lexeme },
    FloatLiteral { lexeme: Lexeme },
    // Keywords
    If,
    Then,
    Elif,
    Else,
    True,
    False,
    And,
    Or,
    While,
    Do,
    For,
    Func,
    Null,
    End,
    Print,
    Println,
    Ret,
    Eof,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::LParen => write!(f, "("),
            TokenKind::RParen => write!(f, ")"),
            TokenKind::LCurly => write!(f, "{{"),
            TokenKind::RCurly => write!(f, "}}"),
            TokenKind::LSquare => write!(f, "]"),
            TokenKind::RSquare => write!(f, "["),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Caret => write!(f, "^"),
            TokenKind::Mod => write!(f, "%"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Question => write!(f, "?"),
            TokenKind::Not => write!(f, "~"),
            TokenKind::Greater => write!(f, ">"),
            TokenKind::Less => write!(f, "<"),
            TokenKind::Equal => write!(f, "="),
            TokenKind::GreaterEqual => write!(f, ">="),
            TokenKind::LessEqual => write!(f, "<="),
            TokenKind::NotEqual => write!(f, "~="),
            TokenKind::EqualEqual => write!(f, "=="),
            TokenKind::Assign => write!(f, ":="),
            TokenKind::GreaterGreater => write!(f, ">>"),
            TokenKind::LessLess => write!(f, "<<"),
            TokenKind::Identifier { lexeme } => write!(f, "identifier: {lexeme}"),
            TokenKind::StringLiteral { lexeme } => write!(f, "string literal: '{lexeme}'"),
            TokenKind::IntegerLiteral { lexeme } => write!(f, "integer literal: {lexeme}"),
            TokenKind::FloatLiteral { lexeme } => write!(f, "float literal: {lexeme}"),
            TokenKind::If => write!(f, "keyword: if"),
            TokenKind::Then => write!(f, "keyword: then"),
            TokenKind::Elif => write!(f, "keyword: elif"),
            TokenKind::Else => write!(f, "keyword: else"),
            TokenKind::True => write!(f, "keyword: true"),
            TokenKind::False => write!(f, "keyword: false"),
            TokenKind::And => write!(f, "keyword: and"),
            TokenKind::Or => write!(f, "keyword: or"),
            TokenKind::While => write!(f, "keyword: while"),
            TokenKind::Do => write!(f, "keyword: do"),
            TokenKind::For => write!(f, "keyword: for"),
            TokenKind::Func => write!(f, "keyword: func"),
            TokenKind::Null => write!(f, "keyword: null"),
            TokenKind::End => write!(f, "keyword: end"),
            TokenKind::Print => write!(f, "keyword: print"),
            TokenKind::Println => write!(f, "keyword: println"),
            TokenKind::Ret => write!(f, "keyword: ret"),
            TokenKind::Eof => write!(f, "EOF"),
        }
    }
}
