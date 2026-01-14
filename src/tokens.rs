#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Lexeme<'src> {
    value: &'src [u8],
}

impl<'src> Lexeme<'src> {
    pub fn new(value: &'src [u8]) -> Self {
        Self { value }
    }

    pub fn value(&self) -> &[u8] {
        self.value
    }
}

// impl Display for Lexeme so the bytes are displayed as chars.
impl std::fmt::Display for Lexeme<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for ch in self.value {
            write!(f, "{}", *ch as char)?;
        }
        Ok(())
    }
}

impl std::fmt::Debug for Lexeme<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Token<'src> {
    kind: TokenKind<'src>,
    line: usize,
}

impl<'src> Token<'src> {
    pub fn new(kind: TokenKind<'src>, line: usize) -> Self {
        Self { kind, line }
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn kind(&self) -> TokenKind<'src> {
        self.kind
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum TokenKind<'src> {
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
    Identifier { lexeme: Lexeme<'src> },
    StringLiteral { lexeme: Lexeme<'src> },
    IntegerLiteral { lexeme: Lexeme<'src> },
    FloatLiteral { lexeme: Lexeme<'src> },
    // Keywords
    If,
    Then,
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
}
