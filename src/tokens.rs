#[derive(PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Lexeme<'tok> {
    value: &'tok [u8],
}

impl<'tok> Lexeme<'tok> {
    pub fn new(value: &'tok [u8]) -> Self {
        Self { value }
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

#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Token<'tok> {
    LParen { line: usize },    // (
    RParen { line: usize },    // )
    LCurly { line: usize },    // {
    RCurly { line: usize },    // }
    LSquare { line: usize },   // ]
    RSquare { line: usize },   // [
    Comma { line: usize },     // ,
    Dot { line: usize },       // .
    Plus { line: usize },      // +
    Minus { line: usize },     // -
    Star { line: usize },      // *
    Slash { line: usize },     // /
    Caret { line: usize },     // ^
    Mod { line: usize },       // %
    Colon { line: usize },     // :
    Semicolon { line: usize }, // ;
    Question { line: usize },  // ?
    Not { line: usize },       // ~
    Greater { line: usize },   // >
    Less { line: usize },      // <
    // Two-char tokens
    GreaterEqual { line: usize },   // >=
    LessEqual { line: usize },      // <=
    NotEqual { line: usize },       // ~=
    Equal { line: usize },          // ==
    Assign { line: usize },         // :=
    GreaterGreater { line: usize }, // >>
    LessLess { line: usize },       // <<
    // Literals
    Identifier { lexeme: Lexeme<'tok>, line: usize },
    StringLiteral { lexeme: Lexeme<'tok>, line: usize },
    IntegerLiteral { lexeme: Lexeme<'tok>, line: usize },
    FloatLiteral { lexeme: Lexeme<'tok>, line: usize },
    // Keywords
    If { line: usize },
    Then { line: usize },
    Else { line: usize },
    True { line: usize },
    False { line: usize },
    And { line: usize },
    Or { line: usize },
    While { line: usize },
    Do { line: usize },
    For { line: usize },
    Func { line: usize },
    Null { line: usize },
    End { line: usize },
    Print { line: usize },
    Println { line: usize },
    Ret { line: usize },
}
