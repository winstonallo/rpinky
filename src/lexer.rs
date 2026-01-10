use crate::tokens::{Lexeme, Token};

pub struct Lexer<'lex> {
    source: &'lex [u8],
    start: usize,
    curr: usize,
    line: usize,
    tokens: Vec<Token<'lex>>,
}

impl<'lex> Lexer<'lex> {
    pub fn new(source: &'lex [u8]) -> Self {
        Self {
            source,
            start: 0,
            curr: 0,
            line: 1,
            tokens: vec![],
        }
    }

    pub fn tokenize(&mut self) -> &Vec<Token<'_>> {
        while self.curr < self.source.len() {
            self.start = self.curr;
            let ch = self.advance();
            match ch {
                b'\n' => self.line += 1,
                ch if ch.is_ascii_whitespace() => (),
                b'#' => {
                    while self.peek() != b'\n' && self.curr < self.source.len() {
                        self.advance();
                    }
                }
                b'(' => self.tokens.push(Token::LParen { line: self.line }),
                b')' => self.tokens.push(Token::RParen { line: self.line }),
                b'{' => self.tokens.push(Token::LCurly { line: self.line }),
                b'}' => self.tokens.push(Token::RCurly { line: self.line }),
                b'[' => self.tokens.push(Token::LSquare { line: self.line }),
                b']' => self.tokens.push(Token::RSquare { line: self.line }),
                b'.' => self.tokens.push(Token::Dot { line: self.line }),
                b',' => self.tokens.push(Token::Comma { line: self.line }),
                b'+' => self.tokens.push(Token::Plus { line: self.line }),
                b'-' => self.tokens.push(Token::Minus { line: self.line }),
                b'*' => self.tokens.push(Token::Star { line: self.line }),
                b'^' => self.tokens.push(Token::Caret { line: self.line }),
                b'/' => self.tokens.push(Token::Slash { line: self.line }),
                b';' => self.tokens.push(Token::Semicolon { line: self.line }),
                b'?' => self.tokens.push(Token::Question { line: self.line }),
                b'%' => self.tokens.push(Token::Mod { line: self.line }),
                b'=' => {
                    if self.match_on(b'=') {
                        self.tokens.push(Token::Equal { line: self.line });
                    }
                }
                b'~' => {
                    if self.match_on(b'=') {
                        self.tokens.push(Token::NotEqual { line: self.line });
                    } else {
                        self.tokens.push(Token::Not { line: self.line });
                    }
                }
                b'<' => {
                    if self.match_on(b'=') {
                        self.tokens.push(Token::LessEqual { line: self.line });
                    } else if self.match_on(b'<') {
                        self.tokens.push(Token::LessLess { line: self.line });
                    } else {
                        self.tokens.push(Token::Less { line: self.line });
                    }
                }
                b'>' => {
                    if self.match_on(b'=') {
                        self.tokens.push(Token::GreaterEqual { line: self.line });
                    } else if self.match_on(b'>') {
                        self.tokens.push(Token::GreaterGreater { line: self.line });
                    } else {
                        self.tokens.push(Token::Greater { line: self.line });
                    }
                }
                b':' => {
                    if self.match_on(b'=') {
                        self.tokens.push(Token::Assign { line: self.line });
                    } else {
                        self.tokens.push(Token::Colon { line: self.line });
                    }
                }
                b'0'..=b'9' => {
                    while self.peek().is_ascii_digit() {
                        self.advance();
                    }
                    if self.peek() == b'.' && self.lookahead(1).is_some_and(|x| x.is_ascii_digit())
                    {
                        self.advance();
                        while self.peek().is_ascii_digit() {
                            self.advance();
                        }
                        self.tokens.push(Token::FloatLiteral {
                            lexeme: Lexeme::new(&self.source[self.start..self.curr]),
                            line: self.line,
                        });
                    } else {
                        self.tokens.push(Token::IntegerLiteral {
                            lexeme: Lexeme::new(&self.source[self.start..self.curr]),
                            line: self.line,
                        });
                    }
                }
                b'\'' => {
                    while self.peek() != b'\'' && self.curr < self.source.len() {
                        self.advance();
                    }
                    self.advance();
                    self.tokens.push(Token::StringLiteral {
                        lexeme: Lexeme::new(&self.source[self.start..self.curr]),
                        line: self.line,
                    });
                }
                b'"' => {
                    while self.peek() != b'"' && self.curr < self.source.len() {
                        self.advance();
                    }
                    self.advance();
                    self.tokens.push(Token::StringLiteral {
                        lexeme: Lexeme::new(&self.source[self.start..self.curr]),
                        line: self.line,
                    });
                }
                _ => (),
            }
        }

        &self.tokens
    }

    pub fn advance(&mut self) -> u8 {
        let ch = self.source[self.curr];
        self.curr += 1;
        ch
    }

    pub fn peek(&self) -> u8 {
        self.source[self.curr]
    }

    pub fn lookahead(&self, n: usize) -> Option<u8> {
        if self.curr >= self.source.len() {
            return None;
        }
        Some(self.source[self.curr + n])
    }

    pub fn match_on(&mut self, expected: u8) -> bool {
        if self.curr >= self.source.len() {
            return false;
        }
        if self.source[self.curr] != expected {
            return false;
        }
        self.curr += 1;
        true
    }
}
