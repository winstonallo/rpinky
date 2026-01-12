use crate::tokens::{Lexeme, Token};

pub struct Lexer<'src> {
    source: &'src [u8],
    start: usize,
    curr: usize,
    line: usize,
    tokens: Vec<Token<'src>>,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src [u8]) -> Self {
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
            let Some(c) = self.advance() else {
                return &self.tokens;
            };
            match c {
                b'\n' => self.line += 1,
                ch if ch.is_ascii_whitespace() => (),
                b'(' => self.tokens.push(Token::LParen { line: self.line }),
                b')' => self.tokens.push(Token::RParen { line: self.line }),
                b'{' => self.tokens.push(Token::LCurly { line: self.line }),
                b'}' => self.tokens.push(Token::RCurly { line: self.line }),
                b'[' => self.tokens.push(Token::LSquare { line: self.line }),
                b']' => self.tokens.push(Token::RSquare { line: self.line }),
                b'.' => self.tokens.push(Token::Dot { line: self.line }),
                b',' => self.tokens.push(Token::Comma { line: self.line }),
                b'+' => self.tokens.push(Token::Plus { line: self.line }),
                b'-' => {
                    if self.match_on(b'-') {
                        while self.peek().is_some_and(|c| c != b'\n') {
                            self.advance();
                        }
                    } else {
                        self.tokens.push(Token::Minus { line: self.line });
                    }
                }
                b'*' => self.tokens.push(Token::Star { line: self.line }),
                b'^' => self.tokens.push(Token::Caret { line: self.line }),
                b'/' => self.tokens.push(Token::Slash { line: self.line }),
                b';' => self.tokens.push(Token::Semicolon { line: self.line }),
                b'?' => self.tokens.push(Token::Question { line: self.line }),
                b'%' => self.tokens.push(Token::Mod { line: self.line }),
                b'=' => {
                    if self.match_on(b'=') {
                        self.tokens.push(Token::EqualEqual { line: self.line });
                    } else {
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
                b'0'..=b'9' => self.handle_number_literal(),
                b'\'' => self.handle_string_literal(b'\''),
                b'"' => self.handle_string_literal(b'"'),
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.handle_identifier(),
                _ => panic!("[Line {}] Unexpected character: '{}'", self.line, c as char),
            }
        }

        &self.tokens
    }

    fn handle_identifier(&mut self) {
        while self
            .peek()
            .is_some_and(|c| c.is_ascii_alphanumeric() || c == b'_')
        {
            self.advance();
        }
        if let Some(keyword) =
            match_reserved_keyword(&self.source[self.start..self.curr], self.line)
        {
            self.tokens.push(keyword);
        } else {
            self.tokens.push(Token::Identifier {
                lexeme: Lexeme::new(&self.source[self.start..self.curr]),
                line: self.line,
            });
        }
    }

    fn handle_number_literal(&mut self) {
        while self.peek().is_some_and(|c| c.is_ascii_digit()) {
            self.advance();
        }
        if self.peek().is_some_and(|c| c == b'.')
            && self.lookahead(1).is_some_and(|x| x.is_ascii_digit())
        {
            self.advance();
            while self.peek().is_some_and(|c| c.is_ascii_digit()) {
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

    fn handle_string_literal(&mut self, quote: u8) {
        while self.peek().is_some_and(|c| c != quote && c != b'\n') {
            self.advance();
        }
        if self.curr >= self.source.len() || self.peek().is_some_and(|c| c == b'\n') {
            panic!("[Line {}] Unterminated string literal", self.line);
        }
        self.advance();
        self.tokens.push(Token::StringLiteral {
            lexeme: Lexeme::new(&self.source[self.start..self.curr]),
            line: self.line,
        });
    }

    pub fn advance(&mut self) -> Option<u8> {
        if self.curr >= self.source.len() {
            return None;
        }
        let c = self.source[self.curr];
        self.curr += 1;
        Some(c)
    }

    pub fn peek(&self) -> Option<u8> {
        if self.curr >= self.source.len() {
            return None;
        }
        Some(self.source[self.curr])
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

pub fn match_reserved_keyword(token: &[u8], line: usize) -> Option<Token<'_>> {
    match token {
        b"if" => Some(Token::If { line }),
        b"else" => Some(Token::Else { line }),
        b"then" => Some(Token::Then { line }),
        b"true" => Some(Token::True { line }),
        b"false" => Some(Token::False { line }),
        b"and" => Some(Token::And { line }),
        b"or" => Some(Token::Or { line }),
        b"while" => Some(Token::While { line }),
        b"do" => Some(Token::Do { line }),
        b"for" => Some(Token::For { line }),
        b"func" => Some(Token::Func { line }),
        b"null" => Some(Token::Null { line }),
        b"end" => Some(Token::End { line }),
        b"print" => Some(Token::Print { line }),
        b"println" => Some(Token::Println { line }),
        b"ret" => Some(Token::Ret { line }),
        _ => None,
    }
}
