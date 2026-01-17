use crate::{
    errors::TokenizationError,
    tokens::{Lexeme, Token, TokenKind},
};

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

    pub fn tokenize(&mut self) -> Result<&Vec<Token<'_>>, TokenizationError> {
        while self.curr < self.source.len() {
            self.start = self.curr;
            let Some(c) = self.advance() else {
                return Ok(&self.tokens);
            };
            match c {
                b'\n' => self.line += 1,
                ch if ch.is_ascii_whitespace() => (),
                b'(' => self.tokens.push(Token::new(TokenKind::LParen, self.line)),
                b')' => self.tokens.push(Token::new(TokenKind::RParen, self.line)),
                b'{' => self.tokens.push(Token::new(TokenKind::LCurly, self.line)),
                b'}' => self.tokens.push(Token::new(TokenKind::RCurly, self.line)),
                b'[' => self.tokens.push(Token::new(TokenKind::LSquare, self.line)),
                b']' => self.tokens.push(Token::new(TokenKind::RSquare, self.line)),
                b'.' => self.tokens.push(Token::new(TokenKind::Dot, self.line)),
                b',' => self.tokens.push(Token::new(TokenKind::Comma, self.line)),
                b'+' => self.tokens.push(Token::new(TokenKind::Plus, self.line)),
                b'-' => {
                    if self.match_curr(b'-') {
                        while self.peek().is_some_and(|c| c != b'\n') {
                            self.advance();
                        }
                    } else {
                        self.tokens.push(Token::new(TokenKind::Minus, self.line));
                    }
                }
                b'*' => self.tokens.push(Token::new(TokenKind::Star, self.line)),
                b'^' => self.tokens.push(Token::new(TokenKind::Caret, self.line)),
                b'/' => self.tokens.push(Token::new(TokenKind::Slash, self.line)),
                b';' => self.tokens.push(Token::new(TokenKind::Semicolon, self.line)),
                b'?' => self.tokens.push(Token::new(TokenKind::Question, self.line)),
                b'%' => self.tokens.push(Token::new(TokenKind::Mod, self.line)),
                b'=' => {
                    if self.match_curr(b'=') {
                        self.tokens.push(Token::new(TokenKind::EqualEqual, self.line));
                    } else {
                        self.tokens.push(Token::new(TokenKind::Equal, self.line));
                    }
                }
                b'~' => {
                    if self.match_curr(b'=') {
                        self.tokens.push(Token::new(TokenKind::NotEqual, self.line));
                    } else {
                        self.tokens.push(Token::new(TokenKind::Not, self.line));
                    }
                }
                b'<' => {
                    if self.match_curr(b'=') {
                        self.tokens.push(Token::new(TokenKind::LessEqual, self.line));
                    } else if self.match_curr(b'<') {
                        self.tokens.push(Token::new(TokenKind::LessLess, self.line));
                    } else {
                        self.tokens.push(Token::new(TokenKind::Less, self.line));
                    }
                }
                b'>' => {
                    if self.match_curr(b'=') {
                        self.tokens.push(Token::new(TokenKind::GreaterEqual, self.line));
                    } else if self.match_curr(b'>') {
                        self.tokens.push(Token::new(TokenKind::GreaterGreater, self.line));
                    } else {
                        self.tokens.push(Token::new(TokenKind::Greater, self.line));
                    }
                }
                b':' => {
                    if self.match_curr(b'=') {
                        self.tokens.push(Token::new(TokenKind::Assign, self.line));
                    } else {
                        self.tokens.push(Token::new(TokenKind::Colon, self.line));
                    }
                }
                b'0'..=b'9' => self.handle_number_literal()?,
                b'\'' => self.handle_string_literal(b'\'')?,
                b'"' => self.handle_string_literal(b'"')?,
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.handle_identifier()?,
                _ => panic!("[Line {}] Unexpected character: '{}'", self.line, c as char),
            }
        }

        Ok(&self.tokens)
    }

    fn handle_identifier(&mut self) -> Result<(), TokenizationError> {
        while self.peek().is_some_and(|c| c.is_ascii_alphanumeric() || c == b'_') {
            self.advance();
        }

        if let Some(keyword) = match_reserved_keyword(&self.source[self.start..self.curr], self.line) {
            self.tokens.push(keyword);
        } else {
            self.tokens.push(Token::new(
                TokenKind::Identifier {
                    lexeme: Lexeme::new(&self.source[self.start..self.curr]),
                },
                self.line,
            ));
        }
        Ok(())
    }

    fn handle_number_literal(&mut self) -> Result<(), TokenizationError> {
        while self.peek().is_some_and(|c| c.is_ascii_digit()) {
            self.advance();
        }

        if self.peek().is_some_and(|c| c == b'.') && self.lookahead(1).is_some_and(|x| x.is_ascii_digit()) {
            self.advance();
            while self.peek().is_some_and(|c| c.is_ascii_digit()) {
                self.advance();
            }
            if self.peek().is_some_and(|c| c.is_ascii_alphabetic() || c == b'_') {
                return Err(TokenizationError::new("invalid character in number literal".into(), self.line));
            }
            self.tokens.push(Token::new(
                TokenKind::FloatLiteral {
                    lexeme: Lexeme::new(&self.source[self.start..self.curr]),
                },
                self.line,
            ));
        } else {
            if self.peek().is_some_and(|c| c.is_ascii_alphabetic() || c == b'_') {
                return Err(TokenizationError::new("invalid character in number literal".into(), self.line));
            }
            self.tokens.push(Token::new(
                TokenKind::IntegerLiteral {
                    lexeme: Lexeme::new(&self.source[self.start..self.curr]),
                },
                self.line,
            ));
        }
        Ok(())
    }

    fn handle_string_literal(&mut self, quote: u8) -> Result<(), TokenizationError> {
        while self.peek().is_some_and(|c| c != quote && c != b'\n') {
            self.advance();
        }
        if self.curr >= self.source.len() || self.peek().is_some_and(|c| c == b'\n') {
            return Err(TokenizationError::new("unterminated string literal".into(), self.line));
        }
        self.advance();
        self.tokens.push(Token::new(
            TokenKind::StringLiteral {
                lexeme: Lexeme::new(&self.source[self.start..self.curr]),
            },
            self.line,
        ));
        Ok(())
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

    pub fn match_curr(&mut self, expected: u8) -> bool {
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
        b"if" => Some(Token::new(TokenKind::If, line)),
        b"else" => Some(Token::new(TokenKind::Else, line)),
        b"then" => Some(Token::new(TokenKind::Then, line)),
        b"true" => Some(Token::new(TokenKind::True, line)),
        b"false" => Some(Token::new(TokenKind::False, line)),
        b"and" => Some(Token::new(TokenKind::And, line)),
        b"or" => Some(Token::new(TokenKind::Or, line)),
        b"while" => Some(Token::new(TokenKind::While, line)),
        b"do" => Some(Token::new(TokenKind::Do, line)),
        b"for" => Some(Token::new(TokenKind::For, line)),
        b"func" => Some(Token::new(TokenKind::Func, line)),
        b"null" => Some(Token::new(TokenKind::Null, line)),
        b"end" => Some(Token::new(TokenKind::End, line)),
        b"print" => Some(Token::new(TokenKind::Print, line)),
        b"println" => Some(Token::new(TokenKind::Println, line)),
        b"ret" => Some(Token::new(TokenKind::Ret, line)),
        _ => None,
    }
}
