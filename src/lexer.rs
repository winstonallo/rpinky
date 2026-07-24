use crate::{
    errors::TokenizationError,
    span::Span,
    tokens::{Lexeme, Token, TokenKind},
};

pub struct Lexer<'src> {
    source: &'src [u8],
    start: usize,
    curr: usize,
    tokens: Vec<Token>,
}

pub fn unescape(bytes: &[u8]) -> Vec<u8> {
    let mut res = Vec::with_capacity(bytes.len());
    let mut i = 0;

    while i < bytes.len() {
        if bytes[i] != b'\\' {
            res.push(bytes[i]);
            i += 1;
            continue;
        }
        if i >= bytes.len() - 1 {
            res.push(b'\\');
            break;
        }

        i += 1;

        match bytes[i] {
            b'n' => res.push(b'\n'),
            b't' => res.push(b'\t'),
            b'r' => res.push(b'\r'),
            b'\\' => res.push(b'\\'),
            _ => {
                res.push(b'\\');
                res.push(bytes[i]);
            }
        }

        i += 1;
    }

    res
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src [u8]) -> Self {
        Self {
            source,
            start: 0,
            curr: 0,
            tokens: vec![],
        }
    }

    /// Span of the token currently being consumed (`[start, curr)`).
    fn span(&self) -> Span {
        Span::new(self.start, self.curr)
    }

    fn push(&mut self, kind: TokenKind) {
        let span = self.span();
        self.tokens.push(Token::new(kind, span));
    }

    pub fn tokenize(&mut self) -> Result<&Vec<Token>, TokenizationError> {
        while self.curr < self.source.len() {
            self.start = self.curr;
            let Some(c) = self.advance() else {
                self.tokens.push(Token::new(TokenKind::Eof, Span::new(self.curr, self.curr)));
                return Ok(&self.tokens);
            };

            match c {
                b'\n' => (),
                ch if ch.is_ascii_whitespace() => (),
                b'(' => self.push(TokenKind::LParen),
                b')' => self.push(TokenKind::RParen),
                b'{' => self.push(TokenKind::LCurly),
                b'}' => self.push(TokenKind::RCurly),
                b'[' => self.push(TokenKind::LSquare),
                b']' => self.push(TokenKind::RSquare),
                b'.' => self.push(TokenKind::Dot),
                b',' => self.push(TokenKind::Comma),
                b'+' => self.push(TokenKind::Plus),
                b'-' => {
                    if self.match_curr(b'-') {
                        while self.peek().is_some_and(|c| c != b'\n') {
                            self.advance();
                        }
                    } else {
                        self.push(TokenKind::Minus);
                    }
                }
                b'*' => self.push(TokenKind::Star),
                b'^' => self.push(TokenKind::Caret),
                b'/' => self.push(TokenKind::Slash),
                b';' => self.push(TokenKind::Semicolon),
                b'?' => self.push(TokenKind::Question),
                b'%' => self.push(TokenKind::Mod),
                b'=' => {
                    if self.match_curr(b'=') {
                        self.push(TokenKind::EqualEqual);
                    } else {
                        self.push(TokenKind::Equal);
                    }
                }
                b'~' => {
                    if self.match_curr(b'=') {
                        self.push(TokenKind::NotEqual);
                    } else {
                        self.push(TokenKind::Not);
                    }
                }
                b'<' => {
                    if self.match_curr(b'=') {
                        self.push(TokenKind::LessEqual);
                    } else if self.match_curr(b'<') {
                        self.push(TokenKind::LessLess);
                    } else {
                        self.push(TokenKind::Less);
                    }
                }
                b'>' => {
                    if self.match_curr(b'=') {
                        self.push(TokenKind::GreaterEqual);
                    } else if self.match_curr(b'>') {
                        self.push(TokenKind::GreaterGreater);
                    } else {
                        self.push(TokenKind::Greater);
                    }
                }
                b':' => {
                    if self.match_curr(b'=') {
                        self.push(TokenKind::Assign);
                    } else {
                        self.push(TokenKind::Colon);
                    }
                }
                b'0'..=b'9' => self.handle_number_literal()?,
                b'\'' => self.handle_string_literal(b'\'')?,
                b'"' => self.handle_string_literal(b'"')?,
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.handle_identifier()?,
                _ => return Err(TokenizationError::new(format!("unexpected character: '{}'", c as char), self.span())),
            }
        }

        self.tokens.push(Token::new(TokenKind::Eof, Span::new(self.curr, self.curr)));
        Ok(&self.tokens)
    }

    fn handle_identifier(&mut self) -> Result<(), TokenizationError> {
        while self.peek().is_some_and(|c| c.is_ascii_alphanumeric() || c == b'_') {
            self.advance();
        }

        if let Some(keyword) = match_reserved_keyword(&self.source[self.start..self.curr], self.span()) {
            self.tokens.push(keyword);
        } else {
            self.push(TokenKind::Identifier {
                lexeme: Lexeme::new((self.source[self.start..self.curr]).to_vec()),
            });
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
                return Err(TokenizationError::new("invalid character in number literal".into(), self.span()));
            }
            self.push(TokenKind::FloatLiteral {
                lexeme: Lexeme::new((self.source[self.start..self.curr]).to_vec()),
            });
        } else {
            if self.peek().is_some_and(|c| c.is_ascii_alphabetic() || c == b'_') {
                return Err(TokenizationError::new("invalid character in number literal".into(), self.span()));
            }
            self.push(TokenKind::IntegerLiteral {
                lexeme: Lexeme::new((self.source[self.start..self.curr]).to_vec()),
            });
        }
        Ok(())
    }

    fn handle_string_literal(&mut self, quote: u8) -> Result<(), TokenizationError> {
        while self.peek().is_some_and(|c| c != quote && c != b'\n') {
            self.advance();
        }
        if self.curr >= self.source.len() || self.peek().is_some_and(|c| c == b'\n') {
            return Err(TokenizationError::new("unterminated string literal".into(), self.span()));
        }
        self.advance();
        self.push(TokenKind::StringLiteral {
            lexeme: Lexeme::new((self.source[self.start..self.curr]).to_vec()),
        });
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

pub fn match_reserved_keyword(token: &[u8], span: Span) -> Option<Token> {
    let kind = match token {
        b"if" => TokenKind::If,
        b"else" => TokenKind::Else,
        b"then" => TokenKind::Then,
        b"true" => TokenKind::True,
        b"false" => TokenKind::False,
        b"and" => TokenKind::And,
        b"or" => TokenKind::Or,
        b"while" => TokenKind::While,
        b"do" => TokenKind::Do,
        b"for" => TokenKind::For,
        b"func" => TokenKind::Func,
        b"null" => TokenKind::Null,
        b"end" => TokenKind::End,
        b"print" => TokenKind::Print,
        b"println" => TokenKind::Println,
        b"ret" => TokenKind::Ret,
        b"elif" => TokenKind::Elif,
        b"local" => TokenKind::Local,
        _ => return None,
    };
    Some(Token::new(kind, span))
}

#[cfg(test)]
mod tests {
    #![allow(clippy::unwrap_used)]
    use super::*;

    #[test]
    fn invalid_character_in_number_literal() {
        let mut lexer = Lexer::new(b"2a");
        assert!(lexer.tokenize().is_err());
    }

    #[test]
    fn punctuation_after_number_literal() {
        let mut lexer = Lexer::new(b"2)");

        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            *tokens,
            vec![
                Token::new(
                    TokenKind::IntegerLiteral {
                        lexeme: Lexeme::new(b"2".to_vec())
                    },
                    Span::new(0, 1)
                ),
                Token::new(TokenKind::RParen, Span::new(1, 2)),
                Token::new(TokenKind::Eof, Span::new(2, 2)),
            ]
        );
    }
}
