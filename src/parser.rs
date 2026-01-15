use crate::{
    errors::ParseError,
    model::{BinOp, Bool, Expr, Float, Integer, StringType, UnOp},
    tokens::{Token, TokenKind},
};

pub struct Parser<'src> {
    tokens: &'src Vec<Token<'src>>,
    curr: usize,
}

impl<'src> Parser<'src> {
    pub fn new(tokens: &'src Vec<Token<'src>>) -> Self {
        Self { tokens, curr: 0 }
    }

    fn advance(&mut self) -> Option<Token<'src>> {
        if self.curr >= self.tokens.len() {
            return None;
        }
        let tok = self.tokens[self.curr];
        self.curr += 1;
        Some(tok)
    }

    fn peek(&self) -> Token<'src> {
        debug_assert!(self.curr < self.tokens.len());
        self.tokens[self.curr]
    }

    fn is_next<F: Fn(&Token) -> bool>(&self, predicate: F) -> bool {
        if self.curr >= self.tokens.len() {
            return false;
        }
        predicate(&self.tokens[self.curr + 1])
    }

    fn expect<F: Fn(Token<'src>) -> Result<Token, String>>(&'src self, predicate: F) -> Result<Token<'src>, String> {
        if self.curr >= self.tokens.len() {
            return Err(format!("Found {:?} at the end of parsing", self.previous_token()));
        }
        predicate(self.peek())
    }

    fn match_curr<F: Fn(&Token) -> bool>(&mut self, predicate: F) -> bool {
        if self.curr >= self.tokens.len() {
            return false;
        }
        if !predicate(&self.tokens[self.curr]) {
            return false;
        }
        self.curr += 1;
        true
    }

    fn previous_token(&self) -> Token<'src> {
        debug_assert!(self.curr > 0);
        self.tokens[self.curr - 1]
    }

    /// `<primary> ::= <integer>
    ///              | <float>
    ///              | <bool>
    ///              | <string>
    ///              | '(' <expr> ')'`
    fn primary(&mut self) -> Result<Expr<'src>, ParseError> {
        let token = self.peek();
        match token.kind() {
            TokenKind::StringLiteral { .. } => {
                self.advance();
                Ok(Expr::String(StringType::try_from(&token)?))
            }
            TokenKind::True | TokenKind::False => {
                self.advance();
                Ok(Expr::Bool(Bool::try_from(&token)?))
            }
            TokenKind::IntegerLiteral { .. } => {
                self.advance();
                Ok(Expr::Integer(Integer::try_from(&token)?))
            }
            TokenKind::FloatLiteral { .. } => {
                self.advance();
                Ok(Expr::Float(Float::try_from(&token)?))
            }
            TokenKind::LParen { .. } => {
                self.advance();
                let expr = self.addition()?;
                if !self.match_curr(|tok| matches!(tok.kind(), TokenKind::RParen { .. })) {
                    return Err(ParseError::new("expected closing ')'".into(), token.line()));
                }
                Ok(Expr::Grouping(Box::new(expr)))
            }
            _ => Err(ParseError::new(format!("unexpected {}", token.kind()), token.line())),
        }
    }

    /// `<exponent> ::= <primary> ( '^' <primary> )*`
    fn exponent(&mut self) -> Result<Expr<'src>, ParseError> {
        let base = self.primary()?;

        if self.match_curr(|tok| matches!(tok.kind(), TokenKind::Caret { .. })) {
            let operator = self.previous_token();
            let exponent = self.unary()?; // have to go back up here to solve expressions like 2 ^ -3
            return Ok(Expr::BinOp(BinOp::new(operator, base, exponent)));
        }

        Ok(base)
    }

    /// `<unary> ::= ( '-' | '~' )* <exponent>`
    fn unary(&mut self) -> Result<Expr<'src>, ParseError> {
        if self.match_curr(|tok| matches!(tok.kind(), TokenKind::Not { .. } | TokenKind::Minus { .. })) {
            let operator = self.previous_token();
            let operand = self.unary()?;
            return Ok(Expr::UnOp(UnOp::new(operator, operand)));
        }
        self.exponent()
    }

    /// `<modulo> ::= <unary> ( ( '%' ) <unary> )*`
    fn modulo(&mut self) -> Result<Expr<'src>, ParseError> {
        let mut expr = self.unary()?;
        while self.match_curr(|tok| matches!(tok.kind(), TokenKind::Mod { .. })) {
            let operator = self.previous_token();
            let right = self.modulo()?;
            expr = Expr::BinOp(BinOp::new(operator, expr, right));
        }
        Ok(expr)
    }

    /// `<multiplication> ::= <modulo> ( ( '*' | '/' ) <modulo> )*`
    // aka `term`
    fn multiplication(&mut self) -> Result<Expr<'src>, ParseError> {
        let mut expr = self.modulo()?;
        while self.match_curr(|tok| matches!(tok.kind(), TokenKind::Star { .. } | TokenKind::Slash { .. })) {
            let operator = self.previous_token();
            let right = self.modulo()?;
            expr = Expr::BinOp(BinOp::new(operator, expr, right));
        }
        Ok(expr)
    }

    /// `<addition> ::= <multiplication> ( ( '+' | '-' ) <multiplication> )*`
    // aka `expr`
    fn addition(&mut self) -> Result<Expr<'src>, ParseError> {
        let mut expr = self.multiplication()?;
        while self.match_curr(|tok| matches!(tok.kind(), TokenKind::Plus { .. } | TokenKind::Minus { .. })) {
            let operator = self.previous_token();
            let right = self.multiplication()?;
            expr = Expr::BinOp(BinOp::new(operator, expr, right));
        }
        Ok(expr)
    }

    /// `<comparison> := <addition> ( ( '<' | '<=' | '>' | '>=' ) <addition> )*`
    fn comparison(&mut self) -> Result<Expr<'src>, ParseError> {
        let mut expr = self.addition()?;
        while self.match_curr(|tok| {
            matches!(
                tok.kind(),
                TokenKind::Less { .. } | TokenKind::LessEqual { .. } | TokenKind::Greater { .. } | TokenKind::GreaterEqual { .. }
            )
        }) {
            let operator = self.previous_token();
            let right = self.addition()?;
            expr = Expr::BinOp(BinOp::new(operator, expr, right));
        }
        Ok(expr)
    }

    /// `<equality> := <comparison> ( ( '==' | '~='  ) <comparison> )*`
    fn equality(&mut self) -> Result<Expr<'src>, ParseError> {
        let mut expr = self.comparison()?;
        while self.match_curr(|tok| matches!(tok.kind(), TokenKind::EqualEqual { .. } | TokenKind::NotEqual { .. })) {
            let operator = self.previous_token();
            let right = self.comparison()?;
            expr = Expr::BinOp(BinOp::new(operator, expr, right));
        }
        Ok(expr)
    }

    pub fn parse(&mut self) -> Result<Expr<'src>, ParseError> {
        self.equality()
    }
}
