use crate::{
    model::{BinOp, Expr, Float, Integer, UnOp},
    tokens::Token,
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

    /// `<primary> ::= <integer> | <float> | '(' <expr> ')'`
    fn primary(&mut self) -> Expr<'src> {
        let token = self.peek();
        match token {
            Token::IntegerLiteral { .. } => {
                self.advance();
                if let Ok(int) = Integer::try_from(&token) {
                    return Expr::Integer(int);
                }
                panic!("brint");
            }
            Token::FloatLiteral { .. } => {
                self.advance();
                if let Ok(float) = Float::try_from(&token) {
                    return Expr::Float(float);
                }
                panic!("broat");
            }
            Token::LParen { .. } => {
                self.advance();
                let expr = self.expr();
                if !self.match_curr(|tok| matches!(tok, Token::RParen { .. })) {
                    panic!("Expected ')'");
                }
                Expr::Grouping(Box::new(expr))
            }
            _ => panic!("bruh"),
        }
    }

    /// `<unary> ::= ( '+' | '-' | '~' ) <unary> | <primary>`
    fn unary(&mut self) -> Expr<'src> {
        if self.match_curr(|tok| matches!(tok, Token::Not { .. } | Token::Minus { .. } | Token::Plus { .. })) {
            let operator = self.previous_token();
            let operand = self.unary();
            return Expr::UnOp(UnOp::new(operator, operand));
        }
        self.primary()
    }

    /// `<factor> ::= <unary>`
    fn factor(&mut self) -> Expr<'src> {
        self.unary()
    }

    /// `<term> ::= <factor> ( ( '*' | '/' ) <factor> )*`
    fn term(&mut self) -> Expr<'src> {
        let mut expr = self.factor();
        while self.match_curr(|tok| matches!(tok, Token::Star { .. } | Token::Slash { .. })) {
            let operator = self.previous_token();
            let right = self.factor();
            expr = Expr::BinOp(BinOp::new(operator, expr, right));
        }
        expr
    }

    /// `<expr> ::= <term> ( ( '+' | '-' ) <term> )*`
    fn expr(&mut self) -> Expr<'src> {
        let mut expr = self.term();
        while self.match_curr(|tok| matches!(tok, Token::Plus { .. } | Token::Minus { .. })) {
            let operator = self.previous_token();
            let right = self.term();
            expr = Expr::BinOp(BinOp::new(operator, expr, right));
        }
        expr
    }

    pub fn parse(&mut self) -> Expr<'src> {
        self.expr()
    }
}
