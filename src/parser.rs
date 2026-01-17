use crate::{
    errors::ParseError,
    model::{BinOp, Bool, Expr, Float, If, Integer, LogicalOp, Print, Println, Stmt, Stmts, StringType, UnOp},
    tokens::{Token, TokenKind},
};

pub struct Parser {
    tokens: Vec<Token>,
    curr: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, curr: 0 }
    }

    fn advance(&mut self) -> Option<&Token> {
        if self.curr >= self.tokens.len() {
            return None;
        }
        let tok = &self.tokens[self.curr];
        self.curr += 1;
        Some(tok)
    }

    fn peek(&self) -> Token {
        debug_assert!(self.curr < self.tokens.len());
        (self.tokens[self.curr]).clone()
    }

    #[allow(unused)]
    fn is_next<F: Fn(&Token) -> bool>(&self, predicate: F) -> bool {
        if self.curr >= self.tokens.len() {
            return false;
        }
        predicate(&self.tokens[self.curr + 1])
    }

    #[allow(unused)]
    fn expect<F: Fn(Token) -> Result<Token, ParseError>>(&self, predicate: F) -> Result<Token, ParseError> {
        if self.curr >= self.tokens.len() {
            return Err(ParseError::new(
                format!("found {:?} at the end of parsing", self.previous_token()),
                self.previous_token().line(),
            ));
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

    fn previous_token(&self) -> Token {
        debug_assert!(self.curr > 0);
        (self.tokens[self.curr - 1]).clone()
    }

    /// `<primary> ::= <integer>
    ///              | <float>
    ///              | <bool>
    ///              | <string>
    ///              | '(' <expr> ')'`
    fn primary(&mut self) -> Result<Expr, ParseError> {
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
                let expr = self.expr()?;
                if !self.match_curr(|tok| matches!(tok.kind(), TokenKind::RParen { .. })) {
                    return Err(ParseError::new("expected closing ')'".into(), token.line()));
                }
                Ok(Expr::Grouping(Box::new(expr)))
            }
            _ => Err(ParseError::new(format!("unexpected {}", token.kind()), token.line())),
        }
    }

    /// `<exponent> ::= <primary> ( '^' <primary> )*`
    fn exponent(&mut self) -> Result<Expr, ParseError> {
        let base = self.primary()?;

        if self.match_curr(|tok| matches!(tok.kind(), TokenKind::Caret { .. })) {
            let operator = self.previous_token();
            let exponent = self.unary()?; // have to go back up here to solve expressions like 2 ^ -3
            return Ok(Expr::BinOp(BinOp::new(operator, base, exponent)));
        }

        Ok(base)
    }

    /// `<unary> ::= ( '-' | '~' )* <exponent>`
    fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_curr(|tok| matches!(tok.kind(), TokenKind::Not { .. } | TokenKind::Minus { .. })) {
            let operator = self.previous_token();
            let operand = self.unary()?;
            return Ok(Expr::UnOp(UnOp::new(operator, operand)));
        }
        self.exponent()
    }

    /// `<modulo> ::= <unary> ( ( '%' ) <unary> )*`
    fn modulo(&mut self) -> Result<Expr, ParseError> {
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
    fn multiplication(&mut self) -> Result<Expr, ParseError> {
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
    fn addition(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.multiplication()?;
        while self.match_curr(|tok| matches!(tok.kind(), TokenKind::Plus { .. } | TokenKind::Minus { .. })) {
            let operator = self.previous_token();
            let right = self.multiplication()?;
            expr = Expr::BinOp(BinOp::new(operator, expr, right));
        }
        Ok(expr)
    }

    /// `<comparison> := <addition> ( ( '<' | '<=' | '>' | '>=' ) <addition> )*`
    fn comparison(&mut self) -> Result<Expr, ParseError> {
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
    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;
        while self.match_curr(|tok| matches!(tok.kind(), TokenKind::EqualEqual { .. } | TokenKind::NotEqual { .. })) {
            let operator = self.previous_token();
            let right = self.comparison()?;
            expr = Expr::BinOp(BinOp::new(operator, expr, right));
        }
        Ok(expr)
    }

    /// `<and> ::= <equality> ( 'and' <equality> )*`
    fn and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;
        while self.match_curr(|tok| matches!(tok.kind(), TokenKind::And)) {
            let operator = self.previous_token();
            let right = self.equality()?;
            expr = Expr::LogicalOp(LogicalOp::new(operator, expr, right));
        }
        Ok(expr)
    }

    /// `<or> ::= <and> ( 'or' <and> )*`
    fn or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.and()?;
        while self.match_curr(|tok| matches!(tok.kind(), TokenKind::Or)) {
            let operator = self.previous_token();
            let right = self.and()?;
            expr = Expr::LogicalOp(LogicalOp::new(operator, expr, right));
        }
        Ok(expr)
    }

    /// Evaluates an expression in the form of an AST.
    pub fn expr(&mut self) -> Result<Expr, ParseError> {
        self.or()
    }

    fn print_stmt(&mut self) -> Result<Stmt, ParseError> {
        if self.match_curr(|tok| matches!(tok.kind(), TokenKind::Print)) {
            return Ok(Stmt::Print(Print::new(self.expr()?)));
        }
        Err(ParseError::new("idk bro".into(), 0))
    }

    fn println_stmt(&mut self) -> Result<Stmt, ParseError> {
        if self.match_curr(|tok| matches!(tok.kind(), TokenKind::Println)) {
            return Ok(Stmt::Println(Println::new(self.expr()?)));
        }
        Err(ParseError::new("idk bro".into(), 0))
    }

    fn if_stmt(&mut self) -> Result<Stmt, ParseError> {
        if !self.match_curr(|tok| matches!(tok.kind(), TokenKind::If)) {
            return Err(ParseError::new("expected token 'if'".into(), self.previous_token().line()));
        }

        let test = self.expr()?;

        if !self.match_curr(|tok| matches!(tok.kind(), TokenKind::Then)) {
            return Err(ParseError::new("expected token 'then'".into(), self.previous_token().line()));
        }

        let then = self.stmts()?;

        let r#else = if self.match_curr(|tok| matches!(tok.kind(), TokenKind::Else)) {
            Some(self.stmts()?)
        } else {
            None
        };

        if !self.match_curr(|tok| matches!(tok.kind(), TokenKind::End)) {
            return Err(ParseError::new("expected token 'end'".into(), self.previous_token().line()));
        }

        Ok(Stmt::If(If::new(test, then, r#else)))
    }

    fn stmt(&mut self) -> Result<Stmt, ParseError> {
        // the next token predicts the next statement
        let token = self.peek();

        match token.kind() {
            TokenKind::Print => self.print_stmt(),
            TokenKind::Println => self.println_stmt(),
            TokenKind::If => self.if_stmt(),
            _ => unimplemented!(),
        }
    }

    fn stmts(&mut self) -> Result<Stmts, ParseError> {
        let mut stmts = vec![];
        // loop until end of block or EOF
        while self.curr < self.tokens.len() && !matches!(self.peek().kind(), TokenKind::Else | TokenKind::End) {
            stmts.push(self.stmt()?);
        }
        Ok(Stmts::new(stmts))
    }

    /// `<program> ::= <stmt>*`
    fn program(&mut self) -> Result<Stmts, ParseError> {
        self.stmts()
    }

    pub fn parse(&mut self) -> Result<Stmts, ParseError> {
        self.program()
    }
}
