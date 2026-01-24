use std::rc::Rc;

use crate::{
    errors::ParseError,
    model::{
        Assignment, BinOp, BoolLiteral, Elif, Expr, FloatLiteral, For, FuncDecl, FuncParam, Identifier, If, IntegerLiteral, LogicalOp, Print, Println, Stmt,
        Stmts, StringLiteral, UnOp, While,
    },
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

    /// Consume and return the current token.
    fn advance(&mut self) -> &Token {
        debug_assert!(self.curr < self.tokens.len(), "called advance when tokens were already exhausted");

        let tok = &self.tokens[self.curr];
        self.curr += 1;
        tok
    }

    /// Return the current token.
    fn peek(&self) -> Token {
        debug_assert!(self.curr < self.tokens.len(), "called peek when tokens where already exhausted");

        (self.tokens[self.curr]).clone()
    }

    /// Check `predicate` against the current token.
    fn is_next<F: Fn(&Token) -> bool>(&self, predicate: F) -> bool {
        debug_assert!(self.curr < self.tokens.len(), "called is_next when tokens where already exhausted");

        predicate(&self.peek())
    }

    /// Consume the current token if `predicate(current)` is true.
    fn match_curr<F: Fn(&Token) -> bool>(&mut self, predicate: F) -> bool {
        debug_assert!(self.curr < self.tokens.len(), "called match_curr when tokens where already exhausted");

        if !predicate(&self.peek()) {
            return false;
        }
        self.curr += 1;
        true
    }

    /// Return the previous token.
    fn previous_token(&self) -> Token {
        debug_assert!(self.curr > 0, "called previous_token while at position 0");

        (self.tokens[self.curr - 1]).clone()
    }

    /// `<primary> ::= <integer>
    ///              | <float>
    ///              | <bool>
    ///              | <string>
    ///              | <identifier>
    ///              | '(' <expr> ')'`
    fn primary(&mut self) -> Result<Expr, ParseError> {
        let token = self.peek();
        match token.kind() {
            TokenKind::StringLiteral { .. } => {
                self.advance();
                Ok(Expr::String(StringLiteral::try_from(&token)?))
            }
            TokenKind::True | TokenKind::False => {
                self.advance();
                Ok(Expr::Bool(BoolLiteral::try_from(&token)?))
            }
            TokenKind::IntegerLiteral { .. } => {
                self.advance();
                Ok(Expr::Integer(IntegerLiteral::try_from(&token)?))
            }
            TokenKind::FloatLiteral { .. } => {
                self.advance();
                Ok(Expr::Float(FloatLiteral::try_from(&token)?))
            }
            TokenKind::LParen => {
                self.advance();
                let expr = self.expr()?;
                if !self.match_curr(|tok| matches!(tok.kind(), TokenKind::RParen)) {
                    return Err(ParseError::new("expected closing ')'".into(), token.line()));
                }
                Ok(Expr::Grouping(Box::new(expr)))
            }
            TokenKind::Identifier { lexeme } => {
                self.advance();
                Ok(Expr::Identifier(Identifier::new(&Rc::new(lexeme.to_string()), token.line())))
            }
            _ => Err(ParseError::new(format!("unexpected {}", token.kind()), token.line())),
        }
    }

    /// `<exponent> ::= <primary> ( '^' <primary> )*`
    fn exponent(&mut self) -> Result<Expr, ParseError> {
        let base = self.primary()?;

        if self.match_curr(|tok| matches!(tok.kind(), TokenKind::Caret)) {
            let operator = self.previous_token();
            let exponent = self.unary()?; // have to go back up here to solve expressions like 2 ^ -3
            return Ok(Expr::BinOp(BinOp::new(operator, base, exponent)));
        }

        Ok(base)
    }

    /// `<unary> ::= ( '-' | '~' )* <exponent>`
    fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_curr(|tok| matches!(tok.kind(), TokenKind::Not | TokenKind::Minus)) {
            let operator = self.previous_token();
            let operand = self.unary()?;
            return Ok(Expr::UnOp(UnOp::new(operator, operand)));
        }
        self.exponent()
    }

    /// `<modulo> ::= <unary> ( ( '%' ) <unary> )*`
    fn modulo(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;

        while self.match_curr(|tok| matches!(tok.kind(), TokenKind::Mod)) {
            let operator = self.previous_token();
            let right = self.modulo()?;
            expr = Expr::BinOp(BinOp::new(operator, expr, right));
        }
        Ok(expr)
    }

    /// `<multiplication> ::= <modulo> ( ( '*' | '/' ) <modulo> )*`
    fn multiplication(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.modulo()?;

        while self.match_curr(|tok| matches!(tok.kind(), TokenKind::Star | TokenKind::Slash)) {
            let operator = self.previous_token();
            let right = self.modulo()?;
            expr = Expr::BinOp(BinOp::new(operator, expr, right));
        }
        Ok(expr)
    }

    /// `<addition> ::= <multiplication> ( ( '+' | '-' ) <multiplication> )*`
    fn addition(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.multiplication()?;

        while self.match_curr(|tok| matches!(tok.kind(), TokenKind::Plus | TokenKind::Minus)) {
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
                TokenKind::Less | TokenKind::LessEqual | TokenKind::Greater | TokenKind::GreaterEqual
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

        while self.match_curr(|tok| matches!(tok.kind(), TokenKind::EqualEqual | TokenKind::NotEqual)) {
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

    // fn func_call(&mut self) -> Result<Expr, ParseError> {

    // }

    /// Creates an AST representation of an expression.
    pub fn expr(&mut self) -> Result<Expr, ParseError> {
        self.or()
    }

    /// `<print_stmt> ::= 'print' expr`
    fn print_stmt(&mut self) -> Result<Stmt, ParseError> {
        let tok = self.match_curr(|tok| matches!(tok.kind(), TokenKind::Print));
        debug_assert!(tok, "called print_stmt without 'print' token");

        Ok(Stmt::Print(Print::new(self.expr()?)))
    }

    /// `<print_stmt> ::= 'print' expr`
    fn println_stmt(&mut self) -> Result<Stmt, ParseError> {
        let tok = self.match_curr(|tok| matches!(tok.kind(), TokenKind::Println));
        debug_assert!(tok, "called println_stmt without 'println' token");

        Ok(Stmt::Println(Println::new(self.expr()?)))
    }

    /// ```ignore
    /// <if_stmt> ::= 'if' <expr> 'then' <stmts>
    ///     ( 'elif' <expr> 'then' stmts )*
    ///     ( 'else' <stmts> )? 'end'
    /// ```
    fn if_stmt(&mut self) -> Result<Stmt, ParseError> {
        let tok = self.match_curr(|tok| matches!(tok.kind(), TokenKind::If));
        debug_assert!(tok, "called if_stmt without 'if' token");

        let test = self.expr()?;

        if !self.match_curr(|tok| matches!(tok.kind(), TokenKind::Then)) {
            return Err(ParseError::new(
                "expected token 'then' before if statement body".into(),
                self.previous_token().line(),
            ));
        }

        let then = self.stmts()?;
        let mut elif = vec![];

        while self.match_curr(|tok| matches!(tok.kind(), TokenKind::Elif)) {
            let test = self.expr()?;

            if !self.match_curr(|tok| matches!(tok.kind(), TokenKind::Then)) {
                return Err(ParseError::new(
                    "expected token 'then' before elif statement body".into(),
                    self.previous_token().line(),
                ));
            }

            elif.push(Elif::new(test, self.stmts()?));
        }

        let r#else = if self.match_curr(|tok| matches!(tok.kind(), TokenKind::Else)) {
            Some(self.stmts()?)
        } else {
            None
        };

        if !self.match_curr(|tok| matches!(tok.kind(), TokenKind::End)) {
            return Err(ParseError::new(
                "expected token 'end' after statement body".into(),
                self.previous_token().line(),
            ));
        }

        Ok(Stmt::If(If::new(test, then, elif, r#else)))
    }

    /// `<while> ::= 'while' <expr> 'do' <stmts> 'end'`
    fn while_stmt(&mut self) -> Result<Stmt, ParseError> {
        let tok = self.match_curr(|tok| matches!(tok.kind(), TokenKind::While));
        debug_assert!(tok, "called while_stmt without 'while' token");

        let test = self.expr()?;

        if !self.match_curr(|tok| matches!(tok.kind(), TokenKind::Do)) {
            return Err(ParseError::new(
                "expected token 'do' before while loop body".into(),
                self.previous_token().line(),
            ));
        }

        let body = self.stmts()?;

        if !self.match_curr(|tok| matches!(tok.kind(), TokenKind::End)) {
            return Err(ParseError::new(
                "expected token 'end' after while loop body".into(),
                self.previous_token().line(),
            ));
        }

        Ok(Stmt::While(While::new(test, body)))
    }

    /// `<for> ::= 'for' <assignment> ',' <expr> ( ',' <expr> )? 'do' <stmts> 'end'`
    fn for_stmt(&mut self) -> Result<Stmt, ParseError> {
        let tok = self.match_curr(|tok| matches!(tok.kind(), TokenKind::For));
        debug_assert!(tok, "called for_stmt without 'for' token");

        let var = self.primary()?;
        if !self.match_curr(|tok| matches!(tok.kind(), TokenKind::Assign)) {
            return Err(ParseError::new("expected assignment after 'for' token".into(), self.previous_token().line()));
        }
        let start = self.expr()?;

        if !self.match_curr(|tok| matches!(tok.kind(), TokenKind::Comma)) {
            return Err(ParseError::new(
                "expected ',' after for loop initialization statement".into(),
                self.previous_token().line(),
            ));
        }

        let end = self.expr()?;

        let mut step = None;
        if self.match_curr(|tok| matches!(tok.kind(), TokenKind::Comma)) {
            step = Some(self.expr()?);
        }

        if !self.match_curr(|tok| matches!(tok.kind(), TokenKind::Do)) {
            return Err(ParseError::new("expected token 'do' before for loop body".into(), self.previous_token().line()));
        }

        let body = self.stmts()?;
        if !self.match_curr(|tok| matches!(tok.kind(), TokenKind::End)) {
            return Err(ParseError::new("expected token 'end' after for loop body".into(), self.previous_token().line()));
        }

        Ok(Stmt::For(For::new(var, start, end, step, body)))
    }

    /// `<func_decl> ::= "func" <name> "(" <params>? ")" <body> "end"`
    fn func_decl_stmt(&mut self) -> Result<Stmt, ParseError> {
        let tok = self.match_curr(|tok| matches!(tok.kind(), TokenKind::Func));
        debug_assert!(tok, "called func_decl_stmt without 'func' token");

        let Expr::Identifier(name) = self.primary()? else {
            return Err(ParseError::new("expected identifier".into(), self.previous_token().line()));
        };

        if !self.match_curr(|tok| matches!(tok.kind(), TokenKind::LParen)) {
            return Err(ParseError::new("expected token '(' after function name".into(), self.previous_token().line()));
        }

        let mut params = vec![];
        while !matches!(self.previous_token().kind(), TokenKind::RParen) {
            let Expr::Identifier(identifier) = self.primary()? else {
                return Err(ParseError::new("expected identifier".into(), self.previous_token().line()));
            };
            params.push(FuncParam::new(identifier.name().as_ref().into(), identifier.line()));
            if !self.match_curr(|tok| matches!(tok.kind(), TokenKind::Comma | TokenKind::RParen)) {
                return Err(ParseError::new(
                    "expected token ')' or ',' after parameter".into(),
                    self.previous_token().line(),
                ));
            }
        }

        let body = self.stmts()?;

        if !self.match_curr(|tok| matches!(tok.kind(), TokenKind::End)) {
            return Err(ParseError::new("expected token 'end' after function body".into(), self.previous_token().line()));
        }

        Ok(Stmt::FuncDecl(FuncDecl::new(name.name().as_ref().into(), params, body, name.line())))
    }

    /// ```ignore
    /// stmt ::= expr_stmt
    ///     | print_stmt
    ///     | assign
    ///     | local_assign
    ///     | println_stmt
    ///     | if_stmt
    ///     | while_stmt
    ///     | for_stmt
    ///     | func_decl
    ///     | func_call
    ///     | ret_stmt
    /// ```
    fn stmt(&mut self) -> Result<Stmt, ParseError> {
        let token = self.peek();

        match token.kind() {
            TokenKind::Print => self.print_stmt(),
            TokenKind::Println => self.println_stmt(),
            TokenKind::If => self.if_stmt(),
            TokenKind::While => self.while_stmt(),
            TokenKind::For => self.for_stmt(),
            TokenKind::Func => self.func_decl_stmt(),
            _ => {
                let lhs = self.expr()?;
                if self.match_curr(|tok| matches!(tok.kind(), TokenKind::Assign)) {
                    let rhs = self.expr()?;
                    Ok(Stmt::Assignment(Assignment::new(lhs, rhs)))
                } else {
                    // println!("{}")
                    unimplemented!("function call {token:?}")
                }
            }
        }
    }

    fn stmts(&mut self) -> Result<Stmts, ParseError> {
        let mut stmts = vec![];
        // loop until end of block or EOF
        while !self.is_next(|tok| matches!(tok.kind(), TokenKind::Else | TokenKind::Elif | TokenKind::End | TokenKind::Eof)) {
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
