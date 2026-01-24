use std::rc::Rc;

use crate::{
    debug,
    errors::ParseError,
    tokens::{Token, TokenKind},
    visitor::{ExprVisitor, StmtVisitor},
};

#[derive(Clone)]
pub enum Node {
    Expr(Expr),
    Stmts(Stmts),
}

/// Evaluates to a result
#[derive(Clone)]
pub enum Expr {
    Integer(IntegerLiteral),
    Float(FloatLiteral),
    Grouping(Box<Expr>),
    UnOp(UnOp),
    BinOp(BinOp),
    String(StringLiteral),
    Bool(BoolLiteral),
    LogicalOp(LogicalOp),
    Identifier(Identifier),
    FuncCall(FuncCall),
}

impl Expr {
    pub fn accept<T>(&self, visitor: &mut impl ExprVisitor<T>) -> T {
        match self {
            Expr::Integer(n) => visitor.visit_integer(n),
            Expr::Float(f) => visitor.visit_float(f),
            Expr::String(s) => visitor.visit_string(s),
            Expr::Bool(b) => visitor.visit_bool(b),
            Expr::Grouping(inner) => visitor.visit_grouping(inner),
            Expr::UnOp(op) => visitor.visit_unop(op),
            Expr::BinOp(op) => visitor.visit_binop(op),
            Expr::LogicalOp(op) => visitor.visit_logical(op),
            Expr::Identifier(i) => visitor.visit_identifier(i),
            Expr::FuncCall(_f) => todo!(),
        }
    }
}

#[derive(Clone)]
pub enum Stmt {
    Print(Print),
    Println(Println),
    If(If),
    Assignment(Assignment),
    While(While),
    For(For),
    FuncDecl(FuncDecl),
}

impl Stmt {
    pub fn accept<T>(&self, visitor: &mut impl StmtVisitor<T>) -> T {
        match self {
            Stmt::Print(p) => visitor.visit_print(p),
            Stmt::Println(p) => visitor.visit_println(p),
            Stmt::If(i) => visitor.visit_if(i),
            Stmt::Assignment(a) => visitor.visit_assignment(a),
            Stmt::While(w) => visitor.visit_while(w),
            Stmt::For(f) => visitor.visit_for(f),
            Stmt::FuncDecl(d) => visitor.visit_func_decl(d),
        }
    }
}

#[derive(Clone)]
pub struct Stmts {
    stmts: Vec<Stmt>,
}

impl Stmts {
    pub fn new(stmts: Vec<Stmt>) -> Self {
        Self { stmts }
    }

    pub fn stmts(&self) -> &Vec<Stmt> {
        &self.stmts
    }
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        debug::dump_expr(self, f, None)
    }
}

impl std::fmt::Debug for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        debug::dump_stmt(self, f, None)
    }
}

impl std::fmt::Debug for Stmts {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        debug::dump_stmts(self, f, None)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IntegerLiteral {
    value: f64,
    line: usize,
}

impl TryFrom<&Token> for IntegerLiteral {
    type Error = ParseError;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        let TokenKind::IntegerLiteral { lexeme } = token.kind() else {
            return Err(ParseError::new(format!("expected integer literal, got {token:?}"), token.line()));
        };
        Ok(Self {
            value: std::str::from_utf8(lexeme.value())
                .map_err(|e| ParseError::new(format!("invalid UTF-8: {e}"), token.line()))?
                .parse()
                .map_err(|e| ParseError::new(format!("could not parse as isize: {e}"), token.line()))?,
            line: token.line(),
        })
    }
}

impl IntegerLiteral {
    pub fn new(value: f64, line: usize) -> Self {
        Self { value, line }
    }

    pub fn value(&self) -> f64 {
        self.value
    }

    pub fn line(&self) -> usize {
        self.line
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FloatLiteral {
    value: f64,
    line: usize,
}

impl TryFrom<&Token> for FloatLiteral {
    type Error = ParseError;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        let TokenKind::FloatLiteral { lexeme } = token.kind() else {
            return Err(ParseError::new(format!("expected float literal, got {token:?}"), token.line()));
        };
        Ok(Self {
            value: std::str::from_utf8(lexeme.value())
                .map_err(|e| ParseError::new(format!("invalid UTF-8 string: {e}"), token.line()))?
                .parse()
                .map_err(|e| ParseError::new(format!("could not parse as f64: {e}"), token.line()))?,
            line: token.line(),
        })
    }
}

impl FloatLiteral {
    pub fn value(&self) -> f64 {
        self.value
    }

    pub fn line(&self) -> usize {
        self.line
    }
}

#[derive(Clone, Copy, Debug)]
pub struct BoolLiteral {
    value: bool,
    line: usize,
}

impl TryFrom<&Token> for BoolLiteral {
    type Error = ParseError;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        Ok(Self {
            value: match token.kind() {
                TokenKind::True => true,
                TokenKind::False => false,
                _ => return Err(ParseError::new(format!("expected bool literal, got {token:?}"), token.line())),
            },
            line: token.line(),
        })
    }
}

impl BoolLiteral {
    pub fn new(value: bool, line: usize) -> Self {
        Self { value, line }
    }

    pub fn value(&self) -> bool {
        self.value
    }

    pub fn line(&self) -> usize {
        self.line
    }
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    value: String,
    line: usize,
}

impl TryFrom<&Token> for StringLiteral {
    type Error = ParseError;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        let TokenKind::StringLiteral { lexeme } = token.kind() else {
            return Err(ParseError::new(format!("expected string literal, got {token:?}"), token.line()));
        };
        Ok(Self {
            // remove the quotes from lexeme
            value: String::from_utf8(lexeme.value()[1..lexeme.value().len() - 1].to_vec())
                .map_err(|e| ParseError::new(format!("Invalid UTF-8 string: {e}"), token.line()))?,
            line: token.line(),
        })
    }
}

impl StringLiteral {
    pub fn new(value: String, line: usize) -> Self {
        Self { value, line }
    }

    pub fn value(&self) -> &str {
        &self.value
    }

    pub fn line(&self) -> usize {
        self.line
    }
}

#[derive(Debug, Clone)]
pub struct BinOp {
    operator: Token,
    lhs: Box<Expr>,
    rhs: Box<Expr>,
}

impl BinOp {
    pub fn new(operator: Token, lhs: Expr, rhs: Expr) -> Self {
        Self {
            operator,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn operator(&self) -> &Token {
        &self.operator
    }

    pub fn lhs(&self) -> &Expr {
        &self.lhs
    }

    pub fn rhs(&self) -> &Expr {
        &self.rhs
    }
}

#[derive(Debug, Clone)]
pub struct UnOp {
    operator: Token,
    operand: Box<Expr>,
}

impl UnOp {
    pub fn new(operator: Token, operand: Expr) -> Self {
        Self {
            operator,
            operand: Box::new(operand),
        }
    }

    pub fn operator(&self) -> &Token {
        &self.operator
    }

    pub fn operand(&self) -> &Expr {
        &self.operand
    }
}

#[derive(Debug, Clone)]
pub struct LogicalOp {
    operator: Token,
    lhs: Box<Expr>,
    rhs: Box<Expr>,
}

impl LogicalOp {
    pub fn new(operator: Token, lhs: Expr, rhs: Expr) -> Self {
        Self {
            operator,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn operator(&self) -> &Token {
        &self.operator
    }

    pub fn lhs(&self) -> &Expr {
        &self.lhs
    }

    pub fn rhs(&self) -> &Expr {
        &self.rhs
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    name: Rc<String>,
    line: usize,
}

impl Identifier {
    pub fn new(name: &Rc<String>, line: usize) -> Self {
        Self { name: name.clone(), line }
    }

    pub fn name(&self) -> &Rc<String> {
        &self.name
    }

    pub fn line(&self) -> usize {
        self.line
    }
}

/// `<assign> ::= <identifier> ':=' <expr>`
#[derive(Clone, Debug)]
pub struct Assignment {
    lhs: Expr, // TODO: add some kind of LValue type
    rhs: Expr,
}

impl Assignment {
    pub fn new(lhs: Expr, rhs: Expr) -> Self {
        Self { lhs, rhs }
    }

    pub fn lhs(&self) -> &Expr {
        &self.lhs
    }

    pub fn rhs(&self) -> &Expr {
        &self.rhs
    }
}

/// `<print> ::= 'print' <expr>`
#[derive(Clone, Debug)]
pub struct Print {
    expr: Expr,
}

impl Print {
    pub fn new(expr: Expr) -> Self {
        Self { expr }
    }

    pub fn expr(&self) -> &Expr {
        &self.expr
    }
}

/// `<println> ::= 'println' <expr>`
#[derive(Clone, Debug)]
pub struct Println {
    expr: Expr,
}

impl Println {
    pub fn new(expr: Expr) -> Self {
        Self { expr }
    }

    pub fn expr(&self) -> &Expr {
        &self.expr
    }
}

#[derive(Clone, Debug)]
pub struct Elif {
    test: Expr,
    then: Stmts,
}

impl Elif {
    pub fn new(test: Expr, then: Stmts) -> Self {
        Self { test, then }
    }

    pub fn test(&self) -> &Expr {
        &self.test
    }

    pub fn then(&self) -> &Stmts {
        &self.then
    }
}

/// `'if' <expr> 'then' <stmts> ( 'else' <stmts> )? 'end' `
#[derive(Clone, Debug)]
pub struct If {
    test: Expr,
    then: Stmts,
    elif: Vec<Elif>,
    r#else: Option<Stmts>,
}

impl If {
    pub fn new(test: Expr, then: Stmts, elif: Vec<Elif>, r#else: Option<Stmts>) -> Self {
        Self { test, then, elif, r#else }
    }

    pub fn test(&self) -> &Expr {
        &self.test
    }

    pub fn then(&self) -> &Stmts {
        &self.then
    }

    pub fn elif(&self) -> &Vec<Elif> {
        &self.elif
    }

    pub fn r#else(&self) -> &Option<Stmts> {
        &self.r#else
    }
}

/// `<while> ::= 'while' <expr> 'do' <stmts> 'end'`
#[derive(Clone, Debug)]
pub struct While {
    test: Expr,
    body: Stmts,
}

impl While {
    pub fn new(test: Expr, body: Stmts) -> Self {
        Self { test, body }
    }

    pub fn test(&self) -> &Expr {
        &self.test
    }

    pub fn body(&self) -> &Stmts {
        &self.body
    }
}

/// `<for> ::= 'for' <assignment> ',' <expr> ( ',' <expr> )? 'do' <stmts> 'end'`
#[derive(Clone, Debug)]
pub struct For {
    var: Expr,
    start: Expr,
    end: Expr,
    step: Option<Expr>,
    body: Stmts,
}

impl For {
    pub fn new(var: Expr, start: Expr, end: Expr, step: Option<Expr>, body: Stmts) -> Self {
        Self { var, start, end, step, body }
    }

    pub fn var(&self) -> &Expr {
        &self.var
    }

    pub fn start(&self) -> &Expr {
        &self.start
    }

    pub fn end(&self) -> &Expr {
        &self.end
    }

    pub fn step(&self) -> &Option<Expr> {
        &self.step
    }

    pub fn body(&self) -> &Stmts {
        &self.body
    }
}

/// `<funcdecl> ::= "func" <identifier> "(" <params>? ")" <body> "end"`
#[derive(Clone, Debug)]
pub struct FuncDecl {
    name: String,
    params: Vec<FuncParam>,
    body: Stmts,
    line: usize,
}

impl FuncDecl {
    pub fn new(name: String, params: Vec<FuncParam>, body: Stmts, line: usize) -> Self {
        Self { name, params, body, line }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn params(&self) -> &Vec<FuncParam> {
        &self.params
    }

    pub fn body(&self) -> &Stmts {
        &self.body
    }

    pub fn line(&self) -> usize {
        self.line
    }
}

/// `<params> ::= <identifier> ( "," <identifier> )*`
#[derive(Clone, Debug)]
pub struct FuncParam {
    name: String,
    line: usize,
}

impl FuncParam {
    pub fn new(name: String, line: usize) -> Self {
        Self { name, line }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn line(&self) -> usize {
        self.line
    }
}

/// `<funccall> ::= <identifier> "(" <args>? ")"
/// `<args> ::= <expr> ( "," <expr> )*`
#[derive(Clone, Debug)]
pub struct FuncCall {
    name: String,
    args: Vec<Expr>,
    line: usize,
}

impl FuncCall {
    pub fn new(name: String, args: Vec<Expr>, line: usize) -> Self {
        Self { name, args, line }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn args(&self) -> &Vec<Expr> {
        &self.args
    }

    pub fn line(&self) -> usize {
        self.line
    }
}
