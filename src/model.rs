use crate::{
    errors::ParseError,
    tokens::{Token, TokenKind},
};

#[derive(Clone)]
pub enum Node {
    Expr(Expr),
    Stmts(Stmts),
}

/// Evaluates to a result
#[derive(Clone)]
pub enum Expr {
    Integer(Integer),
    Float(Float),
    Grouping(Box<Expr>),
    UnOp(UnOp),
    BinOp(BinOp),
    String(StringType),
    Bool(Bool),
    LogicalOp(LogicalOp),
}

#[derive(Clone)]
pub enum Stmt {
    Print(Print),
    Println(Println),
    If(If),
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

fn dump_expr(expr: &Expr, f: &mut std::fmt::Formatter<'_>, indentation: Option<usize>) -> std::fmt::Result {
    let indentation = indentation.unwrap_or(0);

    match expr {
        Expr::Integer(int) => writeln!(f, "{}{int:?}", " ".repeat(indentation)),
        Expr::Float(float) => writeln!(f, "{}{float:?}", " ".repeat(indentation)),
        Expr::Bool(bool) => writeln!(f, "{}{bool:?}", " ".repeat(indentation)),
        Expr::String(str) => writeln!(f, "{}{str:?}", " ".repeat(indentation)),
        Expr::Grouping(group) => {
            writeln!(f, "{}Grouping (", " ".repeat(indentation))?;
            dump_expr(group, f, Some(indentation + 4))?;
            writeln!(f, "{})", " ".repeat(indentation))
        }
        Expr::UnOp(unop) => {
            writeln!(f, "{}UnOp {{", " ".repeat(indentation))?;
            writeln!(f, "{}{:?}", " ".repeat(indentation + 4), unop.operator)?;
            dump_expr(&unop.operand, f, Some(indentation + 4))?;
            writeln!(f, "{}}}", " ".repeat(indentation))
        }
        Expr::BinOp(binop) => {
            writeln!(f, "{}BinOp {{", " ".repeat(indentation))?;
            dump_expr(&binop.lhs, f, Some(indentation + 4))?;
            writeln!(f, "{}{:?}", " ".repeat(indentation + 4), binop.operator)?;
            dump_expr(&binop.rhs, f, Some(indentation + 4))?;
            writeln!(f, "{}}}", " ".repeat(indentation))
        }
        Expr::LogicalOp(logicalop) => {
            writeln!(f, "{}LogicalOp {{", " ".repeat(indentation))?;
            dump_expr(&logicalop.lhs, f, Some(indentation + 4))?;
            writeln!(f, "{}{:?}", " ".repeat(indentation + 4), logicalop.operator)?;
            dump_expr(&logicalop.rhs, f, Some(indentation + 4))?;
            writeln!(f, "{}}}", " ".repeat(indentation))
        }
    }
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        dump_expr(self, f, None)
    }
}

fn dump_stmt(stmt: &Stmt, f: &mut std::fmt::Formatter<'_>, indentation: Option<usize>) -> std::fmt::Result {
    let indentation = indentation.unwrap_or(0);

    match stmt {
        Stmt::If(stmt) => {
            writeln!(f, "{}if", " ".repeat(indentation))?;
            dump_expr(&stmt.test, f, Some(indentation + 4))?;
            writeln!(f, "{}then", " ".repeat(indentation))?;
            dump_stmts(stmt.then(), f, Some(indentation + 4))?;
            if let Some(elsee) = stmt.r#else() {
                writeln!(f, "{}else", " ".repeat(indentation))?;
                dump_stmts(elsee, f, Some(indentation + 4))?;
            }
            writeln!(f, "end")
        }
        Stmt::Print(stmt) => {
            writeln!(f, "{}print", " ".repeat(indentation))?;
            dump_expr(stmt.expr(), f, Some(indentation + 4))
        }
        Stmt::Println(stmt) => {
            writeln!(f, "{}println", " ".repeat(indentation))?;
            dump_expr(stmt.expr(), f, Some(indentation + 4))
        }
    }
}

impl std::fmt::Debug for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        dump_stmt(self, f, None)
    }
}

fn dump_stmts(stmts: &Stmts, f: &mut std::fmt::Formatter<'_>, indentation: Option<usize>) -> std::fmt::Result {
    let indentation = indentation.unwrap_or(0);
    for stmt in &stmts.stmts {
        dump_stmt(stmt, f, Some(indentation))?;
    }
    Ok(())
}

impl std::fmt::Debug for Stmts {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        dump_stmts(self, f, None)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Integer {
    value: f64,
    line: usize,
}

impl TryFrom<&Token> for Integer {
    type Error = ParseError;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        let TokenKind::IntegerLiteral { lexeme } = token.kind() else {
            return Err(ParseError::new(format!("expected integer literal, got {token:?}"), token.line()));
        };
        Ok(Self {
            value: std::str::from_utf8(lexeme.value())
                .map_err(|e| ParseError::new(format!("Invalid UTF-8: {e}"), token.line()))?
                .parse()
                .map_err(|e| ParseError::new(format!("Could not parse as isize: {e}"), token.line()))?,
            line: token.line(),
        })
    }
}

impl Integer {
    pub fn value(&self) -> f64 {
        self.value
    }

    pub fn line(&self) -> usize {
        self.line
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Float {
    value: f64,
    line: usize,
}

impl TryFrom<&Token> for Float {
    type Error = ParseError;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        let TokenKind::FloatLiteral { lexeme } = token.kind() else {
            return Err(ParseError::new(format!("expected float literal, got {token:?}"), token.line()));
        };
        Ok(Self {
            value: std::str::from_utf8(lexeme.value())
                .map_err(|e| ParseError::new(format!("Invalid UTF-8 string: {e}"), token.line()))?
                .parse()
                .map_err(|e| ParseError::new(format!("Could not parse as isize: {e}"), token.line()))?,
            line: token.line(),
        })
    }
}

impl Float {
    pub fn value(&self) -> f64 {
        self.value
    }

    pub fn line(&self) -> usize {
        self.line
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Bool {
    value: bool,
    line: usize,
}

impl TryFrom<&Token> for Bool {
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

impl Bool {
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
pub struct StringType {
    value: String,
    line: usize,
}

impl<'src> TryFrom<&Token> for StringType {
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

impl StringType {
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

#[derive(Clone, Debug)]
pub struct While {}

#[derive(Clone, Debug)]
pub struct Assignment {}

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

/// `'if' <expr> 'then' <stmts> ( 'else' <stmts> )? 'end' `
#[derive(Clone, Debug)]
pub struct If {
    test: Expr,
    then: Stmts,
    r#else: Option<Stmts>,
}

impl If {
    pub fn new(test: Expr, then: Stmts, r#else: Option<Stmts>) -> Self {
        Self { test, then, r#else }
    }

    pub fn test(&self) -> &Expr {
        &self.test
    }

    pub fn then(&self) -> &Stmts {
        &self.then
    }

    pub fn r#else(&self) -> &Option<Stmts> {
        &self.r#else
    }
}
