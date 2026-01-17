use crate::{
    errors::ParseError,
    tokens::{Token, TokenKind},
};

#[derive(Clone)]
pub enum Node<'src> {
    Expr(Expr<'src>),
    Stmt(Stmt<'src>),
}

/// Evaluates to a result
#[derive(Clone)]
pub enum Expr<'src> {
    Integer(Integer),
    Float(Float),
    Grouping(Box<Expr<'src>>),
    UnOp(UnOp<'src>),
    BinOp(BinOp<'src>),
    String(StringType),
    Bool(Bool),
    LogicalOp(LogicalOp<'src>),
}

#[derive(Clone, Debug)]
pub enum Stmt<'src> {
    Print(Print<'src>),
    Println(Println<'src>),
}

#[derive(Debug)]
pub struct Stmts<'src> {
    stmts: Vec<Stmt<'src>>,
}

impl<'src> Stmts<'src> {
    pub fn new(stmts: Vec<Stmt<'src>>) -> Self {
        Self { stmts }
    }

    pub fn stmts(&self) -> &Vec<Stmt<'src>> {
        &self.stmts
    }
}

fn dump_ast(ast: &Expr, f: &mut std::fmt::Formatter<'_>, indentation: Option<usize>) -> std::fmt::Result {
    let indentation = indentation.unwrap_or(0);
    match ast {
        Expr::Integer(int) => writeln!(f, "{}{int:?}", " ".repeat(indentation)),
        Expr::Float(float) => writeln!(f, "{}{float:?}", " ".repeat(indentation)),
        Expr::Bool(bool) => writeln!(f, "{}{bool:?}", " ".repeat(indentation)),
        Expr::String(str) => writeln!(f, "{}{str:?}", " ".repeat(indentation)),
        Expr::Grouping(group) => {
            writeln!(f, "{}Grouping (", " ".repeat(indentation))?;
            dump_ast(group, f, Some(indentation + 4))?;
            writeln!(f, "{}) // grouping", " ".repeat(indentation))
        }
        Expr::UnOp(unop) => {
            writeln!(f, "{}UnOp {{", " ".repeat(indentation))?;
            writeln!(f, "{}{:?}", " ".repeat(indentation + 4), unop.operator)?;
            dump_ast(&unop.operand, f, Some(indentation + 4))?;
            writeln!(f, "{}}} // unop", " ".repeat(indentation))
        }
        Expr::BinOp(binop) => {
            writeln!(f, "{}BinOp {{", " ".repeat(indentation))?;
            dump_ast(&binop.lhs, f, Some(indentation + 4))?;
            writeln!(f, "{}{:?}", " ".repeat(indentation + 4), binop.operator)?;
            dump_ast(&binop.rhs, f, Some(indentation + 4))?;
            writeln!(f, "{}}} // binop", " ".repeat(indentation))
        }
        Expr::LogicalOp(logicalop) => {
            writeln!(f, "{}LogicalOp {{", " ".repeat(indentation))?;
            dump_ast(&logicalop.lhs, f, Some(indentation + 4))?;
            writeln!(f, "{}{:?}", " ".repeat(indentation + 4), logicalop.operator)?;
            dump_ast(&logicalop.rhs, f, Some(indentation + 4))?;
            writeln!(f, "{}}} // logicalop", " ".repeat(indentation))
        }
    }
}

impl<'src> std::fmt::Debug for Expr<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        dump_ast(self, f, None)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Integer {
    value: f64,
    line: usize,
}

impl TryFrom<&Token<'_>> for Integer {
    type Error = ParseError;

    fn try_from(token: &Token<'_>) -> Result<Self, Self::Error> {
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

impl TryFrom<&Token<'_>> for Float {
    type Error = ParseError;

    fn try_from(token: &Token<'_>) -> Result<Self, Self::Error> {
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

impl<'src> TryFrom<&Token<'src>> for Bool {
    type Error = ParseError;

    fn try_from(token: &Token<'src>) -> Result<Self, Self::Error> {
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

impl<'src> TryFrom<&Token<'src>> for StringType {
    type Error = ParseError;

    fn try_from(token: &Token<'src>) -> Result<Self, Self::Error> {
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
pub struct BinOp<'src> {
    operator: Token<'src>,
    lhs: Box<Expr<'src>>,
    rhs: Box<Expr<'src>>,
}

impl<'src> BinOp<'src> {
    pub fn new(operator: Token<'src>, lhs: Expr<'src>, rhs: Expr<'src>) -> Self {
        Self {
            operator,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn operator(&self) -> Token<'src> {
        self.operator
    }

    pub fn lhs(&self) -> &Expr<'src> {
        &self.lhs
    }

    pub fn rhs(&self) -> &Expr<'src> {
        &self.rhs
    }
}

#[derive(Debug, Clone)]
pub struct UnOp<'src> {
    operator: Token<'src>,
    operand: Box<Expr<'src>>,
}

impl<'src> UnOp<'src> {
    pub fn new(operator: Token<'src>, operand: Expr<'src>) -> Self {
        Self {
            operator,
            operand: Box::new(operand),
        }
    }

    pub fn operator(&self) -> Token<'src> {
        self.operator
    }

    pub fn operand(&self) -> &Expr<'src> {
        &self.operand
    }
}

#[derive(Debug, Clone)]
pub struct LogicalOp<'src> {
    operator: Token<'src>,
    lhs: Box<Expr<'src>>,
    rhs: Box<Expr<'src>>,
}

impl<'src> LogicalOp<'src> {
    pub fn new(operator: Token<'src>, lhs: Expr<'src>, rhs: Expr<'src>) -> Self {
        Self {
            operator,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn operator(&self) -> Token<'src> {
        self.operator
    }

    pub fn lhs(&self) -> &Expr<'src> {
        &self.lhs
    }

    pub fn rhs(&self) -> &Expr<'src> {
        &self.rhs
    }
}

#[derive(Clone, Debug)]
pub struct While {}

#[derive(Clone, Debug)]
pub struct Assignment {}

#[derive(Clone, Debug)]
pub struct Print<'src> {
    expr: Expr<'src>,
}

impl<'src> Print<'src> {
    pub fn new(expr: Expr<'src>) -> Self {
        Self { expr }
    }

    pub fn expr(&self) -> &Expr<'src> {
        &self.expr
    }
}

#[derive(Clone, Debug)]
pub struct Println<'src> {
    expr: Expr<'src>,
}

impl<'src> Println<'src> {
    pub fn new(expr: Expr<'src>) -> Self {
        Self { expr }
    }

    pub fn expr(&self) -> &Expr<'src> {
        &self.expr
    }
}
