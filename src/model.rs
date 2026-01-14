use crate::{
    errors::ParseError,
    tokens::{Token, TokenKind},
};

/// Evaluates to a result
#[derive(Clone)]
pub enum Expr<'src> {
    Integer(Integer),
    Float(Float),
    Grouping(Box<Expr<'src>>),
    UnOp(UnOp<'src>),
    BinOp(BinOp<'src>),
}

fn dump_ast(ast: &Expr, f: &mut std::fmt::Formatter<'_>, indentation: Option<usize>) -> std::fmt::Result {
    let indentation = indentation.unwrap_or(0);
    match ast {
        Expr::Integer(int) => writeln!(f, "{}{int:?}", " ".repeat(indentation)),
        Expr::Float(float) => writeln!(f, "{}{float:?}", " ".repeat(indentation)),
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
    }
}

impl<'src> std::fmt::Debug for Expr<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        dump_ast(self, f, None)
    }
}

/// Performs an action
pub trait Stmt {}

#[derive(Debug, Clone, Copy)]
pub struct Integer {
    value: isize,
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
}

pub struct WhileStmt {}

impl Stmt for WhileStmt {}

pub struct Assignment {}

impl Stmt for Assignment {}
