use std::fmt::write;

use crate::tokens::Token;

/// Evaluates to a result
#[derive(Clone)]
pub enum Expr<'src> {
    Integer(Integer),
    Float(Float),
    Grouping(Box<Expr<'src>>),
    UnOp(UnOp<'src>),
    BinOp(BinOp<'src>),
}

fn print_ast(ast: &Expr, f: &mut std::fmt::Formatter<'_>, indentation: Option<usize>) -> std::fmt::Result {
    let indentation = indentation.unwrap_or(0);
    match ast {
        Expr::Integer(int) => writeln!(f, "{}{int:?}", " ".repeat(indentation)),
        Expr::Float(float) => writeln!(f, "{}{float:?}", " ".repeat(indentation)),
        Expr::Grouping(group) => {
            writeln!(f, "{}(", " ".repeat(indentation))?;
            print_ast(group, f, Some(indentation + 4))?;
            writeln!(f, "{})", " ".repeat(indentation))
        }
        Expr::UnOp(unop) => {
            writeln!(f, "{}UnOp {{", " ".repeat(indentation))?;
            writeln!(f, "{}{:?}", " ".repeat(indentation + 4), unop.operator)?;
            print_ast(&unop.operand, f, Some(indentation + 4))?;
            writeln!(f, "{}}} // unop", " ".repeat(indentation))
        }
        Expr::BinOp(binop) => {
            writeln!(f, "{}BinOp {{", " ".repeat(indentation))?;
            print_ast(&binop.lhs, f, Some(indentation + 4))?;
            writeln!(f, "{}{:?}", " ".repeat(indentation + 4), binop.operator)?;
            print_ast(&binop.rhs, f, Some(indentation + 4))?;
            // writeln!(f, "{}{:?}{:?}{:?}", " ".repeat(indentation + 4), binop.lhs, binop.operator, binop.rhs)?;
            writeln!(f, "{}}} // binop", " ".repeat(indentation))
        }
    }
}

impl<'src> std::fmt::Debug for Expr<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        print_ast(self, f, None)
    }
}

/// Performs an action
pub trait Stmt {}

#[derive(Debug, Clone, Copy)]
pub struct Integer {
    value: isize,
}

impl TryFrom<&[u8]> for Integer {
    type Error = String;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        Ok(Self {
            value: std::str::from_utf8(value)
                .map_err(|e| format!("Invalid UTF-8 string: {e}"))?
                .parse()
                .map_err(|e| format!("Could not parse as isize: {e}"))?,
        })
    }
}

impl Integer {
    pub fn new(value: isize) -> Self {
        Self { value }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Float {
    value: f64,
}

impl Float {
    pub fn new(value: f64) -> Self {
        Self { value }
    }
}

impl TryFrom<&[u8]> for Float {
    type Error = String;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        Ok(Self {
            value: std::str::from_utf8(value)
                .map_err(|e| format!("Invalid UTF-8 string: {e}"))?
                .parse()
                .map_err(|e| format!("Could not parse as isize: {e}"))?,
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
