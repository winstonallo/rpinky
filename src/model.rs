use crate::tokens::Token;

/// Evaluates to a result
pub trait Expr {}

/// Performs an action
pub trait Stmt {}

#[derive(Debug)]
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

impl Expr for Integer {}

impl Integer {
    pub fn new(value: isize) -> Self {
        Self { value }
    }
}

#[derive(Debug)]
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

impl Expr for Float {}

#[derive(Debug)]
pub struct BinOp<'src, T: Expr, U: Expr> {
    operator: Token<'src>,
    lhs: T,
    rhs: U,
}

impl<'src, T: Expr, U: Expr> BinOp<'src, T, U> {
    pub fn new(operator: Token<'src>, lhs: T, rhs: U) -> Self {
        Self { operator, lhs, rhs }
    }
}

impl<'src, T: Expr, U: Expr> Expr for BinOp<'src, T, U> {}

#[derive(Debug)]
pub struct UnOp<'src, T: Expr> {
    operator: Token<'src>,
    operand: T,
}

impl<'src, T: Expr> UnOp<'src, T> {
    pub fn new(operator: Token<'src>, operand: T) -> Self {
        Self { operator, operand }
    }
}

impl<'src, T: Expr> Expr for UnOp<'src, T> {}

pub struct WhileStmt {}

impl Stmt for WhileStmt {}

pub struct Assignment {}

impl Stmt for Assignment {}

/// '(' <expr> ')'
pub struct Grouping<T: Expr> {
    value: T,
}

impl<T: Expr> Expr for Grouping<T> {}

impl<T: Expr> Grouping<T> {
    pub fn new(value: T) -> Self {
        Self { value }
    }
}
