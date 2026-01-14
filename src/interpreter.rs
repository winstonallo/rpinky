use crate::{errors::RuntimeError, model::Expr, tokens::TokenKind};

#[derive(Debug)]
pub enum Type {
    Number { value: f64, line: usize },
    Bool { value: bool, line: usize },
    String { value: String, line: usize },
}

impl TryFrom<&Type> for f64 {
    type Error = RuntimeError;

    fn try_from(value: &Type) -> Result<Self, Self::Error> {
        match value {
            Type::Number { value, .. } => Ok(*value),
            Type::Bool { value, .. } => Ok(*value as u8 as f64),
            Type::String { value, line } => Err(RuntimeError::new(format!("cannot convert string to float: {value}"), *line)),
        }
    }
}

impl Type {
    pub fn pow(&self, rhs: Type) -> Result<Type, RuntimeError> {
        let lhs = f64::try_from(self)?;
        let rhs = f64::try_from(&rhs)?;
        Ok(Type::Number {
            value: lhs.powf(rhs),
            line: self.line(),
        })
    }

    pub fn line(&self) -> usize {
        match self {
            Type::Bool { line, .. } | Type::Number { line, .. } | Type::String { line, .. } => *line,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Number { value, .. } => write!(f, "{value}"),
            Type::Bool { value, .. } => write!(f, "{value}"),
            Type::String { value, .. } => write!(f, "{value}"),
        }
    }
}

impl std::ops::Add for Type {
    type Output = Result<Self, RuntimeError>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Type::String { value: lhs, line }, rhs) => Ok(Type::String {
                value: format!("{lhs}{rhs}"),
                line,
            }),
            (lhs, Type::String { value: rhs, line }) => Ok(Type::String {
                value: format!("{lhs}{rhs}"),
                line,
            }),
            (Type::Number { value: lhs, line }, Type::Number { value: rhs, .. }) => Ok(Type::Number { value: lhs + rhs, line }),
            (Type::Bool { value: lhs, line }, Type::Bool { value: rhs, .. }) => Ok(Type::Number {
                value: lhs as u8 as f64 + rhs as u8 as f64,
                line,
            }),
            (Type::Number { value: lhs, line }, Type::Bool { value: rhs, .. }) => Ok(Type::Number {
                value: lhs + rhs as u8 as f64,
                line,
            }),
            (Type::Bool { value: lhs, line }, Type::Number { value: rhs, .. }) => Ok(Type::Number {
                value: lhs as u8 as f64 + rhs,
                line,
            }),
        }
    }
}

macro_rules! impl_numeric_op {
    ($trait:ident, $method:ident, $op:tt, $name:literal, $ty:ty) => {
        impl ::std::ops::$trait for $ty {
            type Output = Result<Self, RuntimeError>;

            fn $method(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Type::Number{value: lhs, line}, Type::Number{value: rhs, ..}) => Ok(Type::Number{value: lhs $op rhs, line}),
                    (Type::Bool{value: lhs, line}, Type::Bool{value: rhs, ..}) => Ok(Type::Number{value: (lhs as u8 as f64) $op (rhs as u8 as f64), line}),
                    (Type::Number{value: lhs, line}, Type::Bool{value: rhs, ..}) => Ok(Type::Number{value: lhs $op (rhs as u8 as f64), line}),
                    (Type::Bool{value: lhs, line}, Type::Number{value: rhs, ..}) => Ok(Type::Number{value: (lhs as u8 as f64) $op rhs, line} ),
                    (Type::String{line, ..}, _) | (_, Type::String{line, ..}) => Err(RuntimeError::new(concat!($name, " is not implemented for string").into(), line)),
                }
            }
        }
    };
}

impl_numeric_op!(Sub, sub, -, "subtraction", Type);
impl_numeric_op!(Mul, mul, *, "multiplication", Type);
impl_numeric_op!(Div, div, /, "division", Type);

impl std::ops::Neg for Type {
    type Output = Result<Self, RuntimeError>;

    fn neg(self) -> Self::Output {
        match self {
            Type::Bool { value, line } => Ok(Type::Bool { value: !value, line }),
            Type::Number { value, line } => Ok(Type::Number { value: -value, line }),
            Type::String { value, line } => Err(RuntimeError::new("bad operand type for unary -: 'string'".into(), line)),
        }
    }
}

impl std::ops::Not for Type {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Type::Bool { value, line } => Type::Bool { value: !value, line },
            Type::Number { value, line } => Type::Bool { value: value == 0f64, line },
            Type::String { value, line } => Type::Bool { value: value.is_empty(), line },
        }
    }
}

pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn interpret<'src>(&mut self, ast: &Expr<'src>) -> Result<Type, RuntimeError> {
        match ast {
            Expr::Integer(i) => Ok(Type::Number {
                value: i.value(),
                line: i.line(),
            }),
            Expr::Float(f) => Ok(Type::Number {
                value: f.value(),
                line: f.line(),
            }),
            Expr::String(s) => Ok(Type::String {
                value: s.value().into(),
                line: s.line(),
            }),
            Expr::Bool(b) => Ok(Type::Bool {
                value: b.value(),
                line: b.line(),
            }),
            Expr::Grouping(expr) => self.interpret(expr),
            Expr::BinOp(binop) => {
                let lhs = self.interpret(binop.lhs())?;
                let rhs = self.interpret(binop.rhs())?;
                match binop.operator().kind() {
                    TokenKind::Plus => lhs + rhs,
                    TokenKind::Minus => lhs - rhs,
                    TokenKind::Star => lhs * rhs,
                    TokenKind::Slash => lhs / rhs,
                    TokenKind::Caret => lhs.pow(rhs),
                    _ => panic!("unsupported binary operation {binop:?}"),
                }
            }
            Expr::UnOp(unop) => {
                let operand = self.interpret(unop.operand())?;
                match unop.operator().kind() {
                    TokenKind::Plus => match operand {
                        Type::String { value, line } => Err(RuntimeError::new("bad operand for unary +: 'string'".into(), line)),
                        _ => Ok(operand),
                    },
                    TokenKind::Minus => -operand,
                    TokenKind::Not => Ok(!operand),
                    _ => panic!("unsupported unary operation {unop:?}"),
                }
            }
        }
    }
}
