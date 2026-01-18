use crate::{
    errors::RuntimeError,
    model::{BinOp, Bool, Expr, Float, If, Integer, LogicalOp, Print, Println, Stmts, StringType, UnOp},
    tokens::TokenKind,
    visitor::{ExprVisitor, StmtVisitor},
};

#[derive(Debug, PartialEq, PartialOrd)]
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
        if matches!(self, Type::String { .. }) || matches!(rhs, Type::String { .. }) {
            return Err(RuntimeError::new("exponentiation is not implemented for string".into(), self.line()));
        }
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

    // Need to implement `cmp` like this because `std::cmd::Ordering` does not support returning a `Result`.
    #[allow(clippy::should_implement_trait)]
    pub fn cmp(&self, rhs: &Self) -> Result<std::cmp::Ordering, RuntimeError> {
        match (self, rhs) {
            (Type::Bool { value: lhs, .. }, Type::Bool { value: rhs, .. }) => Ok(lhs.cmp(rhs)),
            (Type::Number { value: lhs, line }, Type::Number { value: rhs, .. }) => match lhs.partial_cmp(rhs) {
                Some(ordering) => Ok(ordering),
                // one of the values is NaN
                None => Err(RuntimeError::new(format!("comparison not supported between {lhs} and {rhs}"), *line)),
            },
            (Type::String { value: lhs, .. }, Type::String { value: rhs, .. }) => Ok(lhs.cmp(rhs)),
            (Type::Bool { value: lhs, line }, Type::Number { value: rhs, .. }) => match (*lhs as u8 as f64).partial_cmp(rhs) {
                Some(ordering) => Ok(ordering),
                // rhs is NaN
                None => Err(RuntimeError::new(format!("comparison not supported between {lhs} and {rhs}"), *line)),
            },
            (Type::Number { value: lhs, line }, Type::Bool { value: rhs, .. }) => match lhs.partial_cmp(&(*rhs as u8 as f64)) {
                Some(ordering) => Ok(ordering),
                // lhs is NaN
                None => Err(RuntimeError::new(format!("comparison not supported between {lhs} and {rhs}"), *line)),
            },
            (lhs, rhs) => Err(RuntimeError::new(format!("comparison not supported between {lhs} and {rhs}"), lhs.line())),
        }
    }

    pub fn gt(&self, rhs: &Self) -> Result<Type, RuntimeError> {
        Ok(Type::Bool {
            value: self.cmp(rhs)?.is_gt(),
            line: self.line(),
        })
    }

    pub fn ge(&self, rhs: &Self) -> Result<Type, RuntimeError> {
        Ok(Type::Bool {
            value: self.cmp(rhs)?.is_ge(),
            line: self.line(),
        })
    }

    pub fn lt(&self, rhs: &Self) -> Result<Type, RuntimeError> {
        Ok(Type::Bool {
            value: self.cmp(rhs)?.is_lt(),
            line: self.line(),
        })
    }

    pub fn le(&self, rhs: &Self) -> Result<Type, RuntimeError> {
        Ok(Type::Bool {
            value: self.cmp(rhs)?.is_le(),
            line: self.line(),
        })
    }

    // Need to implement `eq` like this because `std::cmd::Eq` does not support returning a `Result`.
    pub fn eq(&self, rhs: &Self) -> Result<Type, RuntimeError> {
        match (self, rhs) {
            (Type::Bool { value: lhs, line }, Type::Bool { value: rhs, .. }) => Ok(Type::Bool {
                value: lhs.eq(rhs),
                line: *line,
            }),
            (Type::Number { value: lhs, line }, Type::Number { value: rhs, .. }) => Ok(Type::Bool {
                value: lhs.eq(rhs),
                line: *line,
            }),
            (Type::String { value: lhs, line }, Type::String { value: rhs, .. }) => Ok(Type::Bool {
                value: lhs.eq(rhs),
                line: *line,
            }),
            (Type::Bool { value: lhs, line }, Type::Number { value: rhs, .. }) => Ok(Type::Bool {
                value: (*lhs as u8 as f64).eq(rhs),
                line: *line,
            }),
            (Type::Number { value: lhs, line }, Type::Bool { value: rhs, .. }) => Ok(Type::Bool {
                value: lhs.eq(&(*rhs as u8 as f64)),
                line: *line,
            }),
            (lhs, rhs) => Err(RuntimeError::new(format!("equality not supported between {rhs} and {lhs}"), lhs.line())),
        }
    }
}

impl From<&Type> for bool {
    fn from(value: &Type) -> Self {
        match value {
            Type::Bool { value, .. } => *value,
            Type::Number { value, .. } => *value != 0f64,
            Type::String { value, .. } => !value.is_empty(),
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

impl ::std::ops::Div for Type {
    type Output = Result<Self, RuntimeError>;

    fn div(self, rhs: Self) -> Self::Output {
        let rhs = match rhs {
            Self::Number { value, .. } => value,
            Self::Bool { value, .. } => value as u8 as f64,
            Self::String { line, .. } => return Err(RuntimeError::new("division is not implemented for string".into(), line)),
        };
        if rhs == 0f64 {
            return Err(RuntimeError::new("division by zero".into(), self.line()));
        }
        let lhs = match self {
            Self::Number { value, .. } => value,
            Self::Bool { value, .. } => value as u8 as f64,
            Self::String { line, .. } => return Err(RuntimeError::new("division is not implemented for string".into(), line)),
        };
        Ok(Type::Number {
            value: lhs / rhs,
            line: self.line(),
        })
    }
}

impl ::std::ops::Rem for Type {
    type Output = Result<Self, RuntimeError>;

    fn rem(self, rhs: Self) -> Self::Output {
        let rhs = match rhs {
            Self::Number { value, .. } => value,
            Self::Bool { value, .. } => value as u8 as f64,
            Self::String { line, .. } => return Err(RuntimeError::new("modulo is not implemented for string".into(), line)),
        };
        if rhs == 0f64 {
            return Err(RuntimeError::new("modulo by zero".into(), self.line()));
        }
        let lhs = match self {
            Self::Number { value, .. } => value,
            Self::Bool { value, .. } => value as u8 as f64,
            Self::String { line, .. } => return Err(RuntimeError::new("modulo is not implemented for string".into(), line)),
        };
        Ok(Type::Number {
            value: lhs % rhs,
            line: self.line(),
        })
    }
}

impl std::ops::Neg for Type {
    type Output = Result<Self, RuntimeError>;

    fn neg(self) -> Self::Output {
        match self {
            Type::Number { value, line } => Ok(Type::Number { value: -value, line }),
            Type::Bool { line, .. } => Err(RuntimeError::new("bad operand type for unary -: bool".into(), line)),
            Type::String { line, .. } => Err(RuntimeError::new("bad operand type for unary -: string".into(), line)),
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

/// Visitor for evaluating AST.
#[derive(Default)]
pub struct Interpreter;

impl Interpreter {
    pub fn new() -> Self {
        Self
    }

    pub fn interpret(&mut self, stmts: &Stmts) -> Result<(), RuntimeError> {
        for stmt in stmts.stmts() {
            stmt.accept(self)?;
        }
        Ok(())
    }
}

impl ExprVisitor<Result<Type, RuntimeError>> for Interpreter {
    fn visit_integer(&mut self, n: &Integer) -> Result<Type, RuntimeError> {
        Ok(Type::Number {
            value: n.value(),
            line: n.line(),
        })
    }

    fn visit_float(&mut self, f: &Float) -> Result<Type, RuntimeError> {
        Ok(Type::Number {
            value: f.value(),
            line: f.line(),
        })
    }

    fn visit_string(&mut self, s: &StringType) -> Result<Type, RuntimeError> {
        Ok(Type::String {
            value: s.value().into(),
            line: s.line(),
        })
    }

    fn visit_bool(&mut self, b: &Bool) -> Result<Type, RuntimeError> {
        Ok(Type::Bool {
            value: b.value(),
            line: b.line(),
        })
    }

    fn visit_grouping(&mut self, inner: &Expr) -> Result<Type, RuntimeError> {
        inner.accept(self)
    }

    fn visit_unop(&mut self, op: &UnOp) -> Result<Type, RuntimeError> {
        let operand = op.operand().accept(self)?;
        match op.operator().kind() {
            TokenKind::Plus => match operand {
                Type::String { line, .. } => Err(RuntimeError::new("bad operand for unary +: 'string'".into(), line)),
                _ => Ok(operand),
            },
            TokenKind::Minus => -operand,
            TokenKind::Not => Ok(!operand),
            _ => panic!("unsupported unary operation {op:?}"),
        }
    }

    fn visit_binop(&mut self, op: &BinOp) -> Result<Type, RuntimeError> {
        let lhs = op.lhs().accept(self)?;
        let rhs = op.rhs().accept(self)?;
        match op.operator().kind() {
            TokenKind::Plus => lhs + rhs,
            TokenKind::Minus => lhs - rhs,
            TokenKind::Star => lhs * rhs,
            TokenKind::Slash => lhs / rhs,
            TokenKind::Caret => lhs.pow(rhs),
            TokenKind::Mod => lhs % rhs,
            TokenKind::Greater => lhs.gt(&rhs),
            TokenKind::GreaterEqual => lhs.ge(&rhs),
            TokenKind::Less => lhs.lt(&rhs),
            TokenKind::LessEqual => lhs.le(&rhs),
            TokenKind::EqualEqual => lhs.eq(&rhs),
            TokenKind::NotEqual => Ok(!lhs.eq(&rhs)?),
            _ => panic!("unsupported binary operation {op:?}"),
        }
    }

    fn visit_logical(&mut self, op: &LogicalOp) -> Result<Type, RuntimeError> {
        // First interpret and check left-hand side to allow for short-circuiting
        let lhs = op.lhs().accept(self)?;
        match op.operator().kind() {
            TokenKind::And => {
                if !bool::from(&lhs) {
                    return Ok(Type::Bool {
                        value: false,
                        line: lhs.line(),
                    });
                }
                let rhs = op.rhs().accept(self)?;
                Ok(Type::Bool {
                    value: bool::from(&rhs),
                    line: lhs.line(),
                })
            }
            TokenKind::Or => {
                if bool::from(&lhs) {
                    return Ok(Type::Bool { value: true, line: lhs.line() });
                }
                let rhs = op.rhs().accept(self)?;
                Ok(Type::Bool {
                    value: bool::from(&rhs),
                    line: lhs.line(),
                })
            }
            _ => panic!("unsupported logical operation {op:?}"),
        }
    }
}

impl StmtVisitor<Result<(), RuntimeError>> for Interpreter {
    fn visit_print(&mut self, p: &Print) -> Result<(), RuntimeError> {
        let value = p.expr().accept(self)?;
        print!("{value}");
        Ok(())
    }

    fn visit_println(&mut self, p: &Println) -> Result<(), RuntimeError> {
        let value = p.expr().accept(self)?;
        println!("{value}");
        Ok(())
    }

    fn visit_if(&mut self, i: &If) -> Result<(), RuntimeError> {
        let test = i.test().accept(self)?;
        let Type::Bool { value, .. } = test else {
            return Err(RuntimeError::new("if conditition is not a boolean expression".into(), test.line()));
        };
        if value {
            self.interpret(i.then())?;
        } else if let Some(r#else) = i.r#else() {
            self.interpret(r#else)?;
        }
        Ok(())
    }
}

/// Evaluate a single expression.
pub fn expr(ast: &Expr) -> Result<Type, RuntimeError> {
    let mut interpreter = Interpreter::new();
    ast.accept(&mut interpreter)
}

/// Interpret a list of statements.
pub fn interpret(stmts: &Stmts) -> Result<(), RuntimeError> {
    let mut interpreter = Interpreter::new();
    interpreter.interpret(stmts)
}
