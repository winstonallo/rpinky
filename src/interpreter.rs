use std::{
    cell::RefCell,
    io::Write,
    ops::{ControlFlow, FromResidual, Residual},
    rc::Rc,
};

use crate::{
    errors::RuntimeError,
    model::{self, Expr},
    span::Span,
    state::{Environment, Function},
    tokens::TokenKind,
    visitor::{ExprVisitor, StmtVisitor},
};

macro_rules! expr {
    ($expr:expr) => {
        Eval(Ok(Outcome::Done($expr)))
    };
}

macro_rules! ret {
    ($expr:expr) => {
        Eval(Ok(Outcome::Return($expr)))
    };
}

macro_rules! err {
    ($expr:expr) => {
        Eval(Err($expr))
    };
}

/// Lift an operator result (message-only error) into an `Eval`, attaching `span`.
fn lift(result: Result<Type, String>, span: Span) -> Eval {
    match result {
        Ok(value) => Eval(Ok(Outcome::Done(value))),
        Err(message) => Eval(Err(RuntimeError::new(message, span))),
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Type {
    Number(f64),
    Bool(bool),
    String(String),
    None,
}

impl TryFrom<&Type> for f64 {
    type Error = String;

    fn try_from(value: &Type) -> Result<Self, Self::Error> {
        match value {
            Type::Number(value) => Ok(*value),
            Type::Bool(value) => Ok(*value as u8 as f64),
            Type::String(value) => Err(format!("cannot convert string to float: {value}")),
            Type::None => Err("cannot convert None to float".into()),
        }
    }
}

impl Type {
    pub fn pow(self, rhs: Type) -> Result<Type, String> {
        if matches!(self, Type::String(_)) || matches!(rhs, Type::String(_)) {
            return Err("exponentiation is not implemented for string".into());
        }
        let base = f64::try_from(&self)?;
        let exp = f64::try_from(&rhs)?;
        Ok(Type::Number(base.powf(exp)))
    }

    // Can't implement `std::cmp::Ord` because comparison here can fail (e.g. NaN).
    #[allow(clippy::should_implement_trait)]
    pub fn cmp(&self, rhs: &Self) -> Result<std::cmp::Ordering, String> {
        match (self, rhs) {
            (Type::Bool(lhs), Type::Bool(rhs)) => Ok(lhs.cmp(rhs)),
            (Type::Number(lhs), Type::Number(rhs)) => lhs.partial_cmp(rhs).ok_or_else(|| format!("comparison not supported between {lhs} and {rhs}")),
            (Type::String(lhs), Type::String(rhs)) => Ok(lhs.cmp(rhs)),
            (Type::Bool(lhs), Type::Number(rhs)) => (*lhs as u8 as f64)
                .partial_cmp(rhs)
                .ok_or_else(|| format!("comparison not supported between {lhs} and {rhs}")),
            (Type::Number(lhs), Type::Bool(rhs)) => lhs
                .partial_cmp(&(*rhs as u8 as f64))
                .ok_or_else(|| format!("comparison not supported between {lhs} and {rhs}")),
            (lhs, rhs) => Err(format!("comparison not supported between {lhs} and {rhs}")),
        }
    }

    pub fn gt(&self, rhs: &Self) -> Result<Type, String> {
        Ok(Type::Bool(self.cmp(rhs)?.is_gt()))
    }

    pub fn ge(&self, rhs: &Self) -> Result<Type, String> {
        Ok(Type::Bool(self.cmp(rhs)?.is_ge()))
    }

    pub fn lt(&self, rhs: &Self) -> Result<Type, String> {
        Ok(Type::Bool(self.cmp(rhs)?.is_lt()))
    }

    pub fn le(&self, rhs: &Self) -> Result<Type, String> {
        Ok(Type::Bool(self.cmp(rhs)?.is_le()))
    }

    // Can't implement `std::cmp::Eq` because equality here can fail (unsupported operands).
    pub fn eq(&self, rhs: &Self) -> Result<Type, String> {
        match (self, rhs) {
            (Type::Bool(lhs), Type::Bool(rhs)) => Ok(Type::Bool(lhs == rhs)),
            (Type::Number(lhs), Type::Number(rhs)) => Ok(Type::Bool(lhs == rhs)),
            (Type::String(lhs), Type::String(rhs)) => Ok(Type::Bool(lhs == rhs)),
            (Type::Bool(lhs), Type::Number(rhs)) => Ok(Type::Bool((*lhs as u8 as f64) == *rhs)),
            (Type::Number(lhs), Type::Bool(rhs)) => Ok(Type::Bool(*lhs == (*rhs as u8 as f64))),
            (lhs, rhs) => Err(format!("equality not supported between {rhs} and {lhs}")),
        }
    }
}

impl From<&Type> for bool {
    fn from(value: &Type) -> Self {
        match value {
            Type::Bool(value) => *value,
            Type::Number(value) => *value != 0f64,
            Type::String(value) => !value.is_empty(),
            Type::None => false,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Number(value) => write!(f, "{value}"),
            Type::Bool(value) => write!(f, "{value}"),
            Type::String(value) => write!(f, "{value}"),
            Type::None => write!(f, "None"),
        }
    }
}

impl std::ops::Add for Type {
    type Output = Result<Type, String>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Type::String(lhs), rhs) => Ok(Type::String(format!("{lhs}{rhs}"))),
            (lhs, Type::String(rhs)) => Ok(Type::String(format!("{lhs}{rhs}"))),
            (Type::Number(lhs), Type::Number(rhs)) => Ok(Type::Number(lhs + rhs)),
            (Type::Bool(lhs), Type::Bool(rhs)) => Ok(Type::Number(lhs as u8 as f64 + rhs as u8 as f64)),
            (Type::Number(lhs), Type::Bool(rhs)) => Ok(Type::Number(lhs + rhs as u8 as f64)),
            (Type::Bool(lhs), Type::Number(rhs)) => Ok(Type::Number(lhs as u8 as f64 + rhs)),
            (lhs, rhs) => Err(format!("invalid operands for addition: {lhs}, {rhs}")),
        }
    }
}

macro_rules! impl_numeric_op {
    ($trait:ident, $method:ident, $op:tt, $name:literal) => {
        impl std::ops::$trait for Type {
            type Output = Result<Type, String>;

            fn $method(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Type::Number(lhs), Type::Number(rhs)) => Ok(Type::Number(lhs $op rhs)),
                    (Type::Bool(lhs), Type::Bool(rhs)) => Ok(Type::Number((lhs as u8 as f64) $op (rhs as u8 as f64))),
                    (Type::Number(lhs), Type::Bool(rhs)) => Ok(Type::Number(lhs $op (rhs as u8 as f64))),
                    (Type::Bool(lhs), Type::Number(rhs)) => Ok(Type::Number((lhs as u8 as f64) $op rhs)),
                    (Type::String(_), _) | (_, Type::String(_)) => Err(concat!($name, " is not implemented for string").into()),
                    (lhs, rhs) => Err(format!("invalid operands for {}: {lhs}, {rhs}", $name)),
                }
            }
        }
    };
}

impl_numeric_op!(Sub, sub, -, "subtraction");
impl_numeric_op!(Mul, mul, *, "multiplication");

impl std::ops::Div for Type {
    type Output = Result<Type, String>;

    fn div(self, rhs: Self) -> Self::Output {
        let divisor = match &rhs {
            Type::Number(value) => *value,
            Type::Bool(value) => *value as u8 as f64,
            Type::String(_) => return Err("division is not implemented for string".into()),
            Type::None => return Err(format!("cannot divide {self} by {rhs}")),
        };
        if divisor == 0f64 {
            return Err("division by zero".into());
        }
        let dividend = match &self {
            Type::Number(value) => *value,
            Type::Bool(value) => *value as u8 as f64,
            Type::String(_) => return Err("division is not implemented for string".into()),
            Type::None => return Err(format!("cannot divide {self} by {rhs}")),
        };
        Ok(Type::Number(dividend / divisor))
    }
}

impl std::ops::Rem for Type {
    type Output = Result<Type, String>;

    fn rem(self, rhs: Self) -> Self::Output {
        let divisor = match &rhs {
            Type::Number(value) => *value,
            Type::Bool(value) => *value as u8 as f64,
            Type::String(_) => return Err("modulo is not implemented for string".into()),
            Type::None => return Err(format!("cannot take modulo of {self} by {rhs}")),
        };
        if divisor == 0f64 {
            return Err("modulo by zero".into());
        }
        let dividend = match &self {
            Type::Number(value) => *value,
            Type::Bool(value) => *value as u8 as f64,
            Type::String(_) => return Err("modulo is not implemented for string".into()),
            Type::None => return Err(format!("cannot take modulo of {self} by {rhs}")),
        };
        Ok(Type::Number(dividend % divisor))
    }
}

impl std::ops::Neg for Type {
    type Output = Result<Type, String>;

    fn neg(self) -> Self::Output {
        match self {
            Type::Number(value) => Ok(Type::Number(-value)),
            Type::Bool(_) => Err("bad operand type for unary -: bool".into()),
            Type::String(_) => Err("bad operand type for unary -: string".into()),
            Type::None => Err("bad operand type for unary -: None".into()),
        }
    }
}

impl std::ops::Not for Type {
    type Output = Result<Type, String>;

    fn not(self) -> Self::Output {
        match self {
            Type::Bool(value) => Ok(Type::Bool(!value)),
            Type::Number(value) => Ok(Type::Bool(value == 0f64)),
            Type::String(value) => Ok(Type::Bool(value.is_empty())),
            Type::None => Ok(Type::Bool(true)),
        }
    }
}

/// Visitor for evaluating AST.
pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
    out: Rc<RefCell<dyn Write>>,
}

impl Interpreter {
    pub fn new(environment: Rc<RefCell<Environment>>, out: Rc<RefCell<dyn Write>>) -> Self {
        Self { environment, out }
    }

    pub fn interpret(&mut self, stmts: &model::Stmts) -> Eval {
        for stmt in stmts.stmts() {
            stmt.accept(self)?;
        }

        Eval(Ok(Outcome::Done(Type::None)))
    }

    pub fn environment(&mut self) -> &Rc<RefCell<Environment>> {
        &self.environment
    }

    pub fn fork(&self) -> Self {
        Self {
            environment: Environment::fork(&self.environment),
            out: self.out.clone(),
        }
    }
}

impl ExprVisitor<Eval> for Interpreter {
    fn visit_integer(&mut self, n: &model::IntegerLiteral) -> Eval {
        expr!(Type::Number(n.value()))
    }

    fn visit_float(&mut self, f: &model::FloatLiteral) -> Eval {
        expr!(Type::Number(f.value()))
    }

    fn visit_string(&mut self, s: &model::StringLiteral) -> Eval {
        expr!(Type::String(s.value().into()))
    }

    fn visit_bool(&mut self, b: &model::BoolLiteral) -> Eval {
        expr!(Type::Bool(b.value()))
    }

    fn visit_grouping(&mut self, inner: &model::Expr) -> Eval {
        inner.accept(self)
    }

    fn visit_unop(&mut self, op: &model::UnOp) -> Eval {
        let operand = op.operand().accept(self)?;
        let result = match op.operator().kind() {
            TokenKind::Plus => match operand {
                Type::String(_) => Err("bad operand for unary +: 'string'".into()),
                _ => Ok(operand),
            },
            TokenKind::Minus => -operand,
            TokenKind::Not => !operand,
            other => return err!(RuntimeError::new(format!("unsupported unary operation {other:?}"), op.span())),
        };
        lift(result, op.span())
    }

    fn visit_binop(&mut self, op: &model::BinOp) -> Eval {
        let lhs = op.lhs().accept(self)?;
        let rhs = op.rhs().accept(self)?;
        let result = match op.operator().kind() {
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
            TokenKind::NotEqual => lhs.eq(&rhs).and_then(|t| !t),
            other => return err!(RuntimeError::new(format!("unsupported binary operation {other:?}"), op.span())),
        };
        lift(result, op.span())
    }

    fn visit_logical(&mut self, op: &model::LogicalOp) -> Eval {
        // First interpret and check left-hand side to allow for short-circuiting
        let lhs = op.lhs().accept(self)?;
        match op.operator().kind() {
            TokenKind::And => {
                if !bool::from(&lhs) {
                    return expr!(Type::Bool(false));
                }
                let rhs = op.rhs().accept(self)?;
                expr!(Type::Bool(bool::from(&rhs)))
            }
            TokenKind::Or => {
                if bool::from(&lhs) {
                    return expr!(Type::Bool(true));
                }
                let rhs = op.rhs().accept(self)?;
                expr!(Type::Bool(bool::from(&rhs)))
            }
            other => err!(RuntimeError::new(format!("unsupported logical operation {other:?}"), op.span())),
        }
    }

    fn visit_identifier(&mut self, i: &model::Identifier) -> Eval {
        match self.environment().borrow().load_var(i.name().clone()) {
            Some(value) => expr!(value),
            None => err!(RuntimeError::new(format!("undeclared identifier {}", i.name()), i.span())),
        }
    }

    fn visit_func_call(&mut self, c: &model::FuncCall) -> Eval {
        let Some(f) = self.environment().borrow().load_func(c.name().clone()) else {
            return err!(RuntimeError::new(format!("call to undeclared function '{}'", c.name()), c.span()));
        };

        if c.args().len() != f.declaration().params().len() {
            return err!(RuntimeError::new(
                format!(
                    "{} expected {} parameters, got {} arguments",
                    f.declaration().name(),
                    f.declaration().params().len(),
                    c.args().len()
                ),
                c.span(),
            ));
        }

        // lexical scoping, the parent environment is the declaration site,
        // call site would be dynamic scoping
        let mut fork = Interpreter::new(f.environment().clone(), self.out.clone());

        for (arg, param) in c.args().iter().zip(f.declaration().params()) {
            let val = arg.accept(self)?;
            fork.environment().borrow_mut().store_var_local(param.name(), val);
        }

        match fork.interpret(f.declaration().body()) {
            Eval(Ok(Outcome::Return(val))) => expr!(val),
            Eval(Ok(Outcome::Done(_))) => expr!(Type::Bool(true)),
            Eval(Err(e)) => err!(e),
        }
    }
}

pub struct Eval(Result<Outcome, RuntimeError>);

#[derive(Clone, PartialEq, Debug)]
pub enum Outcome {
    Done(Type),
    Return(Type),
}

impl std::ops::Try for Eval {
    type Output = Type;
    type Residual = Eval;

    fn from_output(value: Type) -> Self {
        Eval(Ok(Outcome::Done(value)))
    }

    fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
        match self.0 {
            Ok(Outcome::Done(v)) => ControlFlow::Continue(v),
            other => ControlFlow::Break(Eval(other)),
        }
    }
}

impl Residual<Type> for Eval {
    type TryType = Eval;
}

impl FromResidual for Eval {
    fn from_residual(residual: <Self as std::ops::Try>::Residual) -> Self {
        residual
    }
}

impl StmtVisitor<Eval> for Interpreter {
    fn visit_print(&mut self, p: &model::Print) -> Eval {
        let value = p.expr().accept(self)?;
        if let Err(e) = write!(self.out.borrow_mut(), "{value}") {
            return err!(RuntimeError::new(format!("write failure: {e}"), p.expr().span()));
        }
        expr!(Type::None)
    }

    fn visit_println(&mut self, p: &model::Println) -> Eval {
        let value = p.expr().accept(self)?;
        if let Err(e) = writeln!(self.out.borrow_mut(), "{value}") {
            return err!(RuntimeError::new(format!("write failure: {e}"), p.expr().span()));
        }
        expr!(Type::None)
    }

    fn visit_if(&mut self, i: &model::If) -> Eval {
        let test = i.test().accept(self)?;
        let Type::Bool(value) = test else {
            return err!(RuntimeError::new("if condition is not a boolean expression".into(), i.test().span()));
        };
        let mut fork = self.fork(); // create new scope for the block

        if value {
            fork.interpret(i.then())?;
            return expr!(Type::None);
        }

        for elif in i.elif() {
            let test = elif.test().accept(self)?;
            let Type::Bool(value) = test else {
                return err!(RuntimeError::new("elif condition is not a boolean expression".into(), elif.test().span()));
            };
            if value {
                fork.interpret(elif.then())?;
                return expr!(Type::None);
            }
        }

        if let Some(r#else) = i.r#else() {
            fork.interpret(r#else)?;
        }
        expr!(Type::None)
    }

    fn visit_assignment(&mut self, a: &model::Assignment) -> Eval {
        let rvalue = a.rhs().accept(self)?;
        let model::Expr::Identifier(i) = a.lhs() else {
            return err!(RuntimeError::new(format!("cannot assign to {:?}", a.lhs()), a.lhs().span()));
        };

        self.environment().borrow_mut().store_var(i.name(), rvalue);
        expr!(Type::None)
    }

    fn visit_while(&mut self, w: &model::While) -> Eval {
        let mut fork = self.fork();
        loop {
            let test = w.test().accept(&mut fork)?;
            let Type::Bool(value) = test else {
                return err!(RuntimeError::new("while condition is not a boolean expression".into(), w.test().span()));
            };
            if !value {
                break;
            }
            fork.interpret(w.body())?;
        }
        expr!(Type::None)
    }

    fn visit_for(&mut self, f: &model::For) -> Eval {
        let mut fork = self.fork();

        let start = f.start().accept(&mut fork)?;
        let end = f.end().accept(&mut fork)?;
        let step = match f.step() {
            Some(s) => Some(s.accept(&mut fork)?),
            None => None,
        };

        let Type::Number(mut current) = start else {
            return err!(RuntimeError::new("for loop start must be a number".into(), f.start().span()));
        };
        let Type::Number(end_value) = end else {
            return err!(RuntimeError::new("for loop end must be a number".into(), f.end().span()));
        };
        let step_value = match step {
            Some(Type::Number(value)) => value,
            None => 1.0,
            Some(_) => {
                let span = f.step().as_ref().map_or_else(|| f.start().span(), |s| s.span());
                return err!(RuntimeError::new("for loop step must be a number".into(), span));
            }
        };

        let Expr::Identifier(i) = f.var() else {
            return err!(RuntimeError::new(format!("cannot assign to {:?}", f.var()), f.var().span()));
        };

        let name = i.name().clone();

        fork.environment().borrow_mut().store_var(&name, Type::Number(current));

        if step_value > 0.0 {
            while current <= end_value {
                fork.interpret(f.body())?;
                current += step_value;
                fork.environment().borrow_mut().store_var(&name, Type::Number(current));
            }
        } else if step_value < 0.0 {
            while current >= end_value {
                fork.interpret(f.body())?;
                current += step_value;
                fork.environment().borrow_mut().store_var(&name, Type::Number(current));
            }
        }

        expr!(Type::None)
    }

    fn visit_func_decl(&mut self, d: &model::FuncDecl) -> Eval {
        let env = Environment::fork(self.environment());
        self.environment().borrow_mut().store_func(d.name(), Function::new(d.clone(), env));
        expr!(Type::None)
    }

    fn visit_expr(&mut self, e: &model::Expr) -> Eval {
        e.accept(self)?;
        expr!(Type::None)
    }

    fn visit_ret(&mut self, r: &model::Ret) -> Eval {
        ret!(r.value().accept(self)?)
    }
}

/// Evaluate a single expression.
pub fn expr(ast: &model::Expr, out: Rc<RefCell<dyn Write>>) -> Result<Outcome, RuntimeError> {
    let mut interpreter = Interpreter::new(Environment::new(), out);
    ast.accept(&mut interpreter).0
}

/// Interpret a list of statements.
pub fn interpret(stmts: &model::Stmts, out: Rc<RefCell<dyn Write>>) -> Result<Outcome, RuntimeError> {
    let mut interpreter = Interpreter::new(Environment::new(), out);
    interpreter.interpret(stmts).0
}
