use crate::{model::Expr, tokens::TokenKind};

pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn interpret<'src>(&mut self, ast: &Expr<'src>) -> f64 {
        match ast {
            Expr::Integer(int) => int.value(),
            Expr::Float(float) => float.value(),
            Expr::Grouping(expr) => self.interpret(expr),
            Expr::BinOp(binop) => {
                let lhs = self.interpret(binop.lhs());
                let rhs = self.interpret(binop.rhs());
                match binop.operator().kind() {
                    TokenKind::Plus => lhs + rhs,
                    TokenKind::Minus => lhs - rhs,
                    TokenKind::Star => lhs * rhs,
                    TokenKind::Slash => lhs / rhs,
                    _ => panic!("i have no idea how this could ever happen"),
                }
            }
            Expr::UnOp(unop) => {
                let operand = self.interpret(unop.operand());
                match unop.operator().kind() {
                    TokenKind::Plus => operand,
                    TokenKind::Minus => -operand,
                    // TokenKind::Not => (operand == 0f64 || operand.is_nan()) as isize as f64, // don't have booleans yet
                    _ => panic!("i have no idea how this could ever happen"),
                }
            }
            Expr::String(..) => 0f64,
            Expr::Bool(..) => 0f64,
        }
    }
}
