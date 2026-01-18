use crate::model::{BinOp, Bool, Float, If, Integer, LogicalOp, Print, Println, StringType, UnOp};

/// Visitor trait for expression nodes.
pub trait ExprVisitor<T> {
    fn visit_integer(&mut self, n: &Integer) -> T;
    fn visit_float(&mut self, f: &Float) -> T;
    fn visit_string(&mut self, s: &StringType) -> T;
    fn visit_bool(&mut self, b: &Bool) -> T;
    fn visit_grouping(&mut self, inner: &crate::model::Expr) -> T;
    fn visit_unop(&mut self, op: &UnOp) -> T;
    fn visit_binop(&mut self, op: &BinOp) -> T;
    fn visit_logical(&mut self, op: &LogicalOp) -> T;
}

/// Visitor trait for statement nodes.
pub trait StmtVisitor<T> {
    fn visit_print(&mut self, p: &Print) -> T;
    fn visit_println(&mut self, p: &Println) -> T;
    fn visit_if(&mut self, i: &If) -> T;
}
