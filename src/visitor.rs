use crate::model;

/// Visitor trait for expression nodes.
pub trait ExprVisitor<T> {
    fn visit_integer(&mut self, n: &model::Integer) -> T;
    fn visit_float(&mut self, f: &model::Float) -> T;
    fn visit_string(&mut self, s: &model::StringType) -> T;
    fn visit_bool(&mut self, b: &model::Bool) -> T;
    fn visit_grouping(&mut self, inner: &model::Expr) -> T;
    fn visit_unop(&mut self, op: &model::UnOp) -> T;
    fn visit_binop(&mut self, op: &model::BinOp) -> T;
    fn visit_logical(&mut self, op: &model::LogicalOp) -> T;
    fn visit_identifier(&mut self, i: &model::Identifier) -> T;
}

/// Visitor trait for statement nodes.
pub trait StmtVisitor<T> {
    fn visit_print(&mut self, p: &model::Print) -> T;
    fn visit_println(&mut self, p: &model::Println) -> T;
    fn visit_if(&mut self, i: &model::If) -> T;
    fn visit_assignment(&mut self, a: &model::Assignment) -> T;
}
