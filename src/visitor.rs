use crate::model;

pub trait ExprVisitor<T> {
    fn visit_integer(&mut self, n: &model::IntegerLiteral) -> T;
    fn visit_float(&mut self, f: &model::FloatLiteral) -> T;
    fn visit_string(&mut self, s: &model::StringLiteral) -> T;
    fn visit_bool(&mut self, b: &model::BoolLiteral) -> T;
    fn visit_grouping(&mut self, inner: &model::Expr) -> T;
    fn visit_unop(&mut self, op: &model::UnOp) -> T;
    fn visit_binop(&mut self, op: &model::BinOp) -> T;
    fn visit_logical(&mut self, op: &model::LogicalOp) -> T;
    fn visit_identifier(&mut self, i: &model::Identifier) -> T;
    fn visit_func_call(&mut self, i: &model::FuncCall) -> T;
}

pub trait StmtVisitor<T> {
    fn visit_print(&mut self, p: &model::Print) -> T;
    fn visit_println(&mut self, p: &model::Println) -> T;
    fn visit_if(&mut self, i: &model::If) -> T;
    fn visit_assignment(&mut self, a: &model::Assignment) -> T;
    fn visit_while(&mut self, w: &model::While) -> T;
    fn visit_for(&mut self, f: &model::For) -> T;
    fn visit_func_decl(&mut self, d: &model::FuncDecl) -> T;
    fn visit_expr(&mut self, e: &model::Expr) -> T;
    fn visit_ret(&mut self, r: &model::Ret) -> T;
}
