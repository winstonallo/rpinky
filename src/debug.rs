use crate::{
    model::{BinOp, Bool, Expr, Float, If, Integer, LogicalOp, Print, Println, Stmt, Stmts, StringType, UnOp},
    visitor::{ExprVisitor, StmtVisitor},
};

/// Visitor for pretty-printing AST nodes with indentation.
pub struct AstPrinter<'a, 'b> {
    f: &'a mut std::fmt::Formatter<'b>,
    indentation: usize,
}

impl<'a, 'b> AstPrinter<'a, 'b> {
    pub fn new(f: &'a mut std::fmt::Formatter<'b>) -> Self {
        Self { f, indentation: 0 }
    }

    pub fn with_indentation(f: &'a mut std::fmt::Formatter<'b>, indentation: usize) -> Self {
        Self { f, indentation }
    }

    fn indent(&self) -> String {
        " ".repeat(self.indentation)
    }

    fn indented(&mut self) -> &mut Self {
        self.indentation += 2;
        self
    }

    fn dedent(&mut self) {
        self.indentation -= 2;
    }

    pub fn print_stmts(&mut self, stmts: &Stmts) -> std::fmt::Result {
        for stmt in stmts.stmts() {
            stmt.accept(self)?;
        }
        Ok(())
    }
}

impl ExprVisitor<std::fmt::Result> for AstPrinter<'_, '_> {
    fn visit_integer(&mut self, n: &Integer) -> std::fmt::Result {
        writeln!(self.f, "{}{n:?}", self.indent())
    }

    fn visit_float(&mut self, n: &Float) -> std::fmt::Result {
        writeln!(self.f, "{}{n:?}", self.indent())
    }

    fn visit_string(&mut self, s: &StringType) -> std::fmt::Result {
        writeln!(self.f, "{}{s:?}", self.indent())
    }

    fn visit_bool(&mut self, b: &Bool) -> std::fmt::Result {
        writeln!(self.f, "{}{b:?}", self.indent())
    }

    fn visit_grouping(&mut self, inner: &Expr) -> std::fmt::Result {
        writeln!(self.f, "{}Grouping (", self.indent())?;
        self.indented();
        inner.accept(self)?;
        self.dedent();
        writeln!(self.f, "{})", self.indent())
    }

    fn visit_unop(&mut self, op: &UnOp) -> std::fmt::Result {
        writeln!(self.f, "{}UnOp {{", self.indent())?;
        self.indented();
        writeln!(self.f, "{}{:?}", self.indent(), op.operator())?;
        op.operand().accept(self)?;
        self.dedent();
        writeln!(self.f, "{}}}", self.indent())
    }

    fn visit_binop(&mut self, op: &BinOp) -> std::fmt::Result {
        writeln!(self.f, "{}BinOp {{", self.indent())?;
        self.indented();
        op.lhs().accept(self)?;
        writeln!(self.f, "{}{:?}", self.indent(), op.operator())?;
        op.rhs().accept(self)?;
        self.dedent();
        writeln!(self.f, "{}}}", self.indent())
    }

    fn visit_logical(&mut self, op: &LogicalOp) -> std::fmt::Result {
        writeln!(self.f, "{}LogicalOp {{", self.indent())?;
        self.indented();
        op.lhs().accept(self)?;
        writeln!(self.f, "{}{:?}", self.indent(), op.operator())?;
        op.rhs().accept(self)?;
        self.dedent();
        writeln!(self.f, "{}}}", self.indent())
    }
}

impl StmtVisitor<std::fmt::Result> for AstPrinter<'_, '_> {
    fn visit_print(&mut self, p: &Print) -> std::fmt::Result {
        writeln!(self.f, "{}print", self.indent())?;
        self.indented();
        p.expr().accept(self)?;
        self.dedent();
        Ok(())
    }

    fn visit_println(&mut self, p: &Println) -> std::fmt::Result {
        writeln!(self.f, "{}println", self.indent())?;
        self.indented();
        p.expr().accept(self)?;
        self.dedent();
        Ok(())
    }

    fn visit_if(&mut self, i: &If) -> std::fmt::Result {
        writeln!(self.f, "{}if", self.indent())?;
        self.indented();
        i.test().accept(self)?;
        self.dedent();
        writeln!(self.f, "{}then", self.indent())?;
        self.indented();
        self.print_stmts(i.then())?;
        self.dedent();
        if let Some(else_branch) = i.r#else() {
            writeln!(self.f, "{}else", self.indent())?;
            self.indented();
            self.print_stmts(else_branch)?;
            self.dedent();
        }
        writeln!(self.f, "{}end", self.indent())
    }
}

pub fn dump_expr(expr: &Expr, f: &mut std::fmt::Formatter<'_>, indentation: Option<usize>) -> std::fmt::Result {
    let mut printer = AstPrinter::with_indentation(f, indentation.unwrap_or(0));
    expr.accept(&mut printer)
}

pub fn dump_stmt(stmt: &Stmt, f: &mut std::fmt::Formatter<'_>, indentation: Option<usize>) -> std::fmt::Result {
    let mut printer = AstPrinter::with_indentation(f, indentation.unwrap_or(0));
    stmt.accept(&mut printer)
}

pub fn dump_stmts(stmts: &Stmts, f: &mut std::fmt::Formatter<'_>, indentation: Option<usize>) -> std::fmt::Result {
    let mut printer = AstPrinter::with_indentation(f, indentation.unwrap_or(0));
    printer.print_stmts(stmts)
}
