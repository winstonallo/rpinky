use crate::model::{Expr, Stmt, Stmts};

pub fn dump_expr(expr: &Expr, f: &mut std::fmt::Formatter<'_>, indentation: Option<usize>) -> std::fmt::Result {
    let indentation = indentation.unwrap_or(0);

    match expr {
        Expr::Integer(int) => writeln!(f, "{}{int:?}", " ".repeat(indentation)),
        Expr::Float(float) => writeln!(f, "{}{float:?}", " ".repeat(indentation)),
        Expr::Bool(bool) => writeln!(f, "{}{bool:?}", " ".repeat(indentation)),
        Expr::String(str) => writeln!(f, "{}{str:?}", " ".repeat(indentation)),
        Expr::Grouping(group) => {
            writeln!(f, "{}Grouping (", " ".repeat(indentation))?;
            dump_expr(group, f, Some(indentation + 2))?;
            writeln!(f, "{})", " ".repeat(indentation))
        }
        Expr::UnOp(unop) => {
            writeln!(f, "{}UnOp {{", " ".repeat(indentation))?;
            writeln!(f, "{}{:?}", " ".repeat(indentation + 2), unop.operator())?;
            dump_expr(&unop.operand(), f, Some(indentation + 2))?;
            writeln!(f, "{}}}", " ".repeat(indentation))
        }
        Expr::BinOp(binop) => {
            writeln!(f, "{}BinOp {{", " ".repeat(indentation))?;
            dump_expr(&binop.lhs(), f, Some(indentation + 2))?;
            writeln!(f, "{}{:?}", " ".repeat(indentation + 2), binop.operator())?;
            dump_expr(&binop.rhs(), f, Some(indentation + 2))?;
            writeln!(f, "{}}}", " ".repeat(indentation))
        }
        Expr::LogicalOp(logicalop) => {
            writeln!(f, "{}LogicalOp {{", " ".repeat(indentation))?;
            dump_expr(&logicalop.lhs(), f, Some(indentation + 2))?;
            writeln!(f, "{}{:?}", " ".repeat(indentation + 2), logicalop.operator())?;
            dump_expr(&logicalop.rhs(), f, Some(indentation + 2))?;
            writeln!(f, "{}}}", " ".repeat(indentation))
        }
    }
}

pub fn dump_stmt(stmt: &Stmt, f: &mut std::fmt::Formatter<'_>, indentation: Option<usize>) -> std::fmt::Result {
    let indentation = indentation.unwrap_or(0);

    match stmt {
        Stmt::If(stmt) => {
            writeln!(f, "{}if", " ".repeat(indentation))?;
            dump_expr(&stmt.test(), f, Some(indentation + 2))?;
            writeln!(f, "{}then", " ".repeat(indentation))?;
            dump_stmts(stmt.then(), f, Some(indentation + 2))?;
            if let Some(elsee) = stmt.r#else() {
                writeln!(f, "{}else", " ".repeat(indentation))?;
                dump_stmts(elsee, f, Some(indentation + 2))?;
            }
            writeln!(f, "{}end", " ".repeat(indentation))
        }
        Stmt::Print(stmt) => {
            writeln!(f, "{}print", " ".repeat(indentation))?;
            dump_expr(stmt.expr(), f, Some(indentation + 2))
        }
        Stmt::Println(stmt) => {
            writeln!(f, "{}println", " ".repeat(indentation))?;
            dump_expr(stmt.expr(), f, Some(indentation + 2))
        }
    }
}

pub fn dump_stmts(stmts: &Stmts, f: &mut std::fmt::Formatter<'_>, indentation: Option<usize>) -> std::fmt::Result {
    let indentation = indentation.unwrap_or(0);
    for stmt in stmts.stmts() {
        dump_stmt(stmt, f, Some(indentation))?;
    }
    Ok(())
}
