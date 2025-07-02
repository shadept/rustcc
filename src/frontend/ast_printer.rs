use crate::frontend::ast::*;
use std::io::{self, Write};

/// A printer for AST nodes that writes to a given writer.
pub struct AstPrinter<W: Write> {
    writer: W,
    indent: usize,
}

impl<W: Write> AstPrinter<W> {
    /// Creates a new AST printer that writes to the given writer.
    pub fn new(writer: W) -> Self {
        Self { writer, indent: 0 }
    }

    /// Prints the given program.
    pub fn print_program(&mut self, program: &Program) -> io::Result<()> {
        writeln!(self.writer, "Program")?;
        self.indent += 2;
        self.print_function(&program.function_definition)?;
        self.indent -= 2;
        Ok(())
    }

    /// Prints the given function.
    fn print_function(&mut self, function: &Function) -> io::Result<()> {
        self.print_indent()?;
        writeln!(self.writer, "Function: {}", function.name)?;

        if let Some(body) = &function.body {
            self.indent += 2;
            self.print_indent()?;
            writeln!(self.writer, "Body:")?;
            self.indent += 2;
            for item in body {
                self.print_block_item(item)?;
            }
            self.indent -= 4;
        }

        Ok(())
    }

    /// Prints the given block item.
    fn print_block_item(&mut self, item: &BlockItem) -> io::Result<()> {
        match item {
            BlockItem::Decl(decl) => self.print_decl(decl)?,
            BlockItem::Stmt(stmt) => self.print_stmt(stmt)?,
        }
        Ok(())
    }

    /// Prints the given declaration.
    fn print_decl(&mut self, decl: &Decl) -> io::Result<()> {
        self.print_indent()?;
        match &decl.kind {
            DeclKind::Variable(id, init_expr) => {
                writeln!(self.writer, "Declaration")?;
                self.indent += 2;
                self.print_indent()?;
                writeln!(self.writer, "Variable {}", id)?;
                if let Some(expr) = init_expr {
                    self.print_indent()?;
                    writeln!(self.writer, "Initializer: ")?;
                    self.indent += 2;
                    self.print_expr(expr)?;
                    self.indent -= 2;
                } else {
                    writeln!(self.writer)?;
                }
                self.indent -= 2;
            }
        }
        Ok(())
    }

    /// Prints the given statement.
    fn print_stmt(&mut self, stmt: &Stmt) -> io::Result<()> {
        self.print_indent()?;
        match &stmt.kind {
            StmtKind::Expr(expr) => {
                writeln!(self.writer, "Statement: Expression")?;
                self.indent += 2;
                self.print_expr(expr)?;
                self.indent -= 2;
            }
            StmtKind::If(condition, then_stmt, else_stmt) => {
                writeln!(self.writer, "Statement: If")?;
                self.indent += 2;
                self.print_indent()?;
                writeln!(self.writer, "Condition:")?;
                self.indent += 2;
                self.print_expr(condition)?;
                self.indent -= 2;
                self.print_indent()?;
                writeln!(self.writer, "Then:")?;
                self.indent += 2;
                self.print_stmt(then_stmt)?;
                self.indent -= 2;
                if let Some(else_stmt) = else_stmt {
                    self.print_indent()?;
                    writeln!(self.writer, "Else:")?;
                    self.indent += 2;
                    self.print_stmt(else_stmt)?;
                    self.indent -= 2;
                }
                self.indent -= 2;
            }
            StmtKind::Null => {
                writeln!(self.writer, "Statement: Null")?;
            }
            StmtKind::Return(expr) => {
                writeln!(self.writer, "Statement: Return")?;
                self.indent += 2;
                self.print_expr(expr)?;
                self.indent -= 2;
            }
        }
        Ok(())
    }

    /// Prints the given expression.
    fn print_expr(&mut self, expr: &Expr) -> io::Result<()> {
        self.print_indent()?;
        match &expr.kind {
            ExprKind::Assignment(lhs, rhs) => {
                writeln!(self.writer, "Expression: Assignment")?;
                self.indent += 2;
                self.print_indent()?;
                writeln!(self.writer, "Left-hand side:")?;
                self.indent += 2;
                self.print_expr(lhs)?;
                self.indent -= 2;
                self.print_indent()?;
                writeln!(self.writer, "Right-hand side:")?;
                self.indent += 2;
                self.print_expr(rhs)?;
                self.indent -= 4;
            }
            ExprKind::Binary(op, lhs, rhs) => {
                writeln!(self.writer, "Expression: Binary {:?}", op)?;
                self.indent += 2;
                self.print_indent()?;
                writeln!(self.writer, "Left operand:")?;
                self.indent += 2;
                self.print_expr(lhs)?;
                self.indent -= 2;
                self.print_indent()?;
                writeln!(self.writer, "Right operand:")?;
                self.indent += 2;
                self.print_expr(rhs)?;
                self.indent -= 4;
            }
            ExprKind::Cond(cond, then_expr, else_expr) => {
                writeln!(self.writer, "Expression: Conditional")?;
                self.indent += 2;
                self.print_indent()?;
                writeln!(self.writer, "Condition:")?;
                self.indent += 2;
                self.print_expr(cond)?;
                self.indent -= 2;
                self.print_indent()?;
                writeln!(self.writer, "Then expression:")?;
                self.indent += 2;
                self.print_expr(then_expr)?;
                self.indent -= 2;
                self.print_indent()?;
                writeln!(self.writer, "Else expression:")?;
                self.indent += 2;
                self.print_expr(else_expr)?;
                self.indent -= 4;
            }
            ExprKind::Constant(value) => {
                writeln!(self.writer, "Expression: Constant {}", value)?;
            }
            ExprKind::Return(expr_opt) => {
                writeln!(self.writer, "Expression: Return")?;
                if let Some(expr) = expr_opt {
                    self.indent += 2;
                    self.print_expr(expr)?;
                    self.indent -= 2;
                }
            }
            ExprKind::Unary(op, operand) => {
                writeln!(self.writer, "Expression: Unary {:?}", op)?;
                self.indent += 2;
                self.print_expr(operand)?;
                self.indent -= 2;
            }
            ExprKind::Var(id) => {
                writeln!(self.writer, "Expression: Variable {}", id)?;
            }
        }
        Ok(())
    }

    /// Prints the current indentation.
    fn print_indent(&mut self) -> io::Result<()> {
        for _ in 0..self.indent {
            write!(self.writer, " ")?;
        }
        Ok(())
    }
}
