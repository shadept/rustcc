use crate::backend::ast::{Assembly, Function, Inst, Operand};
use crate::frontend::ast::Function as FunctionAST;
use crate::frontend::ast::{Expr, ExprKind, Program as ProgramAST, Stmt, StmtKind};

pub fn assemble(ast: ProgramAST) -> Assembly {
    Assembly::new(assemble_function(ast.function_definition))
}

fn assemble_function(ast: FunctionAST) -> Function {
    let mut instructions = vec![];
    if let Some(body) = ast.body {
        for stmt in body {
            assemble_statement(&mut instructions, stmt)
        }
    }
    Function::new(ast.name, instructions)
}

fn assemble_statement(instructions: &mut Vec<Inst>, ast: Stmt) {
    match ast.kind {
        StmtKind::Expr(expr) => assemble_expression(instructions, *expr),
    }
}

fn assemble_expression(instructions: &mut Vec<Inst>, ast: Expr) {
    match ast.kind {
        ExprKind::Constant(i) => {
            instructions.push(Inst::Mov(Operand::Imm(i), Operand::Register));
        }
        ExprKind::Return(maybe_expr) => {
            if let Some(expr) = maybe_expr {
                assemble_expression(instructions, *expr);
            }
            instructions.push(Inst::Ret);
        }
        ExprKind::Unary(_, _) => {}
    }
}
