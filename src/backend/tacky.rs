use crate::frontend::ast;
use crate::frontend::ast::{ExprKind, StmtKind, UnaryOp};
use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub function_definition: Function,
}

impl Program {
    pub fn new(function_definition: Function) -> Self {
        Self {
            function_definition,
        }
    }
}

pub type Identifier = String;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Identifier,
    pub body: Block,
}

pub type Block = Vec<Instruction>;

impl Function {
    pub fn new(name: Identifier, body: Block) -> Self {
        Self { name, body }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Return(Val),
    Unary(UnaryOperator, Val, Val), // cannot assign to Constant
}

#[derive(Debug, Clone, PartialEq)]
pub enum Val {
    Constant(i32),
    Var(Identifier),
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Complement,
    Negate,
}

pub fn emit_tacky(program: ast::Program) -> Program {
    Program::new(emit_tacky_function(program.function_definition))
}

fn emit_tacky_function(function: ast::Function) -> Function {
    let mut instructions = Vec::new();
    if let Some(body) = function.body {
        for stmt in body {
            emit_tacky_stmt(stmt, &mut instructions);
        }
    }
    Function::new(function.name, instructions)
}
fn emit_tacky_stmt(stmt: ast::Stmt, instructions: &mut Vec<Instruction>) {
    match stmt.kind {
        StmtKind::Expr(expr) => emit_tacky_expr(*expr, instructions),
    };
}

fn emit_tacky_expr(expr: ast::Expr, instructions: &mut Vec<Instruction>) -> Val {
    use crate::backend::tacky::Val::{Constant, Var};
    match expr.kind {
        ExprKind::Constant(c) => Constant(c),
        ExprKind::Unary(op, inner) => {
            let src = emit_tacky_expr(*inner, instructions);
            let dst_name = make_temporary();
            let dst = Var(dst_name);
            let tacky_op = match op {
                UnaryOp::Comp => UnaryOperator::Complement,
                UnaryOp::Neg => UnaryOperator::Negate,
            };
            instructions.push(Instruction::Unary(tacky_op, src, dst.clone()));
            dst
        }
        ExprKind::Return(maybe_inner) => {
            let src = match maybe_inner {
                Some(inner) => emit_tacky_expr(*inner, instructions),
                None => Constant(0),
            };
            instructions.push(Instruction::Return(src.clone()));
            src
        }
    }
}

fn make_temporary() -> Identifier {
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    format!("tmp.{}", COUNTER.fetch_add(1, Ordering::SeqCst))
}
