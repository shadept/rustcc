use crate::backend::common::{make_label, make_temporary};
use crate::frontend::ast;
use crate::frontend::ast::{BinaryOp, BlockItem, DeclKind, Expr, ExprKind, StmtKind, UnaryOp};

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
    Binary(BinaryOperator, Val, Val, Val),
    Return(Val),
    Unary(UnaryOperator, Val, Val), // cannot assign to Constant
    Copy(Val, Val),
    Jump(Identifier),
    JumpIfZero(Val, Identifier),
    JumpIfNotZero(Val, Identifier),
    Label(Identifier),
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
    Not,
}

impl From<UnaryOp> for UnaryOperator {
    fn from(value: UnaryOp) -> Self {
        match value {
            UnaryOp::Complement => UnaryOperator::Complement,
            UnaryOp::Negate => UnaryOperator::Negate,
            UnaryOp::Not => UnaryOperator::Not,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

impl From<BinaryOp> for BinaryOperator {
    fn from(value: BinaryOp) -> Self {
        match value {
            BinaryOp::Add => BinaryOperator::Add,
            BinaryOp::Subtract => BinaryOperator::Subtract,
            BinaryOp::Multiply => BinaryOperator::Multiply,
            BinaryOp::Divide => BinaryOperator::Divide,
            BinaryOp::Remainder => BinaryOperator::Remainder,
            BinaryOp::BitwiseOr => BinaryOperator::BitwiseOr,
            BinaryOp::BitwiseAnd => BinaryOperator::BitwiseAnd,
            BinaryOp::BitwiseXor => BinaryOperator::BitwiseXor,
            BinaryOp::Equal => BinaryOperator::Equal,
            BinaryOp::NotEqual => BinaryOperator::NotEqual,
            BinaryOp::LessThan => BinaryOperator::LessThan,
            BinaryOp::LessThanOrEqual => BinaryOperator::LessThanOrEqual,
            BinaryOp::GreaterThan => BinaryOperator::GreaterThan,
            BinaryOp::GreaterThanOrEqual => BinaryOperator::GreaterThanOrEqual,
            _ => panic!("invalid binary operator"),
        }
    }
}

pub fn emit_tacky(program: ast::Program) -> Program {
    Program::new(emit_tacky_function(program.function_definition))
}

fn emit_tacky_function(function: ast::Function) -> Function {
    let mut instructions = Vec::new();
    if let Some(body) = function.body {
        for item in body {
            match item {
                BlockItem::Decl(decl) => emit_tacky_decl(decl, &mut instructions),
                BlockItem::Stmt(stmt) => emit_tacky_stmt(stmt, &mut instructions),
            }
        }
    }
    instructions.push(Instruction::Return(Val::Constant(0))); // optimization will remove extra return if another return is present
    Function::new(function.name, instructions)
}
fn emit_tacky_decl(decl: ast::Decl, instructions: &mut Vec<Instruction>) {
    match decl.kind {
        DeclKind::Variable(name, init) => {
            if let Some(init) = init {
                let tmp_expr = Expr::new(
                    ExprKind::Assignment(
                        Expr::new(ExprKind::Var(name), decl.span).into(), // TODO decl.span should be the span of the variable name
                        init.into(),
                    ),
                    decl.span,
                );
                emit_tacky_expr(tmp_expr, instructions);
            }
        }
    };
}

fn emit_tacky_stmt(stmt: ast::Stmt, instructions: &mut Vec<Instruction>) {
    match stmt.kind {
        StmtKind::Expr(expr) => {
            emit_tacky_expr(*expr, instructions);
        }
        StmtKind::If(cond, then, maybe_else) => {
            let c = emit_tacky_expr(*cond, instructions);
            let else_label = make_label();
            let end_label = make_label();
            instructions.push(Instruction::JumpIfZero(c, else_label.clone()));
            emit_tacky_stmt(*then, instructions);
            instructions.push(Instruction::Jump(end_label.clone()));
            instructions.push(Instruction::Label(else_label));
            if let Some(else_stmt) = maybe_else {
                emit_tacky_stmt(*else_stmt, instructions);
            }
            instructions.push(Instruction::Label(end_label));
        }
        StmtKind::Null => {}
        StmtKind::Return(expr) => {
            emit_tacky_expr(*expr, instructions);
        }
    };
}

fn emit_tacky_expr(expr: ast::Expr, instructions: &mut Vec<Instruction>) -> Val {
    use crate::backend::tacky::Val::{Constant, Var};
    match expr.kind {
        ExprKind::Binary(BinaryOp::And, left, right) => {
            let left = emit_tacky_expr(*left, instructions);
            let false_label = make_label();
            instructions.push(Instruction::JumpIfZero(left, false_label.clone()));
            let right = emit_tacky_expr(*right, instructions);
            instructions.push(Instruction::JumpIfZero(right, false_label.clone()));
            let dst = Var(make_temporary());
            instructions.push(Instruction::Copy(Constant(1), dst.clone()));
            let end_label = make_label();
            instructions.push(Instruction::Jump(end_label.clone()));
            instructions.push(Instruction::Label(false_label));
            instructions.push(Instruction::Copy(Constant(0), dst.clone()));
            instructions.push(Instruction::Label(end_label));
            dst
        }
        ExprKind::Binary(BinaryOp::Or, left, right) => {
            let left = emit_tacky_expr(*left, instructions);
            let true_label = make_label();
            instructions.push(Instruction::JumpIfNotZero(left, true_label.clone()));
            let right = emit_tacky_expr(*right, instructions);
            instructions.push(Instruction::JumpIfNotZero(right, true_label.clone()));
            let dst = Var(make_temporary());
            instructions.push(Instruction::Copy(Constant(0), dst.clone()));
            let end_label = make_label();
            instructions.push(Instruction::Jump(end_label.clone()));
            instructions.push(Instruction::Label(true_label));
            instructions.push(Instruction::Copy(Constant(1), dst.clone()));
            instructions.push(Instruction::Label(end_label));
            dst
        }
        ExprKind::Binary(op, left, right) => {
            let left = emit_tacky_expr(*left, instructions);
            let right = emit_tacky_expr(*right, instructions);
            let dst_name = make_temporary();
            let dst = Var(dst_name);
            instructions.push(Instruction::Binary(op.into(), left, right, dst.clone()));
            dst
        }
        ExprKind::Cond(cond, if_true, if_false) => {
            let c = emit_tacky_expr(*cond, instructions);
            let dst_name = make_temporary();
            let dst = Var(dst_name);
            let else_label = make_label();
            let end_label = make_label();
            instructions.push(Instruction::JumpIfZero(c, else_label.clone()));
            let e1 = emit_tacky_expr(*if_true, instructions);
            instructions.push(Instruction::Copy(e1, dst.clone()));
            instructions.push(Instruction::Jump(end_label.clone()));
            instructions.push(Instruction::Label(else_label));
            let e2 = emit_tacky_expr(*if_false, instructions);
            instructions.push(Instruction::Copy(e2, dst.clone()));
            instructions.push(Instruction::Label(end_label));
            dst
        }
        ExprKind::Constant(c) => Constant(c),
        ExprKind::Return(maybe_inner) => {
            let src = match maybe_inner {
                Some(inner) => emit_tacky_expr(*inner, instructions),
                None => Constant(0),
            };
            instructions.push(Instruction::Return(src.clone()));
            src
        }
        ExprKind::Unary(op, inner) => {
            let src = emit_tacky_expr(*inner, instructions);
            let dst_name = make_temporary();
            let dst = Var(dst_name);
            instructions.push(Instruction::Unary(op.into(), src, dst.clone()));
            dst
        }
        ExprKind::Var(name) => Var(name),
        ExprKind::Assignment(lhs, rhs) => match lhs.kind {
            ExprKind::Var(name) => {
                let result = emit_tacky_expr(*rhs, instructions);
                instructions.push(Instruction::Copy(result, Var(name.clone())));
                Var(name)
            }
            _ => panic!("invalid assignment"),
        },
    }
}
