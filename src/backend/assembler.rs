use crate::backend::tacky;
use crate::backend::tacky::{Identifier, Instruction, Val};
use std::collections::HashMap;

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

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub body: Block,
}

pub type Block = Vec<Inst>;

impl Function {
    pub fn new(name: String, body: Block) -> Self {
        Self { name, body }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Inst {
    /// Src, Dst
    Mov(Operand, Operand),
    Unary(UnaryOperator, Operand),
    Binary(BinaryOperator, Operand, Operand),
    Cmp(Operand, Operand),
    Idiv(Operand),
    Cdq,
    Jmp(Identifier),
    JmpCC(CondCode, Identifier),
    SetCC(CondCode, Operand),
    Label(Identifier),
    AllocateStack(i32),
    Ret,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Neg,
    Not,
}

impl From<tacky::UnaryOperator> for UnaryOperator {
    fn from(value: tacky::UnaryOperator) -> Self {
        match value {
            tacky::UnaryOperator::Negate => UnaryOperator::Neg,
            tacky::UnaryOperator::Complement => UnaryOperator::Not,
            tacky::UnaryOperator::Not => UnaryOperator::Not,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Or,
    And,
    Xor,
}

impl From<tacky::BinaryOperator> for BinaryOperator {
    fn from(value: tacky::BinaryOperator) -> Self {
        match value {
            tacky::BinaryOperator::Add => BinaryOperator::Add,
            tacky::BinaryOperator::Subtract => BinaryOperator::Sub,
            tacky::BinaryOperator::Multiply => BinaryOperator::Mul,
            tacky::BinaryOperator::BitwiseOr => BinaryOperator::Or,
            tacky::BinaryOperator::BitwiseAnd => BinaryOperator::And,
            tacky::BinaryOperator::BitwiseXor => BinaryOperator::Xor,
            tacky::BinaryOperator::Divide | tacky::BinaryOperator::Remainder => {
                unreachable!("Divide and Remainder must be handled separately")
            }
            _ => panic!("Unknown binary operator"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Imm(i32),
    Pseudo(String),
    Register(Register),
    Stack(i32),
}

#[derive(Debug, Clone, PartialEq)]
pub enum CondCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Register {
    AX,
    DX,
    R10,
    R11,
}

pub fn assemble(program: tacky::Program) -> Program {
    Program::new(assemble_function(program.function_definition))
}

fn assemble_function(function: tacky::Function) -> Function {
    let mut instructions = vec![];
    for instruction in function.body {
        assemble_instruction(instruction, &mut instructions)
    }
    instructions = replace_pseudos(instructions, &mut HashMap::new());
    instructions = fixup_instructions(instructions);
    Function::new(function.name, instructions)
}

fn assemble_instruction(inst: tacky::Instruction, instructions: &mut Vec<Inst>) {
    match inst {
        Instruction::Return(val) => {
            let src = assemble_val(val);
            let dst = Operand::Register(Register::AX);
            instructions.push(Inst::Mov(src, dst));
            instructions.push(Inst::Ret);
        }
        Instruction::Unary(tacky::UnaryOperator::Not, src, dst) => {
            let dst = assemble_val(dst);
            instructions.push(Inst::Cmp(Operand::Imm(0), assemble_val(src)));
            instructions.push(Inst::Mov(Operand::Imm(0), dst.clone()));
            instructions.push(Inst::SetCC(CondCode::E, dst));
        }
        Instruction::Unary(op, src, dst) => {
            let src = assemble_val(src);
            let dst = assemble_val(dst);
            instructions.push(Inst::Mov(src, dst.clone()));
            instructions.push(Inst::Unary(op.into(), dst));
        }
        Instruction::Binary(tacky::BinaryOperator::Divide, left, right, dst) => {
            let left = assemble_val(left);
            let right = assemble_val(right);
            let dst = assemble_val(dst);
            instructions.push(Inst::Mov(left, Operand::Register(Register::AX)));
            instructions.push(Inst::Cdq);
            instructions.push(Inst::Idiv(right));
            instructions.push(Inst::Mov(Operand::Register(Register::AX), dst));
        }
        Instruction::Binary(tacky::BinaryOperator::Remainder, left, right, dst) => {
            let left = assemble_val(left);
            let right = assemble_val(right);
            let dst = assemble_val(dst);
            instructions.push(Inst::Mov(left, Operand::Register(Register::AX)));
            instructions.push(Inst::Cdq);
            instructions.push(Inst::Idiv(right));
            instructions.push(Inst::Mov(Operand::Register(Register::DX), dst));
        }
        Instruction::Binary(op, left, right, dst)
            if op == tacky::BinaryOperator::Equal
                || op == tacky::BinaryOperator::NotEqual
                || op == tacky::BinaryOperator::LessThan
                || op == tacky::BinaryOperator::LessThanOrEqual
                || op == tacky::BinaryOperator::GreaterThan
                || op == tacky::BinaryOperator::GreaterThanOrEqual =>
        {
            instructions.push(Inst::Cmp(assemble_val(right), assemble_val(left)));
            let dst = assemble_val(dst);
            instructions.push(Inst::Mov(Operand::Imm(0), dst.clone()));
            instructions.push(Inst::SetCC(
                match op {
                    tacky::BinaryOperator::Equal => CondCode::E,
                    tacky::BinaryOperator::NotEqual => CondCode::NE,
                    tacky::BinaryOperator::LessThan => CondCode::L,
                    tacky::BinaryOperator::LessThanOrEqual => CondCode::LE,
                    tacky::BinaryOperator::GreaterThan => CondCode::G,
                    tacky::BinaryOperator::GreaterThanOrEqual => CondCode::GE,
                    _ => unreachable!(),
                },
                dst,
            ))
        }
        Instruction::Binary(op, left, right, dst) => {
            let left = assemble_val(left);
            let right = assemble_val(right);
            let dst = assemble_val(dst);
            instructions.push(Inst::Mov(left, dst.clone()));
            instructions.push(Inst::Binary(op.into(), right, dst));
        }
        Instruction::Jump(target) => {
            instructions.push(Inst::Jmp(target));
        }
        Instruction::JumpIfZero(cond, target) => {
            instructions.push(Inst::Cmp(Operand::Imm(0), assemble_val(cond)));
            instructions.push(Inst::JmpCC(CondCode::E, target));
        }
        Instruction::JumpIfNotZero(cond, target) => {
            instructions.push(Inst::Cmp(Operand::Imm(0), assemble_val(cond)));
            instructions.push(Inst::JmpCC(CondCode::NE, target));
        }
        Instruction::Copy(src, dst) => {
            instructions.push(Inst::Mov(assemble_val(src), assemble_val(dst)));
        }
        Instruction::Label(name) => {
            instructions.push(Inst::Label(name));
        }
    }
}

fn assemble_val(val: tacky::Val) -> Operand {
    match val {
        Val::Constant(c) => Operand::Imm(c),
        Val::Var(i) => Operand::Pseudo(i),
    }
}

fn replace_pseudos(instructions: Vec<Inst>, allocation: &mut HashMap<String, i32>) -> Vec<Inst> {
    let mut ret = Vec::with_capacity(instructions.len());
    for inst in instructions {
        match inst {
            Inst::Mov(src, dst) => {
                let src = if let Operand::Pseudo(pseudo) = src {
                    Operand::Stack(allocate_stack(pseudo, allocation))
                } else {
                    src
                };

                let dst = if let Operand::Pseudo(pseudo) = dst {
                    Operand::Stack(allocate_stack(pseudo, allocation))
                } else {
                    dst
                };

                ret.push(Inst::Mov(src, dst));
            }
            Inst::Unary(op, dst) => {
                let dst = if let Operand::Pseudo(pseudo) = dst {
                    Operand::Stack(allocate_stack(pseudo, allocation))
                } else {
                    dst
                };

                ret.push(Inst::Unary(op, dst));
            }
            Inst::Binary(op, right, dst) => {
                let right = if let Operand::Pseudo(pseudo) = right {
                    Operand::Stack(allocate_stack(pseudo, allocation))
                } else {
                    right
                };

                let dst = if let Operand::Pseudo(pseudo) = dst {
                    Operand::Stack(allocate_stack(pseudo, allocation))
                } else {
                    dst
                };

                ret.push(Inst::Binary(op, right, dst));
            }
            Inst::Cmp(left, right) => {
                let left = if let Operand::Pseudo(pseudo) = left {
                    Operand::Stack(allocate_stack(pseudo, allocation))
                } else {
                    left
                };

                let right = if let Operand::Pseudo(pseudo) = right {
                    Operand::Stack(allocate_stack(pseudo, allocation))
                } else {
                    right
                };

                ret.push(Inst::Cmp(left, right));
            }
            Inst::Idiv(dst) => {
                let dst = if let Operand::Pseudo(pseudo) = dst {
                    Operand::Stack(allocate_stack(pseudo, allocation))
                } else {
                    dst
                };

                ret.push(Inst::Idiv(dst));
            }
            Inst::SetCC(cc, dst) => {
                let dst = if let Operand::Pseudo(pseudo) = dst {
                    Operand::Stack(allocate_stack(pseudo, allocation))
                } else {
                    dst
                };

                ret.push(Inst::SetCC(cc, dst));
            }
            _ => ret.push(inst),
        }
    }
    ret.insert(0, Inst::AllocateStack(4 * allocation.len() as i32));
    ret
}

fn allocate_stack(identifier: Identifier, allocation: &mut HashMap<String, i32>) -> i32 {
    let offset = -4 * (allocation.len() + 1) as i32;
    let p = allocation.entry(identifier).or_insert(offset);
    *p
}

fn fixup_instructions(instructions: Vec<Inst>) -> Vec<Inst> {
    let mut ret = Vec::with_capacity(instructions.len());
    for inst in instructions {
        match inst {
            Inst::Mov(src, dst)
                if matches!(src, Operand::Stack(_)) && matches!(dst, Operand::Stack(_)) =>
            {
                let tmp = Operand::Register(Register::R10);
                ret.push(Inst::Mov(src, tmp.clone()));
                ret.push(Inst::Mov(tmp, dst));
            }
            Inst::Binary(BinaryOperator::Mul, right, dst) if matches!(dst, Operand::Stack(_)) => {
                let tmp = Operand::Register(Register::R11);
                ret.push(Inst::Mov(dst.clone(), tmp.clone()));
                ret.push(Inst::Binary(BinaryOperator::Mul, right, tmp.clone()));
                ret.push(Inst::Mov(tmp, dst));
            }
            Inst::Binary(op, right, dst)
                if matches!(right, Operand::Stack(_)) && matches!(dst, Operand::Stack(_)) =>
            {
                let tmp = Operand::Register(Register::R10);
                ret.push(Inst::Mov(right, tmp.clone()));
                ret.push(Inst::Binary(op, tmp, dst));
            }
            Inst::Cmp(left, right)
                if matches!(left, Operand::Stack(_)) && matches!(right, Operand::Stack(_)) =>
            {
                let tmp = Operand::Register(Register::R10);
                ret.push(Inst::Mov(left, tmp.clone()));
                ret.push(Inst::Cmp(tmp, right));
            }
            Inst::Cmp(left, right) if matches!(right, Operand::Imm(_)) => {
                let tmp = Operand::Register(Register::R11);
                ret.push(Inst::Mov(right, tmp.clone()));
                ret.push(Inst::Cmp(left, tmp));
            }
            Inst::Idiv(dst) if matches!(dst, Operand::Imm(_)) => {
                let tmp = Operand::Register(Register::R10);
                ret.push(Inst::Mov(dst, tmp.clone()));
                ret.push(Inst::Idiv(tmp));
            }
            _ => ret.push(inst),
        }
    }
    ret
}
