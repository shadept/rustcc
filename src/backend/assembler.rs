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
            tacky::UnaryOperator::Complement => UnaryOperator::Not,
            tacky::UnaryOperator::Negate => UnaryOperator::Neg,
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Register {
    AX,
    R10,
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
        Instruction::Unary(op, src, dst) => {
            let src = assemble_val(src);
            let dst = assemble_val(dst);
            instructions.push(Inst::Mov(src, dst.clone()));
            instructions.push(Inst::Unary(UnaryOperator::from(op), dst));
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

                if matches!(src, Operand::Stack(_)) && matches!(dst, Operand::Stack(_)) {
                    let tmp = Operand::Register(Register::R10);
                    ret.push(Inst::Mov(src, tmp.clone()));
                    ret.push(Inst::Mov(tmp, dst));
                } else {
                    ret.push(Inst::Mov(src, dst));
                }
            }
            Inst::Unary(op, dst) => {
                let dst = if let Operand::Pseudo(pseudo) = dst {
                    Operand::Stack(allocate_stack(pseudo, allocation))
                } else {
                    dst
                };
                ret.push(Inst::Unary(op, dst));
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
