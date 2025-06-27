use crate::backend::assembler::{
    BinaryOperator, Function, Inst, Operand, Program, Register, UnaryOperator,
};

pub fn codegen(assembly: Program) -> String {
    let mut code = assemble_function(assembly.function_definition);
    if cfg!(target_os = "linux") {
        code.push_str("\n   .section .note.GNU-stack,\"\",@progbits")
    }
    code
}

fn assemble_function(func: Function) -> String {
    let name = if cfg!(target_os = "macos") {
        format!("_{}", func.name)
    } else {
        func.name
    };

    let mut code = String::new();
    code.push_str(&format!("    .globl {}\n", name));
    code.push_str(&format!("{}:\n", name));
    code.push_str("    pushq %rbp\n");
    code.push_str("    movq %rsp, %rbp\n");
    for inst in func.body {
        code.push_str(&assemble_instruction(inst));
        code.push('\n');
    }

    code
}

fn assemble_instruction(inst: Inst) -> String {
    match inst {
        Inst::Mov(src, dst) => format!(
            "    movl {}, {}",
            assemble_operand(src),
            assemble_operand(dst)
        ),
        Inst::Ret => "    movq %rbp, %rsp\n    popq %rbp\n    ret".to_string(),
        Inst::Unary(op, dst) => match op {
            UnaryOperator::Neg => format!("    negl {}", assemble_operand(dst)),
            UnaryOperator::Not => format!("    notl {}", assemble_operand(dst)),
        },
        Inst::Binary(op, right, dst) => match op {
            BinaryOperator::Add => format!("    addl {}, {}", assemble_operand(right), assemble_operand(dst)),
            BinaryOperator::Sub => format!("    subl {}, {}", assemble_operand(right), assemble_operand(dst)),
            BinaryOperator::Mul => format!("    imull {}, {}", assemble_operand(right), assemble_operand(dst)),
            BinaryOperator::Or => format!("    orl {}, {}", assemble_operand(right), assemble_operand(dst)),
            BinaryOperator::And => format!("    andl {}, {}", assemble_operand(right), assemble_operand(dst)),
            BinaryOperator::Xor => format!("    xorl {}, {}", assemble_operand(right), assemble_operand(dst)),
        },
        Inst::Idiv(o) => format!("    idivl {}", assemble_operand(o)),
        Inst::Cdq => "    cdq".to_string(),
        Inst::AllocateStack(s) => format!("    subq ${s}, %rsp"),
    }
}

fn assemble_operand(op: Operand) -> String {
    match op {
        Operand::Imm(i) => format!("${i}"),
        Operand::Pseudo(_) => panic!("Cannot generate code for pseudo-registers"),
        Operand::Register(Register::AX) => "%eax".to_string(),
        Operand::Register(Register::DX) => "%edx".to_string(),
        Operand::Register(Register::R10) => "%r10d".to_string(),
        Operand::Register(Register::R11) => "%r11d".to_string(),
        Operand::Stack(offset) => format!("{offset}(%rbp)"),
    }
}
