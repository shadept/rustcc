use crate::backend::assembler::{BinaryOperator, CondCode, Function, Inst, Operand, Program, Register, UnaryOperator};

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
            assemble_operand(src, 4),
            assemble_operand(dst, 4)
        ),
        Inst::Unary(op, dst) => match op {
            UnaryOperator::Neg => format!("    negl {}", assemble_operand(dst, 4)),
            UnaryOperator::Not => format!("    notl {}", assemble_operand(dst, 4)),
        },
        Inst::Binary(op, right, dst) => match op {
            BinaryOperator::Add => format!("    addl {}, {}", assemble_operand(right, 4), assemble_operand(dst, 4)),
            BinaryOperator::Sub => format!("    subl {}, {}", assemble_operand(right, 4), assemble_operand(dst, 4)),
            BinaryOperator::Mul => format!("    imull {}, {}", assemble_operand(right, 4), assemble_operand(dst, 4)),
            BinaryOperator::Or => format!("    orl {}, {}", assemble_operand(right, 4), assemble_operand(dst, 4)),
            BinaryOperator::And => format!("    andl {}, {}", assemble_operand(right, 4), assemble_operand(dst, 4)),
            BinaryOperator::Xor => format!("    xorl {}, {}", assemble_operand(right, 4), assemble_operand(dst, 4)),
        },
        Inst::Cmp(left, right) => format!("    cmpl {}, {}", assemble_operand(left, 4), assemble_operand(right, 4)),
        Inst::Idiv(o) => format!("    idivl {}", assemble_operand(o, 4)),
        Inst::Cdq => "    cdq".to_string(),
        Inst::Jmp(label) => format!("    jmp {}", assemble_label(label)),
        Inst::JmpCC(cc, label) => format!("    j{} {}", assemble_cc(cc), assemble_label(label)),
        Inst::SetCC(cc, dst) => format!("    set{} {}", assemble_cc(cc), assemble_operand(dst, 1)),
        Inst::Label(label) => format!("{}:", assemble_label(label)),
        Inst::AllocateStack(s) => format!("    subq ${s}, %rsp"),
        Inst::Ret => "    movq %rbp, %rsp\n    popq %rbp\n    ret".to_string(),
    }
}

fn assemble_operand(op: Operand, bits: u8) -> String {
    match op {
        Operand::Imm(i) => format!("${i}"),
        Operand::Pseudo(_) => panic!("Cannot generate code for pseudo-registers"),
        Operand::Register(Register::AX) => if bits == 4 { "%eax" } else { "%al" }.to_string(),
        Operand::Register(Register::DX) => if bits == 4 { "%edx" } else { "%dl" }.to_string(),
        Operand::Register(Register::R10) => if bits == 4 { "%r10d" } else { "%r10b" }.to_string(),
        Operand::Register(Register::R11) => if bits == 4 { "%r11d" } else { "%r11b" }.to_string(),
        Operand::Stack(offset) => format!("{offset}(%rbp)"),
    }
}

fn assemble_cc(cc: CondCode) -> &'static str {
    match cc {
        CondCode::E => "e",
        CondCode::NE => "ne",
        CondCode::L => "l",
        CondCode::LE => "le",
        CondCode::G => "g",
        CondCode::GE => "ge",
    }
}

fn assemble_label<T: AsRef<str>>(label: T) -> String {
    if cfg!(target_os = "macos") {
        format!("L{}", label.as_ref())
    } else {
        format!(".L{}", label.as_ref())
    }
}