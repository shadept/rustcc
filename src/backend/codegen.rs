use crate::backend::ast::{Assembly, Function, Inst, Operand};

pub fn codegen(assembly: Assembly) -> String {
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
        Inst::Ret => "    ret".to_string(),
    }
}

fn assemble_operand(op: Operand) -> String {
    match op {
        Operand::Imm(i) => format!("${i}"),
        Operand::Register => "%eax".to_string(),
    }
}
