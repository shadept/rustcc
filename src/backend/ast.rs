#[derive(Debug, Clone, PartialEq)]
pub struct Assembly {
    pub function_definition: Function,
}

impl Assembly {
    pub fn new(function_definition: Function) -> Self {
        Self {
            function_definition,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub body: Vec<Inst>,
}

impl Function {
    pub fn new(name: String, body: Vec<Inst>) -> Self {
        Self { name, body }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Inst {
    /// Src, Dst
    Mov(Operand, Operand),
    Ret,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Imm(i64),
    Register,
}
