use crate::backend::common::{make_label, make_temporary};
use crate::frontend::ast;
use crate::frontend::ast::StmtKind;

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

impl From<ast::UnaryOp> for UnaryOperator {
    /// Convert an `ast::UnaryOp` into the corresponding `UnaryOperator`.
    ///
    /// # Examples
    ///
    /// ```
    /// let op = ast::UnaryOp::Negate;
    /// let internal = UnaryOperator::from(op);
    /// assert!(matches!(internal, UnaryOperator::Negate));
    /// ```
    fn from(value: ast::UnaryOp) -> Self {
        match value {
            ast::UnaryOp::Complement => UnaryOperator::Complement,
            ast::UnaryOp::Negate => UnaryOperator::Negate,
            ast::UnaryOp::Not => UnaryOperator::Not,
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

impl From<ast::BinaryOp> for BinaryOperator {
    /// Convert an `ast::BinaryOp` into the corresponding `BinaryOperator` variant.
    ///
    /// Maps each AST binary operator to the backend's `BinaryOperator`. Panics if given
    /// an AST variant that has no corresponding backend operator.
    ///
    /// # Examples
    ///
    /// ```
    /// let op = BinaryOperator::from(ast::BinaryOp::Add);
    /// matches!(op, BinaryOperator::Add);
    /// ```
    fn from(value: ast::BinaryOp) -> Self {
        match value {
            ast::BinaryOp::Add => BinaryOperator::Add,
            ast::BinaryOp::Subtract => BinaryOperator::Subtract,
            ast::BinaryOp::Multiply => BinaryOperator::Multiply,
            ast::BinaryOp::Divide => BinaryOperator::Divide,
            ast::BinaryOp::Remainder => BinaryOperator::Remainder,
            ast::BinaryOp::BitwiseOr => BinaryOperator::BitwiseOr,
            ast::BinaryOp::BitwiseAnd => BinaryOperator::BitwiseAnd,
            ast::BinaryOp::BitwiseXor => BinaryOperator::BitwiseXor,
            ast::BinaryOp::Equal => BinaryOperator::Equal,
            ast::BinaryOp::NotEqual => BinaryOperator::NotEqual,
            ast::BinaryOp::LessThan => BinaryOperator::LessThan,
            ast::BinaryOp::LessThanOrEqual => BinaryOperator::LessThanOrEqual,
            ast::BinaryOp::GreaterThan => BinaryOperator::GreaterThan,
            ast::BinaryOp::GreaterThanOrEqual => BinaryOperator::GreaterThanOrEqual,
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
        emit_tacky_stmt(body, &mut instructions);
    }
    instructions.push(Instruction::Return(Val::Constant(0))); // optimization will remove extra return if another return is present
    Function::new(function.name, instructions)
}

/// Dispatches a single AST `BlockItem` to the appropriate emitter, appending any generated
/// Tacky `Instruction`s into `instructions`.
///
/// - If `item` is a declaration, `emit_tacky_decl` is invoked.
/// - If `item` is a statement, `emit_tacky_stmt` is invoked.
///
/// # Examples
///
/// ```no_run
/// let item: ast::BlockItem = /* obtained from parsing */ unimplemented!();
/// let mut instructions: Vec<Instruction> = Vec::new();
/// emit_tacky_block_item(item, &mut instructions);
/// // `instructions` now contains any instructions produced for `item`.
/// ```
fn emit_tacky_block_item(item: ast::BlockItem, instructions: &mut Vec<Instruction>) {
    match item {
        ast::BlockItem::Decl(decl) => emit_tacky_decl(decl, instructions),
        ast::BlockItem::Stmt(stmt) => emit_tacky_stmt(stmt, instructions),
    }
}

/// Emit TAC for a declaration (currently only variable declarations with an initializer).
///
/// If `decl` is a `DeclKind::Variable(name, Some(init))`, this constructs an assignment
/// expression `name = init` and emits TAC for that expression, appending resulting
/// instructions to `instructions`. Declarations without an initializer are a no-op.
///
/// Notes:
/// - The artificial assignment expression uses `decl.span` as its span (TODO: use the
///   variable-name span).
///
/// # Examples
///
/// ```
/// // Construct a variable declaration `let x = 42;` and emit into an instruction vector.
/// let decl = ast::Decl::new(ast::DeclKind::Variable("x".to_string(), Some(ast::Expr::new(ast::ExprKind::Constant(42), Span::default()).into())), Span::default());
/// let mut instrs = Vec::new();
/// emit_tacky_decl(decl, &mut instrs);
/// // `instrs` now contains instructions that assign 42 to `x`.
/// ```
fn emit_tacky_decl(decl: ast::Decl, instructions: &mut Vec<Instruction>) {
    match decl.kind {
        ast::DeclKind::Variable(name, init) => {
            if let Some(init) = init {
                let span = decl.span.clone();
                let tmp_expr = ast::Expr::new(
                    ast::ExprKind::Assignment(
                        ast::Expr::new(ast::ExprKind::Var(name), span.clone()).into(), // TODO decl.span should be the span of the variable name
                        init.into(),
                    ),
                    span,
                );
                emit_tacky_expr(tmp_expr, instructions);
            }
        }
    };
}

/// Emit Tacky IR instructions for a single AST statement.
///
/// Translates the given `ast::Stmt` into one or more `Instruction`s and appends
/// them to `instructions`. Control-flow constructs (e.g., `if`) are lowered to
/// labels and conditional jumps; expression statements and `return` are emitted
/// by delegating to `emit_tacky_expr`. Several loop/flow kinds (break,
/// continue, `do..while`, `for`, `while`) are treated as no-ops in the current
/// emitter.
///
/// # Parameters
///
/// - `stmt`: the AST statement to lower.
/// - `instructions`: mutable vector that receives appended Tacky instructions.
///
/// # Examples
///
/// ```
/// let mut instructions = Vec::new();
/// // `some_stmt` is an `ast::Stmt` previously constructed by the frontend.
/// // emit_tacky_stmt(some_stmt, &mut instructions);
/// ```
fn emit_tacky_stmt(stmt: ast::Stmt, instructions: &mut Vec<Instruction>) {
    match stmt.kind {
        StmtKind::Break(_) => {}
        StmtKind::Continue(_) => {}
        StmtKind::Compound(block) => {
            for item in block {
                emit_tacky_block_item(item, instructions);
            }
        }
        StmtKind::DoWhile(_, _, _) => {}
        StmtKind::Expr(expr) => {
            emit_tacky_expr(*expr, instructions);
        }
        StmtKind::For(_, _, _, _, _) => {}
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
        StmtKind::While(_, _, _) => {}
    };
}

/// Emit Tacky IR for an AST expression, appending resulting instructions to `instructions`.
///
/// Returns a `Val` that represents where the expression's result can be found (a constant or a variable).
/// Side effects: this function appends one or more `Instruction`s to the supplied `instructions` vector as it
/// lowers the AST expression into the TAC-like Tacky IR.
///
/// Behavior notes:
/// - Generates short-circuit control flow for logical `And` and `Or`.
/// - Emits conditional and unconditional jumps, labels, copies, unary/binary ops, and returns as required by the expression.
/// - For assignment to a variable (`lhs` is `Var`), emits code to evaluate the RHS and copies the result into the named variable, returning that variable.
/// - If an assignment's left-hand side is not a variable, this function panics with `"invalid assignment"`.
///
/// Parameters:
/// - `expr`: the AST expression to lower (consumed).
/// - `instructions`: mutable instruction buffer to which emitted Tacky instructions are appended.
///
/// Returns:
/// - `Val` indicating where the result resides (either `Val::Constant(i32)` or `Val::Var(String)`).
///
/// Panics:
/// - Panics on invalid assignment LHS (non-variable).
/// - May panic if other invariants assumed by the emitter are violated.
fn emit_tacky_expr(expr: ast::Expr, instructions: &mut Vec<Instruction>) -> Val {
    use crate::backend::tacky::Val::{Constant, Var};
    match expr.kind {
        ast::ExprKind::Binary(ast::BinaryOp::And, left, right) => {
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
        ast::ExprKind::Binary(ast::BinaryOp::Or, left, right) => {
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
        ast::ExprKind::Binary(op, left, right) => {
            let left = emit_tacky_expr(*left, instructions);
            let right = emit_tacky_expr(*right, instructions);
            let dst_name = make_temporary();
            let dst = Var(dst_name);
            instructions.push(Instruction::Binary(op.into(), left, right, dst.clone()));
            dst
        }
        ast::ExprKind::Cond(cond, if_true, if_false) => {
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
        ast::ExprKind::Constant(c) => Constant(c),
        ast::ExprKind::Return(maybe_inner) => {
            let src = match maybe_inner {
                Some(inner) => emit_tacky_expr(*inner, instructions),
                None => Constant(0),
            };
            instructions.push(Instruction::Return(src.clone()));
            src
        }
        ast::ExprKind::Unary(op, inner) => {
            let src = emit_tacky_expr(*inner, instructions);
            let dst_name = make_temporary();
            let dst = Var(dst_name);
            instructions.push(Instruction::Unary(op.into(), src, dst.clone()));
            dst
        }
        ast::ExprKind::Var(name) => Var(name),
        ast::ExprKind::Assignment(lhs, rhs) => match lhs.kind {
            ast::ExprKind::Var(name) => {
                let result = emit_tacky_expr(*rhs, instructions);
                instructions.push(Instruction::Copy(result, Var(name.clone())));
                Var(name)
            }
            _ => panic!("invalid assignment"),
        },
    }
}
