mod backend;
mod frontend;

use crate::backend::analysis::{SemanticError, resolve_program};
use crate::backend::assembler;
use crate::backend::assembler::assemble;
use crate::backend::codegen::codegen;
use crate::backend::tacky::emit_tacky;
use crate::frontend::ast::Program;
use crate::frontend::lexer::{Lexer, LexerError};
use crate::frontend::parser::{Parser, ParserError};
use crate::frontend::source::{FileName, SourceFile};
use crate::frontend::token::Token;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io;
use std::io::{Read, Write, stdout};
use std::path::PathBuf;
use std::process::{Command, exit};
use std::sync::Arc;

#[derive(Ord, PartialOrd, Eq, PartialEq, Debug, Clone, Copy)]
enum CompilationStage {
    Lex,
    Parse,
    Validate,
    Tacky,
    Codegen,
    CodeEmit,
}

impl CompilationStage {
    // Parse from command line argument
    fn from_arg(arg: &str) -> Option<Self> {
        match arg {
            "--lex" => Some(Self::Lex),
            "--parse" => Some(Self::Parse),
            "--validate" => Some(Self::Validate),
            "--tacky" => Some(Self::Tacky),
            "--codegen" => Some(Self::Codegen),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct MainError {
    message: &'static str,
}

impl Display for MainError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for MainError {}

/// Entry point for the CLI-driven compiler pipeline.
///
/// This function parses command-line arguments, selects a compilation stage, and runs the input
/// C source file through the sequential pipeline:
/// Lex -> Parse -> Validate -> Tacky IR -> Codegen (assembly) -> Code emission.
///
/// Command-line forms:
/// - `program <input.c>` — run full pipeline (CodeEmit).
/// - `program --lex <input.c>`
/// - `program --parse <input.c>`
/// - `program --validate <input.c>`
/// - `program --tacky <input.c>`
/// - `program --codegen <input.c>`
///
/// Behavior:
/// - For stage-specific invocations, the function runs the pipeline up to that stage and then
///   prints or emits the stage output as appropriate (e.g., token kinds for `--lex`, pretty
///   printed AST for `--parse`).
/// - On invalid arguments or unknown stage flags the function prints usage and returns an error.
///
/// Returns:
/// - `Ok(())` on successful completion of the requested stage (including stage-limited runs).
/// - `Err(anyhow::Error)` if argument parsing fails or any pipeline step returns an error.
///
/// Side effects:
/// - Reads the input file (and a preprocessed `.i` file on non-Windows), may write assembly and
///   executable files, and may invoke the system C toolchain to produce an executable.
///
/// # Examples
///
/// ```no_run
/// // Run full compilation on `example.c` (equivalent to invoking the compiled binary)
/// // $ program example.c
/// let _ = std::env::set_var("RUST_BACKTRACE", "0");
/// // Note: running `main()` directly in doctests is `no_run` to avoid side effects.
/// ```
fn main() -> Result<(), anyhow::Error> {
    // Enable ANSI colors on Windows
    #[cfg(windows)]
    colored::control::set_virtual_terminal(true).unwrap_or(());

    // Parse command line arguments
    let args: Vec<String> = std::env::args().collect();
    let executable_name = &args[0];

    // Determine compilation stage and input file
    let (target_stage, input_file) = match args.len() {
        2 => (CompilationStage::CodeEmit, &args[1]),
        3 => {
            let stage = CompilationStage::from_arg(&args[1]).ok_or_else(|| {
                // Print usage and return error for unknown command
                let _ = print_usage(&args[1]);
                MainError {
                    message: "Unknown command.",
                }
            })?;
            (stage, &args[2])
        }
        _ => {
            // Print usage and return error for invalid number of arguments
            let _ = print_usage(executable_name);
            return Err(MainError {
                message: "Invalid number of arguments.",
            }
            .into());
        }
    };

    // Process the input file through the compilation pipeline

    // Stage 1: Lexing
    let preprocessed_file = input_file.replace(".c", ".i");
    let (tokens, source_file) = run_lexer(input_file, &preprocessed_file)?;
    if target_stage == CompilationStage::Lex {
        for token in tokens.iter() {
            println!("{:?}", token.kind);
        }
        return Ok(());
    }

    // Stage 2: Parsing
    let program = run_parser(tokens, source_file.clone());
    if target_stage == CompilationStage::Parse {
        program.pretty_print(stdout())?;
        return Ok(());
    }

    // Stage 3: Perform Semantic Analysis
    let validated_program = run_validate(program, source_file);
    if target_stage == CompilationStage::Validate {
        return Ok(());
    }

    // Stage 4: Tacky IR generation
    let tacky = emit_tacky(validated_program);
    if target_stage == CompilationStage::Tacky {
        return Ok(());
    }

    // Stage 5: Assembly generation
    let assembly = assemble(tacky);
    if target_stage == CompilationStage::Codegen {
        return Ok(());
    }

    // Stage 6: Code emission
    let assembly_file = input_file.replace(".c", ".s");
    let executable_file = input_file.replace(".c", "");
    run_code_emission(assembly, &assembly_file, &executable_file)?;

    Ok(())
}

/// Tokenizes the given input file (optionally after running the C preprocessor) and returns the token list and a shared SourceFile.
///
/// On non-Windows targets this runs `clang -E -P <input_file> -o <preprocessed_file>` and tokenizes the preprocessed output; on Windows it tokenizes the original input file directly.
/// Returns `Ok((tokens, Arc<SourceFile>))` on success. If external preprocessing or file operations fail this function returns an `Err` (from the underlying I/O/command wait). If lexical analysis fails, a diagnostic is printed to stderr and the process exits with code 1.
///
/// # Examples
///
/// ```no_run
/// use std::sync::Arc;
/// // assume `run_lexer` is in scope
/// let (tokens, source) = run_lexer("example.c", "example.i").expect("lexing failed");
/// assert!(!tokens.is_empty());
/// let _source: Arc<_> = source;
/// ```
fn run_lexer(
    input_file: &str,
    preprocessed_file: &str,
) -> anyhow::Result<(Vec<Token>, Arc<SourceFile>)> {
    // Preprocess the file if not on Windows
    if !cfg!(windows) {
        Command::new("clang")
            .args(["-E", "-P", input_file, "-o", preprocessed_file])
            .spawn()
            .expect("Failed to execute clang")
            .wait()?;
    }

    // Open the appropriate file
    let file_path = if cfg!(windows) {
        input_file
    } else {
        preprocessed_file
    };

    // Keep around until windows msvc is supported
    if !cfg!(windows) && false {
        std::fs::remove_file(preprocessed_file)?;
    }

    // Create a SourceFile
    let source_file = SourceFile::new_from_file(PathBuf::from(file_path));
    let source_file = Arc::new(source_file);
    let lexer = Lexer::new(source_file.clone());
    match lexer.to_tokens() {
        Ok(tokens) => Ok((tokens, source_file)),
        Err(err) => {
            // Report the error with diagnostics
            let diagnostic = err.diagnostic(source_file.clone());
            eprintln!("{}", diagnostic);
            exit(1);
        }
    }
}

fn run_parser(tokens: Vec<Token>, source_file: Arc<SourceFile>) -> Program {
    let mut parser = Parser::new(tokens, source_file.clone());
    match parser.parse() {
        Ok(program) => program,
        Err(err) => {
            // Report the error with diagnostics
            let diagnostic = err.diagnostic();
            eprintln!("{}", diagnostic);
            exit(1);
        }
    }
}

fn run_validate(program: Program, source_file: Arc<SourceFile>) -> Program {
    match resolve_program(program) {
        Ok(validated_program) => validated_program,
        Err(err) => {
            // Report the error with diagnostics
            let diagnostic = err.diagnostic();
            eprintln!("{}", diagnostic);
            exit(1);
        }
    }
}

fn run_code_emission(
    assembly: assembler::Program,
    assembly_file: &str,
    executable_file: &str,
) -> io::Result<()> {
    // Generate assembly code and write to file
    let code = codegen(assembly);
    File::create(assembly_file)?.write_all(code.as_bytes())?;

    // On non-Windows platforms, use clang to create the executable
    if !cfg!(windows) {
        Command::new("clang")
            .args([assembly_file, "-o", executable_file, "-arch", "x86_64"])
            .spawn()?
            .wait()?;

        // Keep around until windows msvc is supported
        if false {
            std::fs::remove_file(assembly_file)?;
        }
    }

    Ok(())
}

fn print_usage(arg0: &str) -> io::Result<()> {
    let mut stdout = stdout().lock();
    let name = arg0.split('/').last().unwrap();
    write!(stdout, "Usage: {} <input file>", name)?;
    Ok(())
}
