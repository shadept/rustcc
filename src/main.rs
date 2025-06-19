mod backend;
mod frontend;

use crate::backend::codegen::codegen;
use crate::frontend::ast::Program;
use crate::frontend::lexer::Lexer;
use crate::frontend::parser::{Parser, ParserError};
use crate::frontend::token::Token;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io;
use std::io::{Read, Write, stdout};
use std::process::Command;
use crate::backend::assembler;
use crate::backend::assembler::assemble;
use crate::backend::tacky::emit_tacky;

#[derive(Ord, PartialOrd, Eq, PartialEq, Debug)]
enum CommandArg {
    Lex,
    Parse,
    Tacky,
    Codegen,
    CodeEmit,
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

fn main() -> Result<(), anyhow::Error> {
    let args: Vec<String> = std::env::args().collect();
    let executable_name = &args[0];
    let command: CommandArg;
    let input_file: &str;

    match args.len() {
        2 => {
            command = CommandArg::CodeEmit;
            input_file = &args[1];
        }
        3 => {
            match args[1].as_str() {
                "--lex" => command = CommandArg::Lex,
                "--parse" => command = CommandArg::Parse,
                "--tacky" => command = CommandArg::Tacky,
                "--codegen" => command = CommandArg::Codegen,
                _ => {
                    print_usage(&args[1])?;
                    return Err(MainError {
                        message: "Unknown command.",
                    }
                    .into());
                }
            }
            input_file = &args[2];
        }
        _ => {
            print_usage(executable_name)?;
            return Err(MainError {
                message: "Invalid number of arguments.",
            }
            .into());
        }
    }

    if command >= CommandArg::Lex {
        let preprocessed_file = input_file.replace(".c", ".i");
        let tokens = run_lexer(input_file, &preprocessed_file)?;
        // dbg!(&tokens);
        if command >= CommandArg::Parse {
            let program = run_parser(tokens)?;
            // dbg!(&program);
            if command >= CommandArg::Tacky {
                let tacky = emit_tacky(program);
                dbg!(&tacky);
                // misnomer
                if command >= CommandArg::Codegen {
                    let assembly = assemble(tacky);
                    dbg!(&assembly);
                    if command >= CommandArg::CodeEmit {
                        let assembly_file = input_file.replace(".c", ".s");
                        run_codegen(assembly, &assembly_file)?;
                    }
                }
            }
        }
    }

    Ok(())
}

fn run_lexer(input_file: &str, preprocessed_file: &str) -> io::Result<Vec<Token>> {
    let mut file: File;
    if cfg!(windows) {
        file = File::open(input_file)?;
    } else {
        Command::new("clang")
            .args(["-E", "-P", input_file, "-o", preprocessed_file])
            .spawn()
            .expect("Failed to execute clang")
            .wait()?;
        file = File::open(input_file)?;
    }

    let file_length = file.metadata()?.len() as usize;
    let mut buffer = String::with_capacity(file_length);
    file.read_to_string(&mut buffer)?;
    if buffer.starts_with("\u{FEFF}") {
        // Skip BOM
        buffer.remove(0);
    }
    if buffer.starts_with("#!") {
        // Skip shebang
        buffer.drain(..buffer.find('\n').unwrap_or(buffer.len()));
    }
    drop(file); // force closes the file

    if !cfg!(windows) && false {
        std::fs::remove_file(preprocessed_file)?;
    }

    let src = buffer.as_str();
    let lexer = Lexer::new(src);
    let tokens = lexer.collect::<Vec<_>>();
    Ok(tokens)
}

fn run_parser(tokens: Vec<Token>) -> Result<Program, ParserError> {
    let mut parser = Parser::new(tokens);
    let program = parser.parse()?;
    Ok(program)
}

fn run_codegen(assembly: assembler::Program, assembly_file: &str) -> io::Result<()> {
    let code = codegen(assembly);
    File::create(assembly_file)?.write_all(code.as_bytes())?;

    // Command::new("clang")
    //     .args(["-S", assembly_file, "-o", assembly_file])
    //     .spawn()?
    //     .wait()?;
    // 
    // if false {
    //     std::fs::remove_file(assembly_file)?;
    // }

    Ok(())
}

fn print_usage(arg0: &str) -> io::Result<()> {
    let mut stdout = stdout().lock();
    let name = arg0.split('/').last().unwrap();
    write!(stdout, "Usage: {} <input file>", name)?;
    Ok(())
}
