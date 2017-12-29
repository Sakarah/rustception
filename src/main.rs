mod symbol;
mod location;
mod lexer;
mod ast;
mod parser;
mod typ_ast;
mod typing;
mod bc_ast;
mod borrow_checker;
mod error;

use std::fs::File;
use lexer::Lexer;
use error::Error;
use std::process::exit;

extern crate lalrpop_util;

fn main()
{
    // Read the command line
    let mut filename = String::new();
    let mut parse_only = false;
    let mut type_only = false;
    let mut no_asm = false;
    for arg in std::env::args().skip(1)
    {
        match arg.as_ref()
        {
            "--parse-only" => parse_only = true,
            "--type-only" => type_only = true,
            "--no-asm" => no_asm = true,
            _ => filename = arg
        }
    }
    if filename == ""
    {
        eprintln!("{}", Error::NoInputFile);
        exit(1);
    }

    // Parse the file
    let file = File::open(&filename).unwrap_or_else(|err| {
        eprintln!("{}", Error::OpenFileError(filename.clone(), err));
        exit(1); });
    let lexer = Lexer::from_channel(file, filename);
    let ast = parser::parse_Program(lexer).unwrap_or_else(|err| {
        eprintln!("{}", Error::ParsingError(err));
        exit(1); });
    if parse_only { exit(0); }

    // Check types
    let typ_ast = typing::type_program(ast).unwrap_or_else(|err| {
        eprintln!("{}", Error::TypingError(err));
        exit(1); });
    if type_only { exit(0); }

    // Run the borrow checker
    let _ = borrow_checker::check_program(typ_ast).unwrap_or_else(|err| {
        eprintln!("{}", Error::BorrowCheckingError(err));
        exit(1); });
    if no_asm { exit(0); }

    // Produce assembly
}
