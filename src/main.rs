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
mod alloc_ast;
mod allocator;

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
    let prgm = parser::parse_Program(lexer).unwrap_or_else(|err| {
        eprintln!("{}", Error::ParsingError(err));
        exit(1); });
    if parse_only { exit(0); }

    // Check types
    let typ_prgm = typing::type_program(prgm).unwrap_or_else(|err| {
        eprintln!("{}", Error::TypingError(err));
        exit(1); });
    if type_only { exit(0); }

    // Run the borrow checker
    let bc_prgm = borrow_checker::check_program(typ_prgm).unwrap_or_else(|err| {
        eprintln!("{}", Error::BorrowCheckingError(err));
        exit(1); });
    if no_asm { exit(0); }

    // Produce allocated program
    let alloc_prgm = allocator::allocate_program(bc_prgm);

    // Produce assembly
}
