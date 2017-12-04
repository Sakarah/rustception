mod location;
mod lexer;
mod ast;
mod parser;
mod typ_ast;
mod typing;

use std::fs::File;
use lexer::Lexer;
use std::process::exit;

fn main()
{
    // Read the command line
    let mut filename = String::new();
    let mut parse_only = false;
    let mut type_only = false;
    let mut no_asm = false;
    for arg in std::env::args()
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
        println!("You must give a filename as argument");
        exit(1);
    }

    // Parse the file
    let file = File::open(&filename).unwrap_or_else(|err| {
        println!("Unable to open file '{}': {}", &filename, err);
        exit(1); });
    let lexer = Lexer::from_channel(file);
    let ast = parser::parse_Program(lexer).unwrap_or_else(|err| {
        println!("Parsing error: {}", err);
        exit(1); });
    if parse_only { exit(0); }

    // Check types
    let _ = typing::type_program(ast).unwrap_or_else(|err| {
        println!("Type error: {:?}", err);
        exit(1); });
    if type_only { exit(0); }

    // Run the borrow checker
    if no_asm { exit(0); }

    // Produce assembly
}
