mod lexer;
mod location;

use std::fs::File;
use lexer::Lexer;
use location::Span;
use std::process::exit;

fn main()
{
    let filename = std::env::args().nth(1).unwrap_or_else(|| {
        println!("You must give a filename as argument");
        exit(1); } );
    let file = File::open(filename).unwrap_or_else(|err| {
        println!("Unable to open file: {}", err);
        exit(1); } );
    let mut lexer = Lexer::from_channel(file);
    loop {
        match lexer.next()
        {
            Some(Ok((loc1, tok, loc2))) =>
            {
                println!("{}, {}", tok, Span::new(loc1, loc2));
            },
            Some(Err(err)) => { println!("Lexing error: {}", err); exit(1) },
            None => break
        }
    }
}
