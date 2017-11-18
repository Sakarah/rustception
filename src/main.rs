mod lexer;
mod location;

use std::fs::File;
use lexer::Lexer;
use location::Location;
use std::fmt;
use std::process::exit;

struct Locs(Location, Location);

impl fmt::Display for Locs
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        if self.0.line == self.1.line
        {
            write!(f, ":{}:{}-{}", self.0.line, self.0.column, self.1.column)
        }
        else
        {
            write!(f, ":{}:{} - :{}:{}", self.0.line, self.0.column,
                                         self.1.line, self.1.column)
        }
    }
}

fn main()
{
    let file = File::open("foo")
        .expect("unable to open file");
    let mut printer = Lexer::from_channel(file);
    loop {
        match printer.next() {
            Some(Ok((loc1, tok, loc2))) => {
                println!("{}, {}", tok, Locs(loc1, loc2));
            },
            Some(Err(err)) => { println!("{}", err); exit(1) },
            None => break
        }
    }
}