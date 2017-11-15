use std::io;
use std::io::Write;

mod lexer;

fn main() {

    loop
    {
        print!(">>> ");
        io::stdout().flush().unwrap();
        let mut input = String::new();

        io::stdin().read_line(&mut input)
               .expect("Failed to read line.");

        let mut scanner = lexer::Lexer::new(input.chars());

        loop {
        	match scanner.next() {
        		Some(token) => print!("{} ", token),
        		None => break
        	};
        }
        println!("");
    }
}
