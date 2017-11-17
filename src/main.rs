mod lexer;
mod location;

use std::fs::File;
use lexer::Lexer;

fn main()
{
	let file = File::open("foo")
		.expect("Unable to open file foo");
	let mut printer = Lexer::from_channel(file);
	loop {
		match printer.next() {
			Some(token) => println!("{}", token),
			None => break
		}
	}
}
