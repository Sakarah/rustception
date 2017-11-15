mod location;

use std::io::*;
use location::Location;
use std::str::Chars;
use std::iter::Peekable;

struct InputBuf<'a, R>
{
    location: Location,
    current_line: Peekable<Chars<'a>>,
    file: Lines<BufReader<R>>,
}

impl<'a, R: Read> InputBuf<'a, R>
{
    fn new<'b>(input: R) -> InputBuf<'b, R>
    {
        let mut file = BufReader::new(input).lines();
        let line_string = file.next()
            .expect("File is empty!")
            .unwrap()
            .clone();
        let current_line = line_string.chars();
        InputBuf
        {
            location: Location::new(0,0),
            file: file,
            current_line: current_line.peekable()
        }
    }
}

fn main() {}
