use std::fmt;

//   _                    _   _
//  | |    ___   ___ __ _| |_(_) ___  _ __
//  | |   / _ \ / __/ _` | __| |/ _ \| '_ \
//  | |__| (_) | (_| (_| | |_| | (_) | | | |
//  |_____\___/ \___\__,_|\__|_|\___/|_| |_|
//
#[derive(PartialEq, Clone, Copy, Debug)]
pub struct Location
{
    pub offset: u32,
    pub line: u32,
    pub column: u32,
}

impl Location
{
    pub fn new(offset: u32, line: u32, column: u32) -> Self
    {
        Location {
            line: line,
            column: column,
            offset: offset,
        }
    }
}

impl Default for Location
{
    fn default() -> Self
    {
        Location::new(0,1,1)
    }
}

impl fmt::Display for Location
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, ":{}:{}", self.line, self.column)
    }
}


//   ____
//  / ___| _ __   __ _ _ __
//  \___ \| '_ \ / _` | '_ \
//   ___) | |_) | (_| | | | |
//  |____/| .__/ \__,_|_| |_|
//        |_|
#[derive(Default)]
pub struct Span
{
    pub start: Location,
    pub end: Location
}

impl Span
{
    pub fn new(start:Location, end:Location) -> Self
    {
        Span{start, end}
    }

    /// Extend the location by one character
    pub fn extend(&mut self, c: u8)
    {
        self.end.offset += 1;
        match c {
            b'\n' => {
                self.end.line += 1;
                self.end.column = 0
            },
            _ => self.end.column += 1,
        }
    }

    /// Reduce the location to it's last character.
    pub fn reduce(&mut self)
    {
        self.start.line = self.end.line;
        self.start.column = self.end.column;
        self.start.offset = self.end.offset;
    }
}

impl fmt::Display for Span
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        if self.start.line == self.end.line
        {
            write!(f, ":{}:{}-{}", self.start.line,
                                   self.start.column, self.end.column)
        }
        else
        {
            write!(f, ":{}:{} - :{}:{}", self.start.line, self.start.column,
                                         self.end.line, self.end.column)
        }
    }
}

