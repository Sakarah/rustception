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

const DEFAULT_LOCATION : Location = Location{offset:0, line:1, column:1};
const NOWHERE : Location = Location{offset:0, line:0, column:0};

impl Default for Location
{
    fn default() -> Self { DEFAULT_LOCATION }
}

impl fmt::Display for Location
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(f, "line {}, character {}", self.line, self.column)
    }
}


//   ____
//  / ___| _ __   __ _ _ __
//  \___ \| '_ \ / _` | '_ \
//   ___) | |_) | (_| | | | |
//  |____/| .__/ \__,_|_| |_|
//        |_|
#[derive(Default, Clone, Copy)]
pub struct Span
{
    pub start: Location,
    pub end: Location
}

pub const EMPTY_SPAN : Span = Span{start:NOWHERE, end:NOWHERE};

impl Span
{
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
            write!(f, "line {}, characters {}-{}", self.start.line,
                                   self.start.column, self.end.column)
        }
        else
        {
            write!(f, "{}-{}", self.start, self.end)
        }
    }
}

