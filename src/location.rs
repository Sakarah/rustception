
#[derive(PartialEq, Clone, Debug)]
pub struct Location
{
    pub offset: u32,
	pub line: u32,
	pub column: u32,
}

impl Location
{
	pub fn new(offset: u32, line: u32, column: u32) -> Location
	{
		Location {
			line: line,
            column: column,
            offset: offset,
        }
	}

    // Extend the location by one character
    pub fn extend(&mut self, c: char)
    {
        match c {
            '\n' => {
                self.line += 1;
                self.column = 0;
                self.offset += 1
            },
            _ => {
                self.column += 1;
                self.offset += 1
            },
        }
    }
}