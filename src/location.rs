
#[derive(PartialEq, Clone, Debug)]
pub struct Location
{
	start_line: u32,
	start_column: u32,
	end_line: u32,
	end_column: u32,
}

impl Location
{
	pub fn new(line: u32, column: u32) -> Location
	{
		Location {
			start_line: line,
            start_column: column,
            end_line: line,
            end_column: column
        }
	}

    pub fn reduce(&mut self)
    {
        self.start_line = self.end_line;
        self.start_column = self.end_column;
    }

    pub fn extend(&mut self, c: Option<char>)
    {
        match c {
            Some('\n') => { self.end_line += 1; self.end_column = 0 },
            Some(_) => self.end_column += 1,
            None => ()
        }
    }
}
