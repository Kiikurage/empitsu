use std::fmt::{Debug, Display};

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl From<(usize, usize)> for Position {
    fn from(value: (usize, usize)) -> Position {
        Position::new(value.0, value.1)
    }
}

impl Position {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.column + 1)
    }
}

impl PartialOrd for Position {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.line == other.line {
            self.column.partial_cmp(&other.column)
        } else {
            self.line.partial_cmp(&other.line)
        }
    }
}
