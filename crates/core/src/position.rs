use std::fmt::{Debug, Display};

#[inline(always)]
pub fn pos(line: usize, character: usize) -> Position {
    Position::new(line, character)
}

#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug, Ord)]
pub struct Position {
    pub line: usize,
    pub character: usize,
}

impl Position {
    pub fn new(line: usize, character: usize) -> Position {
        Position { line, character }
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.character + 1)
    }
}

impl PartialOrd for Position {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.line == other.line {
            self.character.partial_cmp(&other.character)
        } else {
            self.line.partial_cmp(&other.line)
        }
    }
}

