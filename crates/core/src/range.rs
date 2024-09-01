use crate::position::Position;
use std::fmt::Debug;

#[derive(PartialEq, Eq, Clone, Copy, Hash, Debug)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

impl Range {
    pub fn new(start: Position, end: Position) -> Range {
        Range { start, end }
    }

    pub fn of(s0:usize, s1:usize, e0:usize, e1:usize) -> Range {
        Range::new(Position::new(s0, s1), Position::new(e0, e1))
    }

    pub fn to(&self, range: Range) -> Range {
        Range::new(self.start, range.end)
    }
}

impl PartialOrd for Range {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.start == other.start {
            self.end.partial_cmp(&other.end)
        } else {
            self.start.partial_cmp(&other.start)
        }
    }
}
