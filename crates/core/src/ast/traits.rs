use std::ops::Range;
use crate::position::Position;

pub trait GetRange {
    fn range(&self) -> Range<Position>;

    fn start(&self) -> Position {
        self.range().start
    }

    fn end(&self) -> Position {
        self.range().end
    }
}