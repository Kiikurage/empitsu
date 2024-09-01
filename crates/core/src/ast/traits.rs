use crate::position::Position;
use crate::range::Range;

pub trait GetRange {
    fn range(&self) -> Range;

    fn start(&self) -> Position {
        self.range().start
    }

    fn end(&self) -> Position {
        self.range().end
    }
}