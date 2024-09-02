use crate::ast::get_range::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct Break {
    range: Range<Position>,
}

impl Break {
    pub fn new(range: Range<Position>) -> Self {
        Self { range }
    }
}

impl GetRange for Break {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}