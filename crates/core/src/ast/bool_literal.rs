use crate::ast::get_range::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct BoolLiteral {
    range: Range<Position>,
    pub value: bool,
}

impl BoolLiteral {
    pub fn new(range: Range<Position>, value: bool) -> Self {
        Self { range, value }
    }
}

impl GetRange for BoolLiteral {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}