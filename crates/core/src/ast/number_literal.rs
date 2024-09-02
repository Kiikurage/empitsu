use crate::ast::traits::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct NumberLiteral {
    range: Range<Position>,
    pub value: f64,
}

impl NumberLiteral {
    pub fn new(range: Range<Position>, value: f64) -> Self {
        Self { range, value }
    }
}

impl GetRange for NumberLiteral {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}