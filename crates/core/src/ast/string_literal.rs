use crate::ast::traits::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteral {
    range: Range<Position>,
    pub value: String,
}

impl StringLiteral {
    pub fn new(range: Range<Position>, value: impl Into<String>) -> Self {
        Self { range, value: value.into() }
    }
}

impl GetRange for StringLiteral {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}