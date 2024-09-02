use crate::ast::traits::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    range: Range<Position>,
    pub name: String,
}

impl Identifier {
    pub fn new(range: Range<Position>, name: impl Into<String>) -> Self {
        Self { range, name: name.into() }
    }
}

impl GetRange for Identifier {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}