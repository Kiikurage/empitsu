use crate::ast::get_range::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeIdentifier {
    range: Range<Position>,
    pub name: String,
}

impl TypeIdentifier {
    pub fn new(range: Range<Position>, name: impl Into<String>) -> Self {
        Self { range, name: name.into() }
    }
}

impl GetRange for TypeIdentifier {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}
