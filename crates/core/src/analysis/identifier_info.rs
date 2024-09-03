use crate::ast::get_range::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Clone, Debug, PartialEq)]
pub struct IdentifierInfo {
    range: Range<Position>,
    pub name: String,
    pub defined_at: Option<Range<Position>>,
}

impl IdentifierInfo {
    pub fn new(range: Range<Position>, name: impl Into<String>, defined_at: Option<Range<Position>>) -> Self {
        IdentifierInfo { range, name: name.into(), defined_at }
    }
}

impl GetRange for IdentifierInfo {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}
