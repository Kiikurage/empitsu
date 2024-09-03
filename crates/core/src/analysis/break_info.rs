use crate::ast::get_range::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Clone, Debug, PartialEq)]
pub struct BreakInfo {
    range: Range<Position>,
    pub scope_range: Range<Position>,
}

impl BreakInfo {
    pub fn new(range: Range<Position>, scope_range: Range<Position>) -> Self {
        BreakInfo { range, scope_range }
    }
}

impl GetRange for BreakInfo {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}
