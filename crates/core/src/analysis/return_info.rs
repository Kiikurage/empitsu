use crate::analysis::type_::Type;
use crate::ast::get_range::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Clone, Debug, PartialEq)]
pub struct ReturnInfo {
    range: Range<Position>,
    pub return_value_type: Type,
}

impl ReturnInfo {
    pub fn new(range: Range<Position>, return_value_type: Type) -> Self {
        ReturnInfo { range, return_value_type }
    }
}

impl GetRange for ReturnInfo {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}
