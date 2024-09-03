use crate::analysis::type_::Type;
use crate::ast::get_range::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Clone, Debug, PartialEq)]
pub struct ExpressionInfo {
    range: Range<Position>,
    pub type_: Type,
}

impl ExpressionInfo {
    pub fn new(range: Range<Position>, type_: Type) -> Self {
        ExpressionInfo { range, type_ }
    }
}

impl GetRange for ExpressionInfo {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}
