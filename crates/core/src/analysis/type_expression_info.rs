use crate::analysis::type_::Type;
use crate::ast::get_range::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Clone, Debug, PartialEq)]
pub struct TypeExpressionInfo {
    range: Range<Position>,
    pub type_: Type,
}

impl TypeExpressionInfo {
    pub fn new(range: Range<Position>, type_: Type) -> Self {
        TypeExpressionInfo { range, type_ }
    }
}

impl GetRange for TypeExpressionInfo {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}
