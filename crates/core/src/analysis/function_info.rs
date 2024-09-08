use std::ops::Range;
use crate::analysis::type_::Type;
use crate::ast::get_range::GetRange;
use crate::position::Position;

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionInfo {
    range: Range<Position>,
    name: String,
    pub type_: Type,
}

impl FunctionInfo {
    pub fn new(range: Range<Position>, name: impl Into<String>, type_: Type) -> Self {
        FunctionInfo { range, name: name.into(), type_ }
    }
}

impl GetRange for FunctionInfo {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}