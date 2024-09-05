use crate::analysis::type_::Type;
use crate::ast::get_range::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Clone, Debug, PartialEq)]
pub struct VariableInfo {
    range: Range<Position>,
    pub name: String,
    pub type_: Type,
}

impl VariableInfo {
    pub fn new(range: Range<Position>, name: impl Into<String>, type_: Type) -> Self {
        VariableInfo { range, name: name.into(), type_ }
    }
}

impl GetRange for VariableInfo {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}