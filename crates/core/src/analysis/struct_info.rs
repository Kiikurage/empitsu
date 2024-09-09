use crate::analysis::type_::Type;
use crate::ast::get_range::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Clone, Debug, PartialEq)]
pub struct StructInfo {
    range: Range<Position>,
    pub name: String,
    pub properties: Vec<(String, Type)>,
}

impl StructInfo {
    pub fn new(range: Range<Position>, name: impl Into<String>, properties: Vec<(String, Type)>) -> Self {
        StructInfo { range, name: name.into(), properties }
    }
}

impl GetRange for StructInfo {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}
