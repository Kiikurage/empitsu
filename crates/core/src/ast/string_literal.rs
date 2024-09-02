use std::ops::Range;
use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteral {
    pub value: String,
    pub range: Range<Position>,
}

impl From<StringLiteral> for Node {
    fn from(value: StringLiteral) -> Node {
        Node::StringLiteral(value)
    }
}

impl GetRange for StringLiteral {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}