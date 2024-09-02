use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct BoolLiteral {
    pub value: bool,
    pub range: Range<Position>,
}

impl From<BoolLiteral> for Node {
    fn from(value: BoolLiteral) -> Node {
        Node::BoolLiteral(value)
    }
}

impl GetRange for BoolLiteral {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}