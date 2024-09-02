use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct NumberLiteral {
    pub value: f64,
    pub range: Range<Position>,
}

impl From<NumberLiteral> for Node {
    fn from(value: NumberLiteral) -> Node {
        Node::NumberLiteral(value)
    }
}

impl GetRange for NumberLiteral {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}