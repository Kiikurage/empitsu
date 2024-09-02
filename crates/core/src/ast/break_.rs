use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct Break {
    pub range: Range<Position>,
}

impl From<Break> for Node {
    fn from(value: Break) -> Node {
        Node::Break(value)
    }
}

impl GetRange for Break {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}