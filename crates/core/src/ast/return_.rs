use std::ops::Range;
use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub value: Option<Box<Node>>,
    pub range: Range<Position>,
}

impl From<Return> for Node {
    fn from(value: Return) -> Node {
        Node::Return(value)
    }
}

impl GetRange for Return {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}