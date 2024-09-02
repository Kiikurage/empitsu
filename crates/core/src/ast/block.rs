use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub nodes: Vec<Node>,
    pub range: Range<Position>,
}

impl From<Block> for Node {
    fn from(value: Block) -> Node {
        Node::Block(value)
    }
}

impl GetRange for Block {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}