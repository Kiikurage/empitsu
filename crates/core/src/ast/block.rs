use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::range::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub nodes: Vec<Node>,
    pub range: Range,
}

impl From<Block> for Node {
    fn from(value: Block) -> Node {
        Node::Block(value)
    }
}

impl GetRange for Block {
    fn range(&self) -> Range {
        self.range
    }
}