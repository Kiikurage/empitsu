use crate::ast::node::Node;
use crate::ast::get_range::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    range: Range<Position>,
    pub nodes: Vec<Node>,
}

impl Block {
    pub fn new(range: Range<Position>, nodes: Vec<Node>) -> Self {
        Self { range, nodes }
    }
}

impl GetRange for Block {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}