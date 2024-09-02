use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Node>,
    pub range: Range<Position>,
}

impl From<Program> for Node {
    fn from(value: Program) -> Node {
        Node::Program(value)
    }
}

impl GetRange for Program {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}