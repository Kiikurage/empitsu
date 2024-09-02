use crate::ast::node::Node;
use crate::ast::get_range::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub condition: Box<Node>,
    pub true_branch: Box<Node>,
    pub false_branch: Option<Box<Node>>,
    range: Range<Position>,
}

impl IfStatement {
    pub fn new(condition: Node, true_branch: Node, false_branch: Option<Node>, range: Range<Position>) -> Self {
        Self {
            condition: Box::new(condition),
            true_branch: Box::new(true_branch),
            false_branch: false_branch.map(Box::new),
            range,
        }
    }
}

impl GetRange for IfStatement {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}