use crate::ast::node::Node;
use crate::ast::get_range::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpression {
    range: Range<Position>,
    pub condition: Box<Node>,
    pub true_branch: Box<Node>,
    pub false_branch: Box<Node>,
}

impl IfExpression {
    pub fn new(range: Range<Position>, condition: Node, true_branch: Node, false_branch: Node) -> Self {
        Self {
            range,
            condition: Box::new(condition),
            true_branch: Box::new(true_branch),
            false_branch: Box::new(false_branch),
        }
    }
}

impl GetRange for IfExpression {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}