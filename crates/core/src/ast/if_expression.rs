use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpression {
    pub condition: Box<Node>,
    pub true_branch: Box<Node>,
    pub false_branch: Box<Node>,
    pub range: Range<Position>,
}

impl From<IfExpression> for Node {
    fn from(value: IfExpression) -> Node {
        Node::IfExpression(value)
    }
}

impl GetRange for IfExpression {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}