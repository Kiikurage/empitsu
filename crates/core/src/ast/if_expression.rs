use crate::ast::node::Node;
use crate::ast::traits::GetPosition;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpression {
    pub condition: Box<Node>,
    pub true_branch: Box<Node>,
    pub false_branch: Box<Node>,
    pub position: Position,
}

impl From<IfExpression> for Node {
    fn from(value: IfExpression) -> Node {
        Node::IfExpression(value)
    }
}

impl GetPosition for IfExpression {
    fn position(&self) -> &Position {
        &self.position
    }
}