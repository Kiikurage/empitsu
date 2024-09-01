use crate::ast::node::Node;
use crate::ast::traits::GetPosition;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct BreakExpression {
    pub position: Position,
}

impl From<BreakExpression> for Node {
    fn from(value: BreakExpression) -> Node {
        Node::BreakExpression(value)
    }
}

impl GetPosition for BreakExpression {
    fn position(&self) -> &Position {
        &self.position
    }
}