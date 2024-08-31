use crate::ast::node::Node;
use crate::ast::traits::GetPosition;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct BreakExpression {
    pub position: Position,
}

impl Into<Node> for BreakExpression {
    fn into(self) -> Node {
        Node::BreakExpressionNode(self)
    }
}

impl GetPosition for BreakExpression {
    fn position(&self) -> &Position {
        &self.position
    }
}