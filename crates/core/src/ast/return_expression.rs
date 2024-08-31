use crate::ast::node::Node;
use crate::ast::traits::GetPosition;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnExpression {
    pub value: Option<Box<Node>>,
    pub position: Position,
}

impl Into<Node> for ReturnExpression {
    fn into(self) -> Node {
        Node::ReturnExpressionNode(self)
    }
}

impl GetPosition for ReturnExpression {
    fn position(&self) -> &Position {
        &self.position
    }
}