use crate::ast::node::Node;
use crate::ast::traits::GetPosition;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct BoolLiteral {
    pub value: bool,
    pub position: Position,
}

impl Into<Node> for BoolLiteral {
    fn into(self) -> Node {
        Node::BoolLiteralNode(self)
    }
}

impl GetPosition for BoolLiteral {
    fn position(&self) -> &Position {
        &self.position
    }
}