use crate::ast::node::Node;
use crate::ast::traits::GetPosition;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteral {
    pub value: String,
    pub position: Position,
}

impl Into<Node> for StringLiteral {
    fn into(self) -> Node {
        Node::StringLiteralNode(self)
    }
}

impl GetPosition for StringLiteral {
    fn position(&self) -> &Position {
        &self.position
    }
}