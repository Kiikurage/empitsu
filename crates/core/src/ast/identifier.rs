use crate::ast::node::Node;
use crate::ast::traits::GetPosition;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub name: String,
    pub position: Position,
}

impl Into<Node> for Identifier {
    fn into(self) -> Node {
        Node::IdentifierNode(self)
    }
}

impl GetPosition for Identifier {
    fn position(&self) -> &Position {
        &self.position
    }
}