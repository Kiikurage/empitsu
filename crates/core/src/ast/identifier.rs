use crate::ast::node::Node;
use crate::ast::traits::GetPosition;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub name: String,
    pub position: Position,
}

impl From<Identifier> for Node {
    fn from(value: Identifier) -> Node {
        Node::Identifier(value)
    }
}

impl GetPosition for Identifier {
    fn position(&self) -> &Position {
        &self.position
    }
}