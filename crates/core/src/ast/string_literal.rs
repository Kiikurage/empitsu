use crate::ast::node::Node;
use crate::ast::traits::GetPosition;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteral {
    pub value: String,
    pub position: Position,
}

impl From<StringLiteral> for Node {
    fn from(value: StringLiteral) -> Node {
        Node::StringLiteral(value)
    }
}

impl GetPosition for StringLiteral {
    fn position(&self) -> &Position {
        &self.position
    }
}