use crate::ast::node::Node;
use crate::ast::traits::GetPosition;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct BoolLiteral {
    pub value: bool,
    pub position: Position,
}

impl From<BoolLiteral> for Node {
    fn from(value: BoolLiteral) -> Node {
        Node::BoolLiteral(value)
    }
}

impl GetPosition for BoolLiteral {
    fn position(&self) -> &Position {
        &self.position
    }
}