use crate::ast::node::Node;
use crate::ast::traits::GetPosition;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct NumberLiteral {
    pub value: f64,
    pub position: Position,
}

impl From<NumberLiteral> for Node {
    fn from(value: NumberLiteral) -> Node {
        Node::NumberLiteral(value)
    }
}

impl GetPosition for NumberLiteral {
    fn position(&self) -> &Position {
        &self.position
    }
}