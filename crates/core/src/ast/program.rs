use crate::ast::node::Node;
use crate::ast::traits::GetPosition;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Node>,
    pub position: Position,
}

impl GetPosition for Program {
    fn position(&self) -> &Position {
        &self.position
    }
}