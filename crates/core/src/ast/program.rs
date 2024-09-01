use crate::ast::node::Node;
use crate::ast::traits::GetPosition;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Node>,
    pub position: Position,
}

impl From<Program> for Node {
    fn from(value: Program) -> Node {
        Node::Program(value)
    }
}

impl GetPosition for Program {
    fn position(&self) -> &Position {
        &self.position
    }
}