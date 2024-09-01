use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::ast::traits::GetPosition;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct ForStatement {
    pub variable: Identifier,
    pub iterable: Box<Node>,
    pub body: Box<Node>,
    pub position: Position,
}

impl From<ForStatement> for Node {
    fn from(value: ForStatement) -> Node {
        Node::ForStatement(value)
    }
}

impl GetPosition for ForStatement {
    fn position(&self) -> &Position {
        &self.position
    }
}