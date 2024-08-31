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

impl Into<Node> for ForStatement {
    fn into(self) -> Node {
        Node::ForStatementNode(self)
    }
}

impl GetPosition for ForStatement {
    fn position(&self) -> &Position {
        &self.position
    }
}