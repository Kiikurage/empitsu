use crate::ast::node::Node;
use crate::ast::traits::GetPosition;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub condition: Box<Node>,
    pub true_branch: Box<Node>,
    pub false_branch: Option<Box<Node>>,
    pub position: Position,
}

impl Into<Node> for IfStatement {
    fn into(self) -> Node {
        Node::IfStatementNode(self)
    }
}

impl GetPosition for IfStatement {
    fn position(&self) -> &Position {
        &self.position
    }
}