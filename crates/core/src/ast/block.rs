use crate::ast::node::Node;
use crate::ast::traits::GetPosition;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub nodes: Vec<Node>,
    pub position: Position,
}

impl Into<Node> for Block {
    fn into(self) -> Node {
        Node::BlockExpressionNode(self)
    }
}

impl GetPosition for Block {
    fn position(&self) -> &Position {
        &self.position
    }
}