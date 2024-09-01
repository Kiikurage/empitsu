use crate::ast::node::Node;
use crate::ast::traits::GetPosition;
use crate::position::Position;
use crate::punctuation_kind::PunctuationKind;

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression {
    pub operator: PunctuationKind,
    pub operand: Box<Node>,
    pub position: Position,
}

impl From<UnaryExpression> for Node {
    fn from(value: UnaryExpression) -> Node {
        Node::UnaryExpression(value)
    }
}

impl GetPosition for UnaryExpression {
    fn position(&self) -> &Position {
        &self.position
    }
}