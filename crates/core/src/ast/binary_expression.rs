use crate::ast::node::Node;
use crate::ast::traits::GetPosition;
use crate::position::Position;
use crate::punctuation_kind::PunctuationKind;

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    pub lhs: Box<Node>,
    pub operator: PunctuationKind,
    pub rhs: Box<Node>,
}

impl Into<Node> for BinaryExpression {
    fn into(self) -> Node {
        Node::BinaryExpressionNode(self)
    }
}

impl GetPosition for BinaryExpression {
    fn position(&self) -> &Position {
        &self.lhs.position()
    }
}