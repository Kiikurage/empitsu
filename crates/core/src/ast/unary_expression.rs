use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::position::Position;
use crate::punctuation_kind::PunctuationKind;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression {
    pub operator: PunctuationKind,
    pub operand: Box<Node>,
    pub range: Range<Position>,
}

impl From<UnaryExpression> for Node {
    fn from(value: UnaryExpression) -> Node {
        Node::UnaryExpression(value)
    }
}

impl GetRange for UnaryExpression {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}