use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::punctuation_kind::PunctuationKind;
use crate::range::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression {
    pub operator: PunctuationKind,
    pub operand: Box<Node>,
    pub range: Range,
}

impl From<UnaryExpression> for Node {
    fn from(value: UnaryExpression) -> Node {
        Node::UnaryExpression(value)
    }
}

impl GetRange for UnaryExpression {
    fn range(&self) -> Range {
        self.range
    }
}