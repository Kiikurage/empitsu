use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::punctuation_kind::PunctuationKind;
use crate::range::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    pub lhs: Box<Node>,
    pub operator: PunctuationKind,
    pub rhs: Box<Node>,
}

impl From<BinaryExpression> for Node {
    fn from(value: BinaryExpression) -> Node {
        Node::BinaryExpression(value)
    }
}

impl GetRange for BinaryExpression {
    fn range(&self) -> Range {
        self.lhs.range()
    }
}