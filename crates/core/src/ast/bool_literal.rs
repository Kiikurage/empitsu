use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::range::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct BoolLiteral {
    pub value: bool,
    pub range: Range,
}

impl From<BoolLiteral> for Node {
    fn from(value: BoolLiteral) -> Node {
        Node::BoolLiteral(value)
    }
}

impl GetRange for BoolLiteral {
    fn range(&self) -> Range {
        self.range
    }
}