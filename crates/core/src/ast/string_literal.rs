use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::range::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteral {
    pub value: String,
    pub range: Range,
}

impl From<StringLiteral> for Node {
    fn from(value: StringLiteral) -> Node {
        Node::StringLiteral(value)
    }
}

impl GetRange for StringLiteral {
    fn range(&self) -> Range {
        self.range
    }
}