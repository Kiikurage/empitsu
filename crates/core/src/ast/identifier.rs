use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::range::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub name: String,
    pub range: Range,
}

impl From<Identifier> for Node {
    fn from(value: Identifier) -> Node {
        Node::Identifier(value)
    }
}

impl GetRange for Identifier {
    fn range(&self) -> Range {
        self.range
    }
}