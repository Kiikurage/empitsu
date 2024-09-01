use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::range::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct Break {
    pub range: Range,
}

impl From<Break> for Node {
    fn from(value: Break) -> Node {
        Node::Break(value)
    }
}

impl GetRange for Break {
    fn range(&self) -> Range {
        self.range
    }
}