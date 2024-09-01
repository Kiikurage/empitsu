use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::range::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub value: Option<Box<Node>>,
    pub range: Range,
}

impl From<Return> for Node {
    fn from(value: Return) -> Node {
        Node::Return(value)
    }
}

impl GetRange for Return {
    fn range(&self) -> Range {
        self.range
    }
}