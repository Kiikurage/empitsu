use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::range::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Node>,
    pub range: Range,
}

impl From<Program> for Node {
    fn from(value: Program) -> Node {
        Node::Program(value)
    }
}

impl GetRange for Program {
    fn range(&self) -> Range {
        self.range
    }
}