use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::range::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub condition: Box<Node>,
    pub true_branch: Box<Node>,
    pub false_branch: Option<Box<Node>>,
    pub range: Range,
}

impl From<IfStatement> for Node {
    fn from(value: IfStatement) -> Node {
        Node::IfStatement(value)
    }
}

impl GetRange for IfStatement {
    fn range(&self) -> Range {
        self.range
    }
}