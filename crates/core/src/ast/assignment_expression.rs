use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::range::Range;

/// Special node of binary expression since implementation is
/// far different from other binary expressions.
#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentExpression {
    pub lhs: Box<Node>,
    pub rhs: Box<Node>,
}

impl From<AssignmentExpression> for Node {
    fn from(value: AssignmentExpression) -> Node {
        Node::AssignmentExpression(value)
    }
}

impl GetRange for AssignmentExpression {
    fn range(&self) -> Range {
        self.lhs.range()
    }
}