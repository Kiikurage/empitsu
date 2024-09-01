use crate::ast::node::Node;
use crate::ast::traits::GetPosition;
use crate::position::Position;

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

impl GetPosition for AssignmentExpression {
    fn position(&self) -> &Position {
        self.lhs.position()
    }
}