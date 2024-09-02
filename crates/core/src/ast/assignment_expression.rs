use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::position::Position;
use std::ops::Range;

/// Special node of binary expression since implementation is
/// far different from other binary expressions.
#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentExpression {
    pub lhs: Box<Node>,
    pub rhs: Box<Node>,
}

impl AssignmentExpression {
    pub fn new(lhs: Node, rhs: Node) -> Self {
        Self {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
}

impl GetRange for AssignmentExpression {
    fn range(&self) -> Range<Position> {
        self.lhs.range()
    }
}