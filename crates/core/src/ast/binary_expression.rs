use crate::ast::node::Node;
use crate::ast::get_range::GetRange;
use crate::position::Position;
use crate::punctuation_kind::PunctuationKind;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    pub lhs: Box<Node>,
    pub operator: PunctuationKind,
    pub rhs: Box<Node>,
}

impl BinaryExpression {
    pub fn new(lhs: Node, operator: PunctuationKind, rhs: Node) -> Self {
        Self {
            lhs: Box::new(lhs),
            operator,
            rhs: Box::new(rhs),
        }
    }
}

impl GetRange for BinaryExpression {
    fn range(&self) -> Range<Position> {
        self.lhs.range()
    }
}