use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::position::Position;
use crate::punctuation_kind::PunctuationKind;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression {
    range: Range<Position>,
    pub operator: PunctuationKind,
    pub operand: Box<Node>,
}

impl UnaryExpression {
    pub fn new(range: Range<Position>, operator: PunctuationKind, operand: Node) -> Self {
        Self {
            range,
            operator,
            operand: Box::new(operand),
        }
    }
}

impl GetRange for UnaryExpression {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}