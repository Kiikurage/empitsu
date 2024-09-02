use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    range: Range<Position>,
    pub value: Option<Box<Node>>,
}

impl Return {
    pub fn new(range: Range<Position>, value: Option<Node>) -> Self {
        Self {
            range,
            value: value.map(Box::new),
        }
    }
}

impl GetRange for Return {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}