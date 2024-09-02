use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct ForStatement {
    pub variable: Identifier,
    pub iterable: Box<Node>,
    pub body: Box<Node>,
    range: Range<Position>,
}

impl ForStatement {
    pub fn new(variable: Identifier, iterable: Node, body: Node, range: Range<Position>) -> Self {
        Self {
            variable,
            iterable: Box::new(iterable),
            body: Box::new(body),
            range,
        }
    }
}

impl GetRange for ForStatement {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}