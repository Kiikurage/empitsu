use crate::ast::node::Node;
use crate::ast::get_range::GetRange;
use crate::position::{pos, Position};
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Node>,
}

impl Program {
    pub fn new(statements: Vec<Node>) -> Self {
        Self { statements }
    }
}

impl GetRange for Program {
    fn range(&self) -> Range<Position> {
        self.statements.first().map(Node::start).unwrap_or(pos(0, 0))..
            self.statements.last().map(Node::start).unwrap_or(pos(0, 0))
    }
}