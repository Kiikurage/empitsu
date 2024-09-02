use crate::ast::function_interface::FunctionInterface;
use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    range: Range<Position>,
    pub interface: FunctionInterface,
    pub body: Vec<Node>,
}

impl Function {
    pub fn new(range: Range<Position>, interface: FunctionInterface, body: Vec<Node>) -> Self {
        Self { range, interface, body }
    }
}

impl GetRange for Function {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}