use crate::ast::function_interface::FunctionInterface;
use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub interface: FunctionInterface,
    pub body: Vec<Node>,
    pub range: Range<Position>,
}

impl GetRange for Function {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}