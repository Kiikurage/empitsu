use crate::ast::node::Node;
use crate::ast::parameter::Parameter;
use crate::ast::get_range::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub callee: Box<Node>,
    pub parameters: Vec<Parameter>,
    range: Range<Position>,
}

impl CallExpression {
    pub fn new(range: Range<Position>, callee: Node, parameters: Vec<Parameter>) -> Self {
        Self {
            callee: Box::new(callee),
            parameters,
            range,
        }
    }
}

impl GetRange for CallExpression {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}