use crate::ast::node::Node;
use crate::ast::parameter::Parameter;
use crate::ast::traits::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub callee: Box<Node>,
    pub parameters: Vec<Parameter>,
}

impl CallExpression {
    pub fn new(callee: Node, parameters: Vec<Parameter>) -> Self {
        Self {
            callee: Box::new(callee),
            parameters,
        }
    }
}

impl GetRange for CallExpression {
    fn range(&self) -> Range<Position> {
        self.callee.range()
    }
}