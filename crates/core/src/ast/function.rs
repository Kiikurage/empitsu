use crate::ast::function_interface::FunctionInterface;
use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::range::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub interface: FunctionInterface,
    pub body: Vec<Node>,
    pub range: Range,
}

impl GetRange for Function {
    fn range(&self) -> Range {
        self.range
    }
}