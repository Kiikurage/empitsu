use crate::ast::node::Node;
use crate::ast::parameter::Parameter;
use crate::ast::traits::GetRange;
use crate::range::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub callee: Box<Node>,
    pub parameters: Vec<Parameter>,
}

impl From<CallExpression> for Node {
    fn from(value: CallExpression) -> Node {
        Node::CallExpression(value)
    }
}

impl GetRange for CallExpression {
    fn range(&self) -> Range {
        self.callee.range()
    }
}