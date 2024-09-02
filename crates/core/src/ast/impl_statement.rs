use crate::ast::function::Function;
use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct ImplStatement {
    pub struct_name: Identifier,
    pub interface_name: Identifier,
    pub instance_methods: Vec<Function>,
    pub range: Range<Position>,
}

impl From<ImplStatement> for Node {
    fn from(value: ImplStatement) -> Node {
        Node::ImplStatement(value)
    }
}

impl GetRange for ImplStatement {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}
