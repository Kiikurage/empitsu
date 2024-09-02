use crate::ast::function_interface::FunctionInterface;
use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceDeclaration {
    pub name: Identifier,
    pub instance_methods: Vec<FunctionInterface>,
    pub range: Range<Position>,
}

impl From<InterfaceDeclaration> for Node {
    fn from(value: InterfaceDeclaration) -> Node {
        Node::InterfaceDeclaration(value)
    }
}

impl GetRange for InterfaceDeclaration {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}