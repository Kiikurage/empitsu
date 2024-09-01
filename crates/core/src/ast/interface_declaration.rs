use crate::ast::function_interface::FunctionInterface;
use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::range::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceDeclaration {
    pub name: Identifier,
    pub instance_methods: Vec<FunctionInterface>,
    pub range: Range,
}

impl From<InterfaceDeclaration> for Node {
    fn from(value: InterfaceDeclaration) -> Node {
        Node::InterfaceDeclaration(value)
    }
}

impl GetRange for InterfaceDeclaration {
    fn range(&self) -> Range {
        self.range
    }
}