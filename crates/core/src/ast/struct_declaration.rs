use crate::ast::function::Function;
use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::ast::property_declaration::PropertyDeclaration;
use crate::ast::traits::GetRange;
use crate::range::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct StructDeclaration {
    pub name: Identifier,
    pub properties: Vec<PropertyDeclaration>,
    pub instance_methods: Vec<Function>,
    pub static_methods: Vec<Function>,
    pub range: Range,
}

impl From<StructDeclaration> for Node {
    fn from(value: StructDeclaration) -> Node {
        Node::StructDeclaration(value)
    }
}

impl GetRange for StructDeclaration {
    fn range(&self) -> Range {
        self.range
    }
}