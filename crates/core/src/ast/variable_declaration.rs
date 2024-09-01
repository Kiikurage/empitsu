use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::ast::type_expression::TypeExpression;
use crate::range::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub name: Identifier,
    pub type_: Option<TypeExpression>,
    pub initializer: Option<Box<Node>>,
    pub range: Range,
}

impl From<VariableDeclaration> for Node {
    fn from(value: VariableDeclaration) -> Node {
        Node::VariableDeclaration(value)
    }
}

impl GetRange for VariableDeclaration {
    fn range(&self) -> Range {
        self.range
    }
}