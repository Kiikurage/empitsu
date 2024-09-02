use crate::ast::identifier::Identifier;
use crate::ast::node::Node;
use crate::ast::traits::GetRange;
use crate::ast::type_expression::TypeExpression;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub name: Identifier,
    pub type_: Option<TypeExpression>,
    pub initializer: Option<Box<Node>>,
    range: Range<Position>,
}

impl VariableDeclaration {
    pub fn new(
        name: Identifier,
        type_: Option<TypeExpression>,
        initializer: Option<Node>,
        range: Range<Position>,
    ) -> Self {
        Self {
            name,
            type_,
            initializer: initializer.map(Box::new),
            range,
        }
    }
}

impl GetRange for VariableDeclaration {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}