use std::ops::Range;
use crate::ast::identifier::Identifier;
use crate::ast::get_range::GetRange;
use crate::ast::type_expression::TypeExpression;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct PropertyDeclaration {
    pub name: Identifier,
    pub type_: TypeExpression,
}

impl PropertyDeclaration {
    pub fn new(name: Identifier, type_: TypeExpression) -> Self {
        Self { name, type_ }
    }
}

impl GetRange for PropertyDeclaration {
    fn range(&self) -> Range<Position> {
        self.name.range()
    }
}