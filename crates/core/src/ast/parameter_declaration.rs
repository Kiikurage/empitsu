use std::ops::Range;
use crate::ast::identifier::Identifier;
use crate::ast::traits::GetRange;
use crate::ast::type_expression::TypeExpression;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct ParameterDeclaration {
    pub name: Identifier,
    pub type_: TypeExpression,
}

impl GetRange for ParameterDeclaration {
    fn range(&self) -> Range<Position> {
        self.name.range()
    }
}