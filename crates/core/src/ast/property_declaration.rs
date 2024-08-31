use crate::ast::identifier::Identifier;
use crate::ast::traits::GetPosition;
use crate::ast::type_expression::TypeExpression;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct PropertyDeclaration {
    pub name: Identifier,
    pub type_: TypeExpression,
}

impl GetPosition for PropertyDeclaration {
    fn position(&self) -> &Position {
        &self.name.position
    }
}