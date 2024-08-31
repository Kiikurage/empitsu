use crate::ast::identifier::Identifier;
use crate::ast::parameter_declaration::ParameterDeclaration;
use crate::ast::traits::GetPosition;
use crate::ast::type_expression::TypeExpression;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionInterface {
    pub name: Option<Identifier>,
    pub parameters: Vec<ParameterDeclaration>,
    pub return_type: Box<TypeExpression>,
    pub position: Position,
}

impl GetPosition for FunctionInterface {
    fn position(&self) -> &Position {
        &self.position
    }
}