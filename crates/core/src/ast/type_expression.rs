use crate::ast::get_range::GetRange;
use crate::ast::type_identifier::TypeIdentifier;
use crate::position::Position;
use std::ops::Range;
use crate::ast::function_type::FunctionType;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpression {
    Identifier(TypeIdentifier),
    Function(FunctionType)
}

impl TypeExpression {
    pub fn identifier(range: Range<Position>, name: impl Into<String>) -> Self {
        Self::Identifier(TypeIdentifier::new(range, name))
    }
    
    pub fn function(range: Range<Position>, parameters: Vec<TypeExpression>, output: TypeExpression) -> Self {
        Self::Function(FunctionType::new(range, parameters, output))
    }
}

impl GetRange for TypeExpression {
    fn range(&self) -> Range<Position> {
        match self {
            TypeExpression::Identifier(identifier) => identifier.range(),
            TypeExpression::Function(function) => function.range(),
        }
    }
}

