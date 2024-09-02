use crate::ast::identifier::Identifier;
use crate::ast::parameter_declaration::ParameterDeclaration;
use crate::ast::get_range::GetRange;
use crate::ast::type_expression::TypeExpression;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionInterface {
    range: Range<Position>,
    pub name: Option<Identifier>,
    pub parameters: Vec<ParameterDeclaration>,
    pub return_type: Box<TypeExpression>,
}

impl FunctionInterface {
    pub fn new(
        range: Range<Position>,
        name: Option<Identifier>,
        parameters: Vec<ParameterDeclaration>,
        return_type: TypeExpression,
    ) -> Self {
        Self {
            range,
            name,
            parameters,
            return_type: Box::new(return_type),
        }
    }
}

impl GetRange for FunctionInterface {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}