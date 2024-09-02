use crate::ast::identifier::Identifier;
use crate::ast::parameter_declaration::ParameterDeclaration;
use crate::ast::traits::GetRange;
use crate::ast::type_expression::TypeExpression;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionInterface {
    pub name: Option<Identifier>,
    pub parameters: Vec<ParameterDeclaration>,
    pub return_type: Box<TypeExpression>,
    pub range: Range<Position>,
}

impl GetRange for FunctionInterface {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}