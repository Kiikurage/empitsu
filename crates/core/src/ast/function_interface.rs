use crate::ast::identifier::Identifier;
use crate::ast::parameter_declaration::ParameterDeclaration;
use crate::ast::traits::GetRange;
use crate::ast::type_expression::TypeExpression;
use crate::range::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionInterface {
    pub name: Option<Identifier>,
    pub parameters: Vec<ParameterDeclaration>,
    pub return_type: Box<TypeExpression>,
    pub range: Range,
}

impl GetRange for FunctionInterface {
    fn range(&self) -> Range {
        self.range
    }
}