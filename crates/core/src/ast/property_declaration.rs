use crate::ast::identifier::Identifier;
use crate::ast::traits::GetRange;
use crate::ast::type_expression::TypeExpression;
use crate::range::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct PropertyDeclaration {
    pub name: Identifier,
    pub type_: TypeExpression,
}

impl GetRange for PropertyDeclaration {
    fn range(&self) -> Range {
        self.name.range
    }
}