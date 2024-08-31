#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpression {
    Identifier(String),
    Optional(Box<TypeExpression>),
    Union(Vec<TypeExpression>),
}

impl TypeExpression {
    pub fn identifier(name: impl Into<String>) -> TypeExpression {
        TypeExpression::Identifier(name.into())
    }

    pub fn optional(type_: TypeExpression) -> TypeExpression {
        TypeExpression::Optional(Box::new(type_))
    }

    pub fn union(types: Vec<TypeExpression>) -> TypeExpression {
        TypeExpression::Union(types)
    }
}