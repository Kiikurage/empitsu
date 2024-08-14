use std::fmt::Debug;
use crate::node::TypeExpression;

#[derive(Clone, PartialEq, Debug)]
pub enum Type {
    // built-in
    Unchecked,
    Null,
    Number,
    Bool,
    String,
    Function(FunctionType), // parameters

    // deprecated
    Ref, // In type checker, Ref should be replaced with Struct

    // user-defined
    Struct(StructType),
    Union(Vec<Type>),
}

impl Type {
    pub fn is_assignable(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Unchecked, _) => true,
            (_, Type::Unchecked) => true,
            (Type::Null, Type::Null) => true,
            (Type::Number, Type::Number) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::String, Type::String) => true,
            (Type::Union(self_types), _) => {
                self_types.iter().all(|self_type| self_type.is_assignable(other))
            }
            (_, Type::Union(other_types)) => {
                other_types.iter().any(|other_type| self.is_assignable(other_type))
            },
            _ => self == other,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct FunctionType {
    pub parameters: Vec<FunctionParameterDefinition>,
    // pub return_type: TypeExpression,
}

#[derive(Clone, PartialEq, Debug)]
pub struct FunctionParameterDefinition {
    pub name: String,
    pub type_: TypeExpression,
}

#[derive(Clone, PartialEq, Debug)]
pub struct StructType {
    pub name: String,
    pub properties: Vec<StructPropertyDefinition>
}

impl StructType {
    pub fn get_property_type(&self, property_name: &str) -> Option<&TypeExpression> {
        for property in self.properties.iter() {
            if property.name == property_name {
                return Some(&property.type_)
            }
        }
        None
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct StructPropertyDefinition {
    pub name: String,
    pub type_: TypeExpression,
}
