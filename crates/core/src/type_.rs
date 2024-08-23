use std::collections::HashSet;
use std::fmt::Debug;

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum Type {
    // built-in
    Unchecked,
    Null,
    Number,
    Bool,
    String,
    Function(FunctionType), // parameters
    StructDefinition(StructDefinitionType),
    Struct(StructType),
    Union(Vec<Type>),
}

impl Type {
    pub fn union_of(types: &[Type]) -> Type {
        let mut unique_types = types.iter().cloned()
            .collect::<HashSet<Type>>().into_iter()
            .collect::<Vec<Type>>();

        if unique_types.len() == 1 {
            unique_types.remove(0)
        } else {
            Type::Union(unique_types)
        }
    }

    pub fn is_assignable(&self, target: &Type) -> bool {
        match (self, target) {
            (Type::Unchecked, _) => true,
            (_, Type::Unchecked) => true,
            (Type::Null, Type::Null) => true,
            (Type::Number, Type::Number) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::String, Type::String) => true,
            (Type::Union(self_types), _) => {
                self_types.iter().all(|self_type| self_type.is_assignable(target))
            }
            (_, Type::Union(target_types)) => {
                target_types.iter().any(|other_type| self.is_assignable(other_type))
            }
            _ => self == target,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct FunctionType {
    pub parameters: Vec<FunctionParameterDefinition>,
    pub return_type: Box<Type>,
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct FunctionParameterDefinition {
    pub name: String,
    pub type_: Type,
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct StructDefinitionType {
    pub name: String,
    pub properties: Vec<StructPropertyDefinition>,
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct StructType {
    pub definition: StructDefinitionType,
}

impl StructDefinitionType {
    pub fn get_property_type(&self, property_name: &str) -> Option<&Type> {
        for property in self.properties.iter() {
            if property.name == property_name {
                return Some(&property.type_);
            }
        }
        None
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct StructPropertyDefinition {
    pub name: String,
    pub type_: Type,
}
