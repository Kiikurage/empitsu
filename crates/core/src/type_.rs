use std::fmt::Debug;

#[derive(Clone, PartialEq, Debug)]
pub enum Type {
    Any,
    Number,
    Bool,
    String,
    Function(Vec<FunctionParameterDefinition>), // parameters
    Ref,
    Struct {
        name: String,
        properties: Vec<StructPropertyDefinition>
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParameterDefinition {
    pub name: String,
    pub type_: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructPropertyDefinition {
    pub name: String,
    pub type_: Type,
}
