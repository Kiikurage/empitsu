use std::fmt::Display;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    /// Type for values the shouldn't be used (e.g. unreachable code)
    Never,

    /// Type cannot be determined.
    Unknown,

    /// Only for testing purpose.
    Any,

    Void,
    Number,
    Bool,
    Struct(StructType),
    Function(FunctionType),
}

impl Type {
    pub fn function(parameters: Vec<Type>, return_type: Type, receiver_type: Option<Type>) -> Type {
        Type::Function(FunctionType {
            parameters,
            return_type: Box::new(return_type),
            receiver_type: receiver_type.map(Box::new),
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructType {
    pub properties: Vec<(String, Type)>,
    pub instance_methods: Vec<(String, Type)>,
}

impl StructType {
    pub fn new(
        properties: Vec<(String, Type)>,
        instance_methods: Vec<(String, Type)>
    ) -> Self {
        StructType { properties, instance_methods }
    }

    pub fn property_index(&self, name: &str) -> Option<usize> {
        self.properties.iter().position(|(property_name, _)| property_name == name)
    }

    pub fn method_index(&self, name: &str) -> Option<usize> {
        self.instance_methods.iter().position(|(method_name, _)| method_name == name)
    }
}

impl Display for StructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "struct {{")?;
        for (i, (name, type_)) in self.properties.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", name, type_)?;
        }
        write!(f, "}}")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionType {
    pub parameters: Vec<Type>,
    pub return_type: Box<Type>,
    pub receiver_type: Option<Box<Type>>,
}

impl Display for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn(")?;
        for (i, parameter) in self.parameters.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", parameter)?;
        }
        write!(f, "): {}", self.return_type)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Any => write!(f, "any"),
            Type::Never => write!(f, "never"),
            Type::Unknown => write!(f, "unknown"),
            Type::Void => write!(f, "void"),
            Type::Number => write!(f, "number"),
            Type::Bool => write!(f, "bool"),
            Type::Struct(struct_type) => {
                write!(f, "{}", struct_type)
            }
            Type::Function(function_type) => {
                write!(f, "{}", function_type)
            }
        }
    }
}