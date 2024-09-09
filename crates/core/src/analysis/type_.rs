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
    Struct(StructType),  // properties
    Function(Vec<Type>, Box<Type>),  // parameters, return type
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructType {
    pub name: String,
    pub properties: Vec<(String, Type)>,
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
            Type::Struct(struct_) => {
                write!(f, "{}", struct_.name)
            }
            Type::Function(params, return_type) => {
                write!(f, "fn(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, "): {}", return_type)
            }
        }
    }
}