use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Error, Formatter};
use std::ops::ControlFlow;
use std::rc::Rc;

use crate::node::Node;
use crate::type_::{FunctionParameterDefinition, FunctionType, StructType, Type};
use crate::vm::{BreakResult, Environment, VM};

pub type NativeFunction = fn(&Vec<Value>, &mut VM) -> ControlFlow<BreakResult, Value>;

#[derive(Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Bool(bool),
    String(String),
    Function(FunctionValue),
    NativeFunction(NativeFunctionValue),
    Ref(usize),
}

#[derive(Clone, PartialEq)]
pub struct FunctionValue {
    pub name: String,
    pub parameters: Vec<FunctionParameterDefinition>,
    pub body: Box<Node>,
    pub closure: Rc<RefCell<Environment>>,
}

#[derive(Clone, PartialEq)]
pub struct NativeFunctionValue {
    pub name: String,
    pub parameters: Vec<FunctionParameterDefinition>,
    pub body: NativeFunction,
}

impl Value {
    pub fn get_type(&self) -> Type {
        match self {
            Value::Number(_) => Type::Number,
            Value::Bool(_) => Type::Bool,
            Value::String(_) => Type::String,
            Value::Function(function) => Type::Function(FunctionType {
                parameters: function.parameters.clone(),
            }),
            Value::NativeFunction(function) => Type::Function(FunctionType {
                parameters: function.parameters.clone(),
            }),
            Value::Ref(_) => Type::Ref
        }
    }

    pub fn as_number(&self) -> Result<f64, String> {
        match self {
            Value::Number(value) => Ok(*value),
            _ => Err(
                format!("TypeError: Expected type Number, actual type {:?}", self.get_type())
            ),
        }
    }

    pub fn as_bool(&self) -> Result<bool, String> {
        match self {
            Value::Bool(value) => Ok(*value),
            _ => Err(
                format!("TypeError: Expected type Bool, actual type {:?}", self.get_type())
            ),
        }
    }

    pub fn into_string(self) -> Result<String, String> {
        match self {
            Value::String(value) => Ok(value.clone()),
            Value::Function(function) => {
                Ok(
                    format!("function {}({})", function.name, function.parameters
                        .iter().map(|parameter| format!("{}:{:?}", parameter.name, parameter.type_))
                        .collect::<Vec<String>>()
                        .join(", "))
                )
            }
            Value::NativeFunction(function) => {
                Ok(
                    format!("function {}({})", function.name, function.parameters
                        .iter().map(|parameter| format!("{}:{:?}", parameter.name, parameter.type_))
                        .collect::<Vec<String>>()
                        .join(", "))
                )
            }
            _ => Err(
                format!("TypeError: Expected type String, actual type {:?}", self.get_type())
            ),
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Value::Number(value) => write!(f, "{}", value),
            Value::Bool(value) => write!(f, "{}", value),
            Value::String(value) => write!(f, "{}", value),
            Value::Function { .. } => write!(f, "{}", self.clone().into_string().unwrap()),
            Value::NativeFunction { .. } => write!(f, "{}", self.clone().into_string().unwrap()),
            Value::Ref(value) => write!(f, "ref {}", value),
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct StructValue {
    pub type_: StructType,
    pub properties: HashMap<String, Value>,
}