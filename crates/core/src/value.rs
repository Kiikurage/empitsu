use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Error, Formatter};
use std::ops::ControlFlow;
use std::rc::Rc;

use crate::node::Node;
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
    Null,
}

#[derive(Clone, PartialEq)]
pub struct FunctionValue {
    pub name: String,
    pub parameters: Vec<String>,
    pub body: Box<Node>,
    pub closure: Rc<RefCell<Environment>>,
}

#[derive(Clone, PartialEq)]
pub struct NativeFunctionValue {
    pub name: String,
    pub parameters: Vec<String>,
    pub body: NativeFunction,
}

impl Value {
    pub fn as_number(&self) -> Result<f64, String> {
        match self {
            Value::Number(value) => Ok(*value),
            _ => Err(
                format!("TypeError: Expected type Number, actual type {:?}", self)
            ),
        }
    }

    pub fn as_bool(&self) -> Result<bool, String> {
        match self {
            Value::Bool(value) => Ok(*value),
            _ => Err(
                format!("TypeError: Expected type Bool, actual type {:?}", self)
            ),
        }
    }

    pub fn into_string(self) -> Result<String, String> {
        match self {
            Value::String(value) => Ok(value.clone()),
            Value::Number(value) => Ok(value.to_string()),
            Value::Bool(value) => Ok(value.to_string()),
            Value::Ref(address) => Ok(format!("ref {}", address)),
            Value::Function(function) => {
                Ok(format!("function {}({})", function.name, function.parameters.join(", ")))
            }
            Value::NativeFunction(function) => {
                Ok(format!("function {}({})", function.name, function.parameters.join(", ")))
            }
            Value::Null => Ok("null".to_string()),
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
            Value::Null => write!(f, "null"),
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct StructValue {
    pub properties: HashMap<String, Value>,
}