use crate::node::Node;
use crate::vm::{BreakResult, StackFrame, VM};
use std::collections::HashMap;
use std::fmt::{Debug, Error, Formatter};
use std::ops::ControlFlow;
use std::rc::Rc;

pub type NativeFunction = fn(&mut VM) -> ControlFlow<BreakResult, Primitive>;

#[derive(Clone, PartialEq)]
pub enum Primitive {
    Number(f64),
    Bool(bool),
    String(String),
    Ref(usize),
    Null,
}

impl Primitive {
    pub fn as_number(&self) -> Result<f64, String> {
        match self {
            Primitive::Number(value) => Ok(*value),
            _ => Err(
                format!("TypeError: Expected type Number, actual type {:?}", self)
            ),
        }
    }

    pub fn as_bool(&self) -> Result<bool, String> {
        match self {
            Primitive::Bool(value) => Ok(*value),
            _ => Err(
                format!("TypeError: Expected type Bool, actual type {:?}", self)
            ),
        }
    }

    pub fn into_string(self) -> String {
        match self {
            Primitive::String(value) => value.clone(),
            Primitive::Number(value) => value.to_string(),
            Primitive::Bool(value) => value.to_string(),
            Primitive::Ref(address) => format!("ref {}", address),
            Primitive::Null => "null".to_string(),
        }
    }
}

impl Debug for Primitive {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Primitive::Number(value) => write!(f, "{}", value),
            Primitive::Bool(value) => write!(f, "{}", value),
            Primitive::String(value) => write!(f, "{}", value),
            Primitive::Ref(value) => write!(f, "ref {}", value),
            Primitive::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Object {
    Function(FunctionValue),
    BoundFunction(BoundFunctionValue),
    NativeFunction(NativeFunctionValue),
    StructDefinition(StructDefinitionValue),
    StructInstance(StructInstanceValue),
}

#[derive(Debug,Clone, PartialEq)]
pub struct FunctionValue {
    pub name: String,
    pub parameters: Vec<String>,

    // Required to be wrapped by Rc since VM requires
    // its reference to execute, while VM itself stores it
    #[allow(clippy::redundant_allocation)]
    pub body: Rc<Box<Node>>,

    // the frame where the function was defined
    pub closure: usize,
}

#[derive(Debug,Clone, PartialEq)]
pub struct BoundFunctionValue {
    pub function: Primitive,
    pub receiver: Primitive,
}

#[derive(Debug,Clone, PartialEq)]
pub struct NativeFunctionValue {
    pub name: String,
    pub parameters: Vec<String>,
    pub body: NativeFunction,
}

#[derive(Debug,Clone, PartialEq)]
pub struct StructDefinitionValue {
    pub name: String,
    pub properties: Vec<String>,

    // heap address of each method function
    pub instance_methods: HashMap<String, Primitive>,
    pub static_methods: HashMap<String, Primitive>,
}

#[derive(Debug,Clone, PartialEq)]
pub struct StructInstanceValue {
    // heap address of struct definition
    pub definition: Primitive,
    pub properties: HashMap<String, Primitive>,
}
