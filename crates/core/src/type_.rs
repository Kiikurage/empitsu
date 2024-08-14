use std::fmt::Debug;

#[derive(Clone, PartialEq, Debug)]
pub enum Type {
    Number,
    Bool,
    String,
    Function(Vec<Type>), // parameters
    Ref,
}
