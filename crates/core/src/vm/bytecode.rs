use crate::vm::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum ByteCode {
    Constant(Value),
    Load(usize),
    Store(usize),
    AllocateHeap(usize),
    LoadHeap(usize),
    StoreHeap(usize),
    Jump(usize),
    JumpIfFalse(usize),
    DefineFunction(usize, String), // size, name
    DefineStruct(usize, String, Vec<String>), // size, name, members
    Call(usize),
    Return,
    Flush(usize),
    LoadProperty(usize),
    LoadMethod(usize),
    StoreProperty(usize),
    Add,
    Subtract,
    Multiply,
    Divide,
    LogicalAnd,
    LogicalOr,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
    Negative,
    LogicalNot,
    NoOp
}
