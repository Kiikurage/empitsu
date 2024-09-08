use crate::vm::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum ByteCode {
    Constant(Value),
    Load(usize),
    Store(usize),
    Jump(usize),
    JumpIfFalse(usize),
    DefineFunction(usize),
    Call(usize),
    PopCallStack,
    Flush(usize),
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
}
