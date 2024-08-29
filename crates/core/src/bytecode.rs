#[derive(Debug, PartialEq)]
pub enum ByteCode {
    /// Push a constant value to the top of the stack
    Constant(f64),

    /// Flush stack until the given index. Stack top index after this operation will be (given_index - 1).
    Flush(usize),

    /// Load a value from the given index of stack and push it to the top
    Load(usize),

    /// Store a value from the top of the stack to the given index. Top value is not popped.
    Store(usize),

    /// Jump to the given instruction pointer
    Jump(usize),

    /// Pop the top value of stack and jump if the value is zero
    JumpIfZero(usize),

    /// Binary operations
    Add,
    Subtract,
    Multiply,
    Divide,
    LogicalAnd,
    LogicalOr,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanEqual,
    Equal,
    NotEqual,

    /// Unary operations
    Negative,
    LogicalNot,
}
