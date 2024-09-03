pub type EMNumber = f64;
pub type EMBool = bool;

#[derive(Debug, Clone, PartialEq)]
pub enum ByteCode {
    /// Load next X-byte from bytecode and push it to the stack
    ConstantNumber,
    ConstantBool,

    /// Load next (usize)bytes from bytecode as index, peak X-bytes
    /// from the given position of stack, and push it to the top of
    /// stack
    LoadNumber,
    LoadBool,

    /// Load next (usize)bytes from bytecode as index, peak X-bytes
    /// from the top of stack, and store it to the given position of
    /// stack
    StoreNumber,
    StoreBool,

    /// Read next 4 bytes from bytecode as index, load binary from
    /// the given literal index, store it in heap, and push the ref 
    /// to the stack
    LoadLiteral,

    /// Load next (usize)bytes from bytecode as index, and jump to
    /// the given position of IP
    Jump,

    /// Pop the top 1byte from stack. If true, load next (usize)bytes
    /// from bytecode, and jump to the given position of IP
    JumpIfFalse,

    /// Load next (usize)bytes from bytecode as size, pop stack until
    /// stack size equals to the given size.
    Flush,

    /// Pop next 2 elements in appropriate byte size from stack, and
    /// push the result of operation to the stack
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

    /// Unary operations. Pop next 1 element in appropriate byte size
    /// from stack, and push the result of operation to the stack
    Negative,
    LogicalNot,
}
