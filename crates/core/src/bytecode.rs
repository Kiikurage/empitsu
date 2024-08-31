pub type EMNumber = f64;
pub type EMBool = bool;
pub type EMRef = usize;

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

    /// Allocate a new heap entry, and push the ref value to the stack
    Allocate,
    /// Pop a reference from the top of stack, and release heap memory
    /// allocated for the given reference
    Release,

    /// Load next (usize)bytes from bytecode as index, and jump to
    /// the given position of IP
    Jump,

    /// Pop the top 1byte from stack. If true, load next (usize)bytes
    /// from bytecode, and jump to the given position of IP
    JumpIfFalse,

    /// Load next (usize)bytes from bytecode as size, pop stack until
    /// stack size equals to the given size.
    Flush,

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
