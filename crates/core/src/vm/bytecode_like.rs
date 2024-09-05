use crate::vm::bytecode::{EMBool, EMNumber};

pub type LabelId = usize;

#[derive(Debug, Clone, PartialEq)]
pub struct Label {
    pub id: LabelId,

    pub address: Option<usize>,

    /// A list of opcodes generated with dummy address.
    /// It needs to be patched when the address this label points to is determined.
    pub pending: Vec<usize>,
}

impl Label {
    pub fn new(id: usize) -> Self {
        Self {
            id,
            address: None,
            pending: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ByteCodeLike {
    /// Load next X-byte from bytecode and push it to the stack
    ConstantNumber(EMNumber),
    ConstantBool(EMBool),

    /// Load next (usize)bytes from bytecode as index, peak X-bytes
    /// from the given position of stack, and push it to the top of
    /// stack
    LoadNumber(usize),
    LoadBool(usize),

    /// Load next (usize)bytes from bytecode as index, peak X-bytes
    /// from the top of stack, and store it to the given position of
    /// stack
    StoreNumber(usize),
    StoreBool(usize),

    /// Read next 4 bytes from bytecode as index, load binary from
    /// the given literal index, store it in heap, and push the ref 
    /// to the stack
    LoadLiteral(u32),

    /// Load next (usize)bytes from bytecode as index, and jump to
    /// the given position of IP
    Jump(usize),

    /// Pop the top 1byte from stack. If true, load next (usize)bytes
    /// from bytecode, and jump to the given position of IP
    JumpIfFalse(LabelId),

    /// Load next (usize)bytes from bytecode as size, pop stack until
    /// stack size equals to the given size.
    Flush(LabelId),

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

    /// Label used for jump. No actual byte code is generated
    Label(LabelId),
}