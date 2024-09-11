use crate::vm::Value;

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
    Constant(Value),

    /// Load next (usize)bytes from bytecode as index, peak X-bytes
    /// from the given position of stack, and push it to the top of
    /// stack
    Load(usize),

    /// Load next (usize)bytes from bytecode as index, peak X-bytes
    /// from the top of stack, and store it to the given position of
    /// stack
    Store(usize),

    /// Pop a value from the top of stack, and store it to a new heap entry.
    /// Push the address of the heap entry to the stack
    AllocateHeap(usize),

    /// Load next (usize)bytes from bytecode as index, get the heap
    /// address of captured variable from function frame data. Peak
    /// a value from the given position of heap, and push it to the
    /// top of stack
    LoadHeap(usize),

    /// Load next (usize)bytes from bytecode as index, get the heap
    /// address of captured variable from function frame data. Peak
    /// a value from the top of stack, and store it to the given
    /// position of heap
    StoreHeap(usize),

    /// Load next (usize)bytes from bytecode as index, and jump to
    /// the given position of IP
    Jump(usize),

    /// Pop the top 1byte from stack. If true, load next (usize)bytes
    /// from bytecode, and jump to the given position of IP
    JumpIfFalse(LabelId),

    /// Load next (usize)bytes from bytecode as size, pop stack until
    /// stack size equals to the given size.
    Flush(LabelId),

    /// Load next (usize) bytes from bytecode as function body,
    /// put it in heap, and push the address to the stack
    DefineFunction(usize, String), // size, name

    /// Pop (usize) items from stack as static members (first pop item
    /// to the first member) of struct, put it in heap, and push the 
    /// address to the stack
    DefineStruct(usize, String, Vec<String>), // size, name, members

    /// Load function address from ((stack.size) - (usize) - size_of(usize)),
    /// push new call stack entry, and execute the function
    Call(usize),

    /// Escape from the current function with a return value; pop the
    /// current call stack entry, clean up local values from stack,
    /// push the last value in the function env to the stack,
    /// and change instruction pointer to the return address
    Return,

    /// Pop an item from stack as reference to object, read the property
    /// by the given index, and push the value to the stack
    LoadProperty(usize),
    LoadMethod(usize),
    
    /// Pop an item from stack as reference to object, read the property
    /// by the given index, peak a value from stack, and push the value
    /// to the property
    StoreProperty(usize),

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