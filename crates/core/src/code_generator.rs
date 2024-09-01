use crate::ast::node::Node;
use crate::ast::program::Program;
use crate::ast::traits::GetPosition;
use crate::ast::type_expression::TypeExpression;
use crate::bytecode::ByteCode;
use crate::checker::check;
use crate::error::Error;
use crate::punctuation_kind::PunctuationKind;
use crate::util::AsU8Slice;
use std::collections::{HashMap, HashSet};
use std::ops::Deref;

enum PrimitiveType {
    Number,
    Bool,
}

impl PrimitiveType {
    fn size(&self) -> usize {
        match self {
            PrimitiveType::Number => size_of::<f64>(),
            PrimitiveType::Bool => size_of::<bool>(),
        }
    }
}

struct Variable {
    offset: usize,
    type_: PrimitiveType,
}

struct StackFrame {
    stack_offset: usize,
    stack_size: usize,

    /// Instruction pointer of the start of this frame
    ip_start: usize,

    /// If this frame is breakable (i.e. frame made by for-loop)
    breakable: bool,

    /// Maps of variable names to their stack index in each scope
    variables: HashMap<String, Variable>,

    /// List of instruction pointer that requires to be patched with ip_end
    patch_ips: Vec<usize>,

    /// Variables initialized in this scope
    initialized: HashSet<String>,
}

impl StackFrame {
    fn new(offset: usize, breakable: bool) -> Self {
        Self {
            stack_offset: offset,
            stack_size: 0,
            variables: HashMap::new(),
            breakable,
            ip_start: 0usize,
            patch_ips: vec![],
            initialized: HashSet::new(),
        }
    }
}

struct CodeGenerator {
    opcodes: Vec<u8>,
    frames: Vec<StackFrame>,
    literals: Vec<Vec<u8>>,
}

///
/// - 4byte: number of literal in literal tables (N)
/// - 16byte * N: (offset: usize, length: usize) of each literal.
///               Offset is from the start of the literal table.
/// - Literal table: each literal binary
/// - op codes
///

impl CodeGenerator {
    fn new() -> Self {
        Self {
            literals: vec![],
            opcodes: vec![],
            frames: vec![StackFrame::new(0, false)],
        }
    }

    pub fn get_codes(&self) -> Vec<u8> {
        let mut codes = Vec::new();

        codes.extend((self.literals.len() as u32).as_u8_slice());
        let mut offset = 0;

        for i in 0..self.literals.len() {
            let binary = &self.literals[i];

            codes.extend(offset.as_u8_slice());
            codes.extend(binary.len().as_u8_slice());

            offset += binary.len();
        }

        for i in 0..self.literals.len() {
            codes.extend(&self.literals[i]);
        }

        codes.extend(&self.opcodes);

        codes
    }

    fn patch_address(&mut self, ip: usize) {
        let address = self.opcodes.len();
        self.opcodes[ip..ip + size_of::<usize>()].copy_from_slice(address.as_u8_slice());
    }

    fn generate_node(&mut self, node: &Node) -> Result<(), Error> {
        match node {
            Node::ProgramNode(program) => {
                for statement in program.statements.iter() {
                    self.generate_node(statement)?;
                }
            }
            Node::IfStatementNode(if_statement) => {
                self.generate_node(&if_statement.condition)?;
                let ip_conditional_jump = self.write_jump_if_false(/*dummy*/ 0);

                self.generate_node(&if_statement.true_branch)?;

                match &if_statement.false_branch {
                    Some(false_branch) => {
                        let ip_after_true_branch = self.write_jump(/*dummy*/ 0);
                        self.patch_address(ip_conditional_jump);
                        self.generate_node(false_branch)?;
                        self.patch_address(ip_after_true_branch);
                    }
                    None => {
                        self.patch_address(ip_conditional_jump);
                    }
                }
            }
            Node::ForStatementNode(for_) => {
                self.enter_scope(true);
                {
                    // initializer
                    self.write_constant_number(0.0);
                    self.declare_variable(for_.variable.name.clone(), PrimitiveType::Number, true);

                    // condition
                    let ip_condition_start = self.opcodes.len();
                    self.enter_scope(true);
                    {
                        match self.load(&for_.variable.name) {
                            Ok(()) => (),
                            Err(message) => return Err(Error::invalid_syntax(for_.position.clone(), message)),
                        }
                        self.write_constant_number(5.0);
                        self.write_code(ByteCode::LessThan);
                        let ip_conditional_jump = self.write_jump_if_false(/*dummy*/ 0);

                        // body
                        self.generate_node(&for_.body)?;

                        // update
                        match self.load(&for_.variable.name) {
                            Ok(()) => (),
                            Err(message) => return Err(Error::invalid_syntax(for_.position.clone(), message)),
                        }
                        self.write_constant_number(1.0);
                        self.write_code(ByteCode::Add);
                        match self.store(&for_.variable.name) {
                            Ok(()) => (),
                            Err(message) => return Err(Error::invalid_syntax(for_.position.clone(), message)),
                        }

                        // loop
                        self.write_jump(ip_condition_start);
                        self.patch_address(ip_conditional_jump);
                    }
                    self.exit_scope();
                }
                self.exit_scope();
            }
            Node::VariableDeclarationNode(ref variable_declaration) => {
                match &variable_declaration.initializer {
                    Some(initializer) => {
                        self.generate_node(initializer)?;
                        self.declare_variable(variable_declaration.name.name.clone(), PrimitiveType::Number, true);
                    }
                    None => {
                        match variable_declaration.type_ {
                            Some(TypeExpression::Identifier(ref name)) => {
                                match name.as_str() {
                                    "number" => {
                                        self.declare_variable(variable_declaration.name.name.clone(), PrimitiveType::Number, false)
                                    }
                                    "bool" => {
                                        self.declare_variable(variable_declaration.name.name.clone(), PrimitiveType::Bool, false)
                                    }
                                    _ => return Err(Error::not_implemented(variable_declaration.position.clone(), format!("Unsupported type: {}", name))),
                                }
                            }
                            Some(..) => return Err(Error::not_implemented(variable_declaration.position.clone(), "Only identifier type is supported")),
                            None => return Err(Error::invalid_syntax(variable_declaration.position.clone(), "Type or initializer is required for variable declaration")),
                        };
                    }
                }
            }
            Node::FunctionDeclarationNode(_) => return Err(Error::not_implemented(node.position().clone(), "FunctionDeclaration")),
            Node::StructDeclarationNode(_) => return Err(Error::not_implemented(node.position().clone(), "StructDeclaration")),
            Node::InterfaceDeclarationNode(_) => return Err(Error::not_implemented(node.position().clone(), "InterfaceDeclaration")),
            Node::ImplStatementNode(_) => return Err(Error::not_implemented(node.position().clone(), "ImplStatement")),
            Node::ReturnExpressionNode(_) => return Err(Error::not_implemented(node.position().clone(), "ReturnExpression")),
            Node::BreakExpressionNode(..) => {
                let ip_break = self.write_jump(/*dummy*/ 0);
                match self.register_break_to_patch(ip_break) {
                    Ok(()) => (),
                    Err(message) => return Err(Error::invalid_syntax(node.position().clone(), message)),
                }
            }
            Node::FunctionExpressionNode(_) => return Err(Error::not_implemented(node.position().clone(), "FunctionExpression")),
            Node::IfExpressionNode(if_expression) => {
                self.generate_node(&if_expression.condition)?;
                let ip_conditional_jump = self.write_jump_if_false(/*dummy*/ 0);

                self.generate_node(&if_expression.true_branch)?;
                let ip_after_true_branch = self.write_jump(/*dummy*/ 0);

                self.patch_address(ip_conditional_jump);
                self.generate_node(&if_expression.false_branch)?;

                self.patch_address(ip_after_true_branch);
            }
            Node::BlockExpressionNode(block) => {
                self.enter_scope(false);
                for node in block.nodes.iter() {
                    self.generate_node(node)?;
                }
                self.exit_scope();
            }
            Node::AssignmentExpressionNode(expression) => {
                match &expression.lhs.deref() {
                    Node::IdentifierNode(name) => {
                        self.generate_node(&expression.rhs)?;
                        match self.store(&name.name) {
                            Ok(()) => (),
                            Err(message) => return Err(Error::invalid_syntax(node.position().clone(), message)),
                        }
                    }
                    _ => return Err(Error::not_implemented(node.position().clone(), "Assign to complex target is not supported")),
                }
            }
            Node::BinaryExpressionNode(expression) => {
                self.generate_node(&expression.lhs)?;
                self.generate_node(&expression.rhs)?;
                match &expression.operator {
                    PunctuationKind::Plus => self.write_code(ByteCode::Add),
                    PunctuationKind::Minus => self.write_code(ByteCode::Subtract),
                    PunctuationKind::Asterisk => self.write_code(ByteCode::Multiply),
                    PunctuationKind::Slash => self.write_code(ByteCode::Divide),
                    PunctuationKind::AndAnd => self.write_code(ByteCode::LogicalAnd),
                    PunctuationKind::VerticalLineVerticalLine => self.write_code(ByteCode::LogicalOr),
                    PunctuationKind::LeftChevron => self.write_code(ByteCode::LessThan),
                    PunctuationKind::LeftChevronEqual => self.write_code(ByteCode::LessThanOrEqual),
                    PunctuationKind::RightChevron => self.write_code(ByteCode::GreaterThan),
                    PunctuationKind::RightChevronEqual => self.write_code(ByteCode::GreaterThanEqual),
                    PunctuationKind::EqualEqual => self.write_code(ByteCode::Equal),
                    PunctuationKind::ExclamationEqual => self.write_code(ByteCode::NotEqual),
                    _ => return Err(Error::not_implemented(
                        node.position().clone(),
                        format!("Unsupported binary operator: {:?}", expression.operator),
                    )),
                }
            }
            Node::UnaryExpressionNode(expression) => {
                self.generate_node(&expression.operand)?;
                match &expression.operator {
                    PunctuationKind::Plus => (),
                    PunctuationKind::Minus => self.write_code(ByteCode::Negative),
                    PunctuationKind::Exclamation => self.write_code(ByteCode::LogicalNot),
                    _ => return Err(Error::not_implemented(
                        node.position().clone(),
                        format!("Unsupported unary operator: {:?}", expression.operator),
                    )),
                }
            }
            Node::CallExpressionNode(_expression) =>
                return Err(Error::not_implemented(node.position().clone(), "CallExpression")),
            Node::MemberExpressionNode(_expression) =>
                return Err(Error::not_implemented(node.position().clone(), "MemberExpression")),
            Node::IdentifierNode(name) => {
                match self.load(&name.name) {
                    Ok(()) => (),
                    Err(message) => return Err(Error::invalid_syntax(node.position().clone(), message)),
                }
            }
            Node::NumberLiteralNode(value) => {
                self.write_constant_number(value.value);
            }
            Node::BoolLiteralNode(value) => {
                self.write_constant_bool(value.value);
            }
            Node::StringLiteralNode(string_literal) => {
                self.write_load_literal(string_literal.value.as_bytes().to_vec());
            }
        }

        Ok(())
    }

    fn write_bytes(&mut self, bytes: &[u8]) {
        self.opcodes.extend(bytes);
    }

    fn write_code(&mut self, code: ByteCode) {
        self.write_bytes(&code.as_u8_slice());
    }

    fn write_constant_number(&mut self, value: f64) {
        self.write_code(ByteCode::ConstantNumber);
        self.write_bytes(&value.as_u8_slice());
    }

    fn write_constant_bool(&mut self, value: bool) {
        self.write_code(ByteCode::ConstantBool);
        self.write_bytes(&value.as_u8_slice());
    }

    fn write_load_number(&mut self, offset: usize) {
        self.write_code(ByteCode::LoadNumber);
        self.write_bytes(&offset.as_u8_slice());
    }

    fn write_load_bool(&mut self, offset: usize) {
        self.write_code(ByteCode::LoadBool);
        self.write_bytes(&offset.as_u8_slice());
    }

    fn write_store_number(&mut self, offset: usize) {
        self.write_code(ByteCode::StoreNumber);
        self.write_bytes(&offset.as_u8_slice());
    }

    fn write_store_bool(&mut self, offset: usize) {
        self.write_code(ByteCode::StoreBool);
        self.write_bytes(&offset.as_u8_slice());
    }

    fn write_jump(&mut self, address: usize) -> usize {
        self.write_code(ByteCode::Jump);
        let ip = self.opcodes.len();
        self.write_bytes(&address.as_u8_slice());
        ip
    }

    fn write_jump_if_false(&mut self, address: usize) -> usize {
        self.write_code(ByteCode::JumpIfFalse);
        let ip = self.opcodes.len();
        self.write_bytes(&address.as_u8_slice());
        ip
    }

    fn write_flush(&mut self, expected_size: usize) {
        self.write_code(ByteCode::Flush);
        self.write_bytes(&expected_size.as_u8_slice());
    }

    fn write_load_literal(&mut self, value: Vec<u8>) {
        let index = self.literals.len();
        self.literals.push(value);

        self.write_code(ByteCode::LoadLiteral);
        self.write_bytes(&(index as u32).as_u8_slice());
    }

    /// Check if the given variable is initialized in the current scope
    fn initialized(&mut self, name: &String) -> bool {
        for frame in self.frames.iter_mut().rev() {
            if frame.initialized.contains(name) {
                return true;
            }
            if let Some(..) = frame.variables.get_mut(name) {
                return false;
            }
        }

        // Variable is not declared
        false
    }

    fn store(&mut self, name: &String) -> Result<(), String> {
        for frame in self.frames.iter_mut().rev() {
            if let Some(variable) = frame.variables.get_mut(name) {
                let offset = variable.offset.clone();
                match variable.type_ {
                    PrimitiveType::Number => self.write_store_number(offset),
                    PrimitiveType::Bool => self.write_store_bool(offset),
                }
                return Ok(());
            }
        }

        Err(format!("Variable {} is not declared", name))
    }

    /// Load a value from the variable and push it to the stack
    fn load(&mut self, name: &str) -> Result<(), String> {
        if !self.initialized(&name.to_string()) {
            return Err(format!("Variable \"{}\" is used before initialized", name));
        }

        for frame in self.frames.iter_mut().rev() {
            if let Some(variable) = frame.variables.get(name) {
                let offset = variable.offset.clone();
                match variable.type_ {
                    PrimitiveType::Number => self.write_load_number(offset),
                    PrimitiveType::Bool => self.write_load_bool(offset),
                }
                return Ok(());
            }
        }

        Err(format!("Variable {} is not declared", name))
    }

    fn allocate(&mut self) {
        self.write_code(ByteCode::Allocate);
    }

    fn release(&mut self) {
        self.write_code(ByteCode::Release);
    }

    /// Declare a new variable. Current stack top value is used as the initial value.
    fn declare_variable(&mut self, name: String, type_: PrimitiveType, initialized: bool) {
        let frame = self.frames.last_mut().unwrap();
        let size = type_.size();
        frame.variables.insert(name.clone(), Variable {
            offset: frame.stack_offset + frame.stack_size,
            type_,
        });
        frame.stack_size += size;
        if initialized {
            frame.initialized.insert(name);
        }
    }

    fn enter_scope(&mut self, breakable: bool) {
        let frame = self.frames.last().unwrap();
        let mut new_frame = StackFrame::new(frame.stack_offset + frame.stack_size, breakable);
        new_frame.ip_start = self.opcodes.len();
        self.frames.push(new_frame);
    }

    fn exit_scope(&mut self) {
        let frame = self.frames.pop().unwrap();
        for ip in frame.patch_ips.into_iter() {
            self.patch_address(ip);
        }
        self.write_flush(frame.stack_offset);
    }

    fn register_break_to_patch(&mut self, ip: usize) -> Result<(), String> {
        for frame in self.frames.iter_mut().rev() {
            if frame.breakable {
                frame.patch_ips.push(ip);
                return Ok(());
            }
        }

        Err("Break statement is used outside of loop".to_string())
    }
}

pub fn generate(program: &Program) -> Result<Vec<u8>, Error> {
    let mut generator = CodeGenerator::new();
    let errors = check(program).errors;
    if !errors.is_empty() {
        return Err(errors[0].clone());
    }

    for node in program.statements.iter() {
        generator.generate_node(node)?;
    }
    Ok(generator.get_codes())
}
