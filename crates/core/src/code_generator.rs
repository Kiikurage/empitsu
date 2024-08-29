use crate::node::Node;
use crate::punctuation_kind::PunctuationKind;
use std::collections::HashMap;
use std::ops::Deref;
use crate::bytecode::ByteCode;

struct StackFrame {
    stack_offset: usize,
    ip_start: usize,

    /// If this frame is breakable (i.e. frame made by for-loop)
    breakable: bool,

    /// Maps of variable names to their stack index in each scope
    variables: HashMap<String, usize>,

    /// List of instruction pointer that requires to be patched with ip_end
    patch_ips: Vec<usize>,
}

impl StackFrame {
    fn new(offset: usize, breakable: bool) -> Self {
        Self {
            stack_offset: offset,
            variables: HashMap::new(),
            breakable,
            ip_start: 0usize,
            patch_ips: vec![],
        }
    }
}

struct CodeGenerator {
    codes: Vec<ByteCode>,

    frames: Vec<StackFrame>,
}

impl CodeGenerator {
    fn new() -> Self {
        Self {
            codes: vec![],
            frames: vec![StackFrame::new(0, false)],
        }
    }

    fn generate_node(&mut self, node: &Node) {
        match node {
            Node::Program(nodes) => {
                self.generate_nodes(nodes);
            }
            Node::IfStatement(condition, true_branch, false_branch) => {
                self.generate_node(condition);

                let ip_conditional_jump = self.codes.len();
                self.write(ByteCode::JumpIfZero(/*dummy*/ 0));

                self.generate_node(true_branch);

                let ip_after_true_branch = self.codes.len();
                if false_branch.is_some() {
                    self.write(ByteCode::Jump(/*dummy*/ 0));
                }
                self.codes[ip_conditional_jump] = ByteCode::JumpIfZero(self.codes.len());

                if let Some(false_branch) = false_branch {
                    self.generate_node(false_branch);
                    self.codes[ip_after_true_branch] = ByteCode::Jump(self.codes.len());
                }
            }
            Node::EmptyStatement => {}
            Node::ForStatement(for_) => {
                self.enter_scope(true);

                // initializer
                self.write(ByteCode::Constant(0.0));
                self.declare_variable(for_.variable.clone());

                // condition
                let ip_condition_start = self.codes.len();
                self.load(&for_.variable);
                self.write(ByteCode::Constant(5.0));
                self.write(ByteCode::LessThan);
                let ip_conditional_jump = self.codes.len();
                self.write(ByteCode::JumpIfZero(/*dummy*/ 0));

                // body
                self.generate_node(&for_.body);

                // update
                self.load(&for_.variable);
                self.write(ByteCode::Constant(1.0));
                self.write(ByteCode::Add);
                self.store(&for_.variable);

                // loop
                self.write(ByteCode::Jump(ip_condition_start));
                let ip_after_loop = self.codes.len();

                self.codes[ip_conditional_jump] = ByteCode::JumpIfZero(ip_after_loop);
                self.exit_scope();
            }
            Node::VariableDeclaration(name, _type, initializer) => {
                match initializer {
                    Some(initializer) => {
                        self.declare_variable(name.clone());
                        self.generate_node(initializer);
                    }
                    None => {
                        // Use usize::MAX as a marker of "uninitialized" value
                        self.declare_uninitialized_variable(name.clone());
                    }
                }
            }
            Node::FunctionDeclaration(_) => unimplemented!("FunctionDeclaration"),
            Node::StructDeclaration(_) => unimplemented!("StructDeclaration"),
            Node::InterfaceDeclaration(_) => unimplemented!("InterfaceDeclaration"),
            Node::ImplStatement(_) => unimplemented!("ImplStatement"),
            Node::ReturnExpression(_) => unimplemented!("ReturnExpression"),
            Node::BreakExpression => {
                let ip_break = self.codes.len();
                self.write(ByteCode::Jump(/*dummy*/ 0));
                self.register_break_to_patch(ip_break);
            }
            Node::FunctionExpression(_) => unimplemented!("FunctionExpression"),
            Node::IfExpression(condition, true_branch, false_branch) => {
                self.generate_node(condition);

                let ip_conditional_jump = self.codes.len();
                self.write(ByteCode::JumpIfZero(/*dummy*/ 0));

                self.generate_node(true_branch);

                let ip_after_true_branch = self.codes.len();
                self.write(ByteCode::Jump(/*dummy*/ 0));

                self.codes[ip_conditional_jump] = ByteCode::JumpIfZero(self.codes.len());

                self.generate_node(false_branch);

                self.codes[ip_after_true_branch] = ByteCode::Jump(self.codes.len());
            }
            Node::BlockExpression(block) => {
                self.enter_scope(false);
                self.generate_nodes(&block.nodes);
                self.exit_scope();
            }
            Node::AssignmentExpression(lhs, rhs) => {
                match lhs.deref() {
                    Node::Identifier(name) => {
                        self.generate_node(rhs);
                        self.store(name);
                    }
                    _ => unimplemented!("AssignmentExpression"),
                }
            }
            Node::BinaryExpression(lhs, op, rhs) => {
                self.generate_node(lhs);
                self.generate_node(rhs);
                match op {
                    PunctuationKind::Plus => self.write(ByteCode::Add),
                    PunctuationKind::Minus => self.write(ByteCode::Subtract),
                    PunctuationKind::Asterisk => self.write(ByteCode::Multiply),
                    PunctuationKind::Slash => self.write(ByteCode::Divide),
                    PunctuationKind::AndAnd => self.write(ByteCode::LogicalAnd),
                    PunctuationKind::VerticalLineVerticalLine => self.write(ByteCode::LogicalOr),
                    PunctuationKind::LeftChevron => self.write(ByteCode::LessThan),
                    PunctuationKind::LeftChevronEqual => self.write(ByteCode::LessThanOrEqual),
                    PunctuationKind::RightChevron => self.write(ByteCode::GreaterThan),
                    PunctuationKind::RightChevronEqual => self.write(ByteCode::GreaterThanEqual),
                    PunctuationKind::EqualEqual => self.write(ByteCode::Equal),
                    PunctuationKind::ExclamationEqual => self.write(ByteCode::NotEqual),
                    _ => panic!("Unsupported binary operator: {:?}", op),
                }
            }
            Node::UnaryExpression(operator, operand) => {
                self.generate_node(operand);
                match operator {
                    PunctuationKind::Plus => (),
                    PunctuationKind::Minus => self.write(ByteCode::Negative),
                    PunctuationKind::Exclamation => self.write(ByteCode::LogicalNot),
                    _ => panic!("Unsupported unary operator: {:?}", operator),
                }
            }
            Node::CallExpression(_, _) => unimplemented!("CallExpression"),
            Node::MemberExpression(_, _) => unimplemented!("MemberExpression"),
            Node::Identifier(name) => {
                self.write(ByteCode::Load(self.get_variable_index(name).unwrap()));
            }
            Node::Number(value) => {
                self.write(ByteCode::Constant(*value));
            }
            Node::Bool(value) => {
                self.write(ByteCode::Constant(if *value { 1.0 } else { 0.0 }));
            }
            Node::String(..) => unimplemented!("String"),
            Node::RangeIterator(..) => unimplemented!("RangeIterator"),
        }
    }

    fn generate_nodes(&mut self, nodes: &[Node]) {
        for node in nodes.iter() {
            self.generate_node(node);
        }
    }

    fn write(&mut self, code: ByteCode) {
        self.codes.push(code);
    }

    fn get_variable_index(&self, name: &str) -> Option<usize> {
        for frame in self.frames.iter().rev() {
            if let Some(index) = frame.variables.get(name) {
                if index == &usize::MAX {
                    // uninitialized variable
                    return Some(usize::MAX);
                }

                return Some(frame.stack_offset + index);
            }
        }
        None
    }

    /// Peek a top value of stack, and store it to the variable
    fn store(&mut self, name: &str) {
        if let Some(index) = self.get_variable_index(name) {
            if index == usize::MAX {
                self.mark_variable_initialized(name.to_string());
            } else {
                self.write(ByteCode::Store(index));
            }
            return;
        }
        panic!("Variable {} is not declared", name);
    }

    /// Load a value from the variable and push it to the stack
    fn load(&mut self, name: &str) {
        if let Some(index) = self.get_variable_index(name) {
            if index == usize::MAX {
                panic!("Variable {} is not initialized", name);
            }

            self.write(ByteCode::Load(index));
            return;
        }
        panic!("Variable {} is not declared", name);
    }

    /// Declare a new variable. Current stack top value is used as the initial value.
    fn declare_variable(&mut self, name: String) {
        let frame = self.frames.last_mut().unwrap();
        frame.variables.insert(name, frame.variables.len());
    }

    /// Declare a new variable without incrementing the stack pointer
    fn declare_uninitialized_variable(&mut self, name: String) {
        self.frames.last_mut().unwrap().variables.insert(name, usize::MAX);
    }

    /// Mark a variable as initialized
    fn mark_variable_initialized(&mut self, name: String) {
        for frame in self.frames.iter_mut().rev() {
            if let Some(index) = frame.variables.get(&name) {
                if index != &usize::MAX {
                    panic!("Variable {} is already initialized", name);
                }
                frame.variables.insert(name, frame.variables.len());
                return;
            }
        }

        panic!("Variable {} is not declared", name);
    }

    fn enter_scope(&mut self, breakable: bool) {
        let frame = self.frames.last().unwrap();
        let mut new_frame = StackFrame::new(frame.stack_offset + frame.variables.len(), breakable);
        new_frame.ip_start = self.codes.len();
        self.frames.push(new_frame);
    }

    fn exit_scope(&mut self) {
        let frame = self.frames.pop().unwrap();
        let ip_end = self.codes.len();

        for ip in frame.patch_ips.into_iter() {
            match self.codes[ip] {
                ByteCode::Jump(..) => {
                    self.codes[ip] = ByteCode::Jump(ip_end);
                }
                ByteCode::JumpIfZero(..) => {
                    self.codes[ip] = ByteCode::JumpIfZero(ip_end);
                }
                _ => panic!("Invalid patch target"),
            }
        }

        self.write(ByteCode::Flush(frame.stack_offset));
    }

    fn register_break_to_patch(&mut self, ip: usize) {
        for frame in self.frames.iter_mut().rev() {
            if frame.breakable {
                frame.patch_ips.push(ip);
                return;
            }
        }
        panic!("Break statement is not inside a loop");
    }
}

pub fn generate(node: &Node) -> Vec<ByteCode> {
    let mut generator = CodeGenerator::new();
    generator.generate_node(node);
    generator.codes
}
