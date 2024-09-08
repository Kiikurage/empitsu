use crate::analysis::expression_info::ExpressionInfo;
use crate::analysis::type_::Type;
use crate::analysis::Analysis;
use crate::analyze::analyze_program;
use crate::ast::function::Function;
use crate::ast::get_range::GetRange;
use crate::ast::node::Node;
use crate::ast::program::Program;
use crate::error::Error;
use crate::position::Position;
use crate::punctuation_kind::PunctuationKind;
use crate::vm::bytecode::ByteCode;
use crate::vm::bytecode_like::{ByteCodeLike, Label, LabelId};
use crate::vm::Value;
use std::collections::HashMap;
use std::ops::{Deref, Range};

struct Scope {
    /// Range of the node that produces this stack frame
    range: Range<Position>,

    /// Maps of the position where variable is defined, to their stack offset in each scope
    stack_offset: usize,

    /// List of instruction pointer that requires to be patched with ip_end
    patch_ips: Vec<usize>,

    /// Label to jump when break statement is executed
    break_label_id: Option<LabelId>,
}

impl Scope {
    fn new(offset: usize, range: Range<Position>, break_label_id: Option<LabelId>) -> Self {
        Self {
            range,
            stack_offset: offset,
            patch_ips: vec![],
            break_label_id,
        }
    }
}

struct StackItem {
    offset: usize,
}

struct StackFrame {
    scopes: Vec<Scope>,
    offsets: HashMap<Range<Position>, StackItem>,
    stack_size: usize,
}

impl StackFrame {
    fn new() -> Self {
        Self {
            scopes: vec![],
            offsets: HashMap::new(),
            stack_size: 0,
        }
    }
}

pub struct Generator {
    labels: HashMap<usize, Label>,
    opcodes: Vec<ByteCodeLike>,
    frames: Vec<StackFrame>,
    analysis: Analysis,
}

///
/// - 4byte: number of literal in literal tables (N)
/// - 16byte * N: (offset: usize, length: usize) of each literal.
///               Offset is from the start of the literal table.
/// - Literal table: each literal binary
/// - op codes
///

impl Generator {
    pub fn generate(program: &Program) -> Result<Vec<ByteCode>, Error> {
        let analysis = analyze_program(program);
        if !analysis.errors.is_empty() {
            return Err(analysis.errors.first().unwrap().clone());
        }

        let mut frame = StackFrame::new();
        frame.scopes.push(Scope::new(0, program.range(), None));
        let frames = vec![frame];

        let mut generator = Self {
            opcodes: vec![],
            frames,
            analysis,
            labels: HashMap::new(),
        };

        generator.generate_program(program);
        Ok(generator.emit_codes())
    }

    fn generate_nodes(&mut self, nodes: &[Node]) {
        for node in nodes.iter() {
            self.generate_node(node);
        }
    }

    fn generate_program(&mut self, program: &Program) {
        self.generate_nodes(&program.statements);
    }

    fn generate_node(&mut self, node: &Node) {
        match node {
            Node::Program(program) => {
                self.generate_program(program);
            }
            Node::IfStatement(if_statement) => {
                let label_false_branch = self.create_label();

                self.generate_node(&if_statement.condition);
                self.jump_if_false(label_false_branch);

                let stack_size = self.get_current_frame().stack_size;
                self.generate_node(&if_statement.true_branch);

                match &if_statement.false_branch {
                    Some(false_branch) => {
                        let label_end = self.create_label();
                        self.jump(label_end);

                        self.insert_label(label_false_branch);
                        self.get_current_frame_mut().stack_size = stack_size;
                        self.generate_node(false_branch);

                        self.insert_label(label_end);
                    }
                    None => {
                        self.insert_label(label_false_branch);
                    }
                }
            }
            Node::ForStatement(for_) => {
                let label_end_for = self.create_label();
                let label_main_loop = self.create_label();

                self.enter_scope(for_.range(), Some(label_end_for));
                {
                    // initializer
                    self.push_constant(Value::Number(0.0));
                    self.allocate_symbol_in_stack(for_.variable.range());

                    // condition
                    self.insert_label(label_main_loop);

                    self.load(&for_.variable.range());
                    self.push_constant(Value::Number(5.0));
                    self.less_than();
                    self.jump_if_false(label_end_for);

                    // body
                    self.generate_node(&for_.body);

                    // update
                    self.load(&for_.variable.range());
                    self.push_constant(Value::Number(1.0));
                    self.add();
                    self.store(&for_.variable.range());

                    // loop
                    self.jump(label_main_loop);

                    self.insert_label(label_end_for);
                }
                self.exit_scope(self.get_current_scope().stack_offset);
            }
            Node::VariableDeclaration(ref variable_declaration) => {
                let variable = self.analysis
                    .get_variable_info(&variable_declaration.range()).unwrap();

                match &variable_declaration.initializer {
                    Some(initializer) => self.generate_node(initializer),
                    None => {
                        match variable.type_ {
                            Type::Number => self.push_constant(Value::Number(0.0)),
                            Type::Bool => self.push_constant(Value::Bool(false)),
                            _ => unreachable!("Expected primitive type"),
                        }
                    }
                }

                self.allocate_symbol_in_stack(variable_declaration.range());
            }
            Node::FunctionDeclaration(function) => self.define_function(function),
            Node::StructDeclaration(_) => unreachable!("StructDeclaration"),
            Node::InterfaceDeclaration(_) => unreachable!("InterfaceDeclaration"),
            Node::ImplStatement(_) => unreachable!("ImplStatement"),
            Node::Return(_) => unreachable!("Return"),
            Node::Break(_break_) => {
                let label = self.get_current_loop_label();
                self.jump(label);
            }
            Node::FunctionExpression(function) => self.define_function(function),
            Node::IfExpression(if_expression) => {
                let label_false_branch = self.create_label();
                let label_end_if = self.create_label();

                self.generate_node(&if_expression.condition);
                self.jump_if_false(label_false_branch);

                let stack_size = self.get_current_frame().stack_size;
                self.generate_node(&if_expression.true_branch);
                self.jump(label_end_if);

                self.insert_label(label_false_branch);
                self.get_current_frame_mut().stack_size = stack_size;
                self.generate_node(&if_expression.false_branch);
                self.insert_label(label_end_if)
            }
            Node::Block(block) => {
                self.enter_scope(block.range(), None);
                for node in block.nodes.iter() {
                    self.generate_node(node);
                }

                // Keep the last value in the stack
                let scope = self.get_current_scope();
                let stack_offset = scope.stack_offset;
                let type_ = self.analysis.get_expression_type(&block.range()).unwrap_or(&Type::Void);
                let flush_dest = stack_offset + 1;

                if type_ != &Type::Void {
                    self.opcodes.push(ByteCodeLike::Store(stack_offset));
                }

                self.exit_scope(flush_dest);
            }
            Node::AssignmentExpression(expression) => {
                match &expression.lhs.deref() {
                    Node::Identifier(name) => {
                        self.generate_node(&expression.rhs);
                        self.store(&name.range())
                    }
                    _ => unreachable!("Assign to complex target is not supported")
                }
            }
            Node::BinaryExpression(expression) => {
                self.generate_node(&expression.lhs);
                self.generate_node(&expression.rhs);
                match &expression.operator {
                    PunctuationKind::Plus => self.add(),
                    PunctuationKind::Minus => self.subtract(),
                    PunctuationKind::Asterisk => self.multiply(),
                    PunctuationKind::Slash => self.divide(),
                    PunctuationKind::AndAnd => self.logical_and(),
                    PunctuationKind::VerticalLineVerticalLine => self.logical_or(),
                    PunctuationKind::LeftChevron => self.less_than(),
                    PunctuationKind::LeftChevronEqual => self.less_than_or_equal(),
                    PunctuationKind::RightChevron => self.greater_than(),
                    PunctuationKind::RightChevronEqual => self.greater_than_or_equal(),
                    PunctuationKind::EqualEqual => self.equal(),
                    PunctuationKind::ExclamationEqual => self.not_equal(),
                    _ => unreachable!("Unsupported binary operator: {:?}", expression.operator),
                }
            }
            Node::UnaryExpression(expression) => {
                self.generate_node(&expression.operand);
                match &expression.operator {
                    PunctuationKind::Plus => (),
                    PunctuationKind::Minus => self.negative(),
                    PunctuationKind::Exclamation => self.logical_not(),
                    _ => unreachable!("Unsupported unary operator: {:?}", expression.operator),
                }
            }
            Node::CallExpression(expression) => {
                let stack_address = self.get_current_frame().stack_size;

                // calleeの評価
                self.generate_node(expression.callee.deref());

                // 引数をスタックに積む
                for parameter in expression.parameters.iter() {
                    self.generate_node(parameter.value.deref());
                }

                self.call(stack_address);

                // fn_addr
                // - 関数本体
                // - 戻り先ip
                // - スタックのオフセット

                // CALL(fn_addr)
                // - 関数本体へジャンプ
                // - 変数の解決時は、関数のスタックフレームを参照する
                // - 戻り値をスタックの一番下に配置
                // - スタックをクリーンアップ
                // - 元の場所へジャンプ
            }
            Node::MemberExpression(_expression) => unreachable!("MemberExpression"),
            Node::Identifier(name) => {
                self.load(&name.range());
            }
            Node::NumberLiteral(value) => {
                self.push_constant(Value::Number(value.value));
            }
            Node::BoolLiteral(value) => {
                self.push_constant(Value::Bool(value.value));
            }
            Node::StringLiteral(_string_literal) => {
                unreachable!("StringLiteral")
            }
            Node::TypeExpression(_) => unreachable!("TypeExpression"),
        }
    }

    // Push codes into buffer

    fn push_constant(&mut self, value: Value) {
        self.opcodes.push(ByteCodeLike::Constant(value));
        self.get_current_frame_mut().stack_size += 1;
    }

    fn store(&mut self, range: &Range<Position>) {
        let variable = self.analysis
            .get_variable_info(range).unwrap();

        let range = variable.range();
        let frame = self.get_current_frame();
        if let Some(stack_item) = frame.offsets.get(&range) {
            let offset = stack_item.offset;
            self.opcodes.push(ByteCodeLike::Store(offset));
            return;
        }

        unreachable!("Variable is not declared")
    }

    fn load(&mut self, range: &Range<Position>) {
        let defined_at = match self.analysis.get_expression_info(range) {
            Some(ExpressionInfo::Function(symbol_ref)) => symbol_ref.defined_at.clone().unwrap(),
            Some(ExpressionInfo::Variable(symbol_ref)) => symbol_ref.defined_at.clone().unwrap(),
            _ => unreachable!("Expected function or variable"),
        };

        let stack_item = self.get_current_frame().offsets.get(&defined_at).unwrap();
        let offset = stack_item.offset;

        self.opcodes.push(ByteCodeLike::Load(offset));
        self.get_current_frame_mut().stack_size += 1;
    }

    fn jump(&mut self, label_id: LabelId) {
        self.opcodes.push(ByteCodeLike::Jump(label_id));
    }

    fn jump_if_false(&mut self, label_id: LabelId) {
        self.get_current_frame_mut().stack_size -= 1;
        self.opcodes.push(ByteCodeLike::JumpIfFalse(label_id));
    }

    fn define_function(&mut self, function: &Function) {
        let opcodes_start = self.opcodes.len();

        self.frames.push(StackFrame::new());
        for parameter in function.interface.parameters.iter() {
            self.get_current_frame_mut().stack_size += 1;
            self.allocate_symbol_in_stack(parameter.range());
        }
        self.generate_nodes(&function.body);
        self.opcodes.push(ByteCodeLike::PopCallStack);
        self.frames.pop();

        let body_size = self.opcodes.len() - opcodes_start;

        self.opcodes.insert(opcodes_start, ByteCodeLike::DefineFunction(body_size));
        self.get_current_frame_mut().stack_size += 1;
        self.allocate_symbol_in_stack(function.range());
    }

    fn call(&mut self, stack_address: usize) {
        self.opcodes.push(ByteCodeLike::Call(stack_address));
    }

    fn add(&mut self) {
        self.get_current_frame_mut().stack_size -= 1;
        self.opcodes.push(ByteCodeLike::Add);
    }

    fn subtract(&mut self) {
        self.get_current_frame_mut().stack_size -= 1;
        self.opcodes.push(ByteCodeLike::Subtract);
    }

    fn multiply(&mut self) {
        self.get_current_frame_mut().stack_size -= 1;
        self.opcodes.push(ByteCodeLike::Multiply);
    }

    fn divide(&mut self) {
        self.get_current_frame_mut().stack_size -= 1;
        self.opcodes.push(ByteCodeLike::Divide);
    }

    fn logical_and(&mut self) {
        self.get_current_frame_mut().stack_size -= 1;
        self.opcodes.push(ByteCodeLike::LogicalAnd);
    }

    fn logical_or(&mut self) {
        self.get_current_frame_mut().stack_size -= 1;
        self.opcodes.push(ByteCodeLike::LogicalOr);
    }

    fn less_than(&mut self) {
        self.get_current_frame_mut().stack_size -= 1;
        self.opcodes.push(ByteCodeLike::LessThan);
    }

    fn less_than_or_equal(&mut self) {
        self.get_current_frame_mut().stack_size -= 1;
        self.opcodes.push(ByteCodeLike::LessThanOrEqual);
    }

    fn greater_than(&mut self) {
        self.get_current_frame_mut().stack_size -= 1;
        self.opcodes.push(ByteCodeLike::GreaterThan);
    }

    fn greater_than_or_equal(&mut self) {
        self.get_current_frame_mut().stack_size -= 1;
        self.opcodes.push(ByteCodeLike::GreaterThanOrEqual);
    }

    fn equal(&mut self) {
        self.get_current_frame_mut().stack_size -= 1;
        self.opcodes.push(ByteCodeLike::Equal);
    }

    fn not_equal(&mut self) {
        self.get_current_frame_mut().stack_size -= 1;
        self.opcodes.push(ByteCodeLike::NotEqual);
    }

    fn negative(&mut self) {
        self.opcodes.push(ByteCodeLike::Negative);
    }

    fn logical_not(&mut self) {
        self.opcodes.push(ByteCodeLike::LogicalNot);
    }

    fn create_label(&mut self) -> LabelId {
        let id = self.labels.len();
        let label = Label::new(id);
        self.labels.insert(id, label);
        id
    }

    fn insert_label(&mut self, label_id: LabelId) {
        self.opcodes.push(ByteCodeLike::Label(label_id));
    }

    fn allocate_symbol_in_stack(&mut self, defined_at: Range<Position>) {
        let frame = self.get_current_frame_mut();
        let offset = frame.stack_size - 1;
        frame.offsets.insert(defined_at, StackItem { offset });
    }

    fn get_current_frame(&self) -> &StackFrame {
        self.frames.last().unwrap()
    }

    fn get_current_frame_mut(&mut self) -> &mut StackFrame {
        self.frames.last_mut().unwrap()
    }

    fn get_current_scope(&self) -> &Scope {
        self.get_current_frame().scopes.last().unwrap()
    }

    fn get_current_scope_mut(&mut self) -> &mut Scope {
        self.get_current_frame_mut().scopes.last_mut().unwrap()
    }

    fn get_current_loop_label(&self) -> LabelId {
        for scope in self.get_current_frame().scopes.iter().rev() {
            if let Some(label) = &scope.break_label_id {
                return *label;
            }
        }

        unreachable!("Break statement is not in loop")
    }

    fn enter_scope(&mut self, scope_range: Range<Position>, break_label_id: Option<LabelId>) {
        let frame = self.get_current_frame_mut();
        let new_scope = Scope::new(frame.stack_size, scope_range, break_label_id);
        frame.scopes.push(new_scope);
    }

    fn exit_scope(&mut self, expected_size: usize) {
        self.get_current_frame_mut().scopes.pop().unwrap();
        self.opcodes.push(ByteCodeLike::Flush(expected_size));
        self.get_current_frame_mut().stack_size = expected_size;
    }

    // Emit codes

    fn emit_codes(&mut self) -> Vec<ByteCode> {
        let mut buffer = vec![];

        let opcodes = self.opcodes.clone();

        let label_to_ip = {
            let mut label_to_ip = HashMap::new();
            let mut ip = 0;
            for code in opcodes.iter() {
                if let ByteCodeLike::Label(label_id) = code {
                    label_to_ip.insert(*label_id, ip);
                } else {
                    ip += 1;
                }
            }
            label_to_ip
        };

        for code in opcodes.iter() {
            match code {
                ByteCodeLike::Constant(value) => {
                    buffer.push(ByteCode::Constant(value.clone()));
                }
                ByteCodeLike::Load(address) => {
                    buffer.push(ByteCode::Load(*address));
                }
                ByteCodeLike::Store(address) => {
                    buffer.push(ByteCode::Store(*address));
                }
                ByteCodeLike::Jump(label_id) => {
                    buffer.push(ByteCode::Jump(label_to_ip[label_id]));
                }
                ByteCodeLike::JumpIfFalse(label_id) => {
                    buffer.push(ByteCode::JumpIfFalse(label_to_ip[label_id]));
                }
                ByteCodeLike::Flush(size) => {
                    buffer.push(ByteCode::Flush(*size));
                }
                ByteCodeLike::DefineFunction(size) => {
                    buffer.push(ByteCode::DefineFunction(*size));
                }
                ByteCodeLike::Call(stack_address) => {
                    buffer.push(ByteCode::Call(*stack_address));
                }
                ByteCodeLike::PopCallStack => {
                    buffer.push(ByteCode::PopCallStack);
                }
                ByteCodeLike::Add => {
                    buffer.push(ByteCode::Add);
                }
                ByteCodeLike::Subtract => {
                    buffer.push(ByteCode::Subtract);
                }
                ByteCodeLike::Multiply => {
                    buffer.push(ByteCode::Multiply);
                }
                ByteCodeLike::Divide => {
                    buffer.push(ByteCode::Divide);
                }
                ByteCodeLike::LogicalAnd => {
                    buffer.push(ByteCode::LogicalAnd);
                }
                ByteCodeLike::LogicalOr => {
                    buffer.push(ByteCode::LogicalOr);
                }
                ByteCodeLike::LessThan => {
                    buffer.push(ByteCode::LessThan);
                }
                ByteCodeLike::LessThanOrEqual => {
                    buffer.push(ByteCode::LessThanOrEqual);
                }
                ByteCodeLike::GreaterThan => {
                    buffer.push(ByteCode::GreaterThan);
                }
                ByteCodeLike::GreaterThanOrEqual => {
                    buffer.push(ByteCode::GreaterThanOrEqual);
                }
                ByteCodeLike::Equal => {
                    buffer.push(ByteCode::Equal);
                }
                ByteCodeLike::NotEqual => {
                    buffer.push(ByteCode::NotEqual);
                }
                ByteCodeLike::Negative => {
                    buffer.push(ByteCode::Negative);
                }
                ByteCodeLike::LogicalNot => {
                    buffer.push(ByteCode::LogicalNot);
                }
                ByteCodeLike::Label(..) => {}
            };
        }

        buffer
    }
}
