use crate::error::Error;
use crate::node::{FunctionInterfaceNode, FunctionNode, FunctionParameterDeclaration, ImplStatementNode, InterfaceDeclarationNode, Node, Parameter, StructDeclarationNode, StructPropertyDeclaration, TypeExpression};
use crate::punctuation_kind::PunctuationKind;
use crate::token::{Token, TokenKind};
use crate::token_iterator::TokenIterator;

pub struct ParseResult {
    pub node: Node,
    pub errors: Vec<Error>,
}

impl ParseResult {
    pub fn new(node: Node, errors: Vec<Error>) -> Self {
        Self { node, errors }
    }
}

fn is_reserved(word: &str) -> bool {
    matches!(
        word,
        "break"  | "else" | "false" | "fn" | "for" | "if" | "impl" | "in" | "interface" |
        "let" | "null" | "return" | "struct" | "true"
    )
}

fn assert_keyword(token: &Result<Token, Error>, expected: &str) -> Result<(), Error> {
    match token {
        Ok(Token { kind: TokenKind::Identifier(ref keyword), .. }) if keyword == expected => Ok(()),
        Ok(token) => Err(Error::unexpected_token(token.position.clone(), expected)),
        Err(err) => Err(err.clone()),
    }
}

fn assert_non_reserved_word(token: &Result<Token, Error>) -> Result<String, Error> {
    match token {
        Ok(Token { position, kind: TokenKind::Identifier(word), .. }) => {
            if is_reserved(word) {
                Err(Error::reserved_word(position.clone(), word.clone()))
            } else {
                Ok(word.clone())
            }
        }
        Ok(token) => Err(Error::unexpected_token(token.position.clone(), "identifier")),
        Err(err) => Err(err.clone()),
    }
}

macro_rules! assert_punctuation {
    ($token:expr, $expected:ident) => {
        match $token {
            Ok(Token { kind: TokenKind::Punctuation(ref kind @ PunctuationKind::$expected), .. }) => Ok(kind),
            Ok(token) => Err(Error::unexpected_token(token.position.clone(), PunctuationKind::$expected.get_text())),
            Err(err) => Err(err.clone()),
        }
    };
    ($token:expr, $($expected:ident),+) => {
        match $token {
            Ok(Token { kind: TokenKind::Punctuation(ref kind @ ($(PunctuationKind::$expected)|+)), .. }) => Ok(kind),
            Ok(token) => {
                let expected_tokens = [$(PunctuationKind::$expected.get_text()),+].join(", ");
                Err(Error::unexpected_token(token.position.clone(), expected_tokens))
            }
            Err(err) => Err(err.clone()),
        }
    };
}

type Parser = fn(tokens: &mut TokenIterator) -> Result<Node, Error>;

pub fn parse(input: &str) -> ParseResult {
    let mut iter = TokenIterator::new(input);
    parse_program(&mut iter)
}

fn parse_one_of(tokens: &mut TokenIterator, parsers: Vec<Parser>, expected_node: &str) -> Result<Node, Error> {
    let current = tokens.raw_offset;
    let mut best_result = parsers.first().unwrap()(tokens);
    let mut best_current = tokens.raw_offset;

    for parser in parsers.iter().skip(1) {
        tokens.raw_offset = current;
        let result = parser(tokens);

        if (result.is_ok() && (best_result.is_err() || tokens.raw_offset > best_current)) ||
            (result.is_err() && (best_result.is_err() && tokens.raw_offset > best_current)) {
            best_result = result;
            best_current = tokens.raw_offset;
        }
    }

    if best_current == current {
        Err(Error::unexpected_token(tokens.get_position().clone(), expected_node))
    } else {
        tokens.raw_offset = best_current;
        best_result
    }
}

fn parse_program(tokens: &mut TokenIterator) -> ParseResult {
    let mut statements = Vec::new();
    let mut errors = Vec::new();

    while tokens.has_next() {
        match parse_statement(tokens) {
            Ok(statement) => {
                statements.push(statement);
            }
            Err(err) => {
                errors.push(err);

                // Skip until the next statement
                let mut block_depth = 0;
                while tokens.has_next() {
                    match tokens.raw_next() {
                        Ok(Token { kind: TokenKind::Punctuation(PunctuationKind::SemiColon), .. }) |
                        Ok(Token { kind: TokenKind::LineTerminator, .. }) => {
                            break;
                        }
                        Ok(Token { kind: TokenKind::Punctuation(PunctuationKind::LeftBrace), .. }) => {
                            block_depth += 1;
                        }
                        Ok(Token { kind: TokenKind::Punctuation(PunctuationKind::RightBrace), .. }) => {
                            block_depth -= 1;
                            if block_depth == 0 {
                                break;
                            }
                        }
                        Ok(..) => (),
                        Err(err) => errors.push(err.clone()),
                    }
                }
            }
        }
    }

    ParseResult::new(Node::Program(statements), errors)
}

// Statement

fn parse_statement(tokens: &mut TokenIterator) -> Result<Node, Error> {
    parse_semicolon(tokens);
    let ret = parse_one_of(tokens, vec![
        parse_if_statement,
        parse_block_statement,
        parse_return_statement,
        parse_break_statement,
        parse_for_statement,
        parse_variable_declaration,
        parse_function_declaration,
        parse_struct_declaration,
        parse_interface_declaration,
        parse_impl_statement,
        parse_expression_statement,
    ], "statement");
    parse_semicolon(tokens);

    ret
}

fn parse_if_statement(tokens: &mut TokenIterator) -> Result<Node, Error> {
    assert_keyword(tokens.next(), "if")?;

    assert_punctuation!(tokens.next(), LeftParen)?;
    let condition = parse_expression(tokens)?;
    assert_punctuation!(tokens.next(), RightParen)?;

    let true_branch = parse_statement(tokens)?;
    let false_branch = if assert_keyword(tokens.peek(), "else").is_ok() {
        tokens.next();
        Some(Box::new(parse_statement(tokens)?))
    } else {
        None
    };

    Ok(Node::IfStatement(Box::new(condition), Box::new(true_branch), false_branch))
}

fn parse_block_statement(tokens: &mut TokenIterator) -> Result<Node, Error> {
    let block_expression = parse_block_expression(tokens)?;
    parse_statement_end(tokens)?;

    Ok(block_expression)
}

fn parse_return_statement(tokens: &mut TokenIterator) -> Result<Node, Error> {
    let return_expression = parse_return_expression(tokens)?;
    parse_statement_end(tokens)?;

    Ok(return_expression)
}

fn parse_break_statement(tokens: &mut TokenIterator) -> Result<Node, Error> {
    let break_expression = parse_break_expression(tokens)?;
    parse_statement_end(tokens)?;

    Ok(break_expression)
}

fn parse_for_statement(tokens: &mut TokenIterator) -> Result<Node, Error> {
    assert_keyword(tokens.next(), "for")?;

    assert_punctuation!(tokens.next(), LeftParen)?;
    let variable = assert_non_reserved_word(tokens.next())?;
    assert_keyword(tokens.next(), "in")?;
    let iterable = parse_expression(tokens)?;
    assert_punctuation!(tokens.next(), RightParen)?;

    let body = parse_statement(tokens)?;

    Ok(Node::ForStatement(variable, Box::new(iterable), Box::new(body)))
}

fn parse_variable_declaration(tokens: &mut TokenIterator) -> Result<Node, Error> {
    assert_keyword(tokens.next(), "let")?;
    let name = assert_non_reserved_word(tokens.next())?;

    let type_ = if assert_punctuation!(tokens.peek(), Colon).is_ok() {
        tokens.next();
        Some(parse_type_expression(tokens)?)
    } else {
        None
    };

    let initial_value = if assert_punctuation!(tokens.peek(), Equal).is_ok() {
        tokens.next();
        Some(Box::new(parse_expression(tokens)?))
    } else {
        None
    };

    parse_statement_end(tokens)?;

    Ok(Node::VariableDeclaration(name, type_, initial_value))
}

fn parse_function_declaration(tokens: &mut TokenIterator) -> Result<Node, Error> {
    let interface = parse_function_interface(tokens)?;
    let body = parse_block_expression(tokens)?;

    Ok(Node::FunctionDeclaration(FunctionNode {
        interface,
        body: Box::new(body),
    }))
}

fn parse_function_interface(tokens: &mut TokenIterator) -> Result<FunctionInterfaceNode, Error> {
    assert_keyword(tokens.next(), "fn")?;
    let name = assert_non_reserved_word(tokens.next())?;

    assert_punctuation!(tokens.next(), LeftParen)?;
    let mut parameters = vec![];
    while assert_punctuation!(tokens.peek(), RightParen).is_err() {
        let name = assert_non_reserved_word(tokens.next())?;
        let parameter = if name == "self" {
            FunctionParameterDeclaration { name, type_: TypeExpression::Identifier("self".to_string()) }
        } else {
            assert_punctuation!(tokens.next(), Colon)?;
            let type_ = parse_type_expression(tokens)?;

            FunctionParameterDeclaration { name, type_ }
        };
        parameters.push(parameter);

        if assert_punctuation!(tokens.peek(), Comma).is_ok() {
            tokens.next();
        } else {
            break;
        }
    }
    assert_punctuation!(tokens.next(), RightParen)?;

    assert_punctuation!(tokens.next(), Colon)?;
    let return_type = parse_type_expression(tokens)?;

    Ok(FunctionInterfaceNode {
        name,
        parameters,
        return_type: Box::new(return_type),
    })
}

fn parse_struct_declaration(tokens: &mut TokenIterator) -> Result<Node, Error> {
    assert_keyword(tokens.next(), "struct")?;
    let name = assert_non_reserved_word(tokens.next())?;

    assert_punctuation!(tokens.next(), LeftParen)?;
    let mut properties = vec![];
    while assert_punctuation!(tokens.peek(), RightParen).is_err() {
        let name = assert_non_reserved_word(tokens.next())?;
        assert_punctuation!(tokens.next(), Colon)?;
        let type_ = parse_type_expression(tokens)?;
        properties.push(StructPropertyDeclaration { name, type_ });

        if assert_punctuation!(tokens.peek(), Comma).is_ok() {
            tokens.next();
        } else {
            break;
        }
    }
    assert_punctuation!(tokens.next(), RightParen)?;

    let mut instance_methods = vec![];
    let mut static_methods = vec![];
    if assert_punctuation!(tokens.peek(), LeftBrace).is_ok() {
        tokens.next();

        while assert_punctuation!(tokens.peek(), RightBrace).is_err() {
            let position = tokens.get_position().clone();
            match parse_function_declaration(tokens)? {
                Node::FunctionDeclaration(function) => {
                    if matches!(function.interface.parameters.first(), Some(FunctionParameterDeclaration { name, .. }) if name == "self") {
                        instance_methods.push(function);
                    } else {
                        static_methods.push(function);
                    }
                }
                _ => return Err(Error::unexpected_token(position, "function")),
            }
        }
        assert_punctuation!(tokens.next(), RightBrace)?;
    }

    Ok(Node::StructDeclaration(StructDeclarationNode {
        name,
        properties,
        instance_methods,
        static_methods,
    }))
}

fn parse_interface_declaration(tokens: &mut TokenIterator) -> Result<Node, Error> {
    assert_keyword(tokens.next(), "interface")?;
    let name = assert_non_reserved_word(tokens.next())?;

    let mut instance_methods = vec![];
    if assert_punctuation!(tokens.peek(), LeftBrace).is_ok() {
        tokens.next();

        while assert_punctuation!(tokens.peek(), RightBrace).is_err() {
            instance_methods.push(parse_function_interface(tokens)?);
        }
        assert_punctuation!(tokens.next(), RightBrace)?;
    }

    Ok(Node::InterfaceDeclaration(InterfaceDeclarationNode {
        name,
        instance_methods,
    }))
}

fn parse_impl_statement(tokens: &mut TokenIterator) -> Result<Node, Error> {
    assert_keyword(tokens.next(), "impl")?;
    let interface_name = assert_non_reserved_word(tokens.next())?;

    assert_keyword(tokens.next(), "for")?;
    let struct_name = assert_non_reserved_word(tokens.next())?;

    let mut instance_methods = vec![];
    if assert_punctuation!(tokens.peek(), LeftBrace).is_ok() {
        tokens.next();

        while assert_punctuation!(tokens.peek(), RightBrace).is_err() {
            let position = tokens.get_position().clone();
            match parse_function_declaration(tokens)? {
                Node::FunctionDeclaration(function) => {
                    instance_methods.push(function);
                }
                _ => return Err(Error::unexpected_token(position, "function")),
            }
        }
        assert_punctuation!(tokens.next(), RightBrace)?;
    }

    Ok(Node::ImplStatement(ImplStatementNode {
        interface_name,
        struct_name,
        instance_methods,
    }))
}

fn parse_expression_statement(tokens: &mut TokenIterator) -> Result<Node, Error> {
    let expression = parse_expression(tokens)?;
    parse_statement_end(tokens)?;

    Ok(expression)
}

fn parse_statement_end(tokens: &mut TokenIterator) -> Result<(), Error> {
    match tokens.raw_peek() {
        Ok(Token { kind: TokenKind::Punctuation(PunctuationKind::SemiColon), .. }) |
        Ok(Token { kind: TokenKind::LineTerminator, .. }) => {
            tokens.raw_next();
            Ok(())
        }
        Ok(Token { kind: TokenKind::Punctuation(PunctuationKind::RightBrace), .. }) |
        Ok(Token { kind: TokenKind::EndOfInput, .. }) => {
            Ok(())
        }
        Ok(other) => Err(Error::unexpected_token(other.position.clone(), ";")),
        Err(err) => Err(err.clone()),
    }
}

fn parse_semicolon(tokens: &mut TokenIterator) {
    while assert_punctuation!(tokens.peek(), SemiColon).is_ok() {
        tokens.next();
    }
}

// Expression

fn parse_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    parse_one_of(tokens, vec![
        parse_assignment_expression,
        parse_range_expression,
        parse_logical_or_expression,
    ], "expression")
}

fn parse_assignment_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    let lhs = parse_left_hand_side_expression(tokens)?;
    assert_punctuation!(tokens.next(), Equal)?;
    let rhs = parse_expression(tokens)?;

    Ok(Node::AssignmentExpression(Box::new(lhs), Box::new(rhs)))
}

fn parse_range_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    let start = parse_logical_or_expression(tokens)?;
    assert_keyword(tokens.next(), "to")?;
    let end = parse_logical_or_expression(tokens)?;

    Ok(Node::RangeIterator(Box::new(start), Box::new(end)))
}

fn parse_logical_or_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    let mut lhs = parse_logical_and_expression(tokens)?;

    while let Ok(operator) = assert_punctuation!(tokens.peek(),
        VerticalLineVerticalLine
    ) {
        let operator = operator.clone();
        tokens.next();

        let rhs = parse_logical_and_expression(tokens)?;
        lhs = Node::BinaryExpression(Box::new(lhs), operator, Box::new(rhs));
    }

    Ok(lhs)
}

fn parse_logical_and_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    let mut lhs = parse_equality_expression(tokens)?;

    while let Ok(operator) = assert_punctuation!(tokens.peek(),
        AndAnd
    ) {
        let operator = operator.clone();
        tokens.next();

        let rhs = parse_equality_expression(tokens)?;
        lhs = Node::BinaryExpression(Box::new(lhs), operator, Box::new(rhs));
    }

    Ok(lhs)
}

fn parse_equality_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    let mut lhs = parse_relational_expression(tokens)?;

    while let Ok(operator) = assert_punctuation!(tokens.peek(),
        EqualEqual, ExclamationEqual
    ) {
        let operator = operator.clone();
        tokens.next();

        let rhs = parse_relational_expression(tokens)?;
        lhs = Node::BinaryExpression(Box::new(lhs), operator, Box::new(rhs));
    }

    Ok(lhs)
}

fn parse_relational_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    let mut lhs = parse_additive_expression(tokens)?;

    while let Ok(operator) = assert_punctuation!(tokens.peek(),
        LeftChevron, LeftChevronEqual, RightChevron,RightChevronEqual
    ) {
        let operator = operator.clone();
        tokens.next();

        let rhs = parse_additive_expression(tokens)?;
        lhs = Node::BinaryExpression(Box::new(lhs), operator, Box::new(rhs));
    }

    Ok(lhs)
}

fn parse_additive_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    let mut lhs = parse_multiplicative_expression(tokens)?;

    while let Ok(operator) = assert_punctuation!(tokens.peek(), Plus, Minus) {
        let operator = operator.clone();
        tokens.next();

        let rhs = parse_multiplicative_expression(tokens)?;
        lhs = Node::BinaryExpression(Box::new(lhs), operator, Box::new(rhs));
    }

    Ok(lhs)
}

fn parse_multiplicative_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    let mut lhs = parse_unary_expression(tokens)?;

    while let Ok(operator) = assert_punctuation!(tokens.peek(), Asterisk, Slash) {
        let operator = operator.clone();
        tokens.next();

        let rhs = parse_unary_expression(tokens)?;
        lhs = Node::BinaryExpression(Box::new(lhs), operator, Box::new(rhs));
    }

    Ok(lhs)
}

fn parse_unary_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    let operator = match tokens.peek() {
        Ok(Token { kind: TokenKind::Punctuation(PunctuationKind::Plus), .. }) => PunctuationKind::Plus,
        Ok(Token { kind: TokenKind::Punctuation(PunctuationKind::Minus), .. }) => PunctuationKind::Minus,
        Ok(Token { kind: TokenKind::Punctuation(PunctuationKind::Exclamation), .. }) => PunctuationKind::Exclamation,
        _ => return parse_statement_expression(tokens),
    };
    tokens.next();

    let position = tokens.get_position().clone();
    let operand = parse_unary_expression(tokens)?;
    if matches!(operand, Node::UnaryExpression(..)) {
        return Err(Error::unexpected_token(position, "expression"));
    }

    Ok(Node::UnaryExpression(operator, Box::new(operand)))
}

fn parse_statement_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    parse_one_of(tokens, vec![
        parse_if_expression,
        parse_block_expression,
        parse_return_expression,
        parse_break_expression,
        parse_function_expression,
        parse_call_expression,
    ], "expression")
}

fn parse_if_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    assert_keyword(tokens.next(), "if")?;

    assert_punctuation!(tokens.next(), LeftParen)?;
    let condition = parse_expression(tokens)?;
    assert_punctuation!(tokens.next(), RightParen)?;

    let true_branch = parse_expression(tokens)?;
    assert_keyword(tokens.next(), "else")?;
    let false_branch = parse_expression(tokens)?;

    Ok(Node::IfExpression(Box::new(condition), Box::new(true_branch), Box::new(false_branch)))
}

fn parse_block_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    assert_punctuation!(tokens.next(), LeftBrace)?;

    let mut statements = Vec::new();
    while assert_punctuation!(tokens.peek(), RightBrace).is_err() {
        statements.push(parse_statement(tokens)?);
    }

    assert_punctuation!(tokens.next(), RightBrace)?;

    Ok(Node::BlockExpression(statements))
}

fn parse_return_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    assert_keyword(tokens.next(), "return")?;

    let current = tokens.raw_offset;
    let expression = match parse_expression(tokens) {
        Ok(expression) => Some(expression),
        Err(..) => {
            tokens.raw_offset = current;
            None
        }
    };

    Ok(Node::ReturnExpression(expression.map(Box::new)))
}

fn parse_break_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    assert_keyword(tokens.next(), "break")?;

    Ok(Node::BreakExpression)
}

fn parse_function_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    assert_keyword(tokens.next(), "fn")?;

    assert_punctuation!(tokens.next(), LeftParen)?;
    let mut parameters = vec![];
    while assert_punctuation!(tokens.peek(), RightParen).is_err() {
        let name = assert_non_reserved_word(tokens.next())?;
        assert_punctuation!(tokens.next(), Colon)?;
        let type_ = parse_type_expression(tokens)?;
        parameters.push(FunctionParameterDeclaration { name, type_ });

        if assert_punctuation!(tokens.peek(), Comma).is_ok() {
            tokens.next();
        } else {
            break;
        }
    }
    assert_punctuation!(tokens.next(), RightParen)?;

    assert_punctuation!(tokens.next(), Colon)?;
    let return_type = parse_type_expression(tokens)?;

    let body = parse_block_expression(tokens)?;

    Ok(Node::FunctionExpression(FunctionNode {
        interface: FunctionInterfaceNode {
            name: "(anonymous)".to_string(),
            parameters,
            return_type: Box::new(return_type),
        },
        body: Box::new(body),
    }))
}

fn parse_call_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    let callee = parse_member_expression(tokens)?;

    if assert_punctuation!(tokens.peek(), LeftParen).is_ok() {
        tokens.next();
    } else {
        return Ok(callee);
    }
    let mut parameters = vec![];
    while assert_punctuation!(tokens.peek(), RightParen).is_err() {
        parameters.push(parse_parameter(tokens)?);

        if assert_punctuation!(tokens.peek(), Comma).is_ok() {
            tokens.next();
        } else {
            break;
        }
    }
    assert_punctuation!(tokens.next(), RightParen)?;

    Ok(Node::CallExpression(Box::new(callee), parameters))
}

fn parse_parameter(tokens: &mut TokenIterator) -> Result<Parameter, Error> {
    let current = tokens.raw_offset;
    match parse_named_parameter(tokens) {
        Ok(parameter) => return Ok(parameter),
        Err(..) => tokens.raw_offset = current,
    }

    let value = parse_expression(tokens)?;
    Ok(Parameter::new(None, value))
}

fn parse_named_parameter(tokens: &mut TokenIterator) -> Result<Parameter, Error> {
    let name = assert_non_reserved_word(tokens.next())?;
    assert_punctuation!(tokens.next(), Equal)?;
    let value = parse_expression(tokens)?;

    Ok(Parameter::new(Some(name), value))
}

fn parse_left_hand_side_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    parse_member_expression(tokens)
}

fn parse_member_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    let mut object = parse_primary_expression(tokens)?;

    while assert_punctuation!(tokens.peek(), Dot).is_ok() {
        tokens.next();

        let name = assert_non_reserved_word(tokens.next())?;

        object = Node::MemberExpression(Box::new(object), name);
    }

    Ok(object)
}

fn parse_primary_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    match tokens.next() {
        Ok(Token { kind: TokenKind::Number(value), .. }) => Ok(Node::Number(*value)),
        Ok(Token { kind: TokenKind::Bool(value), .. }) => Ok(Node::Bool(*value)),
        Ok(Token { kind: TokenKind::String(value), .. }) => Ok(Node::String(value.clone())),
        Ok(Token { position, kind: TokenKind::Identifier(name), .. }) => {
            if is_reserved(name) {
                return Err(Error::reserved_word(position.clone(), name.clone()));
            }

            Ok(Node::Identifier(name.clone()))
        }
        Ok(Token { kind: TokenKind::Punctuation(PunctuationKind::LeftParen), .. }) => {
            let expression = parse_expression(tokens)?;
            assert_punctuation!(tokens.next(), RightParen)?;
            Ok(expression)
        }
        Ok(other) => Err(Error::unexpected_token(other.position.clone(), "expression")),
        Err(err) => Err(err.clone()),
    }
}

// Type

fn parse_type_expression(tokens: &mut TokenIterator) -> Result<TypeExpression, Error> {
    parse_union_type(tokens)
}

fn parse_union_type(tokens: &mut TokenIterator) -> Result<TypeExpression, Error> {
    let mut types = vec![parse_optional_type(tokens)?];

    while assert_punctuation!(tokens.peek(), VerticalLine).is_ok() {
        tokens.next();
        types.push(parse_optional_type(tokens)?);
    }

    if types.len() == 1 {
        Ok(types.pop().unwrap())
    } else {
        Ok(TypeExpression::Union(types))
    }
}

fn parse_optional_type(tokens: &mut TokenIterator) -> Result<TypeExpression, Error> {
    let type_ = parse_primary_type(tokens)?;

    if assert_punctuation!(tokens.peek(), Question).is_ok() {
        tokens.next();
        Ok(TypeExpression::Optional(Box::new(type_)))
    } else {
        Ok(type_)
    }
}

fn parse_primary_type(tokens: &mut TokenIterator) -> Result<TypeExpression, Error> {
    match tokens.peek() {
        Ok(Token { kind: TokenKind::Punctuation(PunctuationKind::LeftParen), .. }) => {
            tokens.next();
            let type_ = parse_type_expression(tokens)?;
            assert_punctuation!(tokens.next(), RightParen)?;
            Ok(type_)
        }
        Ok(Token { position, kind: TokenKind::Identifier(name), .. }) => {
            match name.as_str() {
                "number" | "string" | "bool" | "null" => {
                    let name = name.clone();
                    tokens.next();
                    Ok(TypeExpression::Identifier(name))
                }
                _ => {
                    if is_reserved(name) {
                        Err(Error::reserved_word(position.clone(), name.clone()))
                    } else {
                        let name = name.clone();
                        tokens.next();
                        Ok(TypeExpression::Identifier(name))
                    }
                }
            }
        }
        Ok(Token { position, .. }) => Err(Error::unexpected_token(position.clone(), "type")),
        Err(err) => Err(err.clone()),
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::parse;

    mod if_statement {
        use crate::node::Node;
        use crate::parser::parse;

        #[test]
        fn if_statement() {
            assert_eq!(
                parse("if (1) 2; else 3").node,
                Node::Program(vec![
                    Node::IfStatement(
                        Box::new(Node::Number(1f64)),
                        Box::new(Node::Number(2f64)),
                        Some(Box::new(Node::Number(3f64))),
                    ),
                ])
            );
        }

        #[test]
        fn if_statement_without_false_branch() {
            assert_eq!(
                parse("if (1) 2").node,
                Node::Program(vec![
                    Node::IfStatement(
                        Box::new(Node::Number(1f64)),
                        Box::new(Node::Number(2f64)),
                        None,
                    ),
                ])
            );
        }

        #[test]
        fn chained_if() {
            assert_eq!(
                parse("if (1) 2; else if (3) 4; else 5").node,
                Node::Program(vec![
                    Node::IfStatement(
                        Box::new(Node::Number(1f64)),
                        Box::new(Node::Number(2f64)),
                        Some(Box::new(Node::IfStatement(
                            Box::new(Node::Number(3f64)),
                            Box::new(Node::Number(4f64)),
                            Some(Box::new(Node::Number(5f64))),
                        ))),
                    ),
                ])
            );
        }
    }

    mod block_statement {
        use crate::node::Node;
        use crate::parser::parse;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn block() {
            assert_eq!(
                parse("{1; 2}").node,
                Node::Program(vec![
                    Node::BlockExpression(vec![
                        Node::Number(1f64),
                        Node::Number(2f64),
                    ]),
                ])
            );
        }

        #[test]
        fn nested_block() {
            assert_eq!(
                parse("{{1+2}}").node,
                Node::Program(vec![
                    Node::BlockExpression(vec![
                        Node::BlockExpression(vec![
                            Node::BinaryExpression(
                                Box::new(Node::Number(1f64)),
                                PunctuationKind::Plus,
                                Box::new(Node::Number(2f64)),
                            ),
                        ]),
                    ]),
                ])
            );
        }
    }

    mod variable_declaration {
        use crate::node::{Node, TypeExpression};
        use crate::parser::parse;

        #[test]
        fn variable_declaration() {
            assert_eq!(
                parse("let x").node,
                Node::Program(vec![
                    Node::VariableDeclaration("x".to_string(), None, None),
                ])
            );
        }

        #[test]
        fn with_initial_value() {
            assert_eq!(
                parse("let x = 1").node,
                Node::Program(vec![
                    Node::VariableDeclaration("x".to_string(), None, Some(Box::new(Node::Number(1.0f64)))),
                ])
            );
        }

        #[test]
        fn with_type_annotation() {
            assert_eq!(
                parse("let x: number = 1").node,
                Node::Program(vec![
                    Node::VariableDeclaration(
                        "x".to_string(),
                        Some(TypeExpression::Identifier("number".to_string())),
                        Some(Box::new(Node::Number(1.0f64)))
                    ),
                ])
            );
        }
    }

    mod for_statement {
        use crate::error::Error;
        use crate::node::Node;
        use crate::parser::parse;
        use crate::position::Position;

        #[test]
        fn for_statement() {
            assert_eq!(
                parse("for (i in range) 1").node,
                Node::Program(vec![
                    Node::ForStatement(
                        "i".to_string(),
                        Box::new(Node::Identifier("range".to_string())),
                        Box::new(Node::Number(1f64)),
                    ),
                ])
            );
        }

        #[test]
        fn no_left_paren() {
            assert_eq!(
                parse("for i in range) 1").errors,
                vec![
                    Error::unexpected_token(Position::new(0, 4), "(")
                ]
            );
        }

        #[test]
        fn no_in() {
            assert_eq!(
                parse("for (i range) 1").errors,
                vec![
                    Error::unexpected_token(Position::new(0, 7), "in")
                ]
            );
        }

        #[test]
        fn no_iterator() {
            assert_eq!(
                parse("for (i in) 1").errors,
                vec![
                    Error::unexpected_token(Position::new(0, 9), "expression")
                ]
            );
        }

        #[test]
        fn no_right_paren() {
            assert_eq!(
                parse("for (i in range 1").errors,
                vec![
                    Error::unexpected_token(Position::new(0, 16), ")")
                ]
            );
        }

        #[test]
        fn no_body() {
            assert_eq!(
                parse("for (i in range)").errors,
                vec![
                    Error::unexpected_token(Position::new(0, 16), "statement")
                ]
            );
        }

        #[test]
        fn with_block() {
            assert_eq!(
                parse("for (i in range) { 1 }").node,
                Node::Program(vec![
                    Node::ForStatement(
                        "i".to_string(),
                        Box::new(Node::Identifier("range".to_string())),
                        Box::new(Node::BlockExpression(vec![
                            Node::Number(1f64),
                        ])),
                    ),
                ])
            );
        }
    }

    mod struct_declaration {
        use crate::error::Error;
        use crate::node::{Node, StructDeclarationNode, StructPropertyDeclaration, TypeExpression};
        use crate::parser::parse;
        use crate::position::Position;

        #[test]
        fn struct_declaration() {
            assert_eq!(
                parse("struct User(name: string, id: number)").node,
                Node::Program(vec![
                    Node::StructDeclaration(StructDeclarationNode::new(
                        "User".to_string(),
                        vec![
                            StructPropertyDeclaration {
                                name: "name".to_string(),
                                type_: TypeExpression::Identifier("string".to_string()),
                            },
                            StructPropertyDeclaration {
                                name: "id".to_string(),
                                type_: TypeExpression::Identifier("number".to_string()),
                            },
                        ],
                        vec![],
                        vec![],
                    )),
                ])
            );
        }

        #[test]
        fn struct_declaration_without_properties() {
            assert_eq!(
                parse("struct User()").node,
                Node::Program(vec![
                    Node::StructDeclaration(StructDeclarationNode::new(
                        "User".to_string(),
                        vec![],
                        vec![],
                        vec![],
                    )),
                ])
            );
        }

        #[test]
        fn struct_declaration_without_name() {
            assert_eq!(
                parse("struct {}").errors,
                vec![
                    Error::unexpected_token(Position::new(0, 7), "identifier")
                ]
            );
        }
    }

    mod interface_declaration {
        use crate::node::{FunctionInterfaceNode, FunctionParameterDeclaration, InterfaceDeclarationNode, Node, TypeExpression};
        use crate::parser::parse;

        #[test]
        fn interface_declaration() {
            assert_eq!(
                parse("interface ToString { fn toString(self): string }").node,
                Node::Program(vec![
                    Node::InterfaceDeclaration(InterfaceDeclarationNode {
                        name: "ToString".to_string(),
                        instance_methods: vec![
                            FunctionInterfaceNode {
                                name: "toString".to_string(),
                                parameters: vec![
                                    FunctionParameterDeclaration {
                                        name: "self".to_string(),
                                        type_: TypeExpression::Identifier("self".to_string()),
                                    },
                                ],
                                return_type: Box::new(TypeExpression::Identifier("string".to_string())),
                            }
                        ]
                    }),
                ])
            );
        }
    }

    mod impl_statement {
        use crate::node::{FunctionInterfaceNode, FunctionNode, FunctionParameterDeclaration, ImplStatementNode, Node, TypeExpression};
        use crate::parser::parse;

        #[test]
        fn impl_statement() {
            assert_eq!(
                parse("
                impl ToString for User {
                    fn toString(self): string {
                        return user.name
                    }
                }").node,
                Node::Program(vec![
                    Node::ImplStatement(ImplStatementNode {
                        interface_name: "ToString".to_string(),
                        struct_name: "User".to_string(),
                        instance_methods: vec![
                            FunctionNode {
                                interface: FunctionInterfaceNode {
                                    name: "toString".to_string(),
                                    parameters: vec![
                                        FunctionParameterDeclaration {
                                            name: "self".to_string(),
                                            type_: TypeExpression::Identifier("self".to_string()),
                                        },
                                    ],
                                    return_type: Box::new(TypeExpression::Identifier("string".to_string())),
                                },
                                body: Box::new(Node::BlockExpression(vec![
                                    Node::ReturnExpression(
                                        Some(Box::new(Node::MemberExpression(
                                            Box::new(Node::Identifier("user".to_string())),
                                            "name".to_string(),
                                        )))
                                    ),
                                ])),
                            }
                        ]
                    }),
                ])
            );
        }
    }

    mod if_expression {
        use crate::error::Error;
        use crate::node::Node;
        use crate::parser::parse;
        use crate::position::Position;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn if_expression() {
            assert_eq!(
                parse("1 + if (1) 2 else 3").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::Number(1f64)),
                        PunctuationKind::Plus,
                        Box::new(Node::IfExpression(
                            Box::new(Node::Number(1f64)),
                            Box::new(Node::Number(2f64)),
                            Box::new(Node::Number(3f64)),
                        )),
                    )
                ])
            );
        }

        #[test]
        fn if_expression_requires_false_branch() {
            assert_eq!(
                parse("1 + if (1) 2").errors,
                vec![
                    Error::unexpected_token(Position::new(0, 12), "else")
                ]
            );
        }

        #[test]
        fn chained_if() {
            assert_eq!(
                parse("1 + if (2) 3 else if (4) 5 else 6").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::Number(1f64)),
                        PunctuationKind::Plus,
                        Box::new(Node::IfExpression(
                            Box::new(Node::Number(2f64)),
                            Box::new(Node::Number(3f64)),
                            Box::new(Node::IfExpression(
                                Box::new(Node::Number(4f64)),
                                Box::new(Node::Number(5f64)),
                                Box::new(Node::Number(6f64)),
                            )),
                        )),
                    ),
                ])
            );
        }
    }

    mod function_expression {
        use crate::error::Error;
        use crate::node::{FunctionInterfaceNode, FunctionNode, FunctionParameterDeclaration, Node, TypeExpression};
        use crate::parser::parse;
        use crate::position::Position;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn function_expression() {
            assert_eq!(
                parse("let x = fn(y: number): number { y * 2 }").node,
                Node::Program(vec![
                    Node::VariableDeclaration(
                        "x".to_string(),
                        None,
                        Some(Box::new(Node::FunctionExpression(FunctionNode {
                            interface: FunctionInterfaceNode {
                                name: "(anonymous)".to_string(),
                                parameters: vec![
                                    FunctionParameterDeclaration {
                                        name: "y".to_string(),
                                        type_: TypeExpression::Identifier("number".to_string()),
                                    },
                                ],
                                return_type: Box::new(TypeExpression::Identifier("number".to_string())),
                            },
                            body: Box::new(Node::BlockExpression(vec![
                                Node::BinaryExpression(
                                    Box::new(Node::Identifier("y".to_string())),
                                    PunctuationKind::Asterisk,
                                    Box::new(Node::Number(2f64)),
                                ),
                            ])),
                        }))),
                    )
                ])
            );
        }

        #[test]
        fn function_expression_with_name() {
            assert_eq!(
                parse("let x = fn test(y: number): number { y * 2 }").errors,
                vec![
                    Error::unexpected_token(Position { line: 0, column: 11 }, "(")
                ]
            );
        }
    }

    mod assignment_expression {
        use crate::node::Node;
        use crate::parser::parse;

        #[test]
        fn assign() {
            assert_eq!(
                parse("x = 1").node,
                Node::Program(vec![
                    Node::AssignmentExpression(
                        Box::new(Node::Identifier("x".to_string())),
                        Box::new(Node::Number(1f64))
                    ),
                ])
            );
        }

        #[test]
        fn no_value() {
            assert!(!parse("x =").errors.is_empty());
        }
    }

    mod logical_or_expression {
        use crate::node::Node;
        use crate::parser::parse;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn logical_or() {
            assert_eq!(
                parse("true || false").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::Bool(true)),
                        PunctuationKind::VerticalLineVerticalLine,
                        Box::new(Node::Bool(false)),
                    )
                ])
            );
        }

        #[test]
        fn prioritize_logical_and_over_logical_or() {
            assert_eq!(
                parse("true || false && true").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::Bool(true)),
                        PunctuationKind::VerticalLineVerticalLine,
                        Box::new(Node::BinaryExpression(
                            Box::new(Node::Bool(false)),
                            PunctuationKind::AndAnd,
                            Box::new(Node::Bool(true)),
                        )),
                    )
                ])
            );
        }
    }

    mod logical_and_expression {
        use crate::node::Node;
        use crate::parser::parse;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn logical_and() {
            assert_eq!(
                parse("true && false").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::Bool(true)),
                        PunctuationKind::AndAnd,
                        Box::new(Node::Bool(false)),
                    )
                ])
            );
        }

        #[test]
        fn prioritize_additive_over_logical_and() {
            assert_eq!(
                parse("true && false + 1").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::Bool(true)),
                        PunctuationKind::AndAnd,
                        Box::new(Node::BinaryExpression(
                            Box::new(Node::Bool(false)),
                            PunctuationKind::Plus,
                            Box::new(Node::Number(1f64)),
                        )),
                    )
                ])
            );
        }
    }

    mod equality_expression {
        use crate::node::Node;
        use crate::parser::parse;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn equal() {
            assert_eq!(
                parse("1==2").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::Number(1f64)),
                        PunctuationKind::EqualEqual,
                        Box::new(Node::Number(2f64)),
                    )
                ])
            );
        }

        #[test]
        fn not_equal() {
            assert_eq!(
                parse("1!=2").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::Number(1f64)),
                        PunctuationKind::ExclamationEqual,
                        Box::new(Node::Number(2f64)),
                    )
                ])
            );
        }

        #[test]
        fn not_equal_and_equal() {
            assert_eq!(
                parse("1!=2==3").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::BinaryExpression(
                            Box::new(Node::Number(1f64)),
                            PunctuationKind::ExclamationEqual,
                            Box::new(Node::Number(2f64)),
                        )),
                        PunctuationKind::EqualEqual,
                        Box::new(Node::Number(3f64)),
                    )
                ])
            );
        }

        #[test]
        fn equality_and_relational_expressions() {
            assert_eq!(
                parse("1>2==3>4").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::BinaryExpression(
                            Box::new(Node::Number(1f64)),
                            PunctuationKind::RightChevron,
                            Box::new(Node::Number(2f64)),
                        )),
                        PunctuationKind::EqualEqual,
                        Box::new(Node::BinaryExpression(
                            Box::new(Node::Number(3f64)),
                            PunctuationKind::RightChevron,
                            Box::new(Node::Number(4f64)),
                        )),
                    )
                ])
            );
        }
    }

    mod relational_expression {
        use crate::node::Node;
        use crate::parser::parse;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn greater_than() {
            assert_eq!(
                parse("1>2").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::Number(1f64)),
                        PunctuationKind::RightChevron,
                        Box::new(Node::Number(2f64)),
                    )
                ])
            );
        }

        #[test]
        fn greater_than_or_equal() {
            assert_eq!(
                parse("1>=2").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::Number(1f64)),
                        PunctuationKind::RightChevronEqual,
                        Box::new(Node::Number(2f64)),
                    )
                ])
            );
        }

        #[test]
        fn less_than() {
            assert_eq!(
                parse("1<2").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::Number(1f64)),
                        PunctuationKind::LeftChevron,
                        Box::new(Node::Number(2f64)),
                    )
                ])
            );
        }

        #[test]
        fn less_than_or_equal() {
            assert_eq!(
                parse("1<=2").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::Number(1f64)),
                        PunctuationKind::LeftChevronEqual,
                        Box::new(Node::Number(2f64)),
                    )
                ])
            );
        }

        #[test]
        fn greater_than_expressions() {
            assert_eq!(
                parse("1>2>3").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::BinaryExpression(
                            Box::new(Node::Number(1f64)),
                            PunctuationKind::RightChevron,
                            Box::new(Node::Number(2f64)),
                        )),
                        PunctuationKind::RightChevron,
                        Box::new(Node::Number(3f64)),
                    )
                ])
            );
        }

        #[test]
        fn greater_than_and_additive_expression() {
            assert_eq!(
                parse("1+2>3+4").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::BinaryExpression(
                            Box::new(Node::Number(1f64)),
                            PunctuationKind::Plus,
                            Box::new(Node::Number(2f64)),
                        )),
                        PunctuationKind::RightChevron,
                        Box::new(Node::BinaryExpression(
                            Box::new(Node::Number(3f64)),
                            PunctuationKind::Plus,
                            Box::new(Node::Number(4f64)),
                        )),
                    )
                ])
            );
        }
    }

    mod additive_expression {
        use crate::node::Node;
        use crate::parser::parse;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn add() {
            assert_eq!(
                parse("1+2").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::Number(1f64)),
                        PunctuationKind::Plus,
                        Box::new(Node::Number(2f64)),
                    )
                ])
            );
        }

        #[test]
        fn subtract() {
            assert_eq!(
                parse("1-2").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::Number(1f64)),
                        PunctuationKind::Minus,
                        Box::new(Node::Number(2f64)),
                    )
                ])
            );
        }

        #[test]
        fn add_and_subtract() {
            assert_eq!(
                parse("1+2-3").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::BinaryExpression(
                            Box::new(Node::Number(1f64)),
                            PunctuationKind::Plus,
                            Box::new(Node::Number(2f64)),
                        )),
                        PunctuationKind::Minus,
                        Box::new(Node::Number(3f64)),
                    )
                ])
            );
        }

        #[test]
        fn newline_at_middle() {
            assert_eq!(
                parse("1\n+2").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::Number(1f64)),
                        PunctuationKind::Plus,
                        Box::new(Node::Number(2f64)),
                    )
                ])
            );
        }
    }

    mod multiplicative_expression {
        use crate::node::Node;
        use crate::parser::parse;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn multiply() {
            assert_eq!(
                parse("1*2").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::Number(1f64)),
                        PunctuationKind::Asterisk,
                        Box::new(Node::Number(2f64)),
                    )
                ])
            );
        }

        #[test]
        fn divide() {
            assert_eq!(
                parse("1/2").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::Number(1f64)),
                        PunctuationKind::Slash,
                        Box::new(Node::Number(2f64)),
                    )
                ])
            );
        }

        #[test]
        fn multiply_and_divide() {
            assert_eq!(
                parse("1*2/3").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::BinaryExpression(
                            Box::new(Node::Number(1f64)),
                            PunctuationKind::Asterisk,
                            Box::new(Node::Number(2f64)),
                        )),
                        PunctuationKind::Slash,
                        Box::new(Node::Number(3f64)),
                    )
                ])
            );
        }

        #[test]
        fn multiply_after_add() {
            assert_eq!(
                parse("1+2*3").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::Number(1f64)),
                        PunctuationKind::Plus,
                        Box::new(Node::BinaryExpression(
                            Box::new(Node::Number(2f64)),
                            PunctuationKind::Asterisk,
                            Box::new(Node::Number(3f64)),
                        )),
                    )
                ])
            );
        }

        #[test]
        fn add_after_multiply() {
            assert_eq!(
                parse("1*2+3").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::BinaryExpression(
                            Box::new(Node::Number(1f64)),
                            PunctuationKind::Asterisk,
                            Box::new(Node::Number(2f64)),
                        )),
                        PunctuationKind::Plus,
                        Box::new(Node::Number(3f64)),
                    )
                ])
            );
        }

        #[test]
        fn add_multiplicative_expressions() {
            assert_eq!(
                parse("1*2+3/4").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::BinaryExpression(
                            Box::new(Node::Number(1f64)),
                            PunctuationKind::Asterisk,
                            Box::new(Node::Number(2f64)),
                        )),
                        PunctuationKind::Plus,
                        Box::new(Node::BinaryExpression(
                            Box::new(Node::Number(3f64)),
                            PunctuationKind::Slash,
                            Box::new(Node::Number(4f64)),
                        )),
                    )
                ])
            );
        }
    }

    mod unary_expression {
        use crate::error::Error;
        use crate::node::Node;
        use crate::parser::parse;
        use crate::position::Position;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn plus() {
            assert_eq!(
                parse("+1").node,
                Node::Program(vec![
                    Node::UnaryExpression(
                        PunctuationKind::Plus,
                        Box::new(Node::Number(1f64)),
                    )
                ])
            );
        }

        #[test]
        fn minus() {
            assert_eq!(
                parse("-1").node,
                Node::Program(vec![
                    Node::UnaryExpression(
                        PunctuationKind::Minus,
                        Box::new(Node::Number(1f64)),
                    )
                ])
            );
        }

        #[test]
        fn disallow_multiple_unary_operator() {
            assert_eq!(
                parse("--1").errors,
                vec![
                    Error::unexpected_token(Position::new(0, 1), "expression")
                ]
            );
        }

        #[test]
        fn unary_operator_in_additive_expression() {
            assert_eq!(
                parse("1++1").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::Number(1f64)),
                        PunctuationKind::Plus,
                        Box::new(Node::UnaryExpression(
                            PunctuationKind::Plus,
                            Box::new(Node::Number(1f64)),
                        )),
                    ),
                ])
            );
        }
    }

    mod call_expression {
        use crate::node::{Node, Parameter};
        use crate::parser::parse;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn without_parameters() {
            assert_eq!(
                parse("func()").node,
                Node::Program(vec![
                    Node::CallExpression(
                        Box::new(Node::Identifier("func".to_string())),
                        vec![],
                    )
                ])
            );
        }

        #[test]
        fn with_parameters() {
            assert_eq!(
                parse("func(1, 2, 3)").node,
                Node::Program(vec![
                    Node::CallExpression(
                        Box::new(Node::Identifier("func".to_string())),
                        vec![
                            Parameter::new(None, Node::Number(1f64)),
                            Parameter::new(None, Node::Number(2f64)),
                            Parameter::new(None, Node::Number(3f64)),
                        ],
                    )
                ])
            );
        }

        #[test]
        fn complex_expression_in_parameter() {
            assert_eq!(
                parse("func(1, 2*(3+4))").node,
                Node::Program(vec![
                    Node::CallExpression(
                        Box::new(Node::Identifier("func".to_string())),
                        vec![
                            Parameter::new(None, Node::Number(1f64)),
                            Parameter::new(None, Node::BinaryExpression(
                                Box::new(Node::Number(2f64)),
                                PunctuationKind::Asterisk,
                                Box::new(Node::BinaryExpression(
                                    Box::new(Node::Number(3f64)),
                                    PunctuationKind::Plus,
                                    Box::new(Node::Number(4f64)),
                                )),
                            )),
                        ],
                    )
                ])
            );
        }

        #[test]
        fn function_call_in_parameter() {
            assert_eq!(
                parse("f(g(1), h(2))").node,
                Node::Program(vec![
                    Node::CallExpression(
                        Box::new(Node::Identifier("f".to_string())),
                        vec![
                            Parameter::new(None, Node::CallExpression(
                                Box::new(Node::Identifier("g".to_string())),
                                vec![
                                    Parameter::new(None, Node::Number(1f64)),
                                ],
                            )),
                            Parameter::new(None, Node::CallExpression(
                                Box::new(Node::Identifier("h".to_string())),
                                vec![
                                    Parameter::new(None, Node::Number(2f64)),
                                ],
                            )),
                        ],
                    )
                ])
            );
        }
    }

    mod member_expression {
        use crate::node::Node;
        use crate::parser::parse;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn member_expression() {
            assert_eq!(
                parse("x.y.z").node,
                Node::Program(vec![
                    Node::MemberExpression(
                        Box::new(Node::MemberExpression(
                            Box::new(Node::Identifier("x".to_string())),
                            "y".to_string(),
                        )),
                        "z".to_string(),
                    ),
                ])
            );
        }

        #[test]
        fn member_expression_with_call_expression() {
            assert_eq!(
                parse("x.y()").node,
                Node::Program(vec![
                    Node::CallExpression(
                        Box::new(Node::MemberExpression(
                            Box::new(Node::Identifier("x".to_string())),
                            "y".to_string(),
                        )),
                        vec![],
                    ),
                ])
            );
        }

        #[test]
        fn member_expression_with_unary_expression() {
            assert_eq!(
                parse("-x.y").node,
                Node::Program(vec![
                    Node::UnaryExpression(
                        PunctuationKind::Minus,
                        Box::new(Node::MemberExpression(
                            Box::new(Node::Identifier("x".to_string())),
                            "y".to_string(),
                        )),
                    ),
                ])
            );
        }
    }

    mod primary_expression {
        use crate::error::Error;
        use crate::node::{Node, Parameter};
        use crate::parser::parse;
        use crate::position::Position;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn number() {
            assert_eq!(
                parse("1").node,
                Node::Program(vec![
                    Node::Number(1f64),
                ])
            );
        }

        #[test]
        fn bool_true() {
            assert_eq!(
                parse("true").node,
                Node::Program(vec![
                    Node::Bool(true),
                ])
            );
        }

        #[test]
        fn bool_false() {
            assert_eq!(
                parse("false").node,
                Node::Program(vec![
                    Node::Bool(false),
                ])
            );
        }

        #[test]
        fn bool_in_expression() {
            assert_eq!(
                parse("-true+false").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::UnaryExpression(
                            PunctuationKind::Minus,
                            Box::new(Node::Bool(true)),
                        )),
                        PunctuationKind::Plus,
                        Box::new(Node::Bool(false)),
                    ),
                ])
            );
        }

        #[test]
        fn string() {
            assert_eq!(
                parse("print(\"hello\")").node,
                Node::Program(vec![
                    Node::CallExpression(
                        Box::new(Node::Identifier("print".to_string())),
                        vec![
                            Parameter::new(None, Node::String("hello".to_string())),
                        ],
                    )
                ])
            );
        }
        #[test]
        fn identifier() {
            assert_eq!(
                parse("x").node,
                Node::Program(vec![
                    Node::Identifier("x".to_string()),
                ])
            );
        }

        #[test]
        fn number_with_paren() {
            assert_eq!(
                parse("(1)").node,
                Node::Program(vec![
                    Node::Number(1f64),
                ])
            );
        }

        #[test]
        fn multiply_additive_expressions() {
            assert_eq!(
                parse("(1+2)*(3+4)").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::BinaryExpression(
                            Box::new(Node::Number(1f64)),
                            PunctuationKind::Plus,
                            Box::new(Node::Number(2f64)),
                        )),
                        PunctuationKind::Asterisk,
                        Box::new(Node::BinaryExpression(
                            Box::new(Node::Number(3f64)),
                            PunctuationKind::Plus,
                            Box::new(Node::Number(4f64)),
                        )),
                    )
                ])
            );
        }

        #[test]
        fn nested_parens() {
            assert_eq!(
                parse("((1+((2))))").node,
                Node::Program(vec![
                    Node::BinaryExpression(
                        Box::new(Node::Number(1f64)),
                        PunctuationKind::Plus,
                        Box::new(Node::Number(2f64)),
                    ),
                ])
            );
        }

        #[test]
        fn unpaired_paren() {
            assert_eq!(
                parse("(1+2").errors,
                vec![
                    Error::unexpected_token(Position::new(0, 4), ")")
                ]
            );
        }
    }

    mod range_iterator {
        use crate::node::Node;
        use crate::parser::parse;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn range_iterator() {
            assert_eq!(
                parse("for(x in 0 to 10) 1").node,
                Node::Program(vec![
                    Node::ForStatement(
                        "x".to_string(),
                        Box::new(Node::RangeIterator(
                            Box::new(Node::Number(0f64)),
                            Box::new(Node::Number(10f64)),
                        )),
                        Box::new(Node::Number(1f64)),
                    ),
                ])
            );
        }

        #[test]
        fn complex_expression() {
            assert_eq!(
                parse("for(x in y to 1+2) 1").node,
                Node::Program(vec![
                    Node::ForStatement(
                        "x".to_string(),
                        Box::new(Node::RangeIterator(
                            Box::new(Node::Identifier("y".to_string())),
                            Box::new(Node::BinaryExpression(
                                Box::new(Node::Number(1f64)),
                                PunctuationKind::Plus,
                                Box::new(Node::Number(2f64)),
                            )),
                        )),
                        Box::new(Node::Number(1f64)),
                    ),
                ])
            );
        }
    }

    mod object_literal {
        use crate::node::{Node, Parameter, StructDeclarationNode, StructPropertyDeclaration, TypeExpression};
        use crate::parser::parse;

        #[test]
        fn empty_object() {
            assert_eq!(
                parse("struct Obj(); Obj()").node,
                Node::Program(vec![
                    Node::StructDeclaration(StructDeclarationNode::new("Obj".to_string(), vec![], vec![], vec![], )),
                    Node::CallExpression(Box::new(Node::Identifier("Obj".to_string())), vec![]),
                ])
            );
        }

        #[test]
        fn object_having_properties() {
            assert_eq!(
                parse("struct Obj(x: number, y: string); Obj(x=1, y=\"hello\")").node,
                Node::Program(vec![
                    Node::StructDeclaration(StructDeclarationNode::new(
                        "Obj".to_string(),
                        vec![
                            StructPropertyDeclaration {
                                name: "x".to_string(),
                                type_: TypeExpression::Identifier("number".to_string()),
                            },
                            StructPropertyDeclaration {
                                name: "y".to_string(),
                                type_: TypeExpression::Identifier("string".to_string()),
                            },
                        ],
                        vec![],
                        vec![],
                    )),
                    Node::CallExpression(
                        Box::new(Node::Identifier("Obj".to_string())),
                        vec![
                            Parameter::new(Some("x".to_string()), Node::Number(1f64)),
                            Parameter::new(Some("y".to_string()), Node::String("hello".to_string())),
                        ],
                    ),
                ])
            );
        }

        #[test]
        fn nested_object() {
            assert_eq!(
                parse("
                    struct Obj1(x: Obj2, z: string)
                    struct Obj2(y: number)
                    Obj1(x=Obj2(y=1), z=\"hello\")
                ").node,
                Node::Program(vec![
                    Node::StructDeclaration(StructDeclarationNode::new(
                        "Obj1".to_string(),
                        vec![
                            StructPropertyDeclaration {
                                name: "x".to_string(),
                                type_: TypeExpression::Identifier("Obj2".to_string()),
                            },
                            StructPropertyDeclaration {
                                name: "z".to_string(),
                                type_: TypeExpression::Identifier("string".to_string()),
                            },
                        ],
                        vec![],
                        vec![],
                    )),
                    Node::StructDeclaration(StructDeclarationNode::new(
                        "Obj2".to_string(),
                        vec![
                            StructPropertyDeclaration {
                                name: "y".to_string(),
                                type_: TypeExpression::Identifier("number".to_string()),
                            },
                        ],
                        vec![],
                        vec![],
                    )),
                    Node::CallExpression(
                        Box::new(Node::Identifier("Obj1".to_string())),
                        vec![
                            Parameter::new(
                                Some("x".to_string()),
                                Node::CallExpression(
                                    Box::new(Node::Identifier("Obj2".to_string())),
                                    vec![
                                        Parameter::new(
                                            Some("y".to_string()),
                                            Node::Number(1f64)
                                        ),
                                    ],
                                ),
                            ),
                            Parameter::new(
                                Some("z".to_string()),
                                Node::String("hello".to_string()),
                            ),
                        ]
                    ),
                ])
            );
        }

        #[test]
        fn assign_object_into_variable() {
            assert_eq!(
                parse("struct Obj(y: number); x = Obj(y=1)").node,
                Node::Program(vec![
                    Node::StructDeclaration(StructDeclarationNode::new(
                        "Obj".to_string(),
                        vec![
                            StructPropertyDeclaration {
                                name: "y".to_string(),
                                type_: TypeExpression::Identifier("number".to_string()),
                            },
                        ],
                        vec![],
                        vec![],
                    )),
                    Node::AssignmentExpression(
                        Box::new(Node::Identifier("x".to_string())),
                        Box::new(Node::CallExpression(
                            Box::new(Node::Identifier("Obj".to_string())),
                            vec![
                                Parameter::new(
                                    Some("y".to_string()),
                                    Node::Number(1f64),
                                ),
                            ]
                        )),
                    ),
                ])
            );
        }
    }

    mod union_type {
        use crate::node::{Node, TypeExpression};
        use crate::parser::parse;

        #[test]
        fn union_type() {
            assert_eq!(
                parse("let x:T|U|V").node,
                Node::Program(vec![
                    Node::VariableDeclaration(
                        "x".to_string(),
                        Some(TypeExpression::Union(vec![
                            TypeExpression::Identifier("T".to_string()),
                            TypeExpression::Identifier("U".to_string()),
                            TypeExpression::Identifier("V".to_string()),
                        ])),
                        None,
                    ),
                ])
            );
        }

        #[test]
        fn wrapped_type() {
            assert_eq!(
                parse("let x:(T|U)|V").node,
                Node::Program(vec![
                    Node::VariableDeclaration(
                        "x".to_string(),
                        Some(TypeExpression::Union(vec![
                            TypeExpression::Union(vec![
                                TypeExpression::Identifier("T".to_string()),
                                TypeExpression::Identifier("U".to_string()),
                            ]),
                            TypeExpression::Identifier("V".to_string())
                        ])),
                        None,
                    ),
                ])
            );
        }

        #[test]
        fn with_optional() {
            assert_eq!(
                parse("let x:T|U?").node,
                Node::Program(vec![
                    Node::VariableDeclaration(
                        "x".to_string(),
                        Some(TypeExpression::Union(vec![
                            TypeExpression::Identifier("T".to_string()),
                            TypeExpression::Optional(
                                Box::new(TypeExpression::Identifier("U".to_string()))
                            ),
                        ])),
                        None,
                    ),
                ])
            );
        }
    }

    mod optional_type {
        use crate::node::{Node, TypeExpression};
        use crate::parser::parse;

        #[test]
        fn optional_type() {
            assert_eq!(
                parse("let x:T?").node,
                Node::Program(vec![
                    Node::VariableDeclaration(
                        "x".to_string(),
                        Some(TypeExpression::Optional(
                            Box::new(TypeExpression::Identifier("T".to_string()))
                        )),
                        None,
                    ),
                ])
            );
        }

        #[test]
        fn optional_of_wrapped_type() {
            assert_eq!(
                parse("let x:(T)?").node,
                Node::Program(vec![
                    Node::VariableDeclaration(
                        "x".to_string(),
                        Some(TypeExpression::Optional(
                            Box::new(TypeExpression::Identifier("T".to_string()))
                        )),
                        None,
                    ),
                ])
            );
        }
    }

    mod primary_type {
        use crate::node::{Node, TypeExpression};
        use crate::parser::parse;

        #[test]
        fn type_identifier() {
            assert_eq!(
                parse("let x:T").node,
                Node::Program(vec![
                    Node::VariableDeclaration(
                        "x".to_string(),
                        Some(TypeExpression::Identifier("T".to_string())),
                        None,
                    ),
                ])
            );
        }

        #[test]
        fn type_wrapped_by_paren() {
            assert_eq!(
                parse("let x:(T)").node,
                Node::Program(vec![
                    Node::VariableDeclaration(
                        "x".to_string(),
                        Some(TypeExpression::Identifier("T".to_string())),
                        None,
                    ),
                ])
            );
        }
    }

    mod semicolon {
        use crate::parser::parse;

        #[test]
        fn semicolon_at_end_of_input_is_optional() {
            assert!(parse("1;").errors.is_empty());
            assert!(parse("1").errors.is_empty());
        }

        #[test]
        fn semicolon_at_end_of_line_is_optional() {
            assert!(parse("1;
            ").errors.is_empty());
            assert!(parse("1
            ").errors.is_empty());
        }

        #[test]
        fn semicolon_at_end_of_block_is_optional() {
            assert!(parse("{1;}").errors.is_empty());
            assert!(parse("{1}").errors.is_empty());
        }

        #[test]
        fn semicolon_at_middle_of_block_is_required() {
            assert!(!parse("{1 2}").errors.is_empty());
            assert!(parse("{1;2}").errors.is_empty());
        }
    }
}