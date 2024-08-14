use crate::lexer::scan;
use crate::node::{FunctionParameterDefinition, Node};
use crate::punctuator_kind::PunctuatorKind;
use crate::type_::Type;
use crate::token::Token;
use crate::token_iter::TokenIter;

pub fn parse(input: &str) -> Result<Node, String> {
    parse_program(&mut TokenIter::new(&scan(input)?))
}

pub fn parse_program(tokens: &mut TokenIter) -> Result<Node, String> {
    let mut nodes = Vec::new();

    while !tokens.is_empty() {
        match parse_statement(tokens)? {
            Some(node) => nodes.push(node),
            None => return Err("Expected statement".to_string()),
        }
    }

    Ok(Node::Program(nodes))
}

// Err -> 回復不可能なパース失敗 = 現在パースしようとしている構文にマッチすべきであることが確定している
// Ok(None) -> 回復可能なパース失敗 = 現在パースしようとしている構文以外にマッチする可能性がある
// Ok(Some(node)) -> パース成功

// Statement

fn parse_statement(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    let current = tokens.current;

    if matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::SemiColon))) {
        tokens.next();
        return Ok(Some(Node::EmptyStatement));
    }

    if let Some(node) = parse_if_statement(tokens)? {
        return Ok(Some(node));
    }
    tokens.set_current(current);

    if let Some(node) = parse_block_statement(tokens)? {
        return Ok(Some(node));
    }
    tokens.set_current(current);

    if let Some(node) = parse_return_statement(tokens)? {
        return Ok(Some(node));
    }
    tokens.set_current(current);

    if let Some(node) = parse_break_statement(tokens)? {
        return Ok(Some(node));
    }
    tokens.set_current(current);

    if let Some(node) = parse_for_statement(tokens)? {
        return Ok(Some(node));
    }
    tokens.set_current(current);

    if let Some(node) = parse_variable_declaration(tokens)? {
        return Ok(Some(node));
    }
    tokens.set_current(current);

    if let Some(node) = parse_function_declaration(tokens)? {
        return Ok(Some(node));
    }
    tokens.set_current(current);

    if let Some(node) = parse_expression_statement(tokens)? {
        return Ok(Some(node));
    }
    tokens.set_current(current);

    Ok(None)
}

fn parse_if_statement(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    if !matches!(tokens.peek(), Some(Token::Identifier(ref name)) if name == "if") {
        return Ok(None);
    }
    tokens.next();

    if !matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::LeftParen))) {
        return Err("Expected '('".to_string());
    }
    tokens.next();

    let condition = match parse_expression(tokens)? {
        Some(node) => node,
        None => return Err("Expected expression".to_string()),
    };

    if !matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::RightParen))) {
        return Err("Expected ')'".to_string());
    }
    tokens.next();

    let true_branch = match parse_statement(tokens)? {
        Some(node) => node,
        None => return Err("Expected statement".to_string()),
    };

    let false_branch = if matches!(tokens.peek(), Some(Token::Identifier(ref name)) if name == "else") {
        tokens.next();

        match parse_statement(tokens)? {
            Some(node) => Some(node),
            None => return Err("Expected statement".to_string()),
        }
    } else {
        None
    };

    Ok(Some(Node::IfStatement(Box::new(condition), Box::new(true_branch), false_branch.map(Box::new))))
}

fn parse_block_statement(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    parse_block_expression(tokens)
}

fn parse_return_statement(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    parse_return_expression(tokens)
}

fn parse_break_statement(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    parse_break_expression(tokens)
}

fn parse_for_statement(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    if !matches!(tokens.peek(), Some(Token::Identifier(ref name)) if name == "for") {
        return Ok(None);
    }
    tokens.next();

    if !matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::LeftParen))) {
        return Err("Expected '('".to_string());
    }
    tokens.next();

    let identifier = match tokens.peek() {
        Some(Token::Identifier(identifier)) => identifier.clone(),
        _ => return Err("Expected identifier".to_string()),
    };
    tokens.next();

    if !matches!(tokens.peek(), Some(Token::Identifier(ref name)) if name == "in") {
        return Err("Expected 'in'".to_string());
    }
    tokens.next();

    let iterator = match parse_expression(tokens)? {
        Some(node) => node,
        None => return Err("Expected expression".to_string()),
    };

    if !matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::RightParen))) {
        return Err("Expected ')'".to_string());
    }
    tokens.next();

    let body = match parse_statement(tokens)? {
        Some(node) => node,
        None => return Err("Expected statement".to_string()),
    };

    Ok(Some(Node::ForStatement(identifier, Box::new(iterator), Box::new(body))))
}

fn parse_variable_declaration(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    if !matches!(tokens.peek(), Some(Token::Identifier(ref name)) if name == "let") {
        return Ok(None);
    }
    tokens.next();

    let identifier = match tokens.peek() {
        Some(Token::Identifier(identifier)) => {
            if is_reserved_words(identifier) {
                return Err(format!("SyntaxError: \"{}\" is a reserved word", identifier));
            }
            identifier.clone()
        }
        _ => return Err("Expected identifier".to_string()),
    };
    tokens.next();

    let typ = if matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::Colon))) {
        tokens.next();

        match parse_type_annotation(tokens)? {
            Some(node) => Some(node),
            None => return Err("Expected type annotation".to_string()),
        }
    } else {
        None
    };

    let value = if matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::Equal))) {
        tokens.next();

        match parse_expression(tokens)? {
            Some(node) => Some(node),
            None => return Err("Expected expression".to_string()),
        }
    } else {
        None
    };

    Ok(Some(Node::VariableDeclaration(identifier, typ, value.map(Box::new))))
}

fn parse_function_declaration(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    parse_function(tokens, true)
}

fn parse_expression_statement(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    parse_expression(tokens)
}

// Expression

fn parse_expression(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    let current = tokens.current;
    let mut best_current = current;
    let mut best_result = None;

    if let Some(node) = parse_assignment_expression(tokens)? {
        if tokens.current >= best_current {
            best_current = tokens.current;
            best_result = Some(node);
        }
    }
    tokens.set_current(current);

    if let Some(node) = parse_range_expression(tokens)? {
        if tokens.current >= best_current {
            best_current = tokens.current;
            best_result = Some(node);
        }
    }
    tokens.set_current(current);

    if let Some(node) = parse_logical_or_expression(tokens)? {
        if tokens.current >= best_current {
            best_current = tokens.current;
            best_result = Some(node);
        }
    }
    tokens.set_current(current);

    tokens.set_current(best_current);
    Ok(best_result)
}

fn parse_assignment_expression(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    let lhs = match parse_left_hand_side_expression(tokens)? {
        Some(node) => node,
        None => return Ok(None)
    };

    if !matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::Equal))) {
        return Ok(None)
    }
    tokens.next();

    let rhs = match parse_expression(tokens)? {
        Some(node) => node,
        None => return Err("Expected expression".to_string()),
    };

    Ok(Some(Node::AssignmentExpression(Box::new(lhs), Box::new(rhs))))
}

fn parse_range_expression(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    let start = match parse_logical_or_expression(tokens)? {
        Some(node) => node,
        None => return Ok(None),
    };

    if !matches!(tokens.peek(), Some(Token::Identifier(ref name)) if name == "to") {
        return Ok(None);
    }
    tokens.next();

    let end = match parse_logical_or_expression(tokens)? {
        Some(node) => node,
        None => return Err("Expected expression".to_string()),
    };

    Ok(Some(Node::RangeIterator(Box::new(start), Box::new(end))))
}

fn parse_logical_or_expression(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    let mut lhs = match parse_logical_and_expression(tokens)? {
        Some(node) => node,
        None => return Ok(None),
    };

    loop {
        if !matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::LogicalOr))) {
            break;
        }
        tokens.next();

        let rhs = match parse_logical_and_expression(tokens)? {
            Some(node) => node,
            None => return Err("Expected expression".to_string()),
        };

        lhs = Node::BinaryExpression(Box::new(lhs), PunctuatorKind::LogicalOr, Box::new(rhs));
    }

    Ok(Some(lhs))
}

fn parse_logical_and_expression(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    let mut lhs = match parse_additive_expression(tokens)? {
        Some(node) => node,
        None => return Ok(None),
    };

    loop {
        if !matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::LogicalAnd))) {
            break;
        }
        tokens.next();

        let rhs = match parse_additive_expression(tokens)? {
            Some(node) => node,
            None => return Err("Expected expression".to_string()),
        };

        lhs = Node::BinaryExpression(Box::new(lhs), PunctuatorKind::LogicalAnd, Box::new(rhs));
    }

    Ok(Some(lhs))
}

fn parse_additive_expression(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    let mut lhs = match parse_multiplicative_expression(tokens)? {
        Some(node) => node,
        None => return Ok(None),
    };

    loop {
        let operator = match tokens.peek() {
            Some(Token::Punctuator(PunctuatorKind::Plus)) => {
                tokens.next();
                PunctuatorKind::Plus
            }
            Some(Token::Punctuator(PunctuatorKind::Minus)) => {
                tokens.next();
                PunctuatorKind::Minus
            }
            _ => break
        };

        let rhs = match parse_multiplicative_expression(tokens)? {
            Some(node) => node,
            None => return Err("Expected expression".to_string()),
        };

        lhs = Node::BinaryExpression(Box::new(lhs), operator, Box::new(rhs));
    }

    Ok(Some(lhs))
}

fn parse_multiplicative_expression(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    let mut lhs = match parse_unary_expression(tokens)? {
        Some(node) => node,
        None => return Ok(None),
    };

    loop {
        let operator = match tokens.peek() {
            Some(Token::Punctuator(PunctuatorKind::Multiply)) => {
                tokens.next();
                PunctuatorKind::Multiply
            }
            Some(Token::Punctuator(PunctuatorKind::Divide)) => {
                tokens.next();
                PunctuatorKind::Divide
            }
            _ => break
        };

        let rhs = match parse_unary_expression(tokens)? {
            Some(node) => node,
            None => return Err("Expected expression".to_string()),
        };

        lhs = Node::BinaryExpression(Box::new(lhs), operator, Box::new(rhs));
    }

    Ok(Some(lhs))
}

fn parse_unary_expression(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    let operator = match tokens.peek() {
        Some(Token::Punctuator(PunctuatorKind::Plus)) => {
            tokens.next();
            PunctuatorKind::Plus
        }
        Some(Token::Punctuator(PunctuatorKind::Minus)) => {
            tokens.next();
            PunctuatorKind::Minus
        }
        Some(Token::Punctuator(PunctuatorKind::LogicalNot)) => {
            tokens.next();
            PunctuatorKind::LogicalNot
        }
        _ => return parse_statement_expression(tokens),
    };

    let operand = match parse_statement_expression(tokens)? {
        Some(node) => node,
        None => return Err("Expected expression".to_string()),
    };

    Ok(Some(Node::UnaryExpression(operator, Box::new(operand))))
}

fn parse_statement_expression(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    let current = tokens.current;

    if let Some(node) = parse_function_expression(tokens)? {
        return Ok(Some(node));
    }
    tokens.set_current(current);

    if let Some(node) = parse_if_expression(tokens)? {
        return Ok(Some(node));
    }
    tokens.set_current(current);

    if let Some(node) = parse_return_expression(tokens)? {
        return Ok(Some(node));
    }
    tokens.set_current(current);

    if let Some(node) = parse_break_expression(tokens)? {
        return Ok(Some(node));
    }
    tokens.set_current(current);

    if let Some(node) = parse_block_expression(tokens)? {
        return Ok(Some(node));
    }
    tokens.set_current(current);

    parse_call_expression(tokens)
}

fn parse_function_expression(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    parse_function(tokens, false)
}

fn parse_if_expression(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    if !matches!(tokens.peek(), Some(Token::Identifier(ref name)) if name == "if") {
        return Ok(None);
    }
    tokens.next();

    if !matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::LeftParen))) {
        return Err("Expected '('".to_string());
    }
    tokens.next();

    let condition = match parse_expression(tokens)? {
        Some(node) => node,
        None => return Err("Expected expression".to_string()),
    };

    if !matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::RightParen))) {
        return Err("Expected ')'".to_string());
    }
    tokens.next();

    let true_branch = match parse_expression(tokens)? {
        Some(node) => node,
        None => return Err("Expected expression".to_string()),
    };

    if !matches!(tokens.peek(), Some(Token::Identifier(ref name)) if name == "else") {
        return Err("Expected 'else'".to_string());
    }
    tokens.next();

    let false_branch = match parse_expression(tokens)? {
        Some(node) => node,
        None => return Err("Expected expression".to_string()),
    };

    Ok(Some(Node::IfExpression(Box::new(condition), Box::new(true_branch), Box::new(false_branch))))
}

fn parse_block_expression(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    if !matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::LeftBrace))) {
        return Ok(None);
    }
    tokens.next();

    let mut statements = Vec::new();

    while !tokens.is_empty() {
        match parse_statement(tokens)? {
            Some(statement) => statements.push(statement),
            None => break
        }
    }
    if statements.is_empty() {
        return Ok(None);
    }

    if !matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::RightBrace))) {
        return Ok(None);
    }
    tokens.next();

    Ok(Some(Node::BlockExpression(statements)))
}

fn parse_return_expression(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    if !matches!(tokens.peek(), Some(Token::Identifier(ref name)) if name == "return") {
        return Ok(None);
    }
    tokens.next();

    let expression = parse_expression(tokens)?;

    Ok(Some(Node::ReturnExpression(expression.map(Box::new))))
}

fn parse_break_expression(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    if !matches!(tokens.peek(), Some(Token::Identifier(ref name)) if name == "break") {
        return Ok(None);
    }
    tokens.next();

    Ok(Some(Node::BreakExpression))
}

fn parse_call_expression(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    let callee = match parse_member_expression(tokens)? {
        Some(node) => node,
        None => return Ok(None),
    };

    if !matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::LeftParen))) {
        return Ok(Some(callee));
    }
    tokens.next();

    let mut arguments = Vec::new();

    while !tokens.is_empty() {
        match parse_expression(tokens)? {
            Some(expression) => {
                arguments.push(expression);

                if !matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::Comma))) {
                    break;
                }
                tokens.next();
            },
            None => break
        }
    }

    if !matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::RightParen))) {
        return Err("Expected ')'".to_string());
    }
    tokens.next();

    Ok(Some(Node::CallExpression(Box::new(callee), arguments)))
}

fn parse_left_hand_side_expression(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    parse_member_expression(tokens)
}

fn parse_member_expression(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    let mut lhs = match parse_primary_expression(tokens)? {
        Some(node) => node,
        None => return Ok(None),
    };

    loop {
        if !matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::Dot))) {
            break;
        }
        tokens.next();

        let rhs = match parse_primary_expression(tokens)? {
            Some(node) => node,
            None => return Err("Expected primary expression".to_string()),
        };

        lhs = Node::MemberExpression(Box::new(lhs), Box::new(rhs));
    }

    Ok(Some(lhs))
}

fn parse_primary_expression(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    let current = tokens.current;

    if let Ok(Some(node)) = parse_object_literal(tokens) {
        return Ok(Some(node));
    }
    tokens.set_current(current);

    match tokens.peek() {
        Some(Token::Punctuator(PunctuatorKind::LeftParen)) => {
            tokens.next();

            let expression = parse_expression(tokens)?;

            if !matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::RightParen))) {
                return Err("Expected ')'".to_string());
            }
            tokens.next();

            Ok(expression)
        }
        Some(Token::Identifier(identifier)) => {
            let identifier = identifier.clone();

            tokens.next();
            Ok(Some(Node::Identifier(identifier)))
        }
        Some(Token::Number(value)) => {
            let value = *value;

            tokens.next();
            Ok(Some(Node::Number(value)))
        }
        Some(Token::Bool(value)) => {
            let value = *value;

            tokens.next();
            Ok(Some(Node::Bool(value)))
        }
        Some(Token::String(value)) => {
            let value = value.clone();
            tokens.next();
            Ok(Some(Node::String(value)))
        }
        _ => Ok(None),
    }
}

fn parse_object_literal(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    if !matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::LeftBrace))) {
        return Ok(None);
    }
    tokens.next();

    let mut definitions = Vec::new();

    while !tokens.is_empty() {
        match parse_object_property_definition(tokens)? {
            Some(definition) => definitions.push(definition),
            None => break
        }

        if !matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::Comma))) {
            break;
        }
        tokens.next();
    }

    if !matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::RightBrace))) {
        return Err("Expected '}'".to_string());
    }
    tokens.next();

    Ok(Some(Node::Object(definitions)))
}

fn parse_object_property_definition(tokens: &mut TokenIter) -> Result<Option<Node>, String> {
    let name = match parse_primary_expression(tokens)? {
        Some(node) => node,
        None => return Ok(None),
    };

    if !matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::Colon))) {
        return Ok(None);
    }
    tokens.next();

    let value = match parse_expression(tokens)? {
        Some(node) => node,
        None => return Err("Expected expression".to_string()),
    };

    Ok(Some(Node::ObjectPropertyDefinition(Box::new(name), Box::new(value))))
}

fn parse_function(tokens: &mut TokenIter, is_declaration: bool) -> Result<Option<Node>, String> {
    if !matches!(tokens.peek(), Some(Token::Identifier(ref name)) if name == "function") {
        return Ok(None);
    }
    tokens.next();

    let name = match tokens.peek() {
        Some(Token::Identifier(name)) => {
            if is_reserved_words(name) {
                return Err(format!("SyntaxError: \"{}\" is a reserved word", name));
            }

            let name = name.clone();
            tokens.next();
            Some(name)
        }
        _ => None,
    };

    if !matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::LeftParen))) {
        return Err("Expected '('".to_string());
    }
    tokens.next();

    let mut parameters = Vec::new();
    while let Some(Token::Identifier(identifier)) = tokens.peek() {
        if is_reserved_words(identifier) {
            return Err(format!("SyntaxError: \"{}\" is a reserved word", identifier));
        }

        let identifier = identifier.clone();
        tokens.next();

        if !matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::Colon))) {
            return Err("Expected ':'".to_string());
        }
        tokens.next();
        
        let type_ = match parse_type_annotation(tokens)? {
            Some(node) => node,
            None => return Err("Expected type annotation".to_string()),
        };
        
        parameters.push(FunctionParameterDefinition { 
            name: identifier, 
            type_
        });
        
        if !matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::Comma))) {
            break;
        }
        tokens.next();
    }

    if !matches!(tokens.peek(), Some(Token::Punctuator(PunctuatorKind::RightParen))) {
        return Err("Expected ')'".to_string());
    }
    tokens.next();

    let block = match parse_block_statement(tokens)? {
        Some(node) => node,
        None => return Err("Expected block statement".to_string()),
    };

    if is_declaration {
        match name {
            Some(name) => Ok(Some(Node::FunctionDeclaration(name, parameters, Box::new(block)))),
            None => Err("Expected identifier".to_string()),
        }
    } else {
        Ok(Some(Node::FunctionExpression(name, parameters, Box::new(block))))
    }
}

fn is_reserved_words(word: &str) -> bool {
    matches!(word, "if" | "let" | "for" | "function" | "true" | "false" | "else" | "return" | "break")
}

fn parse_type_annotation(tokens: &mut TokenIter) -> Result<Option<Type>, String> {
    parse_primitive_type_annotation(tokens)
}

fn parse_primitive_type_annotation(tokens: &mut TokenIter) -> Result<Option<Type>, String> {
    match tokens.peek() {
        Some(Token::Identifier(identifier)) => {
            match identifier.as_str() {
                "number" => {
                    tokens.next();
                    Ok(Some(Type::Number))
                }
                "bool" => {
                    tokens.next();
                    Ok(Some(Type::Bool))
                }
                "string" => {
                    tokens.next();
                    Ok(Some(Type::String))
                }
                _ => Ok(None),
            }
        }
        _ => Ok(None),
    }
}

#[cfg(test)]
mod tests {
    mod statement {
        mod if_statement {
            use crate::node::Node;
            use crate::parser::parse;

            #[test]
            fn if_statement() {
                assert_eq!(
                    parse("if (1) 2 else 3"),
                    Ok(Node::Program(vec![
                        Node::IfStatement(
                            Box::new(Node::Number(1f64)),
                            Box::new(Node::Number(2f64)),
                            Some(Box::new(Node::Number(3f64))),
                        ),
                    ]))
                );
            }

            #[test]
            fn if_statement_without_false_branch() {
                assert_eq!(
                    parse("if (1) 2"),
                    Ok(Node::Program(vec![
                        Node::IfStatement(
                            Box::new(Node::Number(1f64)),
                            Box::new(Node::Number(2f64)),
                            None,
                        ),
                    ]))
                );
            }

            #[test]
            fn chained_if() {
                assert_eq!(
                    parse("if (1) 2 else if (3) 4 else 5"),
                    Ok(Node::Program(vec![
                        Node::IfStatement(
                            Box::new(Node::Number(1f64)),
                            Box::new(Node::Number(2f64)),
                            Some(Box::new(Node::IfStatement(
                                Box::new(Node::Number(3f64)),
                                Box::new(Node::Number(4f64)),
                                Some(Box::new(Node::Number(5f64))),
                            ))),
                        ),
                    ]))
                );
            }
        }

        mod block_statement {
            use crate::node::Node;
            use crate::parser::parse;
            use crate::punctuator_kind::PunctuatorKind;

            #[test]
            fn block() {
                assert_eq!(
                    parse("{1 2}"),
                    Ok(Node::Program(vec![
                        Node::BlockExpression(vec![
                            Node::Number(1f64),
                            Node::Number(2f64),
                        ]),
                    ]))
                );
            }

            #[test]
            fn nested_block() {
                assert_eq!(
                    parse("{{1+2}}"),
                    Ok(Node::Program(vec![
                        Node::BlockExpression(vec![
                            Node::BlockExpression(vec![
                                Node::BinaryExpression(
                                    Box::new(Node::Number(1f64)),
                                    PunctuatorKind::Plus,
                                    Box::new(Node::Number(2f64)),
                                ),
                            ]),
                        ]),
                    ]))
                );
            }
        }

        mod variable_declaration {
            use crate::node::Node;
            use crate::parser::parse;
            use crate::type_::Type;

            #[test]
            fn variable_declaration() {
                assert_eq!(
                    parse("let x"),
                    Ok(Node::Program(vec![
                        Node::VariableDeclaration("x".to_string(), None, None),
                    ]))
                );
            }

            #[test]
            fn with_initial_value() {
                assert_eq!(
                    parse("let x = 1"),
                    Ok(Node::Program(vec![
                        Node::VariableDeclaration("x".to_string(), None, Some(Box::new(Node::Number(1.0f64)))),
                    ]))
                );
            }

            #[test]
            fn with_type_annotation() {
                assert_eq!(
                    parse("let x: number = 1"),
                    Ok(Node::Program(vec![
                        Node::VariableDeclaration("x".to_string(), Some(Type::Number), Some(Box::new(Node::Number(1.0f64)))),
                    ]))
                );
            }
        }

        mod for_statement {
            use crate::node::Node;
            use crate::parser::parse;

            #[test]
            fn for_statement() {
                assert_eq!(
                    parse("for (i in range) 1"),
                    Ok(Node::Program(vec![
                        Node::ForStatement(
                            "i".to_string(),
                            Box::new(Node::Identifier("range".to_string())),
                            Box::new(Node::Number(1f64)),
                        ),
                    ]))
                );
            }

            #[test]
            fn no_left_paren() {
                assert_eq!(
                    parse("for i in range) 1"),
                    Err("Expected '('".to_string())
                );
            }

            #[test]
            fn no_in() {
                assert_eq!(
                    parse("for (i range) 1"),
                    Err("Expected 'in'".to_string())
                );
            }

            #[test]
            fn no_iterator() {
                assert_eq!(
                    parse("for (i in) 1"),
                    Err("Expected expression".to_string())
                );
            }

            #[test]
            fn no_right_paren() {
                assert_eq!(
                    parse("for (i in range 1"),
                    Err("Expected ')'".to_string())
                );
            }

            #[test]
            fn no_body() {
                assert_eq!(
                    parse("for (i in range)"),
                    Err("Expected statement".to_string())
                );
            }

            #[test]
            fn with_block() {
                assert_eq!(
                    parse("for (i in range) { 1 }"),
                    Ok(Node::Program(vec![
                        Node::ForStatement(
                            "i".to_string(),
                            Box::new(Node::Identifier("range".to_string())),
                            Box::new(Node::BlockExpression(vec![
                                Node::Number(1f64),
                            ])),
                        ),
                    ]))
                );
            }
        }
    }

    mod expression {
        mod if_expression {
            use crate::node::Node;
            use crate::parser::parse;
            use crate::punctuator_kind::PunctuatorKind;

            #[test]
            fn if_expression() {
                assert_eq!(
                    parse("1 + if (1) 2 else 3"),
                    Ok(Node::Program(vec![
                        Node::BinaryExpression(
                            Box::new(Node::Number(1f64)),
                            PunctuatorKind::Plus,
                            Box::new(Node::IfExpression(
                                Box::new(Node::Number(1f64)),
                                Box::new(Node::Number(2f64)),
                                Box::new(Node::Number(3f64)),
                            )),
                        )
                    ]))
                );
            }

            #[test]
            fn if_expression_requires_false_branch() {
                assert_eq!(
                    parse("1 + if (1) 2"),
                    Err("Expected 'else'".to_string())
                );
            }

            #[test]
            fn chained_if() {
                assert_eq!(
                    parse("1 + if (2) 3 else if (4) 5 else 6"),
                    Ok(Node::Program(vec![
                        Node::BinaryExpression(
                            Box::new(Node::Number(1f64)),
                            PunctuatorKind::Plus,
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
                    ]))
                );
            }
        }

        mod assignment_expression {
            use crate::node::Node;
            use crate::parser::parse;

            #[test]
            fn assign() {
                assert_eq!(
                    parse("x = 1"),
                    Ok(Node::Program(vec![
                        Node::AssignmentExpression(
                            Box::new(Node::Identifier("x".to_string())),
                            Box::new(Node::Number(1f64))
                        ),
                    ]))
                );
            }

            #[test]
            fn no_value() {
                assert_eq!(
                    parse("x ="),
                    Err("Expected expression".to_string())
                );
            }
        }

        mod logical_or_expression {
            use crate::node::Node;
            use crate::parser::parse;
            use crate::punctuator_kind::PunctuatorKind;

            #[test]
            fn logical_or() {
                assert_eq!(
                    parse("true || false"),
                    Ok(Node::Program(vec![
                        Node::BinaryExpression(
                            Box::new(Node::Bool(true)),
                            PunctuatorKind::LogicalOr,
                            Box::new(Node::Bool(false)),
                        )
                    ]))
                );
            }

            #[test]
            fn prioritize_logical_and_over_logical_or() {
                assert_eq!(
                    parse("true || false && true"),
                    Ok(Node::Program(vec![
                        Node::BinaryExpression(
                            Box::new(Node::Bool(true)),
                            PunctuatorKind::LogicalOr,
                            Box::new(Node::BinaryExpression(
                                Box::new(Node::Bool(false)),
                                PunctuatorKind::LogicalAnd,
                                Box::new(Node::Bool(true)),
                            )),
                        )
                    ]))
                );
            }
        }

        mod logical_and_expression {
            use crate::node::Node;
            use crate::parser::parse;
            use crate::punctuator_kind::PunctuatorKind;

            #[test]
            fn logical_and() {
                assert_eq!(
                    parse("true && false"),
                    Ok(Node::Program(vec![
                        Node::BinaryExpression(
                            Box::new(Node::Bool(true)),
                            PunctuatorKind::LogicalAnd,
                            Box::new(Node::Bool(false)),
                        )
                    ]))
                );
            }

            #[test]
            fn prioritize_additive_over_logical_and() {
                assert_eq!(
                    parse("true && false + 1"),
                    Ok(Node::Program(vec![
                        Node::BinaryExpression(
                            Box::new(Node::Bool(true)),
                            PunctuatorKind::LogicalAnd,
                            Box::new(Node::BinaryExpression(
                                Box::new(Node::Bool(false)),
                                PunctuatorKind::Plus,
                                Box::new(Node::Number(1f64)),
                            )),
                        )
                    ]))
                );
            }
        }

        mod additive_expression {
            use crate::node::Node;
            use crate::parser::parse;
            use crate::punctuator_kind::PunctuatorKind;

            #[test]
            fn add() {
                assert_eq!(
                    parse("1+2"),
                    Ok(Node::Program(vec![
                        Node::BinaryExpression(
                            Box::new(Node::Number(1f64)),
                            PunctuatorKind::Plus,
                            Box::new(Node::Number(2f64)),
                        )
                    ]))
                );
            }

            #[test]
            fn subtract() {
                assert_eq!(
                    parse("1-2"),
                    Ok(Node::Program(vec![
                        Node::BinaryExpression(
                            Box::new(Node::Number(1f64)),
                            PunctuatorKind::Minus,
                            Box::new(Node::Number(2f64)),
                        )
                    ]))
                );
            }

            #[test]
            fn add_and_subtract() {
                assert_eq!(
                    parse("1+2-3"),
                    Ok(Node::Program(vec![
                        Node::BinaryExpression(
                            Box::new(Node::BinaryExpression(
                                Box::new(Node::Number(1f64)),
                                PunctuatorKind::Plus,
                                Box::new(Node::Number(2f64)),
                            )),
                            PunctuatorKind::Minus,
                            Box::new(Node::Number(3f64)),
                        )
                    ]))
                );
            }
        }

        mod multiplicative_expression {
            use crate::node::Node;
            use crate::parser::parse;
            use crate::punctuator_kind::PunctuatorKind;

            #[test]
            fn multiply() {
                assert_eq!(
                    parse("1*2"),
                    Ok(Node::Program(vec![
                        Node::BinaryExpression(
                            Box::new(Node::Number(1f64)),
                            PunctuatorKind::Multiply,
                            Box::new(Node::Number(2f64)),
                        )
                    ]))
                );
            }

            #[test]
            fn divide() {
                assert_eq!(
                    parse("1/2"),
                    Ok(Node::Program(vec![
                        Node::BinaryExpression(
                            Box::new(Node::Number(1f64)),
                            PunctuatorKind::Divide,
                            Box::new(Node::Number(2f64)),
                        )
                    ]))
                );
            }

            #[test]
            fn multiply_and_divide() {
                assert_eq!(
                    parse("1*2/3"),
                    Ok(Node::Program(vec![
                        Node::BinaryExpression(
                            Box::new(Node::BinaryExpression(
                                Box::new(Node::Number(1f64)),
                                PunctuatorKind::Multiply,
                                Box::new(Node::Number(2f64)),
                            )),
                            PunctuatorKind::Divide,
                            Box::new(Node::Number(3f64)),
                        )
                    ]))
                );
            }

            #[test]
            fn multiply_after_add() {
                assert_eq!(
                    parse("1+2*3"),
                    Ok(Node::Program(vec![
                        Node::BinaryExpression(
                            Box::new(Node::Number(1f64)),
                            PunctuatorKind::Plus,
                            Box::new(Node::BinaryExpression(
                                Box::new(Node::Number(2f64)),
                                PunctuatorKind::Multiply,
                                Box::new(Node::Number(3f64)),
                            )),
                        )
                    ]))
                );
            }

            #[test]
            fn add_after_multiply() {
                assert_eq!(
                    parse("1*2+3"),
                    Ok(Node::Program(vec![
                        Node::BinaryExpression(
                            Box::new(Node::BinaryExpression(
                                Box::new(Node::Number(1f64)),
                                PunctuatorKind::Multiply,
                                Box::new(Node::Number(2f64)),
                            )),
                            PunctuatorKind::Plus,
                            Box::new(Node::Number(3f64)),
                        )
                    ]))
                );
            }

            #[test]
            fn add_multiplicative_expressions() {
                assert_eq!(
                    parse("1*2+3/4"),
                    Ok(Node::Program(vec![
                        Node::BinaryExpression(
                            Box::new(Node::BinaryExpression(
                                Box::new(Node::Number(1f64)),
                                PunctuatorKind::Multiply,
                                Box::new(Node::Number(2f64)),
                            )),
                            PunctuatorKind::Plus,
                            Box::new(Node::BinaryExpression(
                                Box::new(Node::Number(3f64)),
                                PunctuatorKind::Divide,
                                Box::new(Node::Number(4f64)),
                            )),
                        )
                    ]))
                );
            }
        }

        mod unary_expression {
            use crate::node::Node;
            use crate::parser::parse;
            use crate::punctuator_kind::PunctuatorKind;

            #[test]
            fn plus() {
                assert_eq!(
                    parse("+1"),
                    Ok(Node::Program(vec![
                        Node::UnaryExpression(
                            PunctuatorKind::Plus,
                            Box::new(Node::Number(1f64)),
                        )
                    ]))
                );
            }

            #[test]
            fn minus() {
                assert_eq!(
                    parse("-1"),
                    Ok(Node::Program(vec![
                        Node::UnaryExpression(
                            PunctuatorKind::Minus,
                            Box::new(Node::Number(1f64)),
                        )
                    ]))
                );
            }

            #[test]
            fn disallow_multiple_unary_operator() {
                assert_eq!(
                    parse("--1"),
                    Err("Expected expression".to_string()),
                );
            }

            #[test]
            fn unary_operator_in_additive_expression() {
                assert_eq!(
                    parse("1++1"),
                    Ok(Node::Program(vec![
                        Node::BinaryExpression(
                            Box::new(Node::Number(1f64)),
                            PunctuatorKind::Plus,
                            Box::new(Node::UnaryExpression(
                                PunctuatorKind::Plus,
                                Box::new(Node::Number(1f64)),
                            )),
                        ),
                    ]))
                );
            }
        }

        mod call_expression {
            use crate::node::Node;
            use crate::parser::parse;
            use crate::punctuator_kind::PunctuatorKind;

            #[test]
            fn without_arguments() {
                assert_eq!(
                    parse("func()"),
                    Ok(Node::Program(vec![
                        Node::CallExpression(
                            Box::new(Node::Identifier("func".to_string())),
                            vec![],
                        )
                    ]))
                );
            }

            #[test]
            fn with_arguments() {
                assert_eq!(
                    parse("func(1, 2, 3)"),
                    Ok(Node::Program(vec![
                        Node::CallExpression(
                            Box::new(Node::Identifier("func".to_string())),
                            vec![
                                Node::Number(1f64),
                                Node::Number(2f64),
                                Node::Number(3f64),
                            ],
                        )
                    ]))
                );
            }

            #[test]
            fn complex_expression_in_arguments() {
                assert_eq!(
                    parse("func(1, 2*(3+4))"),
                    Ok(Node::Program(vec![
                        Node::CallExpression(
                            Box::new(Node::Identifier("func".to_string())),
                            vec![
                                Node::Number(1f64),
                                Node::BinaryExpression(
                                    Box::new(Node::Number(2f64)),
                                    PunctuatorKind::Multiply,
                                    Box::new(Node::BinaryExpression(
                                        Box::new(Node::Number(3f64)),
                                        PunctuatorKind::Plus,
                                        Box::new(Node::Number(4f64)),
                                    )),
                                ),
                            ],
                        )
                    ]))
                );
            }

            #[test]
            fn function_call_in_arguments() {
                assert_eq!(
                    parse("f(g(1), h(2))"),
                    Ok(Node::Program(vec![
                        Node::CallExpression(
                            Box::new(Node::Identifier("f".to_string())),
                            vec![
                                Node::CallExpression(
                                    Box::new(Node::Identifier("g".to_string())),
                                    vec![Node::Number(1f64)],
                                ),
                                Node::CallExpression(
                                    Box::new(Node::Identifier("h".to_string())),
                                    vec![Node::Number(2f64)],
                                ),
                            ],
                        )
                    ]))
                );
            }
        }

        mod member_expression {
            use crate::node::Node;
            use crate::parser::parse;
            use crate::punctuator_kind::PunctuatorKind;

            #[test]
            fn member_expression() {
                assert_eq!(
                    parse("x.y.z"),
                    Ok(Node::Program(vec![
                        Node::MemberExpression(
                            Box::new(Node::MemberExpression(
                                Box::new(Node::Identifier("x".to_string())),
                                Box::new(Node::Identifier("y".to_string())),
                            )),
                            Box::new(Node::Identifier("z".to_string())),
                        ),
                    ]))
                );
            }

            #[test]
            fn member_expression_with_call_expression() {
                assert_eq!(
                    parse("x.y()"),
                    Ok(Node::Program(vec![
                        Node::CallExpression(
                            Box::new(Node::MemberExpression(
                                Box::new(Node::Identifier("x".to_string())),
                                Box::new(Node::Identifier("y".to_string())),
                            )),
                            vec![],
                        ),
                    ]))
                );
            }

            #[test]
            fn member_expression_with_unary_expression() {
                assert_eq!(
                    parse("-x.y"),
                    Ok(Node::Program(vec![
                        Node::UnaryExpression(
                            PunctuatorKind::Minus,
                            Box::new(Node::MemberExpression(
                                Box::new(Node::Identifier("x".to_string())),
                                Box::new(Node::Identifier("y".to_string())),
                            )),
                        ),
                    ]))
                );
            }
        }

        mod primary_expression {
            use crate::node::Node;
            use crate::parser::parse;
            use crate::punctuator_kind::PunctuatorKind;

            #[test]
            fn number() {
                assert_eq!(
                    parse("1"),
                    Ok(Node::Program(vec![
                        Node::Number(1f64),
                    ]))
                );
            }

            #[test]
            fn bool_true() {
                assert_eq!(
                    parse("true"),
                    Ok(Node::Program(vec![
                        Node::Bool(true),
                    ]))
                );
            }

            #[test]
            fn bool_false() {
                assert_eq!(
                    parse("false"),
                    Ok(Node::Program(vec![
                        Node::Bool(false),
                    ]))
                );
            }

            #[test]
            fn bool_in_expression() {
                assert_eq!(
                    parse("-true+false"),
                    Ok(Node::Program(vec![
                        Node::BinaryExpression(
                            Box::new(Node::UnaryExpression(
                                PunctuatorKind::Minus,
                                Box::new(Node::Bool(true)),
                            )),
                            PunctuatorKind::Plus,
                            Box::new(Node::Bool(false)),
                        ),
                    ]))
                );
            }

            #[test]
            fn string() {
                assert_eq!(
                    parse("print(\"hello\")"),
                    Ok(Node::Program(vec![
                        Node::CallExpression(
                            Box::new(Node::Identifier("print".to_string())),
                            vec![
                                Node::String("hello".to_string()),
                            ],
                        )
                    ]))
                );
            }
            #[test]
            fn identifier() {
                assert_eq!(
                    parse("x"),
                    Ok(Node::Program(vec![
                        Node::Identifier("x".to_string()),
                    ]))
                );
            }

            #[test]
            fn number_with_paren() {
                assert_eq!(
                    parse("(1)"),
                    Ok(Node::Program(vec![
                        Node::Number(1f64),
                    ]))
                );
            }

            #[test]
            fn multiply_additive_expressions() {
                assert_eq!(
                    parse("(1+2)*(3+4)"),
                    Ok(Node::Program(vec![
                        Node::BinaryExpression(
                            Box::new(Node::BinaryExpression(
                                Box::new(Node::Number(1f64)),
                                PunctuatorKind::Plus,
                                Box::new(Node::Number(2f64)),
                            )),
                            PunctuatorKind::Multiply,
                            Box::new(Node::BinaryExpression(
                                Box::new(Node::Number(3f64)),
                                PunctuatorKind::Plus,
                                Box::new(Node::Number(4f64)),
                            )),
                        )
                    ]))
                );
            }

            #[test]
            fn nested_parens() {
                assert_eq!(
                    parse("((1+((2))))"),
                    Ok(Node::Program(vec![
                        Node::BinaryExpression(
                            Box::new(Node::Number(1f64)),
                            PunctuatorKind::Plus,
                            Box::new(Node::Number(2f64)),
                        ),
                    ]))
                );
            }

            #[test]
            fn unpaired_paren() {
                assert_eq!(
                    parse("(1+2"),
                    Err("Expected ')'".to_string())
                );
            }
        }

        mod range_iterator {
            use crate::node::Node;
            use crate::parser::parse;
            use crate::punctuator_kind::PunctuatorKind;

            #[test]
            fn range_iterator() {
                assert_eq!(
                    parse("for(x in 0 to 10) 1"),
                    Ok(Node::Program(vec![
                        Node::ForStatement(
                            "x".to_string(),
                            Box::new(Node::RangeIterator(
                                Box::new(Node::Number(0f64)),
                                Box::new(Node::Number(10f64)),
                            )),
                            Box::new(Node::Number(1f64)),
                        ),
                    ]))
                );
            }

            #[test]
            fn complex_expression() {
                assert_eq!(
                    parse("for(x in y to 1+2) 1"),
                    Ok(Node::Program(vec![
                        Node::ForStatement(
                            "x".to_string(),
                            Box::new(Node::RangeIterator(
                                Box::new(Node::Identifier("y".to_string())),
                                Box::new(Node::BinaryExpression(
                                    Box::new(Node::Number(1f64)),
                                    PunctuatorKind::Plus,
                                    Box::new(Node::Number(2f64)),
                                )),
                            )),
                            Box::new(Node::Number(1f64)),
                        ),
                    ]))
                );
            }
        }

        mod object_literal {
            use crate::node::Node;
            use crate::parser::parse;
            use crate::punctuator_kind::PunctuatorKind;

            #[test]
            fn empty_object() {
                assert_eq!(
                    parse("{}"),
                    Ok(Node::Program(vec![
                        Node::Object(vec![]),
                    ]))
                );
            }

            #[test]
            fn object_having_properties() {
                assert_eq!(
                    parse("{x:1, y:\"hello\"}"),
                    Ok(Node::Program(vec![
                        Node::Object(vec![
                            Node::ObjectPropertyDefinition(
                                Box::new(Node::Identifier("x".to_string())),
                                Box::new(Node::Number(1f64)),
                            ),
                            Node::ObjectPropertyDefinition(
                                Box::new(Node::Identifier("y".to_string())),
                                Box::new(Node::String("hello".to_string())),
                            ),
                        ]),
                    ]))
                );
            }

            #[test]
            fn nested_object() {
                assert_eq!(
                    parse("{x:{y:1}, z:\"hello\"}"),
                    Ok(Node::Program(vec![
                        Node::Object(vec![
                            Node::ObjectPropertyDefinition(
                                Box::new(Node::Identifier("x".to_string())),
                                Box::new(
                                    Node::Object(vec![
                                        Node::ObjectPropertyDefinition(
                                            Box::new(Node::Identifier("y".to_string())),
                                            Box::new(Node::Number(1f64)),
                                        )
                                    ])
                                ),
                            ),
                            Node::ObjectPropertyDefinition(
                                Box::new(Node::Identifier("z".to_string())),
                                Box::new(Node::String("hello".to_string())),
                            ),
                        ]),
                    ]))
                );
            }

            #[test]
            fn assign_object_into_variable() {
                assert_eq!(
                    parse("x = {y:1}"),
                    Ok(Node::Program(vec![
                        Node::AssignmentExpression(
                            Box::new(Node::Identifier("x".to_string())),
                            Box::new(Node::Object(vec![
                                Node::ObjectPropertyDefinition(
                                    Box::new(Node::Identifier("y".to_string())),
                                    Box::new(Node::Number(1f64)),
                                ),
                            ])),
                        ),
                    ]))
                );
            }
        }
    }
}