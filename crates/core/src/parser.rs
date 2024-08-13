use crate::lexer::scan;
use crate::node::Node;
use crate::punctuator_kind::PunctuatorKind;
use crate::token::Token;

pub fn parse(input: &str) -> Result<Node, String> {
    parse_program(&mut scan(input)?)
}

pub fn parse_program(tokens: &mut Vec<Token>) -> Result<Node, String> {
    let mut nodes = Vec::new();

    while !tokens.is_empty() {
        if let Some(node) = parse_statement(tokens)? {
            nodes.push(node);
        } else {
            return Err("Unexpected token".to_string());
        }
    }

    Ok(Node::Program(nodes))
}

// Statement

fn parse_statement(tokens: &mut Vec<Token>) -> Result<Option<Node>, String> {
    if matches!(tokens.first(), Some(Token::Punctuator(PunctuatorKind::SemiColon))) {
        tokens.remove(0);
        return Ok(Some(Node::EmptyStatement));
    }

    if let Some(node) = parse_if_statement(tokens)? {
        return Ok(Some(node));
    }

    if let Some(node) = parse_block_statement(tokens)? {
        return Ok(Some(node));
    }

    if let Some(node) = parse_for_statement(tokens)? {
        return Ok(Some(node));
    }

    if let Some(node) = parse_variable_declaration(tokens)? {
        return Ok(Some(node));
    }

    if let Some(node) = parse_function_declaration(tokens)? {
        return Ok(Some(node));
    }

    if let Some(node) = parse_expression_statement(tokens)? {
        return Ok(Some(node));
    }

    Ok(None)
}

fn parse_if_statement(tokens: &mut Vec<Token>) -> Result<Option<Node>, String> {
    if !matches!(tokens.first(), Some(Token::Identifier(ref name)) if name == "if") {
        return Ok(None);
    }
    tokens.remove(0);

    if !matches!(tokens.first(), Some(Token::Punctuator(PunctuatorKind::LeftParen))) {
        return Err("Expected '('".to_string());
    }
    tokens.remove(0);

    let condition = match parse_expression(tokens)? {
        Some(node) => node,
        None => return Err("Expected expression".to_string()),
    };

    if !matches!(tokens.first(), Some(Token::Punctuator(PunctuatorKind::RightParen))) {
        return Err("Expected ')'".to_string());
    }
    tokens.remove(0);

    let true_branch = match parse_statement(tokens)? {
        Some(node) => node,
        None => return Err("Expected statement".to_string()),
    };

    let false_branch = if matches!(tokens.first(), Some(Token::Identifier(ref name)) if name == "else") {
        tokens.remove(0);

        match parse_statement(tokens)? {
            Some(node) => Some(node),
            None => return Err("Expected expression or block".to_string()),
        }
    } else {
        None
    };

    Ok(Some(Node::IfStatement(Box::new(condition), Box::new(true_branch), false_branch.map(Box::new))))
}

fn parse_block_statement(tokens: &mut Vec<Token>) -> Result<Option<Node>, String> {
    match parse_block_expression(tokens)? {
        Some(Node::BlockExpression(statements)) => Ok(Some(Node::BlockStatement(statements))),
        _ => Ok(None),
    }
}

fn parse_for_statement(tokens: &mut Vec<Token>) -> Result<Option<Node>, String> {
    if !matches!(tokens.first(), Some(Token::Identifier(ref name)) if name == "for") {
        return Ok(None);
    }
    tokens.remove(0);

    if !matches!(tokens.first(), Some(Token::Punctuator(PunctuatorKind::LeftParen))) {
        return Err("Expected '('".to_string());
    }
    tokens.remove(0);

    let identifier = match tokens.first() {
        Some(Token::Identifier(identifier)) => identifier.clone(),
        _ => return Err("Expected identifier".to_string()),
    };
    tokens.remove(0);

    if !matches!(tokens.first(), Some(Token::Identifier(ref name)) if name == "in") {
        return Err("Expected 'in'".to_string());
    }
    tokens.remove(0);

    let iterator = match parse_range_expression(tokens)? {
        Some(iterator) => iterator,
        None => return Err("Expected expression".to_string()),
    };

    if !matches!(tokens.first(), Some(Token::Punctuator(PunctuatorKind::RightParen))) {
        return Err("Expected ')'".to_string());
    }
    tokens.remove(0);

    let body = match parse_statement(tokens)? {
        Some(body) => body,
        None => return Err("Expected statement".to_string()),
    };

    Ok(Some(Node::ForStatement(identifier, Box::new(iterator), Box::new(body))))
}

fn parse_variable_declaration(tokens: &mut Vec<Token>) -> Result<Option<Node>, String> {
    if !matches!(tokens.first(), Some(Token::Identifier(ref name)) if name == "let") {
        return Ok(None);
    }
    tokens.remove(0);

    let identifier = match tokens.first() {
        Some(Token::Identifier(identifier)) => {
            if is_reserved_words(identifier) {
                return Err(format!("SyntaxError: \"{}\" is a reserved word", identifier));
            }
            identifier.clone()
        }
        _ => return Err("Expected identifier".to_string()),
    };
    tokens.remove(0);

    let value = if matches!(tokens.first(), Some(Token::Punctuator(PunctuatorKind::Equal))) {
        tokens.remove(0);

        match parse_expression(tokens)? {
            Some(value) => Some(value),
            None => return Err("Expected expression".to_string()),
        }
    } else {
        None
    };

    Ok(Some(Node::VariableDeclaration(identifier, value.map(Box::new))))
}

fn parse_function_declaration(tokens: &mut Vec<Token>) -> Result<Option<Node>, String> {
    return parse_function(tokens, true);
}

fn parse_expression_statement(tokens: &mut Vec<Token>) -> Result<Option<Node>, String> {
    parse_expression(tokens)
}

// Expression

fn parse_expression(tokens: &mut Vec<Token>) -> Result<Option<Node>, String> {
    if let Some(node) = parse_assignment_expression(tokens)? {
        return Ok(Some(node));
    }

    if let Some(node) = parse_logical_or_expression(tokens)? {
        return Ok(Some(node));
    }

    Ok(None)
}

fn parse_range_expression(tokens: &mut Vec<Token>) -> Result<Option<Node>, String> {
    let start = match parse_expression_statement(tokens)? {
        Some(start) => start,
        _ => return Ok(None),
    };

    if !matches!(tokens.first(), Some(Token::Identifier(ref name)) if name == "to") {
        return Ok(Some(start));
    }
    tokens.remove(0);

    let end = match parse_expression_statement(tokens)? {
        Some(start) => start,
        _ => return Err("Expected expression".to_string()),
    };

    Ok(Some(Node::RangeIterator(Box::new(start), Box::new(end))))
}

fn parse_assignment_expression(tokens: &mut Vec<Token>) -> Result<Option<Node>, String> {
    let identifier = match tokens.first() {
        Some(Token::Identifier(identifier)) => identifier.clone(),
        _ => return Ok(None),
    };
    if !matches!(tokens.get(1), Some(Token::Punctuator(PunctuatorKind::Equal))) {
        return Ok(None);
    }
    tokens.remove(0);
    tokens.remove(0);

    let value = match parse_expression(tokens)? {
        Some(node) => node,
        None => return Err("Expected expression".to_string()),
    };

    Ok(Some(Node::AssignmentExpression(identifier.clone(), Box::new(value))))
}

fn parse_logical_or_expression(tokens: &mut Vec<Token>) -> Result<Option<Node>, String> {
    let mut lhs = match parse_logical_and_expression(tokens)? {
        Some(node) => node,
        None => return Ok(None),
    };

    loop {
        if !matches!(tokens.first(), Some(Token::Punctuator(PunctuatorKind::LogicalOr))) {
            break;
        }
        tokens.remove(0);

        let rhs = match parse_logical_and_expression(tokens)? {
            Some(node) => node,
            None => return Err("Expected expression".to_string()),
        };

        lhs = Node::BinaryExpression(Box::new(lhs), PunctuatorKind::LogicalOr, Box::new(rhs));
    }

    Ok(Some(lhs))
}

fn parse_logical_and_expression(tokens: &mut Vec<Token>) -> Result<Option<Node>, String> {
    let mut lhs = match parse_additive_expression(tokens)? {
        Some(node) => node,
        None => return Ok(None),
    };

    loop {
        if !matches!(tokens.first(), Some(Token::Punctuator(PunctuatorKind::LogicalAnd))) {
            break;
        }
        tokens.remove(0);

        let rhs = match parse_additive_expression(tokens)? {
            Some(node) => node,
            None => return Err("Expected expression".to_string()),
        };

        lhs = Node::BinaryExpression(Box::new(lhs), PunctuatorKind::LogicalAnd, Box::new(rhs));
    }

    Ok(Some(lhs))
}

fn parse_additive_expression(tokens: &mut Vec<Token>) -> Result<Option<Node>, String> {
    let mut lhs = match parse_multiplicative_expression(tokens)? {
        Some(node) => node,
        None => return Ok(None),
    };

    loop {
        let operator = match tokens.first() {
            Some(Token::Punctuator(PunctuatorKind::Plus)) => {
                tokens.remove(0);
                PunctuatorKind::Plus
            }
            Some(Token::Punctuator(PunctuatorKind::Minus)) => {
                tokens.remove(0);
                PunctuatorKind::Minus
            }
            _ => break
        };

        let rhs = match parse_multiplicative_expression(tokens)? {
            Some(node) => node,
            None => return Ok(None),
        };

        lhs = Node::BinaryExpression(Box::new(lhs), operator, Box::new(rhs));
    }

    Ok(Some(lhs))
}

fn parse_multiplicative_expression(tokens: &mut Vec<Token>) -> Result<Option<Node>, String> {
    let mut lhs = match parse_unary_expression(tokens)? {
        Some(node) => node,
        None => return Ok(None),
    };

    loop {
        let operator = match tokens.first() {
            Some(Token::Punctuator(PunctuatorKind::Multiply)) => {
                tokens.remove(0);
                PunctuatorKind::Multiply
            }
            Some(Token::Punctuator(PunctuatorKind::Divide)) => {
                tokens.remove(0);
                PunctuatorKind::Divide
            }
            _ => break
        };

        let rhs = match parse_unary_expression(tokens)? {
            Some(node) => node,
            None => return Ok(None),
        };

        lhs = Node::BinaryExpression(Box::new(lhs), operator, Box::new(rhs));
    }

    Ok(Some(lhs))
}

fn parse_unary_expression(tokens: &mut Vec<Token>) -> Result<Option<Node>, String> {
    let operator = match tokens.first() {
        Some(Token::Punctuator(PunctuatorKind::Plus)) => {
            tokens.remove(0);
            PunctuatorKind::Plus
        }
        Some(Token::Punctuator(PunctuatorKind::Minus)) => {
            tokens.remove(0);
            PunctuatorKind::Minus
        }
        Some(Token::Punctuator(PunctuatorKind::LogicalNot)) => {
            tokens.remove(0);
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

fn parse_statement_expression(tokens: &mut Vec<Token>) -> Result<Option<Node>, String> {
    if let Some(node) = parse_function_expression(tokens)? {
        return Ok(Some(node));
    }

    if let Some(node) = parse_if_expression(tokens)? {
        return Ok(Some(node));
    }

    if let Some(node) = parse_block_expression(tokens)? {
        return Ok(Some(node));
    }

    parse_call_expression(tokens)
}

fn parse_function_expression(tokens: &mut Vec<Token>) -> Result<Option<Node>, String> {
    return parse_function(tokens, false);
}

fn parse_if_expression(tokens: &mut Vec<Token>) -> Result<Option<Node>, String> {
    if !matches!(tokens.first(), Some(Token::Identifier(ref name)) if name == "if") {
        return Ok(None);
    }
    tokens.remove(0);

    if !matches!(tokens.first(), Some(Token::Punctuator(PunctuatorKind::LeftParen))) {
        return Err("Expected '('".to_string());
    }
    tokens.remove(0);

    let condition = match parse_expression(tokens)? {
        Some(node) => node,
        None => return Err("Expected expression".to_string()),
    };

    if !matches!(tokens.first(), Some(Token::Punctuator(PunctuatorKind::RightParen))) {
        return Err("Expected ')'".to_string());
    }
    tokens.remove(0);

    let true_branch = match parse_expression(tokens)? {
        Some(node) => node,
        None => return Err("Expected expression or block".to_string()),
    };

    if !matches!(tokens.first(), Some(Token::Identifier(ref name)) if name == "else") {
        return Err("Expected 'else'".to_string());
    }
    tokens.remove(0);

    let false_branch = match parse_expression(tokens)? {
        Some(node) => node,
        None => return Err("Expected expression or block".to_string()),
    };

    Ok(Some(Node::IfExpression(Box::new(condition), Box::new(true_branch), Box::new(false_branch))))
}

fn parse_block_expression(tokens: &mut Vec<Token>) -> Result<Option<Node>, String> {
    if !matches!(tokens.first(), Some(Token::Punctuator(PunctuatorKind::LeftBrace))) {
        return Ok(None);
    }
    tokens.remove(0);

    let mut statements = Vec::new();

    while let Some(statement) = parse_statement(tokens)? {
        statements.push(statement);
    }

    if !matches!(tokens.first(), Some(Token::Punctuator(PunctuatorKind::RightBrace))) {
        return Err("Expected '}'".to_string());
    }
    tokens.remove(0);

    Ok(Some(Node::BlockExpression(statements)))
}

fn parse_call_expression(tokens: &mut Vec<Token>) -> Result<Option<Node>, String> {
    let callee = match parse_primary_expression(tokens)? {
        Some(node) => node,
        None => return Ok(None),
    };

    if !matches!(tokens.first(), Some(Token::Punctuator(PunctuatorKind::LeftParen))) {
        return Ok(Some(callee));
    }
    tokens.remove(0);

    let mut arguments = Vec::new();

    while let Some(expression) = parse_expression(tokens)? {
        arguments.push(expression);

        if !matches!(tokens.first(), Some(Token::Punctuator(PunctuatorKind::Comma))) {
            break;
        }
        tokens.remove(0);
    };

    if !matches!(tokens.first(), Some(Token::Punctuator(PunctuatorKind::RightParen))) {
        return Err("Expected ')'".to_string());
    }
    tokens.remove(0);

    Ok(Some(Node::CallExpression(Box::new(callee), arguments)))
}

// PrimaryExpression := "(" Expression ")"
//                    | Number
fn parse_primary_expression(tokens: &mut Vec<Token>) -> Result<Option<Node>, String> {
    match tokens.first() {
        Some(Token::Punctuator(PunctuatorKind::LeftParen)) => {
            tokens.remove(0);

            let expression = match parse_expression(tokens)? {
                Some(node) => node,
                None => return Err("Expected expression".to_string()),
            };

            if !matches!(tokens.first(), Some(Token::Punctuator(PunctuatorKind::RightParen))) {
                return Err("Expected ')'".to_string());
            }
            tokens.remove(0);

            Ok(Some(expression))
        }
        Some(Token::Identifier(identifier)) => {
            let identifier = identifier.clone();

            tokens.remove(0);
            Ok(Some(Node::Identifier(identifier)))
        }
        Some(Token::Number(value)) => {
            let value = *value;

            tokens.remove(0);
            Ok(Some(Node::Number(value)))
        }
        Some(Token::Bool(value)) => {
            let value = *value;

            tokens.remove(0);
            Ok(Some(Node::Bool(value)))
        }
        Some(Token::String(value)) => {
            let value = value.clone();
            tokens.remove(0);
            Ok(Some(Node::String(value)))
        }
        _ => Ok(None),
    }
}

fn parse_function(tokens: &mut Vec<Token>, name_required: bool) -> Result<Option<Node>, String> {
    if !matches!(tokens.first(), Some(Token::Identifier(ref name)) if name == "function") {
        return Ok(None);
    }
    tokens.remove(0);

    let name = match tokens.first() {
        Some(Token::Identifier(name)) => {
            if is_reserved_words(name) {
                return Err(format!("SyntaxError: \"{}\" is a reserved word", name));
            }

            let name = name.clone();
            tokens.remove(0);
            Some(name)
        }
        _ => None,
    };

    if !matches!(tokens.first(), Some(Token::Punctuator(PunctuatorKind::LeftParen))) {
        return Err("Expected '('".to_string());
    }
    tokens.remove(0);

    let mut parameters = Vec::new();
    while let Some(Token::Identifier(identifier)) = tokens.first() {
        if is_reserved_words(identifier) {
            return Err(format!("SyntaxError: \"{}\" is a reserved word", identifier));
        }

        parameters.push(identifier.clone());
        tokens.remove(0);

        if !matches!(tokens.first(), Some(Token::Punctuator(PunctuatorKind::Comma))) {
            break;
        }
        tokens.remove(0);
    }

    if !matches!(tokens.first(), Some(Token::Punctuator(PunctuatorKind::RightParen))) {
        return Err("Expected ')'".to_string());
    }
    tokens.remove(0);

    let block = match parse_block_statement(tokens)? {
        Some(block) => block,
        None => return Err("Expected block".to_string()),
    };

    if name_required {
        match name {
            Some(name) => Ok(Some(Node::FunctionDeclaration(name, parameters, Box::new(block)))),
            None => Err("Expected identifier".to_string()),
        }
    } else {
        Ok(Some(Node::FunctionExpression(name, parameters, Box::new(block))))
    }
}

fn is_reserved_words(word: &str) -> bool {
    matches!(word, "if" | "let" | "for" | "function" | "true" | "false" | "else")
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
                        Node::BlockStatement(vec![
                            Node::Number(1f64),
                            Node::Number(2f64),
                        ]),
                    ]))
                );
            }

            #[test]
            fn empty_block() {
                assert_eq!(
                    parse("{}"),
                    Ok(Node::Program(vec![
                        Node::BlockStatement(vec![]),
                    ]))
                );
            }

            #[test]
            fn nested_block() {
                assert_eq!(
                    parse("{{{}1+2{}}}"),
                    Ok(Node::Program(vec![
                        Node::BlockStatement(vec![
                            Node::BlockStatement(vec![
                                Node::BlockStatement(vec![]),
                                Node::BinaryExpression(
                                    Box::new(Node::Number(1f64)),
                                    PunctuatorKind::Plus,
                                    Box::new(Node::Number(2f64)),
                                ),
                                Node::BlockStatement(vec![]),
                            ]),
                        ]),
                    ]))
                );
            }
        }

        mod variable_declaration {
            use crate::node::Node;
            use crate::parser::parse;

            #[test]
            fn variable_declaration() {
                assert_eq!(
                    parse("let x"),
                    Ok(Node::Program(vec![
                        Node::VariableDeclaration("x".to_string(), None),
                    ]))
                );
            }

            #[test]
            fn with_initial_value() {
                assert_eq!(
                    parse("let x = 1"),
                    Ok(Node::Program(vec![
                        Node::VariableDeclaration("x".to_string(), Some(Box::new(Node::Number(1.0f64)))),
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
                            Box::new(Node::BlockStatement(vec![
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
                        Node::AssignmentExpression("x".to_string(), Box::new(Node::Number(1f64))),
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
    }
}