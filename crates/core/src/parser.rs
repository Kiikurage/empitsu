use crate::ast::block::Block;
use crate::ast::break_expression::BreakExpression;
use crate::ast::for_statement::ForStatement;
use crate::ast::function::Function;
use crate::ast::function_interface::FunctionInterface;
use crate::ast::identifier::Identifier;
use crate::ast::if_expression::IfExpression;
use crate::ast::if_statement::IfStatement;
use crate::ast::impl_statement::ImplStatement;
use crate::ast::interface_declaration::InterfaceDeclaration;
use crate::ast::node::Node;
use crate::ast::parameter::Parameter;
use crate::ast::parameter_declaration::ParameterDeclaration;
use crate::ast::program::Program;
use crate::ast::return_expression::ReturnExpression;
use crate::ast::struct_declaration::StructDeclaration;
use crate::ast::traits::GetPosition;
use crate::ast::type_expression::TypeExpression;
use crate::ast::variable_declaration::VariableDeclaration;
use crate::error::Error;
use crate::punctuation_kind::PunctuationKind;
use crate::token::{IdentifierToken, PunctuationToken, Token};
use crate::token_iterator::TokenIterator;

pub struct ParseResult {
    pub program: Program,
    pub errors: Vec<Error>,
}

impl ParseResult {
    pub fn new(program: Program, errors: Vec<Error>) -> Self {
        Self { program, errors }
    }
}

fn is_reserved(word: &str) -> bool {
    matches!(
        word,
        "break"  | "else" | "false" | "fn" | "for" | "if" | "impl" | "in" | "interface" |
        "let" | "null" | "return" | "struct" | "true"
    )
}

fn assert_keyword(token: &Result<Token, Error>, expected: &str) -> Result<IdentifierToken, Error> {
    match token {
        Ok(Token::Identifier(identifier)) if identifier.text == expected => Ok(identifier.clone()),
        Ok(token) => Err(Error::unexpected_token(token.position().clone(), expected)),
        Err(err) => Err(err.clone()),
    }
}

fn assert_non_reserved_identifier(token: &Result<Token, Error>) -> Result<Identifier, Error> {
    match token {
        Ok(Token::Identifier(identifier)) => {
            if is_reserved(&identifier.text) {
                Err(Error::reserved_word(identifier.position.clone(), identifier.text.clone()))
            } else {
                Ok(Node::identifier(identifier.position.clone(), identifier.text.clone()))
            }
        }
        Ok(token) => Err(Error::unexpected_token(token.position().clone(), "identifier")),
        Err(err) => Err(err.clone()),
    }
}

macro_rules! assert_punctuation {
    ($token:expr, $expected:ident) => {
        match $token {
            Ok(Token::Punctuation(token @ PunctuationToken { value: PunctuationKind::$expected, .. })) => Ok(token),
            Ok(token) => Err(Error::unexpected_token(token.position().clone(), PunctuationKind::$expected.text())),
            Err(err) => Err(err.clone()),
        }
    };
    ($token:expr, $($expected:ident),+) => {
        match $token {
            Ok(Token::Punctuation(token @ PunctuationToken { value: $(PunctuationKind::$expected)|+, .. })) => Ok(token),
            Ok(token) => {
                let expected_tokens = [$(PunctuationKind::$expected.text()),+].join(", ");
                Err(Error::unexpected_token(token.position().clone(), expected_tokens))
            }
            Err(err) => Err(err.clone()),
        }
    };
}

pub fn parse(input: &str) -> ParseResult {
    let mut iter = TokenIterator::new(input);
    parse_program(&mut iter)
}

type Parser<T> = fn(tokens: &mut TokenIterator) -> Result<T, Error>;

fn parse_one_of<T>(
    tokens: &mut TokenIterator,
    parsers: Vec<Parser<T>>,
    expected_node: &str,
) -> Result<T, Error> {
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
        Err(Error::unexpected_token(tokens.position().clone(), expected_node))
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
                break;
            }
        }
    }

    ParseResult::new(Node::program(statements), errors)
}

// Statement

fn parse_statement(tokens: &mut TokenIterator) -> Result<Node, Error> {
    parse_semicolon(tokens);
    let ret = parse_one_of(tokens, vec![
        |tokens| parse_if_statement(tokens).map(Into::into),
        |tokens| parse_block_statement(tokens).map(Into::into),
        |tokens| parse_return_statement(tokens).map(Into::into),
        |tokens| parse_break_statement(tokens).map(Into::into),
        |tokens| parse_for_statement(tokens).map(Into::into),
        |tokens| parse_variable_declaration(tokens).map(Into::into),
        |tokens| parse_function_declaration(tokens).map(Node::FunctionDeclaration),
        |tokens| parse_struct_declaration(tokens).map(Into::into),
        |tokens| parse_interface_declaration(tokens).map(Into::into),
        |tokens| parse_impl_statement(tokens).map(Into::into),
        |tokens| parse_expression_statement(tokens),
    ], "statement");
    parse_semicolon(tokens);

    ret
}

fn parse_if_statement(tokens: &mut TokenIterator) -> Result<IfStatement, Error> {
    let if_keyword = assert_keyword(tokens.next(), "if")?;

    assert_punctuation!(tokens.next(), LeftParen)?;
    let condition = parse_expression(tokens)?;
    assert_punctuation!(tokens.next(), RightParen)?;

    let true_branch = parse_statement(tokens)?;
    let false_branch = if assert_keyword(tokens.peek(), "else").is_ok() {
        tokens.next();
        Some(parse_statement(tokens)?)
    } else {
        None
    };

    Ok(Node::if_statement(if_keyword.position, condition, true_branch, false_branch))
}

fn parse_block_statement(tokens: &mut TokenIterator) -> Result<Block, Error> {
    let block_expression = parse_block_expression(tokens)?;

    Ok(block_expression)
}

fn parse_return_statement(tokens: &mut TokenIterator) -> Result<ReturnExpression, Error> {
    let return_expression = parse_return_expression(tokens)?;
    parse_statement_end(tokens)?;

    Ok(return_expression)
}

fn parse_break_statement(tokens: &mut TokenIterator) -> Result<BreakExpression, Error> {
    let break_expression = parse_break_expression(tokens)?;
    parse_statement_end(tokens)?;

    Ok(break_expression)
}

fn parse_for_statement(tokens: &mut TokenIterator) -> Result<ForStatement, Error> {
    let for_keyword = assert_keyword(tokens.next(), "for")?;

    assert_punctuation!(tokens.next(), LeftParen)?;
    let variable = assert_non_reserved_identifier(tokens.next())?;
    assert_keyword(tokens.next(), "in")?;
    let iterable = parse_expression(tokens)?;
    assert_punctuation!(tokens.next(), RightParen)?;

    let body = parse_statement(tokens)?;

    Ok(Node::for_statement(for_keyword.position, variable, iterable, body))
}

fn parse_variable_declaration(tokens: &mut TokenIterator) -> Result<VariableDeclaration, Error> {
    let let_keyword = assert_keyword(tokens.next(), "let")?;
    let name = assert_non_reserved_identifier(tokens.next())?;

    let type_ = if assert_punctuation!(tokens.peek(), Colon).is_ok() {
        tokens.next();
        Some(parse_type_expression(tokens)?)
    } else {
        None
    };

    let initializer = if assert_punctuation!(tokens.peek(), Equal).is_ok() {
        tokens.next();
        Some(parse_expression(tokens)?)
    } else {
        None
    };

    // if type_.is_none() && initializer.is_none() {
    //     return Err(Error::unexpected_token(tokens.position().clone(), "type or initializer"));
    // }

    parse_statement_end(tokens)?;

    Ok(Node::variable_declaration(let_keyword.position, name, type_, initializer))
}

fn parse_function_declaration(tokens: &mut TokenIterator) -> Result<Function, Error> {
    let interface = parse_function_interface(tokens)?;

    assert_punctuation!(tokens.next(), LeftBrace)?;
    let mut body = Vec::new();
    while assert_punctuation!(tokens.peek(), RightBrace).is_err() {
        body.push(parse_statement(tokens)?);
    }
    assert_punctuation!(tokens.next(), RightBrace)?;

    Ok(Node::function(interface, body))
}

fn parse_function_interface(tokens: &mut TokenIterator) -> Result<FunctionInterface, Error> {
    let fn_keyword = assert_keyword(tokens.next(), "fn")?;
    let name = assert_non_reserved_identifier(tokens.next())?;

    assert_punctuation!(tokens.next(), LeftParen)?;
    let mut parameters = vec![];
    while assert_punctuation!(tokens.peek(), RightParen).is_err() {
        let name = assert_non_reserved_identifier(tokens.next())?;
        let parameter = if name.name == "self" {
            Node::parameter_declaration(name, TypeExpression::identifier("self"))
        } else {
            assert_punctuation!(tokens.next(), Colon)?;
            let type_ = parse_type_expression(tokens)?;
            Node::parameter_declaration(name, type_)
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

    Ok(Node::function_interface(fn_keyword.position, Some(name), parameters, return_type))
}

fn parse_struct_declaration(tokens: &mut TokenIterator) -> Result<StructDeclaration, Error> {
    let struct_keyword = assert_keyword(tokens.next(), "struct")?;
    let name = assert_non_reserved_identifier(tokens.next())?;

    assert_punctuation!(tokens.next(), LeftParen)?;
    let mut properties = vec![];
    while assert_punctuation!(tokens.peek(), RightParen).is_err() {
        let name = assert_non_reserved_identifier(tokens.next())?;
        assert_punctuation!(tokens.next(), Colon)?;
        let type_ = parse_type_expression(tokens)?;
        properties.push(Node::property_declaration(name, type_));

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
            let function = parse_function_declaration(tokens)?;
            if matches!(function.interface.parameters.first(), Some(ParameterDeclaration { name, .. }) if name.name == "self") {
                instance_methods.push(function);
            } else {
                static_methods.push(function);
            }
        }
        assert_punctuation!(tokens.next(), RightBrace)?;
    }

    Ok(Node::struct_declaration(struct_keyword.position, name, properties, instance_methods, static_methods))
}

fn parse_interface_declaration(tokens: &mut TokenIterator) -> Result<InterfaceDeclaration, Error> {
    let interface_keyword = assert_keyword(tokens.next(), "interface")?;
    let name = assert_non_reserved_identifier(tokens.next())?;

    let mut instance_methods = vec![];
    if assert_punctuation!(tokens.peek(), LeftBrace).is_ok() {
        tokens.next();

        while assert_punctuation!(tokens.peek(), RightBrace).is_err() {
            instance_methods.push(parse_function_interface(tokens)?);
        }
        assert_punctuation!(tokens.next(), RightBrace)?;
    }

    Ok(Node::interface_declaration(interface_keyword.position, name, instance_methods))
}

fn parse_impl_statement(tokens: &mut TokenIterator) -> Result<ImplStatement, Error> {
    let impl_keyword = assert_keyword(tokens.next(), "impl")?;
    let interface_name = assert_non_reserved_identifier(tokens.next())?;

    assert_keyword(tokens.next(), "for")?;
    let struct_name = assert_non_reserved_identifier(tokens.next())?;

    let mut instance_methods = vec![];
    if assert_punctuation!(tokens.peek(), LeftBrace).is_ok() {
        tokens.next();

        while assert_punctuation!(tokens.peek(), RightBrace).is_err() {
            let function = parse_function_declaration(tokens)?;
            instance_methods.push(function);
        }
        assert_punctuation!(tokens.next(), RightBrace)?;
    }

    Ok(Node::impl_statement(impl_keyword.position, interface_name, struct_name, instance_methods))
}

fn parse_expression_statement(tokens: &mut TokenIterator) -> Result<Node, Error> {
    let expression = parse_expression(tokens)?;
    parse_statement_end(tokens)?;

    Ok(expression)
}

fn parse_statement_end(tokens: &mut TokenIterator) -> Result<(), Error> {
    match tokens.raw_peek() {
        Ok(Token::Punctuation(PunctuationToken { value: PunctuationKind::SemiColon, .. })) |
        Ok(Token::LineTerminator(..)) => {
            tokens.raw_next();
            Ok(())
        }
        Ok(Token::Punctuation(PunctuationToken { value: PunctuationKind::RightBrace, .. })) |
        Ok(Token::Punctuation(PunctuationToken { value: PunctuationKind::LeftBrace, .. })) |
        Ok(Token::EndOfInput(..)) => {
            Ok(())
        }
        Ok(other) => Err(Error::unexpected_token(other.position().clone(), ";")),
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
        parse_logical_or_expression,
    ], "expression")
}

fn parse_assignment_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    let lhs = parse_left_hand_side_expression(tokens)?;
    assert_punctuation!(tokens.next(), Equal)?;
    let rhs = parse_expression(tokens)?;

    Ok(Node::assignment_expression(lhs, rhs).into())
}

fn parse_logical_or_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    let mut lhs = parse_logical_and_expression(tokens)?;

    while let Ok(operator) = assert_punctuation!(tokens.peek(),
        VerticalLineVerticalLine
    ) {
        let operator = operator.clone();
        tokens.next();

        let rhs = parse_logical_and_expression(tokens)?;
        lhs = Node::binary_expression(lhs, operator.value, rhs).into();
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
        lhs = Node::binary_expression(lhs, operator.value, rhs).into();
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
        lhs = Node::binary_expression(lhs, operator.value, rhs).into();
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
        lhs = Node::binary_expression(lhs, operator.value, rhs).into();
    }

    Ok(lhs)
}

fn parse_additive_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    let mut lhs = parse_multiplicative_expression(tokens)?;

    while let Ok(operator) = assert_punctuation!(tokens.peek(), Plus, Minus) {
        let operator = operator.clone();
        tokens.next();

        let rhs = parse_multiplicative_expression(tokens)?;
        lhs = Node::binary_expression(lhs, operator.value, rhs).into();
    }

    Ok(lhs)
}

fn parse_multiplicative_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    let mut lhs = parse_unary_expression(tokens)?;

    while let Ok(operator) = assert_punctuation!(tokens.peek(), Asterisk, Slash) {
        let operator = operator.clone();
        tokens.next();

        let rhs = parse_unary_expression(tokens)?;
        lhs = Node::binary_expression(lhs, operator.value, rhs).into();
    }

    Ok(lhs)
}

fn parse_unary_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    let operator = match tokens.peek() {
        Ok(Token::Punctuation(operator @ PunctuationToken { value: PunctuationKind::Plus, .. })) |
        Ok(Token::Punctuation(operator @ PunctuationToken { value: PunctuationKind::Minus, .. })) |
        Ok(Token::Punctuation(operator @ PunctuationToken { value: PunctuationKind::Exclamation, .. })) => operator.clone(),
        _ => return parse_statement_expression(tokens),
    };
    tokens.next();

    let operand = parse_unary_expression(tokens)?;
    if matches!(operand, Node::UnaryExpression(..)) {
        return Err(Error::unexpected_token(operand.position().clone(), "expression"));
    }

    Ok(Node::unary_expression(operator.position.clone(), operator.value.clone(), operand).into())
}

fn parse_statement_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    parse_one_of(tokens, vec![
        |tokens| parse_if_expression(tokens).map(IfExpression::into),
        |tokens| parse_block_expression(tokens).map(Block::into),
        |tokens| parse_return_expression(tokens).map(ReturnExpression::into),
        |tokens| parse_break_expression(tokens).map(BreakExpression::into),
        |tokens| parse_function_expression(tokens).map(Node::FunctionExpression),
        |tokens| parse_call_expression(tokens),
    ], "expression")
}

fn parse_if_expression(tokens: &mut TokenIterator) -> Result<IfExpression, Error> {
    let if_keyword = assert_keyword(tokens.next(), "if")?;

    assert_punctuation!(tokens.next(), LeftParen)?;
    let condition = parse_expression(tokens)?;
    assert_punctuation!(tokens.next(), RightParen)?;

    let true_branch = parse_expression(tokens)?;
    assert_keyword(tokens.next(), "else")?;
    let false_branch = parse_expression(tokens)?;

    Ok(Node::if_expression(
        if_keyword.position.clone(),
        condition,
        true_branch,
        false_branch,
    ))
}

fn parse_block_expression(tokens: &mut TokenIterator) -> Result<Block, Error> {
    let left_brace = assert_punctuation!(tokens.next(), LeftBrace)?.clone();

    let mut statements = Vec::new();
    while assert_punctuation!(tokens.peek(), RightBrace).is_err() {
        statements.push(parse_statement(tokens)?);
    }

    assert_punctuation!(tokens.next(), RightBrace)?;

    Ok(Node::block(left_brace.position.clone(), statements))
}

fn parse_return_expression(tokens: &mut TokenIterator) -> Result<ReturnExpression, Error> {
    let return_keyword = assert_keyword(tokens.next(), "return")?;

    let current = tokens.raw_offset;
    let expression = match parse_expression(tokens) {
        Ok(expression) => Some(expression),
        Err(..) => {
            tokens.raw_offset = current;
            None
        }
    };

    Ok(Node::return_expression(return_keyword.position, expression))
}

fn parse_break_expression(tokens: &mut TokenIterator) -> Result<BreakExpression, Error> {
    let break_keyword = assert_keyword(tokens.next(), "break")?;

    Ok(Node::break_expression(break_keyword.position))
}

fn parse_function_expression(tokens: &mut TokenIterator) -> Result<Function, Error> {
    let fn_keyword = assert_keyword(tokens.next(), "fn")?;

    assert_punctuation!(tokens.next(), LeftParen)?;
    let mut parameters = vec![];
    while assert_punctuation!(tokens.peek(), RightParen).is_err() {
        let name = assert_non_reserved_identifier(tokens.next())?;
        assert_punctuation!(tokens.next(), Colon)?;
        let type_ = parse_type_expression(tokens)?;
        parameters.push(Node::parameter_declaration(name, type_));

        if assert_punctuation!(tokens.peek(), Comma).is_ok() {
            tokens.next();
        } else {
            break;
        }
    }
    assert_punctuation!(tokens.next(), RightParen)?;

    assert_punctuation!(tokens.next(), Colon)?;
    let return_type = parse_type_expression(tokens)?;

    assert_punctuation!(tokens.next(), LeftBrace)?;
    let mut body = Vec::new();
    while assert_punctuation!(tokens.peek(), RightBrace).is_err() {
        body.push(parse_statement(tokens)?);
    }
    assert_punctuation!(tokens.next(), RightBrace)?;

    Ok(Node::function(
        Node::function_interface(
            fn_keyword.position,
            None,
            parameters,
            return_type,
        ),
        body,
    ))
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

    Ok(Node::call_expression(callee, parameters).into())
}

fn parse_parameter(tokens: &mut TokenIterator) -> Result<Parameter, Error> {
    let current = tokens.raw_offset;
    match parse_named_parameter(tokens) {
        Ok(parameter) => return Ok(parameter),
        Err(..) => tokens.raw_offset = current,
    }

    let value = parse_expression(tokens)?;
    Ok(Node::parameter(None, value))
}

fn parse_named_parameter(tokens: &mut TokenIterator) -> Result<Parameter, Error> {
    let name = assert_non_reserved_identifier(tokens.next())?;
    assert_punctuation!(tokens.next(), Equal)?;
    let value = parse_expression(tokens)?;

    Ok(Node::parameter(Some(name), value))
}

fn parse_left_hand_side_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    parse_member_expression(tokens)
}

fn parse_member_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    let mut object = parse_primary_expression(tokens)?;

    while assert_punctuation!(tokens.peek(), Dot).is_ok() {
        tokens.next();

        let name = assert_non_reserved_identifier(tokens.next())?;

        object = Node::member_expression(object, name).into();
    }

    Ok(object)
}

fn parse_primary_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    match tokens.next() {
        Ok(Token::Number(number)) => Ok(Node::number_literal(number.position.clone(), number.value).into()),
        Ok(Token::Bool(bool)) => Ok(Node::bool_literal(bool.position.clone(), bool.value).into()),
        Ok(Token::String(string)) => Ok(Node::string_literal(string.position.clone(), string.value.clone()).into()),
        Ok(Token::Identifier(identifier)) => {
            if is_reserved(&identifier.text) {
                return Err(Error::reserved_word(identifier.position.clone(), identifier.text.clone()));
            }

            Ok(Node::identifier(identifier.position.clone(), identifier.text.clone()).into())
        }
        Ok(Token::Punctuation(PunctuationToken { value: PunctuationKind::LeftParen, .. })) => {
            let expression = parse_expression(tokens)?;
            assert_punctuation!(tokens.next(), RightParen)?;
            Ok(expression)
        }
        Ok(other) => Err(Error::unexpected_token(other.position().clone(), "expression")),
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
        Ok(TypeExpression::union(types))
    }
}

fn parse_optional_type(tokens: &mut TokenIterator) -> Result<TypeExpression, Error> {
    let type_ = parse_primary_type(tokens)?;

    if assert_punctuation!(tokens.peek(), Question).is_ok() {
        tokens.next();
        Ok(TypeExpression::optional(type_))
    } else {
        Ok(type_)
    }
}

fn parse_primary_type(tokens: &mut TokenIterator) -> Result<TypeExpression, Error> {
    match tokens.peek() {
        Ok(Token::Punctuation(PunctuationToken { value: PunctuationKind::LeftParen, .. })) => {
            tokens.next();
            let type_ = parse_type_expression(tokens)?;
            assert_punctuation!(tokens.next(), RightParen)?;
            Ok(type_)
        }
        Ok(Token::Identifier(identifier)) => {
            match identifier.text.as_str() {
                "number" | "string" | "bool" | "null" => {
                    let name = identifier.text.clone();
                    tokens.next();
                    Ok(TypeExpression::identifier(name))
                }
                _ => {
                    if is_reserved(&identifier.text) {
                        Err(Error::reserved_word(identifier.position.clone(), identifier.text.clone()))
                    } else {
                        let name = identifier.text.clone();
                        tokens.next();
                        Ok(TypeExpression::identifier(name))
                    }
                }
            }
        }
        Ok(token) => Err(Error::unexpected_token(token.position().clone(), "type")),
        Err(err) => Err(err.clone()),
    }
}

#[cfg(test)]
mod tests {
    mod if_statement {
        use crate::ast::node::Node;
        use crate::parser::parse;

        #[test]
        fn if_statement() {
            assert_eq!(
                parse("if (1) 2; else 3").program,
                Node::program(vec![
                    Node::if_statement(
                        (0, 0),
                        Node::number_literal((0, 4), 1f64),
                        Node::number_literal((0, 7), 2f64),
                        Some(Node::number_literal((0, 15), 3f64)),
                    ),
                ])
            );
        }

        #[test]
        fn if_statement_without_false_branch() {
            assert_eq!(
                parse("if (1) 2").program,
                Node::program(vec![
                    Node::if_statement(
                        (0, 0),
                        Node::number_literal((0, 4), 1f64),
                        Node::number_literal((0, 7), 2f64),
                        None::<Node>,
                    ),
                ])
            );
        }

        #[test]
        fn chained_if() {
            assert_eq!(
                parse("if (1) 2; else if (3) 4; else 5").program,
                Node::program(vec![
                    Node::if_statement(
                        (0, 0),
                        Node::number_literal((0, 4), 1f64),
                        Node::number_literal((0, 7), 2f64),
                        Some(Node::if_statement(
                            (0, 15),
                            Node::number_literal((0, 19), 3f64),
                            Node::number_literal((0, 22), 4f64),
                            Some(Node::number_literal((0, 30), 5f64)),
                        )),
                    ),
                ])
            );
        }
    }

    mod block_statement {
        use crate::ast::node::Node;
        use crate::parser::parse;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn block() {
            assert_eq!(
                parse("{1; 2}").program,
                Node::program(vec![
                    Node::block((0, 0), vec![
                        Node::number_literal((0, 1), 1f64),
                        Node::number_literal((0, 4), 2f64),
                    ]),
                ])
            );
        }

        #[test]
        fn nested_block() {
            assert_eq!(
                parse("{{1+2}}").program,
                Node::program(vec![
                    Node::block((0, 0), vec![
                        Node::block((0, 1), vec![
                            Node::binary_expression(
                                Node::number_literal((0, 2), 1f64),
                                PunctuationKind::Plus,
                                Node::number_literal((0, 4), 2f64),
                            ),
                        ]),
                    ]),
                ])
            );
        }
    }

    mod variable_declaration {
        use crate::ast::node::Node;
        use crate::ast::type_expression::TypeExpression;
        use crate::parser::parse;

        #[test]
        fn variable_declaration() {
            assert_eq!(
                parse("let x:number = 0").program,
                Node::program(vec![
                    Node::variable_declaration(
                        (0, 0),
                        Node::identifier((0, 4), "x"),
                        Some(TypeExpression::identifier("number")),
                        Some(Node::number_literal((0, 15), 0f64)),
                    )
                ])
            );
        }

        #[test]
        fn with_initial_value() {
            assert_eq!(
                parse("let x = 1").program,
                Node::program(vec![
                    Node::variable_declaration(
                        (0, 0),
                        Node::identifier((0, 4), "x"),
                        None,
                        Some(Node::number_literal((0, 8), 1.0f64)),
                    ),
                ])
            );
        }

        #[test]
        fn with_type_annotation() {
            assert_eq!(
                parse("let x: number = 1").program,
                Node::program(vec![
                    Node::variable_declaration(
                        (0, 0),
                        Node::identifier((0, 4), "x"),
                        Some(TypeExpression::identifier("number")),
                        Some(Node::number_literal((0, 16), 1.0f64)),
                    ),
                ])
            );
        }

        #[test]
        fn no_type_annotation_nor_initial_value() {
            assert!(parse("let x").errors.len() == 0);
        }
    }

    mod for_statement {
        use crate::ast::node::Node;
        use crate::error::Error;
        use crate::parser::parse;

        #[test]
        fn for_statement() {
            assert_eq!(
                parse("for (i in range) 1").program,
                Node::program(vec![
                    Node::for_statement(
                        (0, 0),
                        Node::identifier((0, 5), "i"),
                        Node::identifier((0, 10), "range"),
                        Node::number_literal((0, 17), 1f64),
                    ),
                ])
            );
        }

        #[test]
        fn no_left_paren() {
            assert_eq!(
                parse("for i in range) 1").errors,
                vec![
                    Error::unexpected_token((0, 4), "(")
                ]
            );
        }

        #[test]
        fn no_in() {
            assert_eq!(
                parse("for (i range) 1").errors,
                vec![
                    Error::unexpected_token((0, 7), "in")
                ]
            );
        }

        #[test]
        fn no_iterator() {
            assert_eq!(
                parse("for (i in) 1").errors,
                vec![
                    Error::unexpected_token((0, 9), "expression")
                ]
            );
        }

        #[test]
        fn no_right_paren() {
            assert_eq!(
                parse("for (i in range 1").errors,
                vec![
                    Error::unexpected_token((0, 16), ")")
                ]
            );
        }

        #[test]
        fn no_body() {
            assert_eq!(
                parse("for (i in range)").errors,
                vec![
                    Error::unexpected_token((0, 16), "statement")
                ]
            );
        }

        #[test]
        fn with_block() {
            assert_eq!(
                parse("for (i in range) { 1 }").program,
                Node::program(vec![
                    Node::for_statement(
                        (0, 0),
                        Node::identifier((0, 5), "i"),
                        Node::identifier((0, 10), "range"),
                        Node::block((0, 17), vec![
                            Node::number_literal((0, 19), 1f64),
                        ]),
                    ),
                ])
            );
        }
    }

    mod struct_declaration {
        use crate::ast::node::Node;
        use crate::ast::type_expression::TypeExpression;
        use crate::error::Error;
        use crate::parser::parse;

        #[test]
        fn struct_declaration() {
            assert_eq!(
                parse("struct User(name: string, id: number)").program,
                Node::program(vec![
                    Node::struct_declaration(
                        (0, 0),
                        Node::identifier((0, 7), "User"),
                        vec![
                            Node::property_declaration(
                                Node::identifier((0, 12), "name"),
                                TypeExpression::identifier("string")
                            ),
                            Node::property_declaration(
                                Node::identifier((0, 26), "id"),
                                TypeExpression::identifier("number")
                            ),
                        ],
                        vec![],
                        vec![],
                    )
                ])
            );
        }

        #[test]
        fn struct_declaration_without_properties() {
            assert_eq!(
                parse("struct User()").program,
                Node::program(vec![
                    Node::struct_declaration(
                        (0, 0),
                        Node::identifier((0, 7), "User"),
                        vec![],
                        vec![],
                        vec![],
                    ),
                ])
            );
        }

        #[test]
        fn struct_declaration_without_name() {
            assert_eq!(
                parse("struct {}").errors,
                vec![
                    Error::unexpected_token((0, 7), "identifier")
                ]
            );
        }
    }

    mod interface_declaration {
        use crate::ast::node::Node;
        use crate::ast::type_expression::TypeExpression;
        use crate::parser::parse;

        #[test]
        fn interface_declaration() {
            assert_eq!(
                parse("interface ToString { fn toString(self): string }").program,
                Node::program(vec![
                    Node::interface_declaration(
                        (0, 0),
                        Node::identifier((0, 10), "ToString"),
                        vec![
                            Node::function_interface(
                                (0, 21),
                                Some(Node::identifier((0, 24), "toString")),
                                vec![
                                    Node::parameter_declaration(
                                        Node::identifier((0, 33), "self"),
                                        TypeExpression::identifier("self")
                                    ),
                                ],
                                TypeExpression::identifier("string"),
                            ),
                        ],
                    ),
                ])
            );
        }
    }

    mod impl_statement {
        use crate::ast::node::Node;
        use crate::ast::type_expression::TypeExpression;
        use crate::parser::parse;

        #[test]
        fn impl_statement() {
            assert_eq!(
                parse(r#"
                impl ToString for User {
                    fn toString(self): string {
                        return user.name
                    }
                }"#).program,
                Node::program(vec![
                    Node::impl_statement(
                        (1, 16),
                        Node::identifier((1, 21), "ToString"),
                        Node::identifier((1, 34), "User"),
                        vec![
                            Node::function(
                                Node::function_interface(
                                    (2, 20),
                                    Some(Node::identifier((2, 23), "toString")),
                                    vec![
                                        Node::parameter_declaration(
                                            Node::identifier((2, 32), "self"),
                                            TypeExpression::identifier("self"),
                                        ),
                                    ],
                                    TypeExpression::identifier("string"),
                                ),
                                vec![
                                    Node::return_expression(
                                        (3, 24),
                                        Some(Node::member_expression(
                                            Node::identifier((3, 31), "user"),
                                            Node::identifier((3, 36), "name"),
                                        )),
                                    ),
                                ],
                            ),
                        ],
                    ),
                ])
            );
        }
    }

    mod if_expression {
        use crate::ast::node::Node;
        use crate::error::Error;
        use crate::parser::parse;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn if_expression() {
            assert_eq!(
                parse("1 + if (1) 2 else 3").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::number_literal((0, 0), 1f64),
                        PunctuationKind::Plus,
                        Node::if_expression(
                            (0, 4),
                            Node::number_literal((0, 8), 1f64),
                            Node::number_literal((0, 11), 2f64),
                            Node::number_literal((0, 18), 3f64),
                        ),
                    )
                ])
            );
        }

        #[test]
        fn if_expression_requires_false_branch() {
            assert_eq!(
                parse("1 + if (1) 2").errors,
                vec![
                    Error::unexpected_token((0, 12), "else")
                ]
            );
        }

        #[test]
        fn chained_if() {
            assert_eq!(
                parse("1 + if (2) 3 else if (4) 5 else 6").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::number_literal((0, 0), 1f64),
                        PunctuationKind::Plus,
                        Node::if_expression(
                            (0, 4),
                            Node::number_literal((0, 8), 2f64),
                            Node::number_literal((0, 11), 3f64),
                            Node::if_expression(
                                (0, 18),
                                Node::number_literal((0, 22), 4f64),
                                Node::number_literal((0, 25), 5f64),
                                Node::number_literal((0, 32), 6f64)
                            ),
                        ),
                    ),
                ])
            );
        }
    }

    mod function_expression {
        use crate::ast::node::Node;
        use crate::ast::type_expression::TypeExpression;
        use crate::error::Error;
        use crate::parser::parse;
        use crate::position::Position;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn function_expression() {
            assert_eq!(
                parse("let x = fn(y: number): number { y * 2 }").program,
                Node::program(vec![
                    Node::variable_declaration(
                        (0, 0),
                        Node::identifier((0, 4), "x"),
                        None,
                        Some(Node::function_expression_node(
                            Node::function_interface(
                                (0, 8),
                                None,
                                vec![
                                    Node::parameter_declaration(
                                        Node::identifier((0, 11), "y"),
                                        TypeExpression::identifier("number"),
                                    ),
                                ],
                                TypeExpression::identifier("number"),
                            ),
                            vec![
                                Node::binary_expression(
                                    Node::identifier((0, 32), "y"),
                                    PunctuationKind::Asterisk,
                                    Node::number_literal((0, 36), 2f64),
                                ),
                            ],
                        )),
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
        use crate::ast::node::Node;
        use crate::parser::parse;

        #[test]
        fn assign() {
            assert_eq!(
                parse("x = 1").program,
                Node::program(vec![
                    Node::assignment_expression(
                        Node::identifier((0, 0), "x"),
                        Node::number_literal((0, 4), 1f64),
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
        use crate::ast::node::Node;
        use crate::parser::parse;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn logical_or() {
            assert_eq!(
                parse("true || false").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::bool_literal((0, 0), true),
                        PunctuationKind::VerticalLineVerticalLine,
                        Node::bool_literal((0, 8), false),
                    )
                ])
            );
        }

        #[test]
        fn prioritize_logical_and_over_logical_or() {
            assert_eq!(
                parse("true || false && true").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::bool_literal((0, 0), true),
                        PunctuationKind::VerticalLineVerticalLine,
                        Node::binary_expression(
                            Node::bool_literal((0, 8), false),
                            PunctuationKind::AndAnd,
                            Node::bool_literal((0, 17), true),
                        ),
                    )
                ])
            );
        }
    }

    mod logical_and_expression {
        use crate::ast::node::Node;
        use crate::parser::parse;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn logical_and() {
            assert_eq!(
                parse("true && false").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::bool_literal((0, 0), true),
                        PunctuationKind::AndAnd,
                        Node::bool_literal((0, 8), false),
                    )
                ])
            );
        }

        #[test]
        fn prioritize_additive_over_logical_and() {
            assert_eq!(
                parse("true && false + 1").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::bool_literal((0, 0), true),
                        PunctuationKind::AndAnd,
                        Node::binary_expression(
                            Node::bool_literal((0, 8), false),
                            PunctuationKind::Plus,
                            Node::number_literal((0, 16), 1f64),
                        ),
                    )
                ])
            );
        }
    }

    mod equality_expression {
        use crate::ast::node::Node;
        use crate::parser::parse;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn equal() {
            assert_eq!(
                parse("1==2").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::number_literal((0, 0), 1f64),
                        PunctuationKind::EqualEqual,
                        Node::number_literal((0, 3), 2f64),
                    )
                ])
            );
        }

        #[test]
        fn not_equal() {
            assert_eq!(
                parse("1!=2").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::number_literal((0, 0), 1f64),
                        PunctuationKind::ExclamationEqual,
                        Node::number_literal((0, 3), 2f64),
                    )
                ])
            );
        }

        #[test]
        fn not_equal_and_equal() {
            assert_eq!(
                parse("1!=2==3").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::binary_expression(
                            Node::number_literal((0, 0), 1f64),
                            PunctuationKind::ExclamationEqual,
                            Node::number_literal((0, 3), 2f64),
                        ),
                        PunctuationKind::EqualEqual,
                        Node::number_literal((0, 6), 3f64),
                    )
                ])
            );
        }

        #[test]
        fn equality_and_relational_expressions() {
            assert_eq!(
                parse("1>2==3>4").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::binary_expression(
                            Node::number_literal((0, 0), 1f64),
                            PunctuationKind::RightChevron,
                            Node::number_literal((0, 2), 2f64),
                        ),
                        PunctuationKind::EqualEqual,
                        Node::binary_expression(
                            Node::number_literal((0, 5), 3f64),
                            PunctuationKind::RightChevron,
                            Node::number_literal((0, 7), 4f64),
                        ),
                    )
                ])
            );
        }
    }

    mod relational_expression {
        use crate::ast::node::Node;
        use crate::parser::parse;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn greater_than() {
            assert_eq!(
                parse("1>2").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::number_literal((0, 0), 1f64),
                        PunctuationKind::RightChevron,
                        Node::number_literal((0, 2), 2f64),
                    )
                ])
            );
        }

        #[test]
        fn greater_than_or_equal() {
            assert_eq!(
                parse("1>=2").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::number_literal((0, 0), 1f64),
                        PunctuationKind::RightChevronEqual,
                        Node::number_literal((0, 3), 2f64),
                    )
                ])
            );
        }

        #[test]
        fn less_than() {
            assert_eq!(
                parse("1<2").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::number_literal((0, 0), 1f64),
                        PunctuationKind::LeftChevron,
                        Node::number_literal((0, 2), 2f64),
                    )
                ])
            );
        }

        #[test]
        fn less_than_or_equal() {
            assert_eq!(
                parse("1<=2").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::number_literal((0, 0), 1f64),
                        PunctuationKind::LeftChevronEqual,
                        Node::number_literal((0, 3), 2f64),
                    )
                ])
            );
        }

        #[test]
        fn greater_than_expressions() {
            assert_eq!(
                parse("1>2>3").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::binary_expression(
                            Node::number_literal((0, 0), 1f64),
                            PunctuationKind::RightChevron,
                            Node::number_literal((0, 2), 2f64),
                        ),
                        PunctuationKind::RightChevron,
                        Node::number_literal((0, 4), 3f64),
                    )
                ])
            );
        }

        #[test]
        fn greater_than_and_additive_expression() {
            assert_eq!(
                parse("1+2>3+4").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::binary_expression(
                            Node::number_literal((0, 0), 1f64),
                            PunctuationKind::Plus,
                            Node::number_literal((0, 2), 2f64),
                        ),
                        PunctuationKind::RightChevron,
                        Node::binary_expression(
                            Node::number_literal((0, 4), 3f64),
                            PunctuationKind::Plus,
                            Node::number_literal((0, 6), 4f64),
                        ),
                    )
                ])
            );
        }
    }

    mod additive_expression {
        use crate::ast::node::Node;
        use crate::parser::parse;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn add() {
            assert_eq!(
                parse("1+2").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::number_literal((0, 0), 1f64),
                        PunctuationKind::Plus,
                        Node::number_literal((0, 2), 2f64),
                    )
                ])
            );
        }

        #[test]
        fn subtract() {
            assert_eq!(
                parse("1-2").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::number_literal((0, 0), 1f64),
                        PunctuationKind::Minus,
                        Node::number_literal((0, 2), 2f64),
                    )
                ])
            );
        }

        #[test]
        fn add_and_subtract() {
            assert_eq!(
                parse("1+2-3").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::binary_expression(
                            Node::number_literal((0, 0), 1f64),
                            PunctuationKind::Plus,
                            Node::number_literal((0, 2), 2f64),
                        ),
                        PunctuationKind::Minus,
                        Node::number_literal((0, 4), 3f64),
                    )
                ])
            );
        }

        #[test]
        fn newline_at_middle() {
            assert_eq!(
                parse("1\n+2").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::number_literal((0, 0), 1f64),
                        PunctuationKind::Plus,
                        Node::number_literal((1, 1), 2f64),
                    )
                ])
            );
        }
    }

    mod multiplicative_expression {
        use crate::ast::node::Node;
        use crate::parser::parse;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn multiply() {
            assert_eq!(
                parse("1*2").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::number_literal((0, 0), 1f64),
                        PunctuationKind::Asterisk,
                        Node::number_literal((0, 2), 2f64),
                    )
                ])
            );
        }

        #[test]
        fn divide() {
            assert_eq!(
                parse("1/2").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::number_literal((0, 0), 1f64),
                        PunctuationKind::Slash,
                        Node::number_literal((0, 2), 2f64),
                    )
                ])
            );
        }

        #[test]
        fn multiply_and_divide() {
            assert_eq!(
                parse("1*2/3").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::binary_expression(
                            Node::number_literal((0, 0), 1f64),
                            PunctuationKind::Asterisk,
                            Node::number_literal((0, 2), 2f64),
                        ),
                        PunctuationKind::Slash,
                        Node::number_literal((0, 4), 3f64),
                    )
                ])
            );
        }

        #[test]
        fn multiply_after_add() {
            assert_eq!(
                parse("1+2*3").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::number_literal((0, 0), 1f64),
                        PunctuationKind::Plus,
                        Node::binary_expression(
                            Node::number_literal((0, 2), 2f64),
                            PunctuationKind::Asterisk,
                            Node::number_literal((0, 4), 3f64),
                        ),
                    )
                ])
            );
        }

        #[test]
        fn add_after_multiply() {
            assert_eq!(
                parse("1*2+3").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::binary_expression(
                            Node::number_literal((0, 0), 1f64),
                            PunctuationKind::Asterisk,
                            Node::number_literal((0, 2), 2f64),
                        ),
                        PunctuationKind::Plus,
                        Node::number_literal((0, 4), 3f64),
                    )
                ])
            );
        }

        #[test]
        fn add_multiplicative_expressions() {
            assert_eq!(
                parse("1*2+3/4").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::binary_expression(
                            Node::number_literal((0, 0), 1f64),
                            PunctuationKind::Asterisk,
                            Node::number_literal((0, 2), 2f64),
                        ),
                        PunctuationKind::Plus,
                        Node::binary_expression(
                            Node::number_literal((0, 4), 3f64),
                            PunctuationKind::Slash,
                            Node::number_literal((0, 6), 4f64),
                        ),
                    )
                ])
            );
        }
    }

    mod unary_expression {
        use crate::ast::node::Node;
        use crate::error::Error;
        use crate::parser::parse;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn plus() {
            assert_eq!(
                parse("+1").program,
                Node::program(vec![
                    Node::unary_expression(
                        (0, 0),
                        PunctuationKind::Plus,
                        Node::number_literal((0, 1), 1f64),
                    )
                ])
            );
        }

        #[test]
        fn minus() {
            assert_eq!(
                parse("-1").program,
                Node::program(vec![
                    Node::unary_expression(
                        (0, 0),
                        PunctuationKind::Minus,
                        Node::number_literal((0, 1), 1f64),
                    )
                ])
            );
        }

        #[test]
        fn disallow_multiple_unary_operator() {
            assert_eq!(
                parse("--1").errors,
                vec![
                    Error::unexpected_token((0, 1), "expression")
                ]
            );
        }

        #[test]
        fn unary_operator_in_additive_expression() {
            assert_eq!(
                parse("1++1").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::number_literal((0, 0), 1f64),
                        PunctuationKind::Plus,
                        Node::unary_expression(
                            (0, 2),
                            PunctuationKind::Plus,
                            Node::number_literal((0, 3), 1f64),
                        ),
                    ),
                ])
            );
        }
    }

    mod call_expression {
        use crate::ast::node::Node;
        use crate::parser::parse;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn without_parameters() {
            assert_eq!(
                parse("func()").program,
                Node::program(vec![
                    Node::call_expression(
                        Node::identifier((0, 0), "func"),
                        vec![],
                    )
                ])
            );
        }

        #[test]
        fn with_parameters() {
            assert_eq!(
                parse("func(1, 2, 3)").program,
                Node::program(vec![
                    Node::call_expression(
                        Node::identifier((0, 0), "func"),
                        vec![
                            Node::parameter(None, Node::number_literal((0, 5), 1f64)),
                            Node::parameter(None, Node::number_literal((0, 8), 2f64)),
                            Node::parameter(None, Node::number_literal((0, 11), 3f64)),
                        ],
                    )
                ])
            );
        }

        #[test]
        fn complex_expression_in_parameter() {
            assert_eq!(
                parse("func(1, 2*(3+4))").program,
                Node::program(vec![
                    Node::call_expression(
                        Node::identifier((0, 0), "func"),
                        vec![
                            Node::parameter(None, Node::number_literal((0, 5), 1f64)),
                            Node::parameter(None, Node::binary_expression(
                                Node::number_literal((0, 8), 2f64),
                                PunctuationKind::Asterisk,
                                Node::binary_expression(
                                    Node::number_literal((0, 11), 3f64),
                                    PunctuationKind::Plus,
                                    Node::number_literal((0, 13), 4f64),
                                ),
                            )),
                        ],
                    )
                ])
            );
        }

        #[test]
        fn function_call_in_parameter() {
            assert_eq!(
                parse("f(g(1), h(2))").program,
                Node::program(vec![
                    Node::call_expression(
                        Node::identifier((0, 0), "f"),
                        vec![
                            Node::parameter(None, Node::call_expression(
                                Node::identifier((0, 2), "g"),
                                vec![
                                    Node::parameter(None, Node::number_literal((0, 4), 1f64)),
                                ],
                            )),
                            Node::parameter(None, Node::call_expression(
                                Node::identifier((0, 8), "h"),
                                vec![
                                    Node::parameter(None, Node::number_literal((0, 10), 2f64)),
                                ],
                            )),
                        ],
                    )
                ])
            );
        }
    }

    mod member_expression {
        use crate::ast::node::Node;
        use crate::parser::parse;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn member_expression() {
            assert_eq!(
                parse("x.y.z").program,
                Node::program(vec![
                    Node::member_expression(
                        Node::member_expression(
                            Node::identifier((0, 0), "x"),
                            Node::identifier((0, 2), "y"),
                        ),
                        Node::identifier((0, 4), "z"),
                    ),
                ])
            );
        }

        #[test]
        fn member_expression_with_call_expression() {
            assert_eq!(
                parse("x.y()").program,
                Node::program(vec![
                    Node::call_expression(
                        Node::member_expression(
                            Node::identifier((0, 0), "x"),
                            Node::identifier((0, 2), "y"),
                        ),
                        vec![],
                    ),
                ])
            );
        }

        #[test]
        fn member_expression_with_unary_expression() {
            assert_eq!(
                parse("-x.y").program,
                Node::program(vec![
                    Node::unary_expression(
                        (0, 0),
                        PunctuationKind::Minus,
                        Node::member_expression(
                            Node::identifier((0, 1), "x"),
                            Node::identifier((0, 3), "y"),
                        ),
                    ),
                ])
            );
        }
    }

    mod primary_expression {
        use crate::ast::node::Node;
        use crate::error::Error;
        use crate::parser::parse;
        use crate::punctuation_kind::PunctuationKind;

        #[test]
        fn number() {
            assert_eq!(
                parse("1").program,
                Node::program(vec![
                    Node::number_literal((0, 0), 1f64),
                ])
            );
        }

        #[test]
        fn bool_true() {
            assert_eq!(
                parse("true").program,
                Node::program(vec![
                    Node::bool_literal((0, 0), true),
                ])
            );
        }

        #[test]
        fn bool_false() {
            assert_eq!(
                parse("false").program,
                Node::program(vec![
                    Node::bool_literal((0, 0), false),
                ])
            );
        }

        #[test]
        fn bool_in_expression() {
            assert_eq!(
                parse("-true+false").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::unary_expression(
                            (0, 0),
                            PunctuationKind::Minus,
                            Node::bool_literal((0, 1), true),
                        ),
                        PunctuationKind::Plus,
                        Node::bool_literal((0, 6), false),
                    ),
                ])
            );
        }

        #[test]
        fn string() {
            assert_eq!(
                parse("print(\"hello\")").program,
                Node::program(vec![
                    Node::call_expression(
                        Node::identifier((0, 0), "print"),
                        vec![
                            Node::parameter(None, Node::string_literal((0, 6), "hello")),
                        ],
                    )
                ])
            );
        }
        #[test]
        fn identifier() {
            assert_eq!(
                parse("x").program,
                Node::program(vec![
                    Node::identifier((0, 0), "x"),
                ])
            );
        }

        #[test]
        fn number_with_paren() {
            assert_eq!(
                parse("(1)").program,
                Node::program(vec![
                    Node::number_literal((0, 1), 1f64),
                ])
            );
        }

        #[test]
        fn multiply_additive_expressions() {
            assert_eq!(
                parse("(1+2)*(3+4)").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::binary_expression(
                            Node::number_literal((0, 1), 1f64),
                            PunctuationKind::Plus,
                            Node::number_literal((0, 3), 2f64),
                        ),
                        PunctuationKind::Asterisk,
                        Node::binary_expression(
                            Node::number_literal((0, 7), 3f64),
                            PunctuationKind::Plus,
                            Node::number_literal((0, 9), 4f64),
                        ),
                    )
                ])
            );
        }

        #[test]
        fn nested_parens() {
            assert_eq!(
                parse("((1+((2))))").program,
                Node::program(vec![
                    Node::binary_expression(
                        Node::number_literal((0, 2), 1f64),
                        PunctuationKind::Plus,
                        Node::number_literal((0, 6), 2f64),
                    ),
                ])
            );
        }

        #[test]
        fn unpaired_paren() {
            assert_eq!(
                parse("(1+2").errors,
                vec![
                    Error::unexpected_token((0, 4), ")")
                ]
            );
        }
    }

    mod object_literal {
        use crate::ast::node::Node;
        use crate::ast::type_expression::TypeExpression;
        use crate::parser::parse;

        #[test]
        fn empty_object() {
            assert_eq!(
                parse("struct Obj(); Obj()").program,
                Node::program(vec![
                    Node::struct_declaration_node(
                        Node::identifier((0, 7), "Obj"),
                        vec![],
                        vec![],
                        vec![],
                        (0, 0),
                    ),
                    Node::call_expression_node(
                        Node::identifier((0, 14), "Obj"),
                        vec![],
                    ),
                ])
            );
        }

        #[test]
        fn object_having_properties() {
            assert_eq!(
                parse("struct Obj(x: number, y: string); Obj(x=1, y=\"hello\")").program,
                Node::program(vec![
                    Node::struct_declaration_node(
                        Node::identifier((0, 7), "Obj"),
                        vec![
                            Node::property_declaration(
                                Node::identifier((0, 11), "x"),
                                TypeExpression::identifier("number"),
                            ),
                            Node::property_declaration(
                                Node::identifier((0, 22), "y"),
                                TypeExpression::identifier("string"),
                            ),
                        ],
                        vec![],
                        vec![],
                        (0, 0),
                    ),
                    Node::call_expression_node(
                        Node::identifier((0, 34), "Obj"),
                        vec![
                            Node::parameter(
                                Some(Node::identifier((0, 38), "x")),
                                Node::number_literal((0, 40), 1f64),
                            ),
                            Node::parameter(
                                Some(Node::identifier((0, 43), "y")),
                                Node::string_literal((0, 45), "hello"),
                            ),
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
                ").program,
                Node::program(vec![
                    Node::struct_declaration_node(
                        Node::identifier((1, 27), "Obj1"),
                        vec![
                            Node::property_declaration(
                                Node::identifier((1, 32), "x"),
                                TypeExpression::identifier("Obj2"),
                            ),
                            Node::property_declaration(
                                Node::identifier((1, 41), "z"),
                                TypeExpression::identifier("string"),
                            ),
                        ],
                        vec![],
                        vec![],
                        (1, 20),
                    ),
                    Node::struct_declaration_node(
                        Node::identifier((2, 27), "Obj2"),
                        vec![
                            Node::property_declaration(
                                Node::identifier((2, 32), "y"),
                                TypeExpression::identifier("number"),
                            ),
                        ],
                        vec![],
                        vec![],
                        (2, 20),
                    ),
                    Node::call_expression_node(
                        Node::identifier((3, 20), "Obj1"),
                        vec![
                            Node::parameter(
                                Some(Node::identifier((3, 25), "x")),
                                Node::call_expression(
                                    Node::identifier((3, 27), "Obj2"),
                                    vec![
                                        Node::parameter(
                                            Some(Node::identifier((3, 32), "y")),
                                            Node::number_literal((3, 34), 1f64),
                                        ),
                                    ],
                                ),
                            ),
                            Node::parameter(
                                Some(Node::identifier((3, 38), "z")),
                                Node::string_literal((3, 40), "hello"),
                            ),
                        ]
                    ),
                ])
            );
        }

        #[test]
        fn assign_object_into_variable() {
            assert_eq!(
                parse("struct Obj(y: number); x = Obj(y=1)").program,
                Node::program(vec![
                    Node::struct_declaration_node(
                        Node::identifier((0, 7), "Obj"),
                        vec![
                            Node::property_declaration(
                                Node::identifier((0, 11), "y"),
                                TypeExpression::identifier("number"),
                            ),
                        ],
                        vec![],
                        vec![],
                        (0, 0),
                    ),
                    Node::assignment_expression_node(
                        Node::identifier((0, 23), "x"),
                        Node::call_expression(
                            Node::identifier((0, 27), "Obj"),
                            vec![
                                Node::parameter(
                                    Some(Node::identifier((0, 31), "y")),
                                    Node::number_literal((0, 33), 1f64),
                                ),
                            ]
                        ),
                    ),
                ])
            );
        }
    }

    mod union_type {
        use crate::ast::node::Node;
        use crate::ast::type_expression::TypeExpression;
        use crate::parser::parse;

        #[test]
        fn union_type() {
            assert_eq!(
                parse("let x:T|U|V").program,
                Node::program(vec![
                    Node::variable_declaration(
                        (0, 0),
                        Node::identifier((0, 4), "x"),
                        Some(TypeExpression::union(vec![
                            TypeExpression::identifier("T"),
                            TypeExpression::identifier("U"),
                            TypeExpression::identifier("V"),
                        ])),
                        None::<Node>,
                    ),
                ])
            );
        }

        #[test]
        fn wrapped_type() {
            assert_eq!(
                parse("let x:(T|U)|V").program,
                Node::program(vec![
                    Node::variable_declaration(
                        (0, 0),
                        Node::identifier((0, 4), "x"),
                        Some(TypeExpression::union(vec![
                            TypeExpression::union(vec![
                                TypeExpression::identifier("T"),
                                TypeExpression::identifier("U"),
                            ]),
                            TypeExpression::identifier("V")
                        ])),
                        None::<Node>,
                    ),
                ])
            );
        }

        #[test]
        fn with_optional() {
            assert_eq!(
                parse("let x:T|U?").program,
                Node::program(vec![
                    Node::variable_declaration(
                        (0, 0),
                        Node::identifier((0, 4), "x"),
                        Some(TypeExpression::union(vec![
                            TypeExpression::identifier("T"),
                            TypeExpression::optional(
                                TypeExpression::identifier("U")
                            ),
                        ])),
                        None::<Node>,
                    ),
                ])
            );
        }
    }

    mod optional_type {
        use crate::ast::node::Node;
        use crate::ast::type_expression::TypeExpression;
        use crate::parser::parse;

        #[test]
        fn optional_type() {
            assert_eq!(
                parse("let x:T?").program,
                Node::program(vec![
                    Node::variable_declaration(
                        (0, 0),
                        Node::identifier((0, 4), "x"),
                        Some(TypeExpression::optional(
                            TypeExpression::identifier("T")
                        )),
                        None::<Node>,
                    ),
                ])
            );
        }

        #[test]
        fn optional_of_wrapped_type() {
            assert_eq!(
                parse("let x:(T)?").program,
                Node::program(vec![
                    Node::variable_declaration(
                        (0, 0),
                        Node::identifier((0, 4), "x"),
                        Some(TypeExpression::optional(
                            TypeExpression::identifier("T")
                        )),
                        None::<Node>,
                    ),
                ])
            );
        }
    }

    mod primary_type {
        use crate::ast::node::Node;
        use crate::ast::type_expression::TypeExpression;
        use crate::parser::parse;

        #[test]
        fn type_identifier() {
            assert_eq!(
                parse("let x:T").program,
                Node::program(vec![
                    Node::variable_declaration(
                        (0, 0),
                        Node::identifier((0, 4), "x"),
                        Some(TypeExpression::identifier("T")),
                        None::<Node>,
                    ),
                ])
            );
        }

        #[test]
        fn type_wrapped_by_paren() {
            assert_eq!(
                parse("let x:(T)").program,
                Node::program(vec![
                    Node::variable_declaration(
                        (0, 0),
                        Node::identifier((0, 4), "x"),
                        Some(TypeExpression::identifier("T")),
                        None::<Node>,
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