use crate::ast::block::Block;
use crate::ast::break_::Break;
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
use crate::ast::return_::Return;
use crate::ast::struct_declaration::StructDeclaration;
use crate::ast::traits::GetRange;
use crate::ast::type_expression::TypeExpression;
use crate::ast::variable_declaration::VariableDeclaration;
use crate::error::Error;
use crate::punctuation_kind::PunctuationKind;
use crate::range::Range;
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
        Ok(token) => Err(Error::unexpected_token(token.range().clone(), expected)),
        Err(err) => Err(err.clone()),
    }
}

fn assert_non_reserved_identifier(token: &Result<Token, Error>) -> Result<Identifier, Error> {
    match token {
        Ok(Token::Identifier(identifier)) => {
            if is_reserved(&identifier.text) {
                Err(Error::reserved_word(identifier.range(), identifier.text.clone()))
            } else {
                Ok(Node::identifier(identifier.range(), identifier.text.clone()))
            }
        }
        Ok(token) => Err(Error::unexpected_token(token.range().clone(), "identifier")),
        Err(err) => Err(err.clone()),
    }
}

macro_rules! assert_punctuation {
    ($token:expr, $expected:ident) => {
        match $token {
            Ok(Token::Punctuation(token @ PunctuationToken { value: PunctuationKind::$expected, .. })) => Ok(token),
            Ok(token) => Err(Error::unexpected_token(token.range().clone(), PunctuationKind::$expected.text())),
            Err(err) => Err(err.clone()),
        }
    };
    ($token:expr, $($expected:ident),+) => {
        match $token {
            Ok(Token::Punctuation(token @ PunctuationToken { value: $(PunctuationKind::$expected)|+, .. })) => Ok(token),
            Ok(token) => {
                let expected_tokens = [$(PunctuationKind::$expected.text()),+].join(", ");
                Err(Error::unexpected_token(token.range().clone(), expected_tokens))
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
        Err(Error::unexpected_token(tokens.peek().range(), expected_node))
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
        |tokens| parse_block(tokens).map(Into::into),
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

    let range = Range::new(
        if_keyword.start(),
        match false_branch {
            Some(ref false_branch) => false_branch.end(),
            None => true_branch.end(),
        },
    );

    Ok(Node::if_statement(range, condition, true_branch, false_branch))
}

fn parse_return_statement(tokens: &mut TokenIterator) -> Result<Return, Error> {
    let return_expression = parse_return_expression(tokens)?;
    parse_statement_end(tokens)?;

    Ok(return_expression)
}

fn parse_break_statement(tokens: &mut TokenIterator) -> Result<Break, Error> {
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

    Ok(Node::for_statement(
        Range::new(for_keyword.start(), body.end()),
        variable, iterable, body)
    )
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

    parse_statement_end(tokens)?;

    let range = Range::new(
        let_keyword.start(),
        match initializer {
            Some(ref initializer) => initializer.end(),
            None => match type_ {
                Some(ref type_) => type_.end(),
                None => name.end(),
            },
        },
    );
    Ok(Node::variable_declaration(range, name, type_, initializer))
}

fn parse_function_declaration(tokens: &mut TokenIterator) -> Result<Function, Error> {
    let interface = parse_function_interface(tokens)?;

    assert_punctuation!(tokens.next(), LeftBrace)?;
    let mut body = Vec::new();
    while assert_punctuation!(tokens.peek(), RightBrace).is_err() {
        body.push(parse_statement(tokens)?);
    }
    let right_brace = assert_punctuation!(tokens.next(), RightBrace)?;

    let range = Range::new(interface.start(), right_brace.end());
    Ok(Node::function(range, interface, body))
}

fn parse_function_interface(tokens: &mut TokenIterator) -> Result<FunctionInterface, Error> {
    let fn_keyword = assert_keyword(tokens.next(), "fn")?;
    let name = assert_non_reserved_identifier(tokens.next())?;

    assert_punctuation!(tokens.next(), LeftParen)?;
    let mut parameters = vec![];
    while assert_punctuation!(tokens.peek(), RightParen).is_err() {
        let name = assert_non_reserved_identifier(tokens.next())?;
        let parameter = if name.name == "self" {
            Node::parameter_declaration(name.clone(), Node::type_expression(name.range, "self"))
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

    let range = Range::new(fn_keyword.start(), return_type.end());
    Ok(Node::function_interface(range, Some(name), parameters, return_type))
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
    let right_paren = assert_punctuation!(tokens.next(), RightParen)?.clone();

    let mut instance_methods = vec![];
    let mut static_methods = vec![];
    let right_brace = if assert_punctuation!(tokens.peek(), LeftBrace).is_ok() {
        tokens.next();

        while assert_punctuation!(tokens.peek(), RightBrace).is_err() {
            let function = parse_function_declaration(tokens)?;
            if matches!(function.interface.parameters.first(), Some(ParameterDeclaration { name, .. }) if name.name == "self") {
                instance_methods.push(function);
            } else {
                static_methods.push(function);
            }
        }
        Some(assert_punctuation!(tokens.next(), RightBrace)?)
    } else {
        None
    };

    let range = Range::new(
        struct_keyword.start(),
        match right_brace {
            Some(ref right_brace) => right_brace.end(),
            None => right_paren.end(),
        },
    );

    Ok(Node::struct_declaration(range, name, properties, instance_methods, static_methods))
}

fn parse_interface_declaration(tokens: &mut TokenIterator) -> Result<InterfaceDeclaration, Error> {
    let interface_keyword = assert_keyword(tokens.next(), "interface")?;
    let name = assert_non_reserved_identifier(tokens.next())?;

    let mut instance_methods = vec![];
    assert_punctuation!(tokens.next(), LeftBrace)?;

    while assert_punctuation!(tokens.peek(), RightBrace).is_err() {
        instance_methods.push(parse_function_interface(tokens)?);
    }
    let right_brace = assert_punctuation!(tokens.next(), RightBrace)?;

    let range = Range::new(
        interface_keyword.start(),
        right_brace.end(),
    );
    Ok(Node::interface_declaration(range, name, instance_methods))
}

fn parse_impl_statement(tokens: &mut TokenIterator) -> Result<ImplStatement, Error> {
    let impl_keyword = assert_keyword(tokens.next(), "impl")?;
    let interface_name = assert_non_reserved_identifier(tokens.next())?;

    assert_keyword(tokens.next(), "for")?;
    let struct_name = assert_non_reserved_identifier(tokens.next())?;

    let mut instance_methods = vec![];
    assert_punctuation!(tokens.next(), LeftBrace)?;

    while assert_punctuation!(tokens.peek(), RightBrace).is_err() {
        let function = parse_function_declaration(tokens)?;
        instance_methods.push(function);
    }
    let right_brace = assert_punctuation!(tokens.next(), RightBrace)?;

    let range = Range::new(
        impl_keyword.start(),
        right_brace.end(),
    );
    Ok(Node::impl_statement(range, interface_name, struct_name, instance_methods))
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
        Ok(other) => Err(Error::unexpected_token(other.range().clone(), ";")),
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
        return Err(Error::unexpected_token(operand.range().clone(), "expression"));
    }

    Ok(Node::unary_expression(Range::new(operator.start(), operand.end()), operator.value.clone(), operand).into())
}

fn parse_statement_expression(tokens: &mut TokenIterator) -> Result<Node, Error> {
    parse_one_of(tokens, vec![
        |tokens| parse_if_expression(tokens).map(IfExpression::into),
        |tokens| parse_block(tokens).map(Block::into),
        |tokens| parse_return_expression(tokens).map(Return::into),
        |tokens| parse_break_expression(tokens).map(Break::into),
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
        Range::new(if_keyword.start(), false_branch.end()),
        condition,
        true_branch,
        false_branch,
    ))
}

fn parse_block(tokens: &mut TokenIterator) -> Result<Block, Error> {
    let left_brace = assert_punctuation!(tokens.next(), LeftBrace)?.clone();

    let mut statements = Vec::new();
    while assert_punctuation!(tokens.peek(), RightBrace).is_err() {
        statements.push(parse_statement(tokens)?);
    }

    let right_brace = assert_punctuation!(tokens.next(), RightBrace)?;

    Ok(Node::block(Range::new(left_brace.start(), right_brace.end()), statements))
}

fn parse_return_expression(tokens: &mut TokenIterator) -> Result<Return, Error> {
    let return_keyword = assert_keyword(tokens.next(), "return")?;

    let current = tokens.raw_offset;
    let expression = match parse_expression(tokens) {
        Ok(expression) => Some(expression),
        Err(..) => {
            tokens.raw_offset = current;
            None
        }
    };

    let range = Range::new(
        return_keyword.start(),
        match expression {
            Some(ref expression) => expression.end(),
            None => return_keyword.end(),
        },
    );

    Ok(Node::return_(range, expression))
}

fn parse_break_expression(tokens: &mut TokenIterator) -> Result<Break, Error> {
    let break_keyword = assert_keyword(tokens.next(), "break")?;

    Ok(Node::break_(break_keyword.range()))
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
    let right_brace = assert_punctuation!(tokens.next(), RightBrace)?;

    Ok(Node::function(
        Range::new(fn_keyword.start(), right_brace.end()),
        Node::function_interface(
            Range::new(fn_keyword.start(), return_type.end()),
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
        Ok(Token::Number(number)) => Ok(Node::number_literal(number.range(), number.value).into()),
        Ok(Token::Bool(bool)) => Ok(Node::bool_literal(bool.range(), bool.value).into()),
        Ok(Token::String(string)) => Ok(Node::string_literal(string.range(), string.value.clone()).into()),
        Ok(Token::Identifier(identifier)) => {
            if is_reserved(&identifier.text) {
                return Err(Error::reserved_word(identifier.range(), identifier.text.clone()));
            }

            Ok(Node::identifier(identifier.range(), identifier.text.clone()).into())
        }
        Ok(Token::Punctuation(PunctuationToken { value: PunctuationKind::LeftParen, .. })) => {
            let expression = parse_expression(tokens)?;
            assert_punctuation!(tokens.next(), RightParen)?;
            Ok(expression)
        }
        Ok(other) => Err(Error::unexpected_token(other.range().clone(), "expression")),
        Err(err) => Err(err.clone()),
    }
}

// Type

fn parse_type_expression(tokens: &mut TokenIterator) -> Result<TypeExpression, Error> {
    parse_primary_type(tokens)
}

fn parse_primary_type(tokens: &mut TokenIterator) -> Result<TypeExpression, Error> {
    match tokens.peek() {
        Ok(Token::Identifier(identifier)) => {
            match identifier.text.as_str() {
                "number" | "string" | "bool" | "null" => {
                    let range = identifier.range();
                    let name = identifier.text.clone();
                    tokens.next();
                    Ok(Node::type_expression(range, name))
                }
                _ => {
                    if is_reserved(&identifier.text) {
                        Err(Error::reserved_word(identifier.range(), identifier.text.clone()))
                    } else {
                        let range = identifier.range();
                        let name = identifier.text.clone();
                        tokens.next();
                        Ok(Node::type_expression(range, name))
                    }
                }
            }
        }
        Ok(token) => Err(Error::unexpected_token(token.range().clone(), "type")),
        Err(err) => Err(err.clone()),
    }
}

#[cfg(test)]
mod tests {
    mod if_statement {
        use crate::ast::node::Node;
        use crate::parser::parse;
        use crate::range::Range;

        #[test]
        fn if_statement() {
            assert_eq!(
                parse("if (1) 2; else 3").program,
                Node::program(vec![
                    Node::if_statement_node(
                        Range::of(0, 0, 0, 16),
                        Node::number_literal_node(Range::of(0, 4, 0, 5), 1f64),
                        Node::number_literal_node(Range::of(0, 7, 0, 8), 2f64),
                        Some(Node::number_literal_node(Range::of(0, 15, 0, 16), 3f64)),
                    ),
                ])
            );
        }

        #[test]
        fn if_statement_without_false_branch() {
            assert_eq!(
                parse("if (1) 2").program,
                Node::program(vec![
                    Node::if_statement_node(
                        Range::of(0, 0, 0, 8),
                        Node::number_literal_node(Range::of(0, 4, 0, 5), 1f64),
                        Node::number_literal_node(Range::of(0, 7, 0, 8), 2f64),
                        None,
                    ),
                ])
            );
        }

        #[test]
        fn chained_if() {
            assert_eq!(
                parse("if (1) 2; else if (3) 4; else 5").program,
                Node::program(vec![
                    Node::if_statement_node(
                        Range::of(0, 0, 0, 31),
                        Node::number_literal_node(Range::of(0, 4, 0, 5), 1f64),
                        Node::number_literal_node(Range::of(0, 7, 0, 8), 2f64),
                        Some(Node::if_statement_node(
                            Range::of(0, 15, 0, 31),
                            Node::number_literal_node(Range::of(0, 19, 0, 20), 3f64),
                            Node::number_literal_node(Range::of(0, 22, 0, 23), 4f64),
                            Some(Node::number_literal_node(Range::of(0, 30, 0, 31), 5f64)),
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
        use crate::range::Range;

        #[test]
        fn block() {
            assert_eq!(
                parse("{1; 2}").program,
                Node::program(vec![
                    Node::block_node(Range::of(0, 0, 0, 6), vec![
                        Node::number_literal_node(Range::of(0, 1, 0, 2), 1f64),
                        Node::number_literal_node(Range::of(0, 4, 0, 5), 2f64),
                    ]),
                ])
            );
        }

        #[test]
        fn nested_block() {
            assert_eq!(
                parse("{{1+2}}").program,
                Node::program(vec![
                    Node::block_node(Range::of(0, 0, 0, 7), vec![
                        Node::block_node(Range::of(0, 1, 0, 6), vec![
                            Node::binary_expression_node(
                                Node::number_literal_node(Range::of(0, 2, 0, 3), 1f64),
                                PunctuationKind::Plus,
                                Node::number_literal_node(Range::of(0, 4, 0, 5), 2f64),
                            ),
                        ]),
                    ]),
                ])
            );
        }
    }

    mod variable_declaration {
        use crate::ast::node::Node;
        use crate::parser::parse;
        use crate::range::Range;

        #[test]
        fn variable_declaration() {
            assert_eq!(
                parse("let x:number = 0").program,
                Node::program(vec![
                    Node::variable_declaration_node(
                        Range::of(0, 0, 0, 16),
                        Node::identifier(Range::of(0, 4, 0, 5), "x"),
                        Some(Node::type_expression(Range::of(0, 6, 0, 12), "number")),
                        Some(Node::number_literal_node(Range::of(0, 15, 0, 16), 0f64)),
                    )
                ])
            );
        }

        #[test]
        fn with_initial_value() {
            assert_eq!(
                parse("let x = 1").program,
                Node::program(vec![
                    Node::variable_declaration_node(
                        Range::of(0, 0, 0, 9),
                        Node::identifier(Range::of(0, 4, 0, 5), "x"),
                        None,
                        Some(Node::number_literal_node(Range::of(0, 8, 0, 9), 1.0f64)),
                    ),
                ])
            );
        }

        #[test]
        fn with_type_annotation() {
            assert_eq!(
                parse("let x: number = 1").program,
                Node::program(vec![
                    Node::variable_declaration_node(
                        Range::of(0, 0, 0, 17),
                        Node::identifier(Range::of(0, 4, 0, 5), "x"),
                        Some(Node::type_expression(Range::of(0, 7, 0, 13), "number")),
                        Some(Node::number_literal_node(Range::of(0, 16, 0, 17), 1.0f64)),
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
        use crate::range::Range;

        #[test]
        fn for_statement() {
            assert_eq!(
                parse("for (i in range) 1").program,
                Node::program(vec![
                    Node::for_statement_node(
                        Range::of(0, 0, 0, 18),
                        Node::identifier(Range::of(0, 5, 0, 6), "i"),
                        Node::identifier_node(Range::of(0, 10, 0, 15), "range"),
                        Node::number_literal_node(Range::of(0, 17, 0, 18), 1f64),
                    ),
                ])
            );
        }

        #[test]
        fn no_left_paren() {
            assert_eq!(
                parse("for i in range) 1").errors,
                vec![
                    Error::unexpected_token(Range::of(0, 4, 0, 5), "(")
                ]
            );
        }

        #[test]
        fn no_in() {
            assert_eq!(
                parse("for (i range) 1").errors,
                vec![
                    Error::unexpected_token(Range::of(0, 7, 0, 12), "in")
                ]
            );
        }

        #[test]
        fn no_iterator() {
            assert_eq!(
                parse("for (i in) 1").errors,
                vec![
                    Error::unexpected_token(Range::of(0, 9, 0, 10), "expression")
                ]
            );
        }

        #[test]
        fn no_right_paren() {
            assert_eq!(
                parse("for (i in range 1").errors,
                vec![
                    Error::unexpected_token(Range::of(0, 16, 0, 17), ")")
                ]
            );
        }

        #[test]
        fn no_body() {
            assert_eq!(
                parse("for (i in range)").errors,
                vec![
                    Error::unexpected_token(Range::of(0, 16, 0, 16), "statement")
                ]
            );
        }

        #[test]
        fn with_block() {
            assert_eq!(
                parse("for (i in range) { 1 }").program,
                Node::program(vec![
                    Node::for_statement_node(
                        Range::of(0, 0, 0, 22),
                        Node::identifier(Range::of(0, 5, 0, 6), "i"),
                        Node::identifier_node(Range::of(0, 10, 0, 15), "range"),
                        Node::block_node(Range::of(0, 17, 0, 22), vec![
                            Node::number_literal_node(Range::of(0, 19, 0, 20), 1f64),
                        ]),
                    ),
                ])
            );
        }
    }

    mod struct_declaration {
        use crate::ast::node::Node;
        use crate::error::Error;
        use crate::parser::parse;
        use crate::range::Range;

        #[test]
        fn struct_declaration() {
            assert_eq!(
                parse("struct User(name: string, id: number)").program,
                Node::program(vec![
                    Node::struct_declaration_node(
                        Range::of(0, 0, 0, 37),
                        Node::identifier(Range::of(0, 7, 0, 11), "User"),
                        vec![
                            Node::property_declaration(
                                Node::identifier(Range::of(0, 12, 0, 16), "name"),
                                Node::type_expression(Range::of(0, 18, 0, 24), "string")
                            ),
                            Node::property_declaration(
                                Node::identifier(Range::of(0, 26, 0, 28), "id"),
                                Node::type_expression(Range::of(0, 30, 0, 36), "number")
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
                    Node::struct_declaration_node(
                        Range::of(0, 0, 0, 13),
                        Node::identifier(Range::of(0, 7, 0, 11), "User"),
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
                    Error::unexpected_token(Range::of(0, 7, 0, 8), "identifier")
                ]
            );
        }
    }

    mod interface_declaration {
        use crate::ast::node::Node;
        use crate::parser::parse;
        use crate::range::Range;

        #[test]
        fn interface_declaration() {
            assert_eq!(
                parse("interface ToString { fn toString(self): string }").program,
                Node::program(vec![
                    Node::interface_declaration_node(
                        Range::of(0, 0, 0, 48),
                        Node::identifier(Range::of(0, 10, 0, 18), "ToString"),
                        vec![
                            Node::function_interface(
                                Range::of(0, 21, 0, 46),
                                Some(Node::identifier(Range::of(0, 24, 0, 32), "toString")),
                                vec![
                                    Node::parameter_declaration(
                                        Node::identifier(Range::of(0, 33, 0, 37), "self"),
                                        Node::type_expression(Range::of(0, 33, 0, 37), "self")
                                    ),
                                ],
                                Node::type_expression(Range::of(0, 40, 0, 46), "string"),
                            ),
                        ],
                    ),
                ])
            );
        }
    }

    mod impl_statement {
        use crate::ast::node::Node;
        use crate::parser::parse;
        use crate::range::Range;

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
                    Node::impl_statement_node(
                        Range::of(1, 16, 5, 17),
                        Node::identifier(Range::of(1, 21, 1, 29), "ToString"),
                        Node::identifier(Range::of(1, 34, 1, 38), "User"),
                        vec![
                            Node::function(
                                Range::of(2, 20, 4, 21),
                                Node::function_interface(
                                    Range::of(2, 20, 2, 45),
                                    Some(Node::identifier(Range::of(2, 23, 2, 31), "toString")),
                                    vec![
                                        Node::parameter_declaration(
                                            Node::identifier(Range::of(2, 32, 2, 36), "self"),
                                            Node::type_expression(Range::of(2, 32, 2, 36), "self"),
                                        ),
                                    ],
                                    Node::type_expression(Range::of(2, 39, 2, 45), "string"),
                                ),
                                vec![
                                    Node::return_node(
                                        Range::of(3, 24, 3, 40),
                                        Some(Node::member_expression_node(
                                            Node::identifier_node(Range::of(3, 31, 3, 35), "user"),
                                            Node::identifier(Range::of(3, 36, 3, 40), "name"),
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
        use crate::range::Range;

        #[test]
        fn if_expression() {
            assert_eq!(
                parse("1 + if (1) 2 else 3").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::number_literal_node(Range::of(0, 0, 0, 1), 1f64),
                        PunctuationKind::Plus,
                        Node::if_expression_node(
                            Range::of(0, 4, 0, 19),
                            Node::number_literal_node(Range::of(0, 8, 0, 9), 1f64),
                            Node::number_literal_node(Range::of(0, 11, 0, 12), 2f64),
                            Node::number_literal_node(Range::of(0, 18, 0, 19), 3f64),
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
                    Error::unexpected_token(Range::of(0, 12, 0, 12), "else")
                ]
            );
        }

        #[test]
        fn chained_if() {
            assert_eq!(
                parse("1 + if (2) 3 else if (4) 5 else 6").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::number_literal_node(Range::of(0, 0, 0, 1), 1f64),
                        PunctuationKind::Plus,
                        Node::if_expression_node(
                            Range::of(0, 4, 0, 33),
                            Node::number_literal_node(Range::of(0, 8, 0, 9), 2f64),
                            Node::number_literal_node(Range::of(0, 11, 0, 12), 3f64),
                            Node::if_expression_node(
                                Range::of(0, 18, 0, 33),
                                Node::number_literal_node(Range::of(0, 22, 0, 23), 4f64),
                                Node::number_literal_node(Range::of(0, 25, 0, 26), 5f64),
                                Node::number_literal_node(Range::of(0, 32, 0, 33), 6f64)
                            ),
                        ),
                    ),
                ])
            );
        }
    }

    mod function_expression {
        use crate::ast::node::Node;
        use crate::error::Error;
        use crate::parser::parse;
        use crate::punctuation_kind::PunctuationKind;
        use crate::range::Range;

        #[test]
        fn function_expression() {
            assert_eq!(
                parse("let x = fn(y: number): number { y * 2 }").program,
                Node::program(vec![
                    Node::variable_declaration_node(
                        Range::of(0, 0, 0, 39),
                        Node::identifier(Range::of(0, 4, 0, 5), "x"),
                        None,
                        Some(Node::function_expression_node(
                            Range::of(0, 8, 0, 39),
                            Node::function_interface(
                                Range::of(0, 8, 0, 29),
                                None,
                                vec![
                                    Node::parameter_declaration(
                                        Node::identifier(Range::of(0, 11, 0, 12), "y"),
                                        Node::type_expression(Range::of(0, 14, 0, 20), "number"),
                                    ),
                                ],
                                Node::type_expression(Range::of(0, 23, 0, 29), "number"),
                            ),
                            vec![
                                Node::binary_expression_node(
                                    Node::identifier_node(Range::of(0, 32, 0, 33), "y"),
                                    PunctuationKind::Asterisk,
                                    Node::number_literal_node(Range::of(0, 36, 0, 37), 2f64),
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
                    Error::unexpected_token(Range::of(0, 11, 0, 15), "(")
                ]
            );
        }
    }

    mod assignment_expression {
        use crate::ast::node::Node;
        use crate::parser::parse;
        use crate::range::Range;

        #[test]
        fn assign() {
            assert_eq!(
                parse("x = 1").program,
                Node::program(vec![
                    Node::assignment_expression_node(
                        Node::identifier_node(Range::of(0, 0, 0, 1), "x"),
                        Node::number_literal_node(Range::of(0, 4, 0, 5), 1f64),
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
        use crate::range::Range;

        #[test]
        fn logical_or() {
            assert_eq!(
                parse("true || false").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::bool_literal_node(Range::of(0, 0, 0, 4), true),
                        PunctuationKind::VerticalLineVerticalLine,
                        Node::bool_literal_node(Range::of(0, 8, 0, 13), false),
                    )
                ])
            );
        }

        #[test]
        fn prioritize_logical_and_over_logical_or() {
            assert_eq!(
                parse("true || false && true").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::bool_literal_node(Range::of(0, 0, 0, 4), true),
                        PunctuationKind::VerticalLineVerticalLine,
                        Node::binary_expression_node(
                            Node::bool_literal_node(Range::of(0, 8, 0, 13), false),
                            PunctuationKind::AndAnd,
                            Node::bool_literal_node(Range::of(0, 17, 0, 21), true),
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
        use crate::range::Range;

        #[test]
        fn logical_and() {
            assert_eq!(
                parse("true && false").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::bool_literal_node(Range::of(0, 0, 0, 4), true),
                        PunctuationKind::AndAnd,
                        Node::bool_literal_node(Range::of(0, 8, 0, 13), false),
                    )
                ])
            );
        }

        #[test]
        fn prioritize_additive_over_logical_and() {
            assert_eq!(
                parse("true && false + 1").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::bool_literal_node(Range::of(0, 0, 0, 4), true),
                        PunctuationKind::AndAnd,
                        Node::binary_expression_node(
                            Node::bool_literal_node(Range::of(0, 8, 0, 13), false),
                            PunctuationKind::Plus,
                            Node::number_literal_node(Range::of(0, 16, 0, 17), 1f64),
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
        use crate::range::Range;

        #[test]
        fn equal() {
            assert_eq!(
                parse("1==2").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::number_literal_node(Range::of(0, 0, 0, 1), 1f64),
                        PunctuationKind::EqualEqual,
                        Node::number_literal_node(Range::of(0, 3, 0, 4), 2f64),
                    )
                ])
            );
        }

        #[test]
        fn not_equal() {
            assert_eq!(
                parse("1!=2").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::number_literal_node(Range::of(0, 0, 0, 1), 1f64),
                        PunctuationKind::ExclamationEqual,
                        Node::number_literal_node(Range::of(0, 3, 0, 4), 2f64),
                    )
                ])
            );
        }

        #[test]
        fn not_equal_and_equal() {
            assert_eq!(
                parse("1!=2==3").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::binary_expression_node(
                            Node::number_literal_node(Range::of(0, 0, 0, 1), 1f64),
                            PunctuationKind::ExclamationEqual,
                            Node::number_literal_node(Range::of(0, 3, 0, 4), 2f64),
                        ),
                        PunctuationKind::EqualEqual,
                        Node::number_literal_node(Range::of(0, 6, 0, 7), 3f64),
                    )
                ])
            );
        }

        #[test]
        fn equality_and_relational_expressions() {
            assert_eq!(
                parse("1>2==3>4").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::binary_expression_node(
                            Node::number_literal_node(Range::of(0, 0, 0, 1), 1f64),
                            PunctuationKind::RightChevron,
                            Node::number_literal_node(Range::of(0, 2, 0, 3), 2f64),
                        ),
                        PunctuationKind::EqualEqual,
                        Node::binary_expression_node(
                            Node::number_literal_node(Range::of(0, 5, 0, 6), 3f64),
                            PunctuationKind::RightChevron,
                            Node::number_literal_node(Range::of(0, 7, 0, 8), 4f64),
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
        use crate::range::Range;

        #[test]
        fn greater_than() {
            assert_eq!(
                parse("1>2").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::number_literal_node(Range::of(0, 0, 0, 1), 1f64),
                        PunctuationKind::RightChevron,
                        Node::number_literal_node(Range::of(0, 2, 0, 3), 2f64),
                    )
                ])
            );
        }

        #[test]
        fn greater_than_or_equal() {
            assert_eq!(
                parse("1>=2").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::number_literal_node(Range::of(0, 0, 0, 1), 1f64),
                        PunctuationKind::RightChevronEqual,
                        Node::number_literal_node(Range::of(0, 3, 0, 4), 2f64),
                    )
                ])
            );
        }

        #[test]
        fn less_than() {
            assert_eq!(
                parse("1<2").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::number_literal_node(Range::of(0, 0, 0, 1), 1f64),
                        PunctuationKind::LeftChevron,
                        Node::number_literal_node(Range::of(0, 2, 0, 3), 2f64),
                    )
                ])
            );
        }

        #[test]
        fn less_than_or_equal() {
            assert_eq!(
                parse("1<=2").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::number_literal_node(Range::of(0, 0, 0, 1), 1f64),
                        PunctuationKind::LeftChevronEqual,
                        Node::number_literal_node(Range::of(0, 3, 0, 4), 2f64),
                    )
                ])
            );
        }

        #[test]
        fn greater_than_expressions() {
            assert_eq!(
                parse("1>2>3").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::binary_expression_node(
                            Node::number_literal_node(Range::of(0, 0, 0, 1), 1f64),
                            PunctuationKind::RightChevron,
                            Node::number_literal_node(Range::of(0, 2, 0, 3), 2f64),
                        ),
                        PunctuationKind::RightChevron,
                        Node::number_literal_node(Range::of(0, 4, 0, 5), 3f64),
                    )
                ])
            );
        }

        #[test]
        fn greater_than_and_additive_expression() {
            assert_eq!(
                parse("1+2>3+4").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::binary_expression_node(
                            Node::number_literal_node(Range::of(0, 0, 0, 1), 1f64),
                            PunctuationKind::Plus,
                            Node::number_literal_node(Range::of(0, 2, 0, 3), 2f64),
                        ),
                        PunctuationKind::RightChevron,
                        Node::binary_expression_node(
                            Node::number_literal_node(Range::of(0, 4, 0, 5), 3f64),
                            PunctuationKind::Plus,
                            Node::number_literal_node(Range::of(0, 6, 0, 7), 4f64),
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
        use crate::range::Range;

        #[test]
        fn add() {
            assert_eq!(
                parse("1+2").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::number_literal_node(Range::of(0, 0, 0, 1), 1f64),
                        PunctuationKind::Plus,
                        Node::number_literal_node(Range::of(0, 2, 0, 3), 2f64),
                    )
                ])
            );
        }

        #[test]
        fn subtract() {
            assert_eq!(
                parse("1-2").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::number_literal_node(Range::of(0, 0, 0, 1), 1f64),
                        PunctuationKind::Minus,
                        Node::number_literal_node(Range::of(0, 2, 0, 3), 2f64),
                    )
                ])
            );
        }

        #[test]
        fn add_and_subtract() {
            assert_eq!(
                parse("1+2-3").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::binary_expression_node(
                            Node::number_literal_node(Range::of(0, 0, 0, 1), 1f64),
                            PunctuationKind::Plus,
                            Node::number_literal_node(Range::of(0, 2, 0, 3), 2f64),
                        ),
                        PunctuationKind::Minus,
                        Node::number_literal_node(Range::of(0, 4, 0, 5), 3f64),
                    )
                ])
            );
        }

        #[test]
        fn newline_at_middle() {
            assert_eq!(
                parse("1\n+2").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::number_literal_node(Range::of(0, 0, 0, 1), 1f64),
                        PunctuationKind::Plus,
                        Node::number_literal_node(Range::of(1, 1, 1, 2), 2f64),
                    )
                ])
            );
        }
    }

    mod multiplicative_expression {
        use crate::ast::node::Node;
        use crate::parser::parse;
        use crate::punctuation_kind::PunctuationKind;
        use crate::range::Range;

        #[test]
        fn multiply() {
            assert_eq!(
                parse("1*2").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::number_literal_node(Range::of(0, 0, 0, 1), 1f64),
                        PunctuationKind::Asterisk,
                        Node::number_literal_node(Range::of(0, 2, 0, 3), 2f64),
                    )
                ])
            );
        }

        #[test]
        fn divide() {
            assert_eq!(
                parse("1/2").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::number_literal_node(Range::of(0, 0, 0, 1), 1f64),
                        PunctuationKind::Slash,
                        Node::number_literal_node(Range::of(0, 2, 0, 3), 2f64),
                    )
                ])
            );
        }

        #[test]
        fn multiply_and_divide() {
            assert_eq!(
                parse("1*2/3").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::binary_expression_node(
                            Node::number_literal_node(Range::of(0, 0, 0, 1), 1f64),
                            PunctuationKind::Asterisk,
                            Node::number_literal_node(Range::of(0, 2, 0, 3), 2f64),
                        ),
                        PunctuationKind::Slash,
                        Node::number_literal_node(Range::of(0, 4, 0, 5), 3f64),
                    )
                ])
            );
        }

        #[test]
        fn multiply_after_add() {
            assert_eq!(
                parse("1+2*3").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::number_literal_node(Range::of(0, 0, 0, 1), 1f64),
                        PunctuationKind::Plus,
                        Node::binary_expression_node(
                            Node::number_literal_node(Range::of(0, 2, 0, 3), 2f64),
                            PunctuationKind::Asterisk,
                            Node::number_literal_node(Range::of(0, 4, 0, 5), 3f64),
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
                    Node::binary_expression_node(
                        Node::binary_expression_node(
                            Node::number_literal_node(Range::of(0, 0, 0, 1), 1f64),
                            PunctuationKind::Asterisk,
                            Node::number_literal_node(Range::of(0, 2, 0, 3), 2f64),
                        ),
                        PunctuationKind::Plus,
                        Node::number_literal_node(Range::of(0, 4, 0, 5), 3f64),
                    )
                ])
            );
        }

        #[test]
        fn add_multiplicative_expressions() {
            assert_eq!(
                parse("1*2+3/4").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::binary_expression_node(
                            Node::number_literal_node(Range::of(0, 0, 0, 1), 1f64),
                            PunctuationKind::Asterisk,
                            Node::number_literal_node(Range::of(0, 2, 0, 3), 2f64),
                        ),
                        PunctuationKind::Plus,
                        Node::binary_expression_node(
                            Node::number_literal_node(Range::of(0, 4, 0, 5), 3f64),
                            PunctuationKind::Slash,
                            Node::number_literal_node(Range::of(0, 6, 0, 7), 4f64),
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
        use crate::range::Range;

        #[test]
        fn plus() {
            assert_eq!(
                parse("+1").program,
                Node::program(vec![
                    Node::unary_expression_node(
                        Range::of(0, 0, 0, 2),
                        PunctuationKind::Plus,
                        Node::number_literal_node(Range::of(0, 1, 0, 2), 1f64),
                    )
                ])
            );
        }

        #[test]
        fn minus() {
            assert_eq!(
                parse("-1").program,
                Node::program(vec![
                    Node::unary_expression_node(
                        Range::of(0, 0, 0, 2),
                        PunctuationKind::Minus,
                        Node::number_literal_node(Range::of(0, 1, 0, 2), 1f64),
                    )
                ])
            );
        }

        #[test]
        fn disallow_multiple_unary_operator() {
            assert_eq!(
                parse("--1").errors,
                vec![
                    Error::unexpected_token(Range::of(0, 1, 0, 3), "expression")
                ]
            );
        }

        #[test]
        fn unary_operator_in_additive_expression() {
            assert_eq!(
                parse("1++1").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::number_literal_node(Range::of(0, 0, 0, 1), 1f64),
                        PunctuationKind::Plus,
                        Node::unary_expression_node(
                            Range::of(0, 2, 0, 4),
                            PunctuationKind::Plus,
                            Node::number_literal_node(Range::of(0, 3, 0, 4), 1f64),
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
        use crate::range::Range;

        #[test]
        fn without_parameters() {
            assert_eq!(
                parse("func()").program,
                Node::program(vec![
                    Node::call_expression_node(
                        Node::identifier_node(Range::of(0, 0, 0, 4), "func"),
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
                    Node::call_expression_node(
                        Node::identifier_node(Range::of(0, 0, 0, 4), "func"),
                        vec![
                            Node::parameter(None, Node::number_literal_node(Range::of(0, 5, 0, 6), 1f64)),
                            Node::parameter(None, Node::number_literal_node(Range::of(0, 8, 0, 9), 2f64)),
                            Node::parameter(None, Node::number_literal_node(Range::of(0, 11, 0, 12), 3f64)),
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
                    Node::call_expression_node(
                        Node::identifier_node(Range::of(0, 0, 0, 4), "func"),
                        vec![
                            Node::parameter(None, Node::number_literal_node(Range::of(0, 5, 0, 6), 1f64)),
                            Node::parameter(None, Node::binary_expression_node(
                                Node::number_literal_node(Range::of(0, 8, 0, 9), 2f64),
                                PunctuationKind::Asterisk,
                                Node::binary_expression_node(
                                    Node::number_literal_node(Range::of(0, 11, 0, 12), 3f64),
                                    PunctuationKind::Plus,
                                    Node::number_literal_node(Range::of(0, 13, 0, 14), 4f64),
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
                    Node::call_expression_node(
                        Node::identifier_node(Range::of(0, 0, 0, 1), "f"),
                        vec![
                            Node::parameter(None, Node::call_expression_node(
                                Node::identifier_node(Range::of(0, 2, 0, 3), "g"),
                                vec![
                                    Node::parameter(None, Node::number_literal_node(Range::of(0, 4, 0, 5), 1f64)),
                                ],
                            )),
                            Node::parameter(None, Node::call_expression_node(
                                Node::identifier_node(Range::of(0, 8, 0, 9), "h"),
                                vec![
                                    Node::parameter(None, Node::number_literal_node(Range::of(0, 10, 0, 11), 2f64)),
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
        use crate::range::Range;

        #[test]
        fn member_expression() {
            assert_eq!(
                parse("x.y.z").program,
                Node::program(vec![
                    Node::member_expression_node(
                        Node::member_expression_node(
                            Node::identifier_node(Range::of(0, 0, 0, 1), "x"),
                            Node::identifier(Range::of(0, 2, 0, 3), "y"),
                        ),
                        Node::identifier(Range::of(0, 4, 0, 5), "z"),
                    ),
                ])
            );
        }

        #[test]
        fn member_expression_with_call_expression() {
            assert_eq!(
                parse("x.y()").program,
                Node::program(vec![
                    Node::call_expression_node(
                        Node::member_expression_node(
                            Node::identifier_node(Range::of(0, 0, 0, 1), "x"),
                            Node::identifier(Range::of(0, 2, 0, 3), "y"),
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
                    Node::unary_expression_node(
                        Range::of(0, 0, 0, 4),
                        PunctuationKind::Minus,
                        Node::member_expression_node(
                            Node::identifier_node(Range::of(0, 1, 0, 2), "x"),
                            Node::identifier(Range::of(0, 3, 0, 4), "y"),
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
        use crate::range::Range;

        #[test]
        fn number() {
            assert_eq!(
                parse("1").program,
                Node::program(vec![
                    Node::number_literal_node(Range::of(0, 0, 0, 1), 1f64),
                ])
            );
        }

        #[test]
        fn bool_true() {
            assert_eq!(
                parse("true").program,
                Node::program(vec![
                    Node::bool_literal_node(Range::of(0, 0, 0, 4), true),
                ])
            );
        }

        #[test]
        fn bool_false() {
            assert_eq!(
                parse("false").program,
                Node::program(vec![
                    Node::bool_literal_node(Range::of(0, 0, 0, 5), false),
                ])
            );
        }

        #[test]
        fn bool_in_expression() {
            assert_eq!(
                parse("-true+false").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::unary_expression_node(
                            Range::of(0, 0, 0, 5),
                            PunctuationKind::Minus,
                            Node::bool_literal_node(Range::of(0, 1, 0, 5), true),
                        ),
                        PunctuationKind::Plus,
                        Node::bool_literal_node(Range::of(0, 6, 0, 11), false),
                    ),
                ])
            );
        }

        #[test]
        fn string() {
            assert_eq!(
                parse("print(\"hello\")").program,
                Node::program(vec![
                    Node::call_expression_node(
                        Node::identifier_node(Range::of(0, 0, 0, 5), "print"),
                        vec![
                            Node::parameter(None, Node::string_literal_node(Range::of(0, 6, 0, 13), "hello")),
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
                    Node::identifier_node(Range::of(0, 0, 0, 1), "x"),
                ])
            );
        }

        #[test]
        fn number_with_paren() {
            assert_eq!(
                parse("(1)").program,
                Node::program(vec![
                    Node::number_literal_node(Range::of(0, 1, 0, 2), 1f64),
                ])
            );
        }

        #[test]
        fn multiply_additive_expressions() {
            assert_eq!(
                parse("(1+2)*(3+4)").program,
                Node::program(vec![
                    Node::binary_expression_node(
                        Node::binary_expression_node(
                            Node::number_literal_node(Range::of(0, 1, 0, 2), 1f64),
                            PunctuationKind::Plus,
                            Node::number_literal_node(Range::of(0, 3, 0, 4), 2f64),
                        ),
                        PunctuationKind::Asterisk,
                        Node::binary_expression_node(
                            Node::number_literal_node(Range::of(0, 7, 0, 8), 3f64),
                            PunctuationKind::Plus,
                            Node::number_literal_node(Range::of(0, 9, 0, 10), 4f64),
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
                    Node::binary_expression_node(
                        Node::number_literal_node(Range::of(0, 2, 0, 3), 1f64),
                        PunctuationKind::Plus,
                        Node::number_literal_node(Range::of(0, 6, 0, 7), 2f64),
                    ),
                ])
            );
        }

        #[test]
        fn unpaired_paren() {
            assert_eq!(
                parse("(1+2").errors,
                vec![
                    Error::unexpected_token(Range::of(0, 4, 0, 4), ")")
                ]
            );
        }
    }

    mod object_literal {
        use crate::ast::node::Node;
        use crate::parser::parse;
        use crate::range::Range;

        #[test]
        fn empty_object() {
            assert_eq!(
                parse("struct Obj(); Obj()").program,
                Node::program(vec![
                    Node::struct_declaration_node(
                        Range::of(0, 0, 0, 12),
                        Node::identifier(Range::of(0, 7, 0, 10), "Obj"),
                        vec![],
                        vec![],
                        vec![],
                    ),
                    Node::call_expression_node(
                        Node::identifier_node(Range::of(0, 14, 0, 17), "Obj"),
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
                        Range::of(0, 0, 0, 32),
                        Node::identifier(Range::of(0, 7, 0, 10), "Obj"),
                        vec![
                            Node::property_declaration(
                                Node::identifier(Range::of(0, 11, 0, 12), "x"),
                                Node::type_expression(Range::of(0, 14, 0, 20), "number"),
                            ),
                            Node::property_declaration(
                                Node::identifier(Range::of(0, 22, 0, 23), "y"),
                                Node::type_expression(Range::of(0, 25, 0, 31), "string"),
                            ),
                        ],
                        vec![],
                        vec![],
                    ),
                    Node::call_expression_node(
                        Node::identifier_node(Range::of(0, 34, 00, 37), "Obj"),
                        vec![
                            Node::parameter(
                                Some(Node::identifier(Range::of(0, 38, 0, 39), "x")),
                                Node::number_literal_node(Range::of(0, 40, 0, 41), 1f64),
                            ),
                            Node::parameter(
                                Some(Node::identifier(Range::of(0, 43, 0, 44), "y")),
                                Node::string_literal_node(Range::of(0, 45, 0, 52), "hello"),
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
                        Range::of(1, 20, 1, 51),
                        Node::identifier(Range::of(1, 27, 1, 31), "Obj1"),
                        vec![
                            Node::property_declaration(
                                Node::identifier(Range::of(1, 32, 1, 33), "x"),
                                Node::type_expression(Range::of(1, 35, 1, 39), "Obj2"),
                            ),
                            Node::property_declaration(
                                Node::identifier(Range::of(1, 41, 1, 42), "z"),
                                Node::type_expression(Range::of(1, 44, 1, 50), "string"),
                            ),
                        ],
                        vec![],
                        vec![],
                    ),
                    Node::struct_declaration_node(
                        Range::of(2, 20, 2, 42),
                        Node::identifier(Range::of(2, 27, 2, 31), "Obj2"),
                        vec![
                            Node::property_declaration(
                                Node::identifier(Range::of(2, 32, 2, 33), "y"),
                                Node::type_expression(Range::of(2, 35, 2, 41), "number"),
                            ),
                        ],
                        vec![],
                        vec![],
                    ),
                    Node::call_expression_node(
                        Node::identifier_node(Range::of(3, 20, 3, 24), "Obj1"),
                        vec![
                            Node::parameter(
                                Some(Node::identifier(Range::of(3, 25, 3, 26), "x")),
                                Node::call_expression_node(
                                    Node::identifier_node(Range::of(3, 27, 3, 31), "Obj2"),
                                    vec![
                                        Node::parameter(
                                            Some(Node::identifier(Range::of(3, 32, 3, 33), "y")),
                                            Node::number_literal_node(Range::of(3, 34, 3, 35), 1f64),
                                        ),
                                    ],
                                ),
                            ),
                            Node::parameter(
                                Some(Node::identifier(Range::of(3, 38, 3, 39), "z")),
                                Node::string_literal_node(Range::of(3, 40, 3, 47), "hello"),
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
                        Range::of(0, 0, 0, 21 ),
                        Node::identifier(Range::of(0, 7, 0, 10), "Obj"),
                        vec![
                            Node::property_declaration(
                                Node::identifier(Range::of(0, 11, 0, 12), "y"),
                                Node::type_expression(Range::of(0, 14, 0, 20), "number"),
                            ),
                        ],
                        vec![],
                        vec![],
                    ),
                    Node::assignment_expression_node(
                        Node::identifier_node(Range::of(0, 23, 0, 24), "x"),
                        Node::call_expression_node(
                            Node::identifier_node(Range::of(0, 27, 0, 30), "Obj"),
                            vec![
                                Node::parameter(
                                    Some(Node::identifier(Range::of(0, 31, 0, 32), "y")),
                                    Node::number_literal_node(Range::of(0, 33, 0, 34), 1f64),
                                ),
                            ]
                        ),
                    ),
                ])
            );
        }
    }

    mod primary_type {
        use crate::ast::node::Node;
        use crate::parser::parse;
        use crate::range::Range;

        #[test]
        fn type_identifier() {
            assert_eq!(
                parse("let x:T").program,
                Node::program(vec![
                    Node::variable_declaration_node(
                        Range::of(0, 0, 0, 7),
                        Node::identifier(Range::of(0, 4, 0, 5), "x"),
                        Some(Node::type_expression(Range::of(0, 6, 0, 7), "T")),
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