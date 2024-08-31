use crate::position::Position;
use crate::punctuation_kind::PunctuationKind;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Number(NumberToken),
    Bool(BoolToken),
    String(StringToken),
    Identifier(IdentifierToken),
    Punctuation(PunctuationToken),
    LineTerminator(LineTerminatorToken),
    EndOfInput(EndOfInputToken),
}

impl Token {
    pub fn position(&self) -> &Position {
        match self {
            Token::Number(token) => &token.position,
            Token::Bool(token) => &token.position,
            Token::String(token) => &token.position,
            Token::Identifier(token) => &token.position,
            Token::Punctuation(token) => &token.position,
            Token::LineTerminator(token) => &token.position,
            Token::EndOfInput(token) => &token.position,
        }
    }

    #[inline(always)]
    pub fn number(position: impl Into<Position>, text: impl Into<String>, value: f64) -> Token {
        Token::Number(NumberToken { position: position.into(), text: text.into(), value })
    }

    #[inline(always)]
    pub fn bool(position: impl Into<Position>, text: impl Into<String>, value: bool) -> Token {
        Token::Bool(BoolToken { position: position.into(), text: text.into(), value })
    }

    #[inline(always)]
    pub fn string(position: impl Into<Position>, text: impl Into<String>, value: impl Into<String>) -> Token {
        Token::String(StringToken { position: position.into(), text: text.into(), value: value.into() })
    }

    #[inline(always)]
    pub fn identifier(position: impl Into<Position>, text: impl Into<String>) -> Token {
        Token::Identifier(IdentifierToken { position: position.into(), text: text.into() })
    }

    #[inline(always)]
    pub fn punctuation(position: impl Into<Position>, value: PunctuationKind) -> Token {
        Token::Punctuation(PunctuationToken { position: position.into(), value })
    }

    #[inline(always)]
    pub fn line_terminator(position: impl Into<Position>) -> Token {
        Token::LineTerminator(LineTerminatorToken { position: position.into() })
    }

    #[inline(always)]
    pub fn end_of_input(position: impl Into<Position>) -> Token {
        Token::EndOfInput(EndOfInputToken { position: position.into() })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct NumberToken {
    pub position: Position,
    pub text: String,
    pub value: f64,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BoolToken {
    pub position: Position,
    pub text: String,
    pub value: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StringToken {
    pub position: Position,
    pub text: String,
    pub value: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IdentifierToken {
    pub position: Position,
    pub text: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct PunctuationToken {
    pub position: Position,
    pub value: PunctuationKind,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LineTerminatorToken {
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct EndOfInputToken {
    pub position: Position,
}