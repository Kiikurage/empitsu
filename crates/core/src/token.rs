use std::ops::Range;
use crate::ast::get_range::GetRange;
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
    #[inline(always)]
    pub fn number(range: Range<Position>, text: impl Into<String>, value: f64) -> Token {
        Token::Number(NumberToken { range, text: text.into(), value })
    }

    #[inline(always)]
    pub fn bool(range: Range<Position>, text: impl Into<String>, value: bool) -> Token {
        Token::Bool(BoolToken { range, text: text.into(), value })
    }

    #[inline(always)]
    pub fn string(range: Range<Position>, text: impl Into<String>, value: impl Into<String>) -> Token {
        Token::String(StringToken { range, text: text.into(), value: value.into() })
    }

    #[inline(always)]
    pub fn identifier(range: Range<Position>, text: impl Into<String>) -> Token {
        Token::Identifier(IdentifierToken { range, text: text.into() })
    }

    #[inline(always)]
    pub fn punctuation(range: Range<Position>, value: PunctuationKind) -> Token {
        Token::Punctuation(PunctuationToken { range, value })
    }

    #[inline(always)]
    pub fn line_terminator(range: Range<Position>) -> Token {
        Token::LineTerminator(LineTerminatorToken { range })
    }

    #[inline(always)]
    pub fn end_of_input(range: Range<Position>) -> Token {
        Token::EndOfInput(EndOfInputToken { range })
    }
}

impl GetRange for Token {
    fn range(&self) -> Range<Position> {
        match self {
            Token::Number(token) => token.range.clone(),
            Token::Bool(token) => token.range.clone(),
            Token::String(token) => token.range.clone(),
            Token::Identifier(token) => token.range.clone(),
            Token::Punctuation(token) => token.range.clone(),
            Token::LineTerminator(token) => token.range.clone(),
            Token::EndOfInput(token) => token.range.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct NumberToken {
    range: Range<Position>,
    pub text: String,
    pub value: f64,
}

impl GetRange for NumberToken {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BoolToken {
    range: Range<Position>,
    pub text: String,
    pub value: bool,
}

impl GetRange for BoolToken {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StringToken {
    range: Range<Position>,
    pub text: String,
    pub value: String,
}

impl GetRange for StringToken {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IdentifierToken {
    range: Range<Position>,
    pub text: String,
}

impl GetRange for IdentifierToken {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct PunctuationToken {
    range: Range<Position>,
    pub value: PunctuationKind,
}

impl GetRange for PunctuationToken {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LineTerminatorToken {
    range: Range<Position>,
}

impl GetRange for LineTerminatorToken {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct EndOfInputToken {
    range: Range<Position>,
}

impl GetRange for EndOfInputToken {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}
