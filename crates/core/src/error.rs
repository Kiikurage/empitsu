use std::ops::Range;
use crate::analyzer::AnalyzedType;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub range: Range<Position>,
    pub message: String,
}

impl Error {
    #[inline(always)]
    pub fn invalid_syntax(range: Range<Position>, message: impl Into<String>) -> Error {
        Error { range, message: message.into() }
    }

    #[inline(always)]
    pub fn unexpected_type(range: Range<Position>, expected: &AnalyzedType, actual: &AnalyzedType) -> Error {
        Error { range, message: format!("Expected type is {:?}, but actual type is {:?}", expected, actual) }
    }

    #[inline(always)]
    pub fn unexpected_type_in_if_condition(range: Range<Position>, actual: &AnalyzedType) -> Error {
        Error { range, message: format!("Condition must be a boolean, but found {:?}", actual) }
    }

    #[inline(always)]
    pub fn unexpected_token(range: Range<Position>, expected: impl Into<String>) -> Error {
        Error { range, message: format!("Expected {}", expected.into()) }
    }

    #[inline(always)]
    pub fn unexpected_end_of_input(range: Range<Position>) -> Error {
        Error { range, message: "Unexpected end of input".to_string() }
    }

    #[inline(always)]
    pub fn reserved_word(range: Range<Position>, word: impl Into<String>) -> Error {
        Error { range, message: format!("\"{}\" is a reserved word", word.into()) }
    }

    #[inline(always)]
    pub fn runtime_error(range: Range<Position>, message: impl Into<String>) -> Error {
        Error { range, message: message.into() }
    }

    #[inline(always)]
    pub fn not_implemented(range: Range<Position>, message: impl Into<String>) -> Error {
        Error { range, message: message.into() }
    }

    #[inline(always)]
    pub fn unreachable_code(range: Range<Position>) -> Error {
        Error { range, message: "Code is unreachable".to_string() }
    }

    #[inline(always)]
    pub fn undefined_symbol(range: Range<Position>, symbol: impl Into<String>) -> Error {
        Error { range, message: format!("\"{}\" is not defined", symbol.into()) }
    }

    #[inline(always)]
    pub fn uninitialized_variable(range: Range<Position>, name: impl Into<String>) -> Error {
        Error { range, message: format!("Variable \"{}\" is used before initialization", name.into()) }
    }
}