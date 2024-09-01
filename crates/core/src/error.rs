use crate::analyzer::AnalyzedType;
use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub position: Position,
    pub message: String,
}

impl Error {
    #[inline(always)]
    pub fn invalid_syntax(position: impl Into<Position>, message: impl Into<String>) -> Error {
        Error { position: position.into(), message: message.into() }
    }

    #[inline(always)]
    pub fn unexpected_type(position: impl Into<Position>, expected: &AnalyzedType, actual: &AnalyzedType) -> Error {
        Error { position: position.into(), message: format!("Expected type is {:?}, but actual type is {:?}", expected, actual) }
    }

    #[inline(always)]
    pub fn unexpected_type_in_if_condition(position: impl Into<Position>, actual: &AnalyzedType) -> Error {
        Error { position: position.into(), message: format!("Condition must be a boolean, but found {:?}", actual) }
    }

    #[inline(always)]
    pub fn unexpected_token(position: impl Into<Position>, expected: impl Into<String>) -> Error {
        Error { position: position.into(), message: format!("Expected {}", expected.into()) }
    }

    #[inline(always)]
    pub fn unexpected_end_of_input(position: impl Into<Position>) -> Error {
        Error { position: position.into(), message: "Unexpected end of input".to_string() }
    }

    #[inline(always)]
    pub fn reserved_word(position: impl Into<Position>, word: impl Into<String>) -> Error {
        Error { position: position.into(), message: format!("\"{}\" is a reserved word", word.into()) }
    }

    #[inline(always)]
    pub fn runtime_error(position: impl Into<Position>, message: impl Into<String>) -> Error {
        Error { position: position.into(), message: message.into() }
    }

    #[inline(always)]
    pub fn not_implemented(position: impl Into<Position>, message: impl Into<String>) -> Error {
        Error { position: position.into(), message: message.into() }
    }

    #[inline(always)]
    pub fn unreachable_code(position: impl Into<Position>) -> Error {
        Error { position: position.into(), message: "Code is unreachable".to_string() }
    }

    #[inline(always)]
    pub fn undefined_symbol(position: impl Into<Position>, symbol: impl Into<String>) -> Error {
        Error { position: position.into(), message: format!("\"{}\" is not defined", symbol.into()) }
    }

    #[inline(always)]
    pub fn uninitialized_variable(position: impl Into<Position>, name: impl Into<String>) -> Error {
        Error { position: position.into(), message: format!("Variable \"{}\" is used before initialization", name.into()) }
    }
}