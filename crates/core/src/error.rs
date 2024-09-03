use crate::analysis::type_::Type;
use crate::ast::get_range::GetRange;
use crate::position::Position;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    range: Range<Position>,
    pub message: String,
}

impl Error {
    #[inline(always)]
    pub fn invalid_syntax(range: Range<Position>, message: impl Into<String>) -> Error {
        Error { range, message: message.into() }
    }

    #[inline(always)]
    pub fn unexpected_type(range: Range<Position>, expected: &Type, actual: &Type) -> Error {
        Error { range, message: format!("Expected type is {:?}, but actual type is {:?}", expected, actual) }
    }

    #[inline(always)]
    pub fn unexpected_type_in_if_condition(range: Range<Position>, actual: &Type) -> Error {
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

    #[inline(always)]
    pub fn conditionally_initialized_variable(range: Range<Position>, name: impl Into<String>) -> Error {
        Error { range, message: format!("Variable \"{}\" is not initialized in some case", name.into()) }
    }

    #[inline(always)]
    pub fn conditionally_initialized_as_different_type(range: Range<Position>, name: impl Into<String>) -> Error {
        Error { range, message: format!("Variable \"{}\" is conditionally initialized as different types", name.into()) }
    }
}

impl GetRange for Error {
    fn range(&self) -> Range<Position> {
        self.range.clone()
    }
}