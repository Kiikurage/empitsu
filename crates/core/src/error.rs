use crate::token::Position;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    SyntaxError { position: Position, message: String },
    UnexpectedTokenError { position: Position, expected: String },
    ReservedWordError { position: Position, word: String },
}

impl Error {
    #[inline(always)]
    pub fn syntax_error(position: Position, message: impl Into<String>) -> Error {
        Error::SyntaxError { position, message: message.into() }
    }

    #[inline(always)]
    pub fn unexpected_token(position: Position, expected: impl Into<String>) -> Error {
        Error::UnexpectedTokenError { position, expected: expected.into() }
    }

    #[inline(always)]
    pub fn reserved_word(position: Position, word: impl Into<String>) -> Error {
        Error::ReservedWordError { position, word: word.into() }
    }
    
    pub fn get_position(&self) -> &Position {
        match self {
            Error::SyntaxError { ref position, .. } => position,
            Error::UnexpectedTokenError { ref position, .. } => position,
            Error::ReservedWordError { ref position, .. } => position,
        }
    }

    pub fn get_message(&self) -> String {
        match self {
            Error::SyntaxError { ref position, ref message } => format!("Syntax error: ({}, {}) {}", position.line + 1, position.column + 1, message),
            Error::UnexpectedTokenError { ref position, ref expected } => format!("Syntax error: ({}, {}) Expected {}", position.line + 1, position.column + 1, expected),
            Error::ReservedWordError { ref position, ref word } => format!("Syntax error: ({}, {}) \"{}\" is a reserved word", position.line + 1, position.column + 1, word),
        }
    }
}