use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    SyntaxError { position: Position, message: String },
    UnexpectedTokenError { position: Position, expected: String },
    UnexpectedEndOfInput { position: Position },
    ReservedWordError { position: Position, word: String },
    RuntimeError { position: Position, message: String },
    NotImplemented { position: Position, message: String },
}

impl Error {
    pub fn position(&self) -> &Position {
        match self {
            Error::SyntaxError { position, .. } => position,
            Error::UnexpectedTokenError { position, .. } => position,
            Error::UnexpectedEndOfInput { position } => position,
            Error::ReservedWordError { position, .. } => position,
            Error::RuntimeError { position, .. } => position,
            Error::NotImplemented { position, .. } => position,
        }
    }

    #[inline(always)]
    pub fn syntax_error(position: impl Into<Position>, message: impl Into<String>) -> Error {
        Error::SyntaxError { position: position.into(), message: message.into() }
    }

    #[inline(always)]
    pub fn unexpected_token(position: impl Into<Position>, expected: impl Into<String>) -> Error {
        Error::UnexpectedTokenError { position: position.into(), expected: expected.into() }
    }

    #[inline(always)]
    pub fn unexpected_end_of_input(position: impl Into<Position>) -> Error {
        Error::UnexpectedEndOfInput { position: position.into() }
    }

    #[inline(always)]
    pub fn reserved_word(position: impl Into<Position>, word: impl Into<String>) -> Error {
        Error::ReservedWordError { position: position.into(), word: word.into() }
    }

    #[inline(always)]
    pub fn runtime_error(position: impl Into<Position>, message: impl Into<String>) -> Error {
        Error::RuntimeError { position: position.into(), message: message.into() }
    }

    #[inline(always)]
    pub fn not_implemented(position: impl Into<Position>, message: impl Into<String>) -> Error {
        Error::NotImplemented { position: position.into(), message: message.into() }
    }

    pub fn message(&self) -> String {
        match self {
            Error::SyntaxError { position, message } => format!("Syntax error: ({}) {}", position, message),
            Error::UnexpectedTokenError { position, expected } => format!("Syntax error: ({}) Expected {}", position, expected),
            Error::UnexpectedEndOfInput { position } => format!("Syntax error: ({}) Unexpected end of input", position),
            Error::ReservedWordError { position, word } => format!("Syntax error: ({}) \"{}\" is a reserved word", position, word),
            Error::RuntimeError { position, message } => format!("Runtime error: ({}) {}", position, message),
            Error::NotImplemented { position, message } => format!("Not implemented: ({}) {}", position, message),
        }
    }
}