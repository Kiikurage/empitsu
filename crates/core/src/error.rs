use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub kind: ErrorKind,
    pub position: Position,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    SyntaxError { message: String },
    UnexpectedTokenError { expected: String },
    UnexpectedEndOfInput,
    ReservedWordError { word: String },
}

impl Error {
    #[inline(always)]
    pub fn syntax_error(position: Position, message: impl Into<String>) -> Error {
        Error { kind: ErrorKind::SyntaxError { message: message.into() }, position }
    }

    #[inline(always)]
    pub fn unexpected_token(position: Position, expected: impl Into<String>) -> Error {
        Error { kind: ErrorKind::UnexpectedTokenError { expected: expected.into() }, position }
    }

    #[inline(always)]
    pub fn unexpected_end_of_input(position: Position) -> Error {
        Error { kind: ErrorKind::UnexpectedEndOfInput, position }
    }

    #[inline(always)]
    pub fn reserved_word(position: Position, word: impl Into<String>) -> Error {
        Error { kind: ErrorKind::ReservedWordError { word: word.into() }, position }
    }

    pub fn get_message(&self) -> String {
        match self.kind {
            ErrorKind::SyntaxError { ref message } => format!("Syntax error: ({}) {}", self.position, message),
            ErrorKind::UnexpectedTokenError { ref expected } => format!("Syntax error: ({}) Expected {}", self.position, expected),
            ErrorKind::UnexpectedEndOfInput => format!("Syntax error: ({}) Unexpected end of input", self.position),
            ErrorKind::ReservedWordError { ref word } => format!("Syntax error: ({}) \"{}\" is a reserved word", self.position, word),
        }
    }
}