use crate::position::Position;
use crate::punctuation_kind::PunctuationKind;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub position: Position,
    pub text: String,
}

impl Token {
    pub fn number(line: usize, column: usize, value: f64, text: impl Into<String>) -> Token {
        Token {
            kind: TokenKind::Number(value),
            position: Position::new(line, column),
            text: text.into(),
        }
    }

    pub fn bool(line: usize, column: usize, value: bool, text: impl Into<String>) -> Token {
        Token {
            kind: TokenKind::Bool(value),
            position: Position::new(line, column),
            text: text.into(),
        }
    }

    pub fn string(line: usize, column: usize, value: impl Into<String>, text: impl Into<String>) -> Token {
        Token {
            kind: TokenKind::String(value.into()),
            position: Position::new(line, column),
            text: text.into(),
        }
    }

    pub fn identifier(line: usize, column: usize, value: impl Into<String>) -> Token {
        let value = value.into();
        Token {
            kind: TokenKind::Identifier(value.clone()),
            position: Position::new(line, column),
            text: value,
        }
    }

    pub fn punctuation(line: usize, column: usize, value: PunctuationKind, text: impl Into<String>) -> Token {
        Token {
            kind: TokenKind::Punctuation(value),
            position: Position::new(line, column),
            text: text.into(),
        }
    }

    pub fn line_terminator(line: usize, column: usize) -> Token {
        Token {
            kind: TokenKind::LineTerminator,
            position: Position::new(line, column),
            text: "\n".to_string(),
        }
    }
    
    pub fn end_of_input(line: usize, column: usize) -> Token {
        Token {
            kind: TokenKind::EndOfInput,
            position: Position::new(line, column),
            text: "".to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Number(f64),
    Bool(bool),
    String(String),
    Identifier(String),
    Punctuation(PunctuationKind),
    LineTerminator,
    EndOfInput,
}
