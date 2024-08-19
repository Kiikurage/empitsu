use crate::punctuation_kind::PunctuationKind;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub position: Position,
    pub text: String,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Number(f64),
    Bool(bool),
    String(String),
    Identifier(String),
    Punctuation(PunctuationKind),
    LineTerminator,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}
