use crate::punctuator_kind::PunctuatorKind;
use crate::token::Token;

// TODO: 必要な機能が全部標準ライブラリで足りる気が絶対する
#[derive(Debug, Clone)]
pub struct TokenIter<'a> {
    tokens: &'a [Token],
    pub current: usize,
}

impl<'a> TokenIter<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn is_empty(&self) -> bool {
        self.peek().is_none()
    }

    pub fn peek(&self) -> Option<&Token> {
        let mut current = self.current;
        while let Some(Token::Punctuator(PunctuatorKind::NewLine)) = self.tokens.get(current) {
            current += 1;
        }
        self.tokens.get(current)
    }

    pub fn peek_including_newline(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    pub fn next(&mut self) {
        while let Some(Token::Punctuator(PunctuatorKind::NewLine)) = self.tokens.get(self.current) {
            self.current += 1;
        }
        if self.tokens.get(self.current).is_some() {
            self.current += 1;
        }
    }

    pub fn next_including_newline(&mut self) {
        if self.tokens.get(self.current).is_some() {
            self.current += 1;
        }
    }

    pub fn set_current(&mut self, current: usize) {
        self.current = current;
    }
}