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
        self.current >= self.tokens.len()
    }

    pub fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    pub fn next(&mut self) -> Option<&Token> {
        if self.current < self.tokens.len() {
            let token = &self.tokens[self.current];
            self.current += 1;
            Some(token)
        } else {
            None
        }
    }

    pub fn set_current(&mut self, current: usize) {
        self.current = current;
    }
}