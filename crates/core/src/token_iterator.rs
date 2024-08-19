use crate::error::Error;
use crate::token::{Position, Token, TokenKind};

#[derive(Debug)]
pub struct TokenIterator<'a> {
    tokens: &'a [Result<Token, Error>],
    current: usize,
}

impl<'a> TokenIterator<'a> {
    pub fn new(tokens: &'a [Result<Token, Error>]) -> Self {
        Self { tokens, current: 0, }
    }

    pub fn peek(&mut self) -> Result<Option<TokenKind>, Error> {
        self.peek_at(0)
    }

    fn peek_at(&mut self, offset: usize) -> Result<Option<TokenKind>, Error> {
        let mut tokens_current = self.current;
        let mut char_offset = 0;

        loop {
            match self.tokens.get(tokens_current) {
                Some(Ok(Token { kind: TokenKind::LineTerminator, .. })) => {
                    tokens_current += 1;
                }
                Some(token) => {
                    if char_offset == offset {
                        return match token {
                            Ok(Token { kind, .. }) => Ok(Some(kind.clone())),
                            Err(error) => Err(error.clone()),
                        };
                    }
                    char_offset += 1;
                    tokens_current += 1;
                }
                None => return Ok(None)
            }
        }
    }

    pub fn peek_including_newline(&mut self) -> Result<Option<TokenKind>, Error> {
        match self.tokens.get(self.current) {
            Some(Ok(token)) => Ok(Some(token.kind.clone())),
            Some(Err(error)) => Err(error.clone()),
            None => Ok(None)
        }
    }

    pub fn next(&mut self) {
        while let Some(Ok(Token { kind: TokenKind::LineTerminator, .. })) = self.tokens.get(self.current) {
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

    pub fn get_position(&self) -> Position {
        match self.tokens.get(self.current) {
            Some(Ok(Token { position, .. })) => position.clone(),
            Some(Err(error)) => error.get_position().clone(),
            None => {
                let last_token = self.tokens.last();
                match last_token {
                    Some(Ok(Token { position, text, .. })) => Position::new(position.line, position.column + text.len()),
                    Some(Err(error)) => error.get_position().clone(),
                    _ => Position::new(0, 0),
                }
            }
        }
    }

    pub fn get_current(&self) -> usize {
        self.current
    }

    pub fn set_current(&mut self, current: usize) {
        self.current = current;
    }

    pub fn try_and_rollback<T>(&mut self, predicate: impl FnOnce(&mut Self) -> T) -> T {
        let current = self.current;
        let result = predicate(self);
        self.current = current;
        result
    }

    pub fn try_or_rollback<T, E>(&mut self, predicate: impl FnOnce(&mut Self) -> Result<T, E>) -> Result<T, E> {
        let current = self.current;
        let result = predicate(self);
        match result {
            Ok(value) => Ok(value),
            Err(error) => {
                self.current = current;
                Err(error)
            }
        }
    }
}