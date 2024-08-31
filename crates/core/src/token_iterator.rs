use crate::error::Error;
use crate::lexer::scan;
use crate::position::Position;
use crate::token::Token;

pub struct TokenIterator {
    tokens: Vec<Result<Token, Error>>,
    pub raw_offset: usize,
    pub last_position: Position,
}

impl TokenIterator {
    pub fn new(input: &str) -> Self {
        Self {
            tokens: scan(input),
            raw_offset: 0,
            last_position: Position::new(0, 0),
        }
    }

    pub fn next(&mut self) -> &Result<Token, Error> {
        while let Ok(Token::LineTerminator(..)) = self.raw_peek() {
            self.raw_next();
        }
        self.raw_next()
    }

    pub fn raw_next(&mut self) -> &Result<Token, Error> {
        // Safe to unwrap because EndOfInput token must be present
        let ret = self.tokens.get(self.raw_offset).unwrap();
        if !matches!(ret, Ok(Token::EndOfInput(..))) {
            self.raw_offset += 1;
        }

        let position = match ret {
            Ok(token) => &token.position(),
            Err(error) => &error.position(),
        };
        self.last_position.line = position.line;
        self.last_position.column = position.column;

        ret
    }

    pub fn has_next(&mut self) -> bool {
        !matches!(self.peek(), Ok(Token::EndOfInput(..)))
    }

    pub fn peek(&mut self) -> &Result<Token, Error> {
        let mut offset = self.raw_offset;
        while let Some(Ok(Token::LineTerminator(..))) = self.tokens.get(offset) {
            offset += 1;
        }

        // Safe to unwrap because EndOfInput token must be present
        self.tokens.get(offset).unwrap()
    }

    pub fn raw_peek(&mut self) -> &Result<Token, Error> {
        // Safe to unwrap because EndOfInput token must be present
        self.tokens.get(self.raw_offset).unwrap()
    }

    pub fn position(&mut self) -> &Position {
        match self.raw_peek() {
            Ok(ref token) => &token.position(),
            Err(ref err) => &err.position()
        }
    }
}

#[cfg(test)]
mod test {
    use crate::position::Position;
    use crate::token::Token;
    use crate::token_iterator::TokenIterator;

    #[test]
    fn next() {
        let mut iter = TokenIterator::new("a\nb\nc");

        assert_eq!(iter.next(), &Ok(Token::identifier((0, 0), "a")));
        assert_eq!(iter.next(), &Ok(Token::identifier((1, 0), "b")));
        assert_eq!(iter.next(), &Ok(Token::identifier((2, 0), "c")));
        assert_eq!(iter.next(), &Ok(Token::end_of_input((2, 1))));
        assert_eq!(iter.next(), &Ok(Token::end_of_input((2, 1))));
    }

    #[test]
    fn raw_next() {
        let mut iter = TokenIterator::new("a\nb\nc");

        assert_eq!(iter.raw_next(), &Ok(Token::identifier((0, 0), "a")));
        assert_eq!(iter.raw_next(), &Ok(Token::line_terminator((0, 1))));
        assert_eq!(iter.raw_next(), &Ok(Token::identifier((1, 0), "b")));
        assert_eq!(iter.raw_next(), &Ok(Token::line_terminator((1, 1))));
        assert_eq!(iter.raw_next(), &Ok(Token::identifier((2, 0), "c")));
        assert_eq!(iter.next(), &Ok(Token::end_of_input((2, 1))));
        assert_eq!(iter.next(), &Ok(Token::end_of_input((2, 1))));
    }

    #[test]
    fn mix_next() {
        let mut iter = TokenIterator::new("a\nb\nc");

        assert_eq!(iter.raw_next(), &Ok(Token::identifier((0, 0), "a")));
        assert_eq!(iter.next(), &Ok(Token::identifier((1, 0), "b")));
        assert_eq!(iter.next(), &Ok(Token::identifier((2, 0), "c")));
        assert_eq!(iter.raw_next(), &Ok(Token::end_of_input((2, 1))));
        assert_eq!(iter.next(), &Ok(Token::end_of_input((2, 1))));
    }

    #[test]
    fn next_and_last_position() {
        let mut iter = TokenIterator::new("a\nb\nc\n\n");

        assert_eq!(iter.last_position, Position::new(0, 0));
        iter.next();
        assert_eq!(iter.last_position, Position::new(0, 0));
        iter.next();
        assert_eq!(iter.last_position, Position::new(1, 0));
        iter.next();
        assert_eq!(iter.last_position, Position::new(2, 0));
        iter.next();
        assert_eq!(iter.last_position, Position::new(4, 0));
        iter.next();
        assert_eq!(iter.last_position, Position::new(4, 0));
    }

    #[test]
    fn raw_next_and_last_position() {
        let mut iter = TokenIterator::new("a\nb\nc\n\n");

        assert_eq!(iter.last_position, Position::new(0, 0));
        iter.raw_next();
        assert_eq!(iter.last_position, Position::new(0, 0));
        iter.raw_next();
        assert_eq!(iter.last_position, Position::new(0, 1));
        iter.raw_next();
        assert_eq!(iter.last_position, Position::new(1, 0));
        iter.raw_next();
        assert_eq!(iter.last_position, Position::new(1, 1));
        iter.raw_next();
        assert_eq!(iter.last_position, Position::new(2, 0));
        iter.raw_next();
        assert_eq!(iter.last_position, Position::new(2, 1));
        iter.raw_next();
        assert_eq!(iter.last_position, Position::new(3, 0));
        iter.raw_next();
        assert_eq!(iter.last_position, Position::new(4, 0));
        iter.raw_next();
        assert_eq!(iter.last_position, Position::new(4, 0));
    }

    #[test]
    fn has_next() {
        let mut iter = TokenIterator::new("a\nb\nc\n\n");

        assert!(iter.has_next());
        iter.next();
        assert!(iter.has_next());
        iter.next();
        assert!(iter.has_next());
        iter.next();
        assert!(!iter.has_next());
        iter.next();
        assert!(!iter.has_next());
    }
}