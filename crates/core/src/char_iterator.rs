use std::collections::VecDeque;
use std::iter::Peekable;
use std::str::Chars;
use crate::token::Position;

pub struct CharIterator<'a> {
    chars: Peekable<Chars<'a>>,
    position: Position,
    buffer: VecDeque<char>,
}

impl <'a> CharIterator<'a> {
    pub fn new(input: &'a str) -> Self {
        CharIterator {
            chars: input.chars().peekable(),
            position: Position { line: 0, column: 0 },
            buffer: VecDeque::new(),
        }
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.peek_at(0)
    }

    pub fn peek_at(&mut self, offset: usize) -> Option<&char> {
        while self.buffer.len() <= offset {
            match self.chars.next() {
                Some(c) => self.buffer.push_back(c),
                None => break
            }
        }
        self.buffer.get(offset)
    }

    pub fn next(&mut self) -> Option<char> {
        match self.buffer.pop_front().or_else(|| self.chars.next()) {
            Some(c) => {
                if c == '\n' {
                    self.position.line += 1;
                    self.position.column = 0;
                } else {
                    self.position.column += 1;
                }
                Some(c)
            }
            None => None
        }
    }
    
    pub fn position(&self) -> &Position {
        &self.position
    }
}

#[cfg(test)]
mod test {
    use crate::char_iterator::CharIterator;

    #[test]
    fn test_peek() {
        let mut iter = CharIterator::new("abc");
        assert_eq!(iter.peek(), Some(&'a'));
        assert_eq!(iter.peek(), Some(&'a'));
        assert_eq!(iter.next(), Some('a'));
        assert_eq!(iter.peek(), Some(&'b'));
        assert_eq!(iter.next(), Some('b'));
        assert_eq!(iter.peek(), Some(&'c'));
        assert_eq!(iter.next(), Some('c'));
        assert_eq!(iter.peek(), None);
    }
    
    #[test]
    fn test_peek_at() {
        let mut iter = CharIterator::new("abc");
        assert_eq!(iter.peek_at(0), Some(&'a'));
        assert_eq!(iter.peek_at(1), Some(&'b'));
        assert_eq!(iter.peek_at(2), Some(&'c'));
        assert_eq!(iter.peek_at(3), None);
        assert_eq!(iter.next(), Some('a'));
        assert_eq!(iter.peek_at(0), Some(&'b'));
        assert_eq!(iter.peek_at(1), Some(&'c'));
        assert_eq!(iter.peek_at(2), None);
        assert_eq!(iter.next(), Some('b'));
        assert_eq!(iter.peek_at(0), Some(&'c'));
        assert_eq!(iter.peek_at(1), None);
        assert_eq!(iter.next(), Some('c'));
        assert_eq!(iter.peek_at(0), None);
    }
}