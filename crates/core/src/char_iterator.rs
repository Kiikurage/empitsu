use crate::multi_peek::MultiPeek;
use crate::position::Position;
use std::str::Chars;

pub struct CharIterator<'a> {
    iter: MultiPeek<Chars<'a>>,
    position: Position,
}

impl<'a> CharIterator<'a> {
    pub fn new(input: &'a str) -> Self {
        CharIterator {
            iter: MultiPeek::new(input.chars()),
            position: Position::new(0, 0),
        }
    }

    pub fn get_position(&self) -> Position {
        self.position
    }

    pub fn peek(&mut self, offset: usize) -> Option<&char> {
        self.iter.peek(offset)
    }
}

impl Iterator for CharIterator<'_> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        let option = self.iter.next();
        if let Some(ch) = option {
            if ch == '\n' {
                self.position.line += 1;
                self.position.character = 0;
            } else {
                self.position.character += 1;
            }
        }
        option
    }
}

#[cfg(test)]
mod test {
    use crate::char_iterator::CharIterator;

    #[test]
    fn test_peek() {
        let mut iter = CharIterator::new("abc");
        assert_eq!(iter.peek(0), Some(&'a'));
        assert_eq!(iter.peek(0), Some(&'a'));
        assert_eq!(iter.next(), Some('a'));
        assert_eq!(iter.peek(0), Some(&'b'));
        assert_eq!(iter.next(), Some('b'));
        assert_eq!(iter.peek(0), Some(&'c'));
        assert_eq!(iter.next(), Some('c'));
        assert_eq!(iter.peek(0), None);
    }

    #[test]
    fn test_peek_at() {
        let mut iter = CharIterator::new("abc");
        assert_eq!(iter.peek(0), Some(&'a'));
        assert_eq!(iter.peek(1), Some(&'b'));
        assert_eq!(iter.peek(2), Some(&'c'));
        assert_eq!(iter.peek(3), None);
        assert_eq!(iter.next(), Some('a'));
        assert_eq!(iter.peek(0), Some(&'b'));
        assert_eq!(iter.peek(1), Some(&'c'));
        assert_eq!(iter.peek(2), None);
        assert_eq!(iter.next(), Some('b'));
        assert_eq!(iter.peek(0), Some(&'c'));
        assert_eq!(iter.peek(1), None);
        assert_eq!(iter.next(), Some('c'));
        assert_eq!(iter.peek(0), None);
    }
}