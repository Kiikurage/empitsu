use std::collections::VecDeque;

#[derive(Debug)]
pub struct MultiPeek<I>
where
    I: Iterator,
{
    iter: I,
    peeked: VecDeque<I::Item>,
}

impl<I> MultiPeek<I>
where
    I: Iterator,
{
    pub fn new(iter: I) -> Self {
        Self {
            iter,
            peeked: VecDeque::new(),
        }
    }

    pub fn get_iter(&self) -> &I {
        &self.iter
    }

    pub fn peek(&mut self, offset: usize) -> Option<&I::Item> {
        while self.peeked.len() <= offset {
            if let Some(next) = self.iter.next() {
                self.peeked.push_back(next);
            } else {
                break;
            }
        }
        self.peeked.get(offset)
    }

    pub fn next(&mut self) -> Option<I::Item> {
        self.peeked.pop_front().or_else(|| self.iter.next())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn peek_next_item() {
        let mut iter = vec![1, 2, 3].into_iter();
        let mut multi_peek = MultiPeek::new(&mut iter);
        assert_eq!(multi_peek.peek(0), Some(&1));
        assert_eq!(multi_peek.peek(0), Some(&1));
        assert_eq!(multi_peek.peek(0), Some(&1));
        assert_eq!(multi_peek.peek(0), Some(&1));
        assert_eq!(multi_peek.next(), Some(1));
        assert_eq!(multi_peek.next(), Some(2));
        assert_eq!(multi_peek.next(), Some(3));
        assert_eq!(multi_peek.next(), None);
    }

    #[test]
    fn peek_multiple_items() {
        let mut iter = vec![1, 2, 3].into_iter();
        let mut multi_peek = MultiPeek::new(&mut iter);
        assert_eq!(multi_peek.peek(0), Some(&1));
        assert_eq!(multi_peek.peek(1), Some(&2));
        assert_eq!(multi_peek.peek(2), Some(&3));
        assert_eq!(multi_peek.peek(3), None);
        assert_eq!(multi_peek.peek(0), Some(&1));
        assert_eq!(multi_peek.peek(1), Some(&2));
        assert_eq!(multi_peek.peek(2), Some(&3));
        assert_eq!(multi_peek.peek(3), None);

        assert_eq!(multi_peek.next(), Some(1));
        assert_eq!(multi_peek.next(), Some(2));
        assert_eq!(multi_peek.next(), Some(3));
        assert_eq!(multi_peek.next(), None);
    }

    #[test]
    fn peek_after_next() {
        let mut iter = vec![1, 2, 3].into_iter();
        let mut multi_peek = MultiPeek::new(&mut iter);
        assert_eq!(multi_peek.next(), Some(1));
        assert_eq!(multi_peek.peek(0), Some(&2));
        assert_eq!(multi_peek.peek(1), Some(&3));
        assert_eq!(multi_peek.peek(2), None);
        assert_eq!(multi_peek.peek(3), None);

        assert_eq!(multi_peek.next(), Some(2));
        assert_eq!(multi_peek.peek(0), Some(&3));
        assert_eq!(multi_peek.peek(1), None);
        assert_eq!(multi_peek.peek(2), None);
        assert_eq!(multi_peek.peek(3), None);

        assert_eq!(multi_peek.next(), Some(3));
        assert_eq!(multi_peek.peek(0), None);
        assert_eq!(multi_peek.peek(1), None);
        assert_eq!(multi_peek.peek(2), None);
        assert_eq!(multi_peek.peek(3), None);

        assert_eq!(multi_peek.next(), None);
        assert_eq!(multi_peek.peek(0), None);
        assert_eq!(multi_peek.peek(1), None);
        assert_eq!(multi_peek.peek(2), None);
        assert_eq!(multi_peek.peek(3), None);
    }
}