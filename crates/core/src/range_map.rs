use crate::position::Position;
use std::collections::BTreeMap;
use crate::range::Range;

///
/// Map of non-overlapped ranges to values. For a given point p,
/// it returns the value of the range that contains p.
///
pub struct RangeMap<V> {
    ranges: BTreeMap<Position, Position>,   // <start, end>
    values: BTreeMap<Position, V>,  // <start, value>
}

impl<V> RangeMap<V> {
    fn new() -> Self {
        Self {
            ranges: BTreeMap::new(),
            values: BTreeMap::new(),
        }
    }

    fn insert(&mut self, range: Range, value: V) -> Result<(), String> {
        if let Some((.., end)) = self.ranges.range(..range.end).next_back() {
            if &range.start < end {
                return Err("Overlapped".to_string())
            }
        }

        self.insert_unchecked(range, value);

        Ok(())
    }

    fn insert_unchecked(&mut self, range: Range, value: V) {
        self.ranges.insert(range.start, range.end);
        self.values.insert(range.start, value);
    }

    fn find(&self, p: Position) -> Option<&V> {
        let (start, end) = self.ranges.range(..=p).next_back()?;
        if !(start <= &p && &p < end) {
            return None;
        }

        self.values.get(start)
    }
}

#[cfg(test)]
mod test {
    use crate::position::Position;
    use crate::range::Range;
    use crate::range_map::RangeMap;

    #[test]
    fn get_range_by_middle_point() {
        let mut range_map = RangeMap::new();
        range_map.insert_unchecked(Range::of(1, 1, 1, 3), 1);
        range_map.insert_unchecked(Range::of(2, 1, 2, 3), 2);
        range_map.insert_unchecked(Range::of(3, 1, 3, 3), 3);

        assert_eq!(range_map.find(Position::new(1, 2)), Some(&1));
        assert_eq!(range_map.find(Position::new(2, 2)), Some(&2));
        assert_eq!(range_map.find(Position::new(3, 2)), Some(&3));
    }

    #[test]
    fn get_range_by_start_point() {
        let mut range_map = RangeMap::new();
        range_map.insert_unchecked(Range::of(1, 1, 1, 3), 1);
        range_map.insert_unchecked(Range::of(2, 1, 2, 3), 2);
        range_map.insert_unchecked(Range::of(3, 1, 3, 3), 3);

        assert_eq!(range_map.find(Position::new(1, 1)), Some(&1));
        assert_eq!(range_map.find(Position::new(2, 1)), Some(&2));
        assert_eq!(range_map.find(Position::new(3, 1)), Some(&3));
    }

    #[test]
    fn get_range_by_end_point() {
        let mut range_map = RangeMap::new();
        range_map.insert_unchecked(Range::of(1, 1, 1, 3), 1);
        range_map.insert_unchecked(Range::of(2, 1, 2, 3), 2);
        range_map.insert_unchecked(Range::of(3, 1, 3, 3), 3);

        assert_eq!(range_map.find(Position::new(1, 3)), None);
        assert_eq!(range_map.find(Position::new(2, 3)), None);
        assert_eq!(range_map.find(Position::new(3, 3)), None);
    }

    #[test]
    fn get_non_exist_key() {
        let mut range_map = RangeMap::new();
        range_map.insert_unchecked(Range::of(1, 1, 1, 3), 1);
        range_map.insert_unchecked(Range::of(2, 1, 2, 3), 2);
        range_map.insert_unchecked(Range::of(3, 1, 3, 3), 3);

        assert_eq!(range_map.find(Position::new(0, 0)), None);
        assert_eq!(range_map.find(Position::new(1, 0)), None);
        assert_eq!(range_map.find(Position::new(1, 4)), None);
        assert_eq!(range_map.find(Position::new(4, 0)), None);
    }

    /// Partially overlapped: the new range is BEFORE the existing range)
    ///     existing:(3, 8), new:(0, 5)
    #[test]
    fn detect_overlapped_key_case_1() {
        let mut range_map = RangeMap::new();
        range_map.insert_unchecked(Range::of(0, 3, 0, 8), 1);
        range_map.insert(Range::of(0, 0, 0, 5), 2).expect_err("overlapped key");
    }

    /// Partially overlapped: the new range is AFTER the existing range)
    ///     existing:(0, 5), new:(3, 8)
    #[test]
    fn detect_overlapped_key_case_2() {
        let mut range_map = RangeMap::new();
        range_map.insert_unchecked(Range::of(0, 0, 0, 5), 1);
        range_map.insert(Range::of(0, 3, 0, 8), 2).expect_err("overlapped key");
    }

    /// The existing range entirely covers the new range
    ///     existing:(0, 8), new:(3, 5)
    #[test]
    fn detect_overlapped_key_case_3() {
        let mut range_map = RangeMap::new();
        range_map.insert_unchecked(Range::of(0, 0, 0, 8), 1);
        range_map.insert(Range::of(0, 3, 0, 5), 2).expect_err("overlapped key");
    }

    /// The new range entirely covers the existing range
    ///     existing:(3, 5), new:(0, 8)
    #[test]
    fn detect_overlapped_key_case_4() {
        let mut range_map = RangeMap::new();
        range_map.insert_unchecked(Range::of(0, 3, 0, 5), 1);
        range_map.insert(Range::of(0, 0, 0, 8), 2).expect_err("overlapped key");
    }

    #[test]
    fn new_range_next_to_existing_range() {
        let mut range_map = RangeMap::new();
        range_map.insert_unchecked(Range::of(0, 3, 0, 5), 1);
        range_map.insert(Range::of(0, 5, 0, 8), 2).expect("adjacent ranges");
    }

    #[test]
    fn existing_range_next_to_new_range() {
        let mut range_map = RangeMap::new();
        range_map.insert_unchecked(Range::of(0, 5, 0, 8), 1);
        range_map.insert(Range::of(0, 3, 0, 5), 2).expect("adjacent ranges");
    }
}