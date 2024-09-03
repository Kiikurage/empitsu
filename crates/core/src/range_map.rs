use std::collections::btree_map::Values;
use std::collections::BTreeMap;
use std::ops::Range;

///
/// Map of non-overlapped ranges to values. For a given point p,
/// it returns the value of the range that contains p.
///
#[derive(Debug)]
pub struct RangeMap<K, V> {
    ranges: BTreeMap<K, K>,   // <start, end>
    values: BTreeMap<K, V>,  // <start, value>
}

impl<K, V> RangeMap<K, V>
where
    K: Copy + Ord,
{
    pub fn new() -> Self {
        Self {
            ranges: BTreeMap::new(),
            values: BTreeMap::new(),
        }
    }

    pub fn insert(&mut self, range: Range<K>, value: V) -> Result<(), String> {
        if let Some((.., end)) = self.ranges.range(..range.end).next_back() {
            if &range.start < end {
                return Err("Overlapped".to_string());
            }
        }

        self.insert_unchecked(range, value);

        Ok(())
    }

    pub fn insert_unchecked(&mut self, range: Range<K>, value: V) {
        self.ranges.insert(range.start, range.end);
        self.values.insert(range.start, value);
    }

    pub fn find(&self, p: &K) -> Option<&V> {
        let (start, end) = self.ranges.range(..=p).next_back()?;
        if !(start <= p && p < end) {
            return None;
        }

        self.values.get(start)
    }

    pub fn find_mut(&mut self, p: &K) -> Option<&mut V> {
        let (start, end) = self.ranges.range(..=p).next_back()?;
        if !(start <= p && p < end) {
            return None;
        }

        self.values.get_mut(start)
    }

    pub fn values(&self) -> Values<'_, K, V> {
        self.values.values()
    }
}

#[cfg(test)]
mod test {
    use crate::range_map::RangeMap;

    #[test]
    fn get_range_by_middle_point() {
        let mut range_map = RangeMap::new();
        range_map.insert_unchecked(1..3, 1);
        range_map.insert_unchecked(4..6, 2);
        range_map.insert_unchecked(7..9, 3);

        assert_eq!(range_map.find(&2), Some(&1));
        assert_eq!(range_map.find(&5), Some(&2));
        assert_eq!(range_map.find(&8), Some(&3));
    }

    #[test]
    fn get_range_by_start_point() {
        let mut range_map = RangeMap::new();
        range_map.insert_unchecked(1..3, 1);
        range_map.insert_unchecked(4..6, 2);
        range_map.insert_unchecked(7..9, 3);

        assert_eq!(range_map.find(&1), Some(&1));
        assert_eq!(range_map.find(&4), Some(&2));
        assert_eq!(range_map.find(&7), Some(&3));
    }

    #[test]
    fn get_range_by_end_point() {
        let mut range_map = RangeMap::new();
        range_map.insert_unchecked(1..3, 1);
        range_map.insert_unchecked(4..6, 2);
        range_map.insert_unchecked(7..9, 3);

        assert_eq!(range_map.find(&3), None);
        assert_eq!(range_map.find(&6), None);
        assert_eq!(range_map.find(&9), None);
    }

    #[test]
    fn get_non_exist_key() {
        let mut range_map = RangeMap::new();
        range_map.insert_unchecked(1..2, 1);
        range_map.insert_unchecked(4..5, 2);

        assert_eq!(range_map.find(&0), None);
        assert_eq!(range_map.find(&3), None);
        assert_eq!(range_map.find(&6), None);
    }

    /// Partially overlapped: the new range is BEFORE the existing range)
    ///     existing:(3, 8), new:(0, 5)
    #[test]
    fn detect_overlapped_key_case_1() {
        let mut range_map = RangeMap::new();
        range_map.insert_unchecked(3..8, 1);
        range_map.insert(0..5, 2).expect_err("overlapped key");
    }

    /// Partially overlapped: the new range is AFTER the existing range)
    ///     existing:(0, 5), new:(3, 8)
    #[test]
    fn detect_overlapped_key_case_2() {
        let mut range_map = RangeMap::new();
        range_map.insert_unchecked(0..5, 1);
        range_map.insert(3..8, 2).expect_err("overlapped key");
    }

    /// The existing range entirely covers the new range
    ///     existing:(0, 8), new:(3, 5)
    #[test]
    fn detect_overlapped_key_case_3() {
        let mut range_map = RangeMap::new();
        range_map.insert_unchecked(0..8, 1);
        range_map.insert(3..5, 2).expect_err("overlapped key");
    }

    /// The new range entirely covers the existing range
    ///     existing:(3, 5), new:(0, 8)
    #[test]
    fn detect_overlapped_key_case_4() {
        let mut range_map = RangeMap::new();
        range_map.insert_unchecked(3..5, 1);
        range_map.insert(0..8, 2).expect_err("overlapped key");
    }

    #[test]
    fn new_range_next_to_existing_range() {
        let mut range_map = RangeMap::new();
        range_map.insert_unchecked(3..5, 1);
        range_map.insert(5..8, 2).expect("adjacent ranges");
    }

    #[test]
    fn existing_range_next_to_new_range() {
        let mut range_map = RangeMap::new();
        range_map.insert_unchecked(5..8, 1);
        range_map.insert(3..5, 2).expect("adjacent ranges");
    }
}