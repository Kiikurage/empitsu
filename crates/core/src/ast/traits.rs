use crate::position::Position;

pub trait GetPosition {
    fn position(&self) -> &Position;
}