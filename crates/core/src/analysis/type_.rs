#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    /// Type for values the shouldn't be used (e.g. unreachable code)
    Never,

    /// Type cannot be determined.
    Unknown,

    Void,
    Number,
    Bool,
    Ref,
}
