#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    /// Type for variables not yet initialized.
    Never,

    /// Type cannot be determined due to an error.
    Any,

    Void,
    Number,
    Bool,
    Ref,
}
