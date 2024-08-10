use crate::punctuator_kind::PunctuatorKind;

#[derive(Debug, PartialEq)]
pub enum Token {
    Number(f64),
    Bool(bool),
    Identifier(String),
    Punctuator(PunctuatorKind),
}
