use crate::punctuator_kind::PunctuatorKind;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Number(f64),
    Bool(bool),
    String(String),
    Identifier(String),
    Punctuator(PunctuatorKind),
}
