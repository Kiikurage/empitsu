use crate::punctuator_kind::PunctuatorKind;

#[derive(Debug, PartialEq)]
pub enum Token {
    Number(f64),
    Identifier(String),
    Punctuator(PunctuatorKind),
}
