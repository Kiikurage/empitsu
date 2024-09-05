#![allow(clippy::new_without_default)]
#![allow(clippy::only_used_in_recursion)]
#![allow(clippy::manual_map)]
#![allow(dead_code)]
pub mod lexer;
pub mod punctuation_kind;
pub mod token;
pub mod parser;
pub mod ast;
pub mod analysis;
pub mod analyzer;
pub mod vm;
mod char_iterator;
mod error;
mod multi_peek;
mod position;
mod token_iterator;
mod util;
mod range_map;
