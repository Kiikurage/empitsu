#[derive(Debug, Clone, PartialEq)]
pub enum PunctuatorKind {
    Plus,
    Minus,
    Multiply,
    Divide,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    SemiColon,
    Comma,
    Equal,
    LogicalNot,
    LogicalAnd,
    LogicalOr,
    Dot,
    Colon,
}
