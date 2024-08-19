use crate::char_iterator::CharIterator;
use crate::error::Error;
use crate::punctuation_kind::PunctuationKind;
use crate::token::{Token, TokenKind};

pub fn scan(input: &str) -> Vec<Result<Token, Error>> {
    let mut chars = CharIterator::new(input);
    let mut tokens = Vec::new();
    while chars.peek().is_some() {
        match scan_token(&mut chars) {
            Ok(Some(token)) => tokens.push(Ok(token)),
            Ok(None) => (),
            Err(error) => {
                tokens.push(Err(error))
                // TODO: Skip until next valid token
            }
        }
    }
    tokens
}

fn scan_token(chars: &mut CharIterator) -> Result<Option<Token>, Error> {
    let position = chars.position().clone();
    match chars.peek() {
        Some(' ' | '\r' | '\t') => {
            chars.next();
            Ok(None)
        }
        Some('\n') => {
            chars.next();
            Ok(Some(Token {
                kind: TokenKind::LineTerminator,
                position,
                text: "\n".to_string(),
            }))
        }
        Some('?') => {
            chars.next();
            Ok(Some(Token {
                kind: TokenKind::Punctuation(PunctuationKind::Question),
                position,
                text: "?".to_string(),
            }))
        }
        Some('+') => {
            chars.next();
            Ok(Some(Token {
                kind: TokenKind::Punctuation(PunctuationKind::Plus),
                position,
                text: "+".to_string(),
            }))
        }
        Some('-') => {
            chars.next();
            Ok(Some(Token {
                kind: TokenKind::Punctuation(PunctuationKind::Minus),
                position,
                text: "-".to_string(),
            }))
        }
        Some('*') => {
            chars.next();
            Ok(Some(Token {
                kind: TokenKind::Punctuation(PunctuationKind::Asterisk),
                position,
                text: "*".to_string(),
            }))
        }
        Some('/') => {
            match chars.peek_at(1) {
                Some('/') => {
                    chars.next();
                    chars.next();

                    let mut body = String::new();
                    while let Some(&c) = chars.peek() {
                        if c == '\n' {
                            break;
                        }
                        body.push(c);
                        chars.next();
                    }
                    // Ok(Some(Token::Comment(body)))
                    Ok(None)
                }
                Some('*') => {
                    chars.next();
                    chars.next();

                    let mut body = String::new();
                    while let Some(&c) = chars.peek() {
                        if c == '*' && matches!(chars.peek_at(1), Some('/')) {
                            chars.next();
                            chars.next();
                            break;
                        }
                        body.push(c);
                        chars.next();
                    }
                    // Ok(Some(Token::Comment(body)))
                    Ok(None)
                }
                _ => {
                    chars.next();
                    Ok(Some(Token {
                        kind: TokenKind::Punctuation(PunctuationKind::Slash),
                        position,
                        text: "/".to_string(),
                    }))
                }
            }
        }
        Some('(') => {
            chars.next();
            Ok(Some(Token {
                kind: TokenKind::Punctuation(PunctuationKind::LeftParen),
                position,
                text: "(".to_string(),
            }))
        }
        Some(')') => {
            chars.next();
            Ok(Some(Token {
                kind: TokenKind::Punctuation(PunctuationKind::RightParen),
                position,
                text: ")".to_string(),
            }))
        }
        Some('{') => {
            chars.next();
            Ok(Some(Token {
                kind: TokenKind::Punctuation(PunctuationKind::LeftBrace),
                position,
                text: "{".to_string(),
            }))
        }
        Some('}') => {
            chars.next();
            Ok(Some(Token {
                kind: TokenKind::Punctuation(PunctuationKind::RightBrace),
                position,
                text: "}".to_string(),
            }))
        }
        Some(';') => {
            chars.next();
            Ok(Some(Token {
                kind: TokenKind::Punctuation(PunctuationKind::SemiColon),
                position,
                text: ";".to_string(),
            }))
        }
        Some(':') => {
            chars.next();
            Ok(Some(Token {
                kind: TokenKind::Punctuation(PunctuationKind::Colon),
                position,
                text: ":".to_string(),
            }))
        }
        Some(',') => {
            chars.next();
            Ok(Some(Token {
                kind: TokenKind::Punctuation(PunctuationKind::Comma),
                position,
                text: ",".to_string(),
            }))
        }
        Some('=') => {
            if matches!(chars.peek_at(1), Some('=')) {
                chars.next();
                chars.next();
                Ok(Some(Token {
                    kind: TokenKind::Punctuation(PunctuationKind::EqualEqual),
                    position,
                    text: "==".to_string(),
                }))
            } else {
                chars.next();
                Ok(Some(Token {
                    kind: TokenKind::Punctuation(PunctuationKind::Equal),
                    position,
                    text: "=".to_string(),
                }))
            }
        }
        Some('!') => {
            if matches!(chars.peek_at(1), Some('=')) {
                chars.next();
                chars.next();
                Ok(Some(Token {
                    kind: TokenKind::Punctuation(PunctuationKind::ExclamationEqual),
                    position,
                    text: "!=".to_string(),
                }))
            } else {
                chars.next();
                Ok(Some(Token {
                    kind: TokenKind::Punctuation(PunctuationKind::Exclamation),
                    position,
                    text: "!".to_string(),
                }))
            }
        }
        Some('<') => {
            if matches!(chars.peek_at(1), Some('=')) {
                chars.next();
                chars.next();
                Ok(Some(Token {
                    kind: TokenKind::Punctuation(PunctuationKind::LeftBracketEqual),
                    position,
                    text: "<=".to_string(),
                }))
            } else {
                chars.next();
                Ok(Some(Token {
                    kind: TokenKind::Punctuation(PunctuationKind::LeftBracket),
                    position,
                    text: "<".to_string(),
                }))
            }
        }
        Some('>') => {
            if matches!(chars.peek_at(1), Some('=')) {
                chars.next();
                chars.next();
                Ok(Some(Token {
                    kind: TokenKind::Punctuation(PunctuationKind::RightBracketEqual),
                    position,
                    text: ">=".to_string(),
                }))
            } else {
                chars.next();
                Ok(Some(Token {
                    kind: TokenKind::Punctuation(PunctuationKind::RightBracket),
                    position,
                    text: ">".to_string(),
                }))
            }
        }
        Some('&') => {
            if matches!(chars.peek_at(1), Some('&')) {
                chars.next();
                chars.next();
                Ok(Some(Token {
                    kind: TokenKind::Punctuation(PunctuationKind::AndAnd),
                    position,
                    text: "&&".to_string(),
                }))
            } else {
                Err(Error::syntax_error(position, "Unexpected character"))
            }
        }
        Some('|') => {
            if matches!(chars.peek_at(1), Some('|')) {
                chars.next();
                chars.next();
                Ok(Some(Token {
                    kind: TokenKind::Punctuation(PunctuationKind::VerticalLineVerticalLine),
                    position,
                    text: "||".to_string(),
                }))
            } else {
                chars.next();
                Ok(Some(Token {
                    kind: TokenKind::Punctuation(PunctuationKind::VerticalLine),
                    position,
                    text: "|".to_string(),
                }))
            }
        }
        Some('.') => {
            chars.next();
            Ok(Some(Token {
                kind: TokenKind::Punctuation(PunctuationKind::Dot),
                position,
                text: ".".to_string(),
            }))
        }
        Some('0'..='9') => scan_number(chars),
        Some('"') => scan_string(chars),
        Some('a'..='z' | 'A'..='Z' | '_') => scan_identifier(chars),
        Some(other) => Err(Error::syntax_error(position, format!("Unexpected character \"{}\"", other))),
        None => Ok(None),
    }
}

fn scan_number(chars: &mut CharIterator) -> Result<Option<Token>, Error> {
    let mut digits: Vec<char> = vec![];
    let mut has_dot = false;
    let position = chars.position().clone();

    loop {
        match chars.peek() {
            Some(&d @ '0'..='9') => {
                digits.push(d);
                chars.next();
            }
            Some('.') => {
                if digits.is_empty() {
                    return Ok(None);
                }
                if has_dot {
                    break;
                }
                match chars.peek_at(1) {
                    Some(&d @ '0'..='9') => {
                        digits.push('.');
                        has_dot = true;
                        digits.push(d);
                        chars.next();
                        chars.next();
                    }
                    _ => break,
                }
            }
            _ => break,
        }
    }

    if digits.is_empty() {
        Ok(None)
    } else {
        Ok(Some(Token {
            kind: TokenKind::Number(digits.iter().collect::<String>().parse().unwrap()),
            position,
            text: digits.iter().collect::<String>(),
        }))
    }
}

fn scan_string(chars: &mut CharIterator) -> Result<Option<Token>, Error> {
    let mut string: Vec<char> = vec![];
    let position = chars.position().clone();

    if !matches!(chars.peek(), Some('"')) {
        return Ok(None);
    }
    chars.next();

    loop {
        match chars.peek() {
            Some('"') => {
                chars.next();
                return Ok(Some(Token {
                    kind: TokenKind::String(string.iter().collect::<String>()),
                    position,
                    text: format!("\"{}\"", string.iter().collect::<String>()),
                }));
            }
            Some(&c) => {
                string.push(c);
                chars.next();
            }
            None => return Err(Error::syntax_error(chars.position().clone(), "Expect \""))
        }
    }
}

fn scan_identifier(chars: &mut CharIterator) -> Result<Option<Token>, Error> {
    let mut identifier_name: Vec<char> = vec![];
    let position = chars.position().clone();

    match chars.peek() {
        Some(&c @ ('a'..='z' | 'A'..='Z' | '_')) => {
            identifier_name.push(c);
            chars.next();
        }
        _ => return Ok(None)
    }

    while let Some(&c @ ('a'..='z' | 'A'..='Z' | '0'..='9' | '_')) = chars.peek() {
        identifier_name.push(c);
        chars.next();
    }

    let name = identifier_name.iter().collect::<String>();

    match name.as_str() {
        "true" => Ok(Some(Token { kind: TokenKind::Bool(true), position, text: "true".to_string() })),
        "false" => Ok(Some(Token { kind: TokenKind::Bool(false), position, text: "false".to_string() })),
        _ => Ok(Some(Token { text: name.to_string(), kind: TokenKind::Identifier(name), position })),
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::{scan};
    use crate::token::{Position, Token, TokenKind};

    trait UnwrapAll {
        type Item;
        fn unwrap_all(self) -> Vec<Self::Item>;
    }

    impl<T, E> UnwrapAll for Vec<Result<T, E>>
    where
        E: std::fmt::Debug,
    {
        type Item = T;
        fn unwrap_all(self) -> Vec<Self::Item> {
            self.into_iter().map(|t| t.unwrap()).collect()
        }
    }

    #[test]
    fn test_whitespace() {
        assert_eq!(scan(" ").unwrap_all(), vec![]);
        assert_eq!(scan("\t").unwrap_all(), vec![]);
        assert_eq!(scan("\r").unwrap_all(), vec![]);
        assert_eq!(scan(" a").unwrap_all(), vec![
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                position: Position { line: 0, column: 1 },
                text: "a".to_string(),
            }
        ]);
        assert_eq!(scan("\ta").unwrap_all(), vec![
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                position: Position { line: 0, column: 1 },
                text: "a".to_string(),
            }
        ]);
        assert_eq!(scan("\ra").unwrap_all(), vec![
            Token {
                kind: TokenKind::Identifier("a".to_string()),
                position: Position { line: 0, column: 1 },
                text: "a".to_string(),
            }
        ]);
    }

    mod number {
        use crate::lexer::tests::UnwrapAll;
        use crate::lexer::scan;
        use crate::punctuation_kind::PunctuationKind;
        use crate::token::{Position, Token, TokenKind};

        #[test]
        fn test_number_decimal_int() {
            assert_eq!(scan("123").unwrap_all(), vec![
                Token {
                    kind: TokenKind::Number(123.0f64),
                    position: Position { line: 0, column: 0 },
                    text: "123".to_string(),
                }
            ]);
            assert_eq!(scan("123a").unwrap_all(), vec![
                Token {
                    kind: TokenKind::Number(123.0f64),
                    position: Position { line: 0, column: 0 },
                    text: "123".to_string(),
                },
                Token {
                    kind: TokenKind::Identifier("a".to_string()),
                    position: Position { line: 0, column: 3 },
                    text: "a".to_string(),
                }
            ]);
        }

        #[test]
        fn test_number_decimal_int_starting_with_zero() {
            assert_eq!(scan("0").unwrap_all(), vec![
                Token {
                    kind: TokenKind::Number(0.0f64),
                    position: Position { line: 0, column: 0 },
                    text: "0".to_string(),
                }
            ]);
            assert_eq!(scan("0a").unwrap_all(), vec![
                Token {
                    kind: TokenKind::Number(0f64),
                    position: Position { line: 0, column: 0 },
                    text: "0".to_string(),
                },
                Token {
                    kind: TokenKind::Identifier("a".to_string()),
                    position: Position { line: 0, column: 1 },
                    text: "a".to_string(),
                }
            ]);
            assert_eq!(scan("0123").unwrap_all(), vec![
                Token {
                    kind: TokenKind::Number(123f64),
                    position: Position { line: 0, column: 0 },
                    text: "0123".to_string(),
                }
            ]);
        }

        #[test]
        fn test_number_decimal_float() {
            assert_eq!(scan("123.456").unwrap_all(), vec![
                Token {
                    kind: TokenKind::Number(123.456f64),
                    position: Position { line: 0, column: 0 },
                    text: "123.456".to_string(),
                }
            ]);
            assert_eq!(scan("123.456a").unwrap_all(), vec![
                Token {
                    kind: TokenKind::Number(123.456f64),
                    position: Position { line: 0, column: 0 },
                    text: "123.456".to_string(),
                },
                Token {
                    kind: TokenKind::Identifier("a".to_string()),
                    position: Position { line: 0, column: 7 },
                    text: "a".to_string(),
                },
            ]);
        }

        #[test]
        fn test_number_decimal_float_starting_with_zero() {
            assert_eq!(scan("0.456").unwrap_all(), vec![
                Token {
                    kind: TokenKind::Number(0.456f64),
                    position: Position { line: 0, column: 0 },
                    text: "0.456".to_string(),
                }
            ]);
            assert_eq!(scan("0.456a").unwrap_all(), vec![
                Token {
                    kind: TokenKind::Number(0.456f64),
                    position: Position { line: 0, column: 0 },
                    text: "0.456".to_string(),
                },
                Token {
                    kind: TokenKind::Identifier("a".to_string()),
                    position: Position { line: 0, column: 5 },
                    text: "a".to_string(),
                }
            ]);
        }

        #[test]
        fn test_number_decimal_ending_with_dot() {
            assert_eq!(scan("123.").unwrap_all(), vec![
                Token {
                    kind: TokenKind::Number(123f64),
                    position: Position { line: 0, column: 0 },
                    text: "123".to_string(),
                },
                Token {
                    kind: TokenKind::Punctuation(PunctuationKind::Dot),
                    position: Position { line: 0, column: 3 },
                    text: ".".to_string(),
                },
            ]);
            assert_eq!(scan("0.").unwrap_all(), vec![
                Token {
                    kind: TokenKind::Number(0f64),
                    position: Position { line: 0, column: 0 },
                    text: "0".to_string(),
                },
                Token {
                    kind: TokenKind::Punctuation(PunctuationKind::Dot),
                    position: Position { line: 0, column: 1 },
                    text: ".".to_string(),
                }
            ]);
        }

        #[test]
        fn test_number_decimal_with_multiple_dots() {
            assert_eq!(scan("123.456.789").unwrap_all(), vec![
                Token {
                    kind: TokenKind::Number(123.456f64),
                    position: Position { line: 0, column: 0 },
                    text: "123.456".to_string(),
                },
                Token {
                    kind: TokenKind::Punctuation(PunctuationKind::Dot),
                    position: Position { line: 0, column: 7 },
                    text: ".".to_string(),
                },
                Token {
                    kind: TokenKind::Number(789f64),
                    position: Position { line: 0, column: 8 },
                    text: "789".to_string(),
                }
            ]);
        }
    }

    mod string {
        use crate::error::Error;
        use crate::lexer::tests::UnwrapAll;
        use crate::lexer::scan;
        use crate::token::{Position, Token, TokenKind};

        #[test]
        fn string() {
            assert_eq!(scan("\"123\"").unwrap_all(), vec![
                Token {
                    kind: TokenKind::String("123".to_string()),
                    position: Position { line: 0, column: 0 },
                    text: "\"123\"".to_string(),
                },
            ]);
            assert_eq!(scan("\"123\"a").unwrap_all(), vec![
                Token {
                    kind: TokenKind::String("123".to_string()),
                    position: Position { line: 0, column: 0 },
                    text: "\"123\"".to_string(),
                },
                Token {
                    kind: TokenKind::Identifier("a".to_string()),
                    position: Position { line: 0, column: 5 },
                    text: "a".to_string(),
                }
            ]);
        }

        #[test]
        fn empty_string() {
            assert_eq!(scan("\"\"").unwrap_all(), vec![
                Token {
                    kind: TokenKind::String("".to_string()),
                    position: Position { line: 0, column: 0 },
                    text: "\"\"".to_string(),
                }
            ]);
            assert_eq!(scan("\"\"a").unwrap_all(), vec![
                Token {
                    kind: TokenKind::String("".to_string()),
                    position: Position { line: 0, column: 0 },
                    text: "\"\"".to_string(),
                },
                Token {
                    kind: TokenKind::Identifier("a".to_string()),
                    position: Position { line: 0, column: 2 },
                    text: "a".to_string(),
                }
            ]);
        }

        #[test]
        fn unterminated_string() {
            assert_eq!(
                scan("\"abc"),
                vec![
                    Err(Error::syntax_error(Position { line: 0, column: 4 }, "Expect \""))
                ]
            );
        }
    }

    #[allow(non_snake_case)]
    mod punctuation {
        use crate::lexer::tests::UnwrapAll;
        use crate::lexer::{scan, PunctuationKind};
        use crate::token::{Position, Token, TokenKind};

        macro_rules! punctuation_test {
            ($kind: ident, $punctuation: literal) => {
                #[test]
                fn $kind() {
                    assert_eq!(scan($punctuation).unwrap_all(), vec![
                        Token {
                            kind: TokenKind::Punctuation(PunctuationKind::$kind),
                            position: Position { line: 0, column: 0 },
                            text: $punctuation.to_string(),
                        },
                    ]);
                    assert_eq!(scan(format!("{}{}", $punctuation, "a").as_str()).unwrap_all(), vec![
                        Token {
                            kind: TokenKind::Punctuation(PunctuationKind::$kind),
                            position: Position { line: 0, column: 0 },
                            text: $punctuation.to_string(),
                        },
                        Token {
                            kind: TokenKind::Identifier("a".to_string()),
                            position: Position { line: 0, column: $punctuation.len() },
                            text: "a".to_string(),
                        },
                    ]);
                }
            }
        }

        punctuation_test!(Plus, "+");
        punctuation_test!(Minus, "-");
        punctuation_test!(Asterisk, "*");
        punctuation_test!(Slash, "/");
        punctuation_test!(LeftParen, "(");
        punctuation_test!(RightParen, ")");
        punctuation_test!(LeftBrace, "{");
        punctuation_test!(RightBrace, "}");
        punctuation_test!(SemiColon, ";");
        punctuation_test!(Comma, ",");
        punctuation_test!(Equal, "=");
        punctuation_test!(Exclamation, "!");
        punctuation_test!(AndAnd, "&&");
        punctuation_test!(VerticalLineVerticalLine, "||");
        punctuation_test!(VerticalLine, "|");
        punctuation_test!(Dot, ".");
        punctuation_test!(Colon, ":");
        punctuation_test!(EqualEqual, "==");
        punctuation_test!(ExclamationEqual, "!=");
        punctuation_test!(LeftBracket, "<");
        punctuation_test!(LeftBracketEqual, "<=");
        punctuation_test!(RightBracket, ">");
        punctuation_test!(RightBracketEqual, ">=");
        punctuation_test!(Question, "?");
    }

    mod identifier {
        use crate::lexer::scan;
        use crate::lexer::tests::UnwrapAll;
        use crate::token::{Position, Token, TokenKind};

        #[test]
        fn bool_true() {
            assert_eq!(scan("true").unwrap_all(), vec![
                Token {
                    kind: TokenKind::Bool(true),
                    position: Position { line: 0, column: 0 },
                    text: "true".to_string(),
                }
            ]);
            assert_eq!(scan("truea").unwrap_all(), vec![
                Token {
                    kind: TokenKind::Identifier("truea".to_string()),
                    position: Position { line: 0, column: 0 },
                    text: "truea".to_string(),
                }
            ]);
        }

        #[test]
        fn bool_false() {
            assert_eq!(scan("false").unwrap_all(), vec![
                Token {
                    kind: TokenKind::Bool(false),
                    position: Position { line: 0, column: 0 },
                    text: "false".to_string(),
                }
            ]);
            assert_eq!(scan("falsea").unwrap_all(), vec![
                Token {
                    kind: TokenKind::Identifier("falsea".to_string()),
                    position: Position { line: 0, column: 0 },
                    text: "falsea".to_string(),
                }
            ]);
        }
    }
}