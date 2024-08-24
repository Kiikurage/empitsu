use crate::char_iterator::CharIterator;
use crate::error::Error;
use crate::punctuation_kind::PunctuationKind;
use crate::token::{Token, TokenKind};

pub fn scan(input: &str) -> Vec<Result<Token, Error>> {
    let mut chars = CharIterator::new(input);
    let mut tokens = Vec::new();

    loop {
        let token = scan_token(&mut chars);
        tokens.push(token);
        if matches!(tokens.last(), Some(Ok(Token { kind: TokenKind::EndOfInput, .. }))) {
            break;
        }
    }

    tokens
}

fn scan_token(chars: &mut CharIterator) -> Result<Token, Error> {
    while matches!(chars.peek(0), Some(' ' | '\r' | '\t')) {
        chars.next();
    }
    let position = chars.get_position().clone();

    match chars.peek(0) {
        Some('\n') => {
            chars.next();
            Ok(Token {
                kind: TokenKind::LineTerminator,
                position,
                text: "\n".to_string(),
            })
        }
        Some('?') => {
            chars.next();
            Ok(Token {
                kind: TokenKind::Punctuation(PunctuationKind::Question),
                position,
                text: "?".to_string(),
            })
        }
        Some('+') => {
            chars.next();
            Ok(Token {
                kind: TokenKind::Punctuation(PunctuationKind::Plus),
                position,
                text: "+".to_string(),
            })
        }
        Some('-') => {
            chars.next();
            Ok(Token {
                kind: TokenKind::Punctuation(PunctuationKind::Minus),
                position,
                text: "-".to_string(),
            })
        }
        Some('*') => {
            chars.next();
            Ok(Token {
                kind: TokenKind::Punctuation(PunctuationKind::Asterisk),
                position,
                text: "*".to_string(),
            })
        }
        Some('/') => {
            match chars.peek(1) {
                Some('/') => {
                    chars.next();
                    chars.next();

                    let mut body = String::new();
                    while let Some(&c) = chars.peek(0) {
                        if c == '\n' {
                            break;
                        }
                        body.push(c);
                        chars.next();
                    }
                    // Ok(Token::Comment(body))
                    scan_token(chars)
                }
                Some('*') => {
                    chars.next();
                    chars.next();

                    let mut body = String::new();
                    while let Some(&c) = chars.peek(0) {
                        if c == '*' && matches!(chars.peek(1), Some('/')) {
                            chars.next();
                            chars.next();
                            break;
                        }
                        body.push(c);
                        chars.next();
                    }
                    // Ok(Token::Comment(body))
                    scan_token(chars)
                }
                _ => {
                    chars.next();
                    Ok(Token {
                        kind: TokenKind::Punctuation(PunctuationKind::Slash),
                        position,
                        text: "/".to_string(),
                    })
                }
            }
        }
        Some('(') => {
            chars.next();
            Ok(Token {
                kind: TokenKind::Punctuation(PunctuationKind::LeftParen),
                position,
                text: "(".to_string(),
            })
        }
        Some(')') => {
            chars.next();
            Ok(Token {
                kind: TokenKind::Punctuation(PunctuationKind::RightParen),
                position,
                text: ")".to_string(),
            })
        }
        Some('{') => {
            chars.next();
            Ok(Token {
                kind: TokenKind::Punctuation(PunctuationKind::LeftBrace),
                position,
                text: "{".to_string(),
            })
        }
        Some('}') => {
            chars.next();
            Ok(Token {
                kind: TokenKind::Punctuation(PunctuationKind::RightBrace),
                position,
                text: "}".to_string(),
            })
        }
        Some(';') => {
            chars.next();
            Ok(Token {
                kind: TokenKind::Punctuation(PunctuationKind::SemiColon),
                position,
                text: ";".to_string(),
            })
        }
        Some(':') => {
            chars.next();
            Ok(Token {
                kind: TokenKind::Punctuation(PunctuationKind::Colon),
                position,
                text: ":".to_string(),
            })
        }
        Some(',') => {
            chars.next();
            Ok(Token {
                kind: TokenKind::Punctuation(PunctuationKind::Comma),
                position,
                text: ",".to_string(),
            })
        }
        Some('=') => {
            if matches!(chars.peek(1), Some('=')) {
                chars.next();
                chars.next();
                Ok(Token {
                    kind: TokenKind::Punctuation(PunctuationKind::EqualEqual),
                    position,
                    text: "==".to_string(),
                })
            } else {
                chars.next();
                Ok(Token {
                    kind: TokenKind::Punctuation(PunctuationKind::Equal),
                    position,
                    text: "=".to_string(),
                })
            }
        }
        Some('!') => {
            if matches!(chars.peek(1), Some('=')) {
                chars.next();
                chars.next();
                Ok(Token {
                    kind: TokenKind::Punctuation(PunctuationKind::ExclamationEqual),
                    position,
                    text: "!=".to_string(),
                })
            } else {
                chars.next();
                Ok(Token {
                    kind: TokenKind::Punctuation(PunctuationKind::Exclamation),
                    position,
                    text: "!".to_string(),
                })
            }
        }
        Some('<') => {
            if matches!(chars.peek(1), Some('=')) {
                chars.next();
                chars.next();
                Ok(Token {
                    kind: TokenKind::Punctuation(PunctuationKind::LeftChevronEqual),
                    position,
                    text: "<=".to_string(),
                })
            } else {
                chars.next();
                Ok(Token {
                    kind: TokenKind::Punctuation(PunctuationKind::LeftChevron),
                    position,
                    text: "<".to_string(),
                })
            }
        }
        Some('>') => {
            if matches!(chars.peek(1), Some('=')) {
                chars.next();
                chars.next();
                Ok(Token {
                    kind: TokenKind::Punctuation(PunctuationKind::RightChevronEqual),
                    position,
                    text: ">=".to_string(),
                })
            } else {
                chars.next();
                Ok(Token {
                    kind: TokenKind::Punctuation(PunctuationKind::RightChevron),
                    position,
                    text: ">".to_string(),
                })
            }
        }
        Some('&') => {
            if matches!(chars.peek(1), Some('&')) {
                chars.next();
                chars.next();
                Ok(Token {
                    kind: TokenKind::Punctuation(PunctuationKind::AndAnd),
                    position,
                    text: "&&".to_string(),
                })
            } else {
                chars.next();
                Err(Error::syntax_error(position, "Unexpected character"))
            }
        }
        Some('|') => {
            if matches!(chars.peek(1), Some('|')) {
                chars.next();
                chars.next();
                Ok(Token {
                    kind: TokenKind::Punctuation(PunctuationKind::VerticalLineVerticalLine),
                    position,
                    text: "||".to_string(),
                })
            } else {
                chars.next();
                Ok(Token {
                    kind: TokenKind::Punctuation(PunctuationKind::VerticalLine),
                    position,
                    text: "|".to_string(),
                })
            }
        }
        Some('.') => {
            chars.next();
            Ok(Token {
                kind: TokenKind::Punctuation(PunctuationKind::Dot),
                position,
                text: ".".to_string(),
            })
        }
        Some('0'..='9') => scan_number(chars),
        Some('"') => scan_string(chars),
        Some('a'..='z' | 'A'..='Z' | '_') => scan_identifier(chars),
        Some(other) => {
            let other = other.clone();
            chars.next();
            Err(Error::syntax_error(position, format!("Unexpected character \"{}\"", other)))
        },
        None => Ok(Token {
            kind: TokenKind::EndOfInput,
            position,
            text: "".to_string(),
        }),
    }
}

fn scan_number(chars: &mut CharIterator) -> Result<Token, Error> {
    let mut digits: Vec<char> = vec![];
    let mut has_dot = false;
    let position = chars.get_position().clone();

    loop {
        match chars.peek(0) {
            Some(&d @ '0'..='9') => {
                digits.push(d);
                chars.next();
            }
            Some('.') => {
                if digits.is_empty() {
                    return Err(Error::unexpected_token(chars.get_position().clone(), "."));
                }
                if has_dot {
                    break;
                }
                match chars.peek(1) {
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
        Err(Error::syntax_error(position, "invalid number format"))
    } else {
        Ok(Token {
            kind: TokenKind::Number(digits.iter().collect::<String>().parse().unwrap()),
            position,
            text: digits.iter().collect::<String>(),
        })
    }
}

fn scan_string(chars: &mut CharIterator) -> Result<Token, Error> {
    let mut string: Vec<char> = vec![];
    let position = chars.get_position().clone();

    if !matches!(chars.peek(0), Some('"')) {
        return Err(Error::unexpected_token(position, "\""));
    }
    chars.next();

    loop {
        match chars.peek(0) {
            Some('"') => {
                chars.next();
                return Ok(Token {
                    kind: TokenKind::String(string.iter().collect::<String>()),
                    position,
                    text: format!("\"{}\"", string.iter().collect::<String>()),
                });
            }
            Some(&c) => {
                string.push(c);
                chars.next();
            }
            None => return Err(Error::unexpected_token(chars.get_position().clone(), "\"")),
        }
    }
}

fn scan_identifier(chars: &mut CharIterator) -> Result<Token, Error> {
    let mut identifier_name: Vec<char> = vec![];
    let position = chars.get_position().clone();

    match chars.peek(0) {
        Some(&c @ ('a'..='z' | 'A'..='Z' | '_')) => {
            identifier_name.push(c);
            chars.next();
        }
        _ => return Err(Error::syntax_error(position, "Invalid identifier name")),
    }

    while let Some(&c @ ('a'..='z' | 'A'..='Z' | '0'..='9' | '_')) = chars.peek(0) {
        identifier_name.push(c);
        chars.next();
    }

    let name = identifier_name.iter().collect::<String>();

    match name.as_str() {
        "true" => Ok(Token { kind: TokenKind::Bool(true), position, text: "true".to_string() }),
        "false" => Ok(Token { kind: TokenKind::Bool(false), position, text: "false".to_string() }),
        _ => Ok(Token { text: name.to_string(), kind: TokenKind::Identifier(name), position }),
    }
}

#[cfg(test)]
mod tests {
    use crate::error::Error;
    use crate::lexer::scan;
    use crate::position::Position;
    use crate::punctuation_kind::PunctuationKind;
    use crate::token::Token;

    fn test(input: &str, expected: Vec<Result<Token, Error>>) {
        let tokens = scan(input);

        assert_eq!(tokens, expected);
    }

    #[test]
    fn skip_whitespace() {
        test(" ", vec![
            Ok(Token::end_of_input(0, 1)),
        ]);
    }

    #[test]
    fn skip_tab() {
        test("\t", vec![
            Ok(Token::end_of_input(0, 1)),
        ]);
    }

    #[test]
    fn skip_carriage_return() {
        test("\r", vec![
            Ok(Token::end_of_input(0, 1)),
        ]);
    }

    #[test]
    fn identifier() {
        test("a", vec![
            Ok(Token::identifier(0, 0, "a")),
            Ok(Token::end_of_input(0, 1)),
        ]);
    }

    #[test]
    fn identifier_after_whitespace() {
        test(" a", vec![
            Ok(Token::identifier(0, 1, "a")),
            Ok(Token::end_of_input(0, 2)),
        ]);
    }

    #[test]
    fn multiple_identifiers() {
        test("a b", vec![
            Ok(Token::identifier(0, 0, "a")),
            Ok(Token::identifier(0, 2, "b")),
            Ok(Token::end_of_input(0, 3)),
        ]);
    }

    #[test]
    fn integer() {
        test("123", vec![
            Ok(Token::number(0, 0, 123f64, "123")),
            Ok(Token::end_of_input(0, 3)),
        ]);
    }

    #[test]
    fn zero() {
        test("0", vec![
            Ok(Token::number(0, 0, 0f64, "0")),
            Ok(Token::end_of_input(0, 1)),
        ]);
    }

    #[test]
    fn integer_with_leading_zero() {
        test("0123", vec![
            Ok(Token::number(0, 0, 123f64, "0123")),
            Ok(Token::end_of_input(0, 4)),
        ]);
    }

    #[test]
    fn integer_with_trailing_dot() {
        test("123.", vec![
            Ok(Token::number(0, 0, 123f64, "123")),
            Ok(Token::punctuation(0, 3, PunctuationKind::Dot, ".")),
            Ok(Token::end_of_input(0, 4)),
        ]);
    }

    #[test]
    fn integer_with_plus() {
        test("+123", vec![
            Ok(Token::punctuation(0, 0, PunctuationKind::Plus, "+")),
            Ok(Token::number(0, 1, 123f64, "123")),
            Ok(Token::end_of_input(0, 4)),
        ]);
    }

    #[test]
    fn integer_with_minus() {
        test("-123", vec![
            Ok(Token::punctuation(0, 0, PunctuationKind::Minus, "-")),
            Ok(Token::number(0, 1, 123f64, "123")),
            Ok(Token::end_of_input(0, 4)),
        ]);
    }

    #[test]
    fn float() {
        test("123.456", vec![
            Ok(Token::number(0, 0, 123.456f64, "123.456")),
            Ok(Token::end_of_input(0, 7)),
        ]);
    }

    #[test]
    fn float_less_than_1() {
        test("0.456", vec![
            Ok(Token::number(0, 0, 0.456f64, "0.456")),
            Ok(Token::end_of_input(0, 5)),
        ]);
    }

    #[test]
    fn float_starting_with_dot() {
        test(".456", vec![
            Ok(Token::punctuation(0, 0, PunctuationKind::Dot, ".")),
            Ok(Token::number(0, 1, 456f64, "456")),
            Ok(Token::end_of_input(0, 4)),
        ]);
    }

    #[test]
    fn float_with_multiple_dots() {
        test("123.456.789", vec![
            Ok(Token::number(0, 0, 123.456f64, "123.456")),
            Ok(Token::punctuation(0, 7, PunctuationKind::Dot, ".")),
            Ok(Token::number(0, 8, 789f64, "789")),
            Ok(Token::end_of_input(0, 11)),
        ]);
    }

    #[test]
    fn string() {
        test("\"ABC\"", vec![
            Ok(Token::string(0, 0, "ABC", "\"ABC\"")),
            Ok(Token::end_of_input(0, 5)),
        ]);
    }

    #[test]
    fn empty_string() {
        test("\"\"", vec![
            Ok(Token::string(0, 0, "", "\"\"")),
            Ok(Token::end_of_input(0, 2)),
        ]);
    }

    #[test]
    fn unterminated_string() {
        test("\"ABC", vec![
            Err(Error::unexpected_token(Position::new(0, 4), "\"")),
            Ok(Token::end_of_input(0, 4)),
        ]);
    }

    #[test]
    fn bool_true() {
        test("true", vec![
            Ok(Token::bool(0, 0, true, "true")),
            Ok(Token::end_of_input(0, 4)),
        ]);
    }

    #[test]
    fn identifier_with_prefix_true() {
        test("trueabc", vec![
            Ok(Token::identifier(0, 0, "trueabc")),
            Ok(Token::end_of_input(0, 7)),
        ]);
    }

    #[test]
    fn bool_false() {
        test("false", vec![
            Ok(Token::bool(0, 0, false, "false")),
            Ok(Token::end_of_input(0, 5)),
        ]);
    }

    #[test]
    fn identifier_with_prefix_false() {
        test("falseabc", vec![
            Ok(Token::identifier(0, 0, "falseabc")),
            Ok(Token::end_of_input(0, 8)),
        ]);
    }

    #[test]
    fn invalid_token() {
        test("[@#$^&%]", vec![
        ]);
    }
}