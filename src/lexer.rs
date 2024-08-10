use crate::punctuator_kind::PunctuatorKind;
use crate::token::Token;

pub fn scan(mut input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    while !input.is_empty() {
        let (new_input, token) = scan_token(input);
        if let Some(token) = token {
            tokens.push(token);
            input = new_input;
        } else if input == new_input {
            break;
        } else {
            input = new_input;
        }
    }
    tokens
}

fn scan_token(input: &str) -> (&str, Option<Token>) {
    let mut chars = input.chars();

    match chars.next() {
        Some(' ' | '\n' | '\r' | '\t') => (chars.as_str(), None),
        Some('+') => (chars.as_str(), Some(Token::Punctuator(PunctuatorKind::Plus))),
        Some('-') => (chars.as_str(), Some(Token::Punctuator(PunctuatorKind::Minus))),
        Some('*') => (chars.as_str(), Some(Token::Punctuator(PunctuatorKind::Multiply))),
        Some('/') => (chars.as_str(), Some(Token::Punctuator(PunctuatorKind::Divide))),
        Some('(') => (chars.as_str(), Some(Token::Punctuator(PunctuatorKind::LeftParen))),
        Some(')') => (chars.as_str(), Some(Token::Punctuator(PunctuatorKind::RightParen))),
        Some('{') => (chars.as_str(), Some(Token::Punctuator(PunctuatorKind::LeftBrace))),
        Some('}') => (chars.as_str(), Some(Token::Punctuator(PunctuatorKind::RightBrace))),
        Some(';') => (chars.as_str(), Some(Token::Punctuator(PunctuatorKind::SemiColon))),
        Some(',') => (chars.as_str(), Some(Token::Punctuator(PunctuatorKind::Comma))),
        Some('=') => (chars.as_str(), Some(Token::Punctuator(PunctuatorKind::Equal))),
        Some('0'..='9') => scan_number(input),
        Some('a'..='z' | 'A'..='Z' | '_') => scan_identifier(input),
        _ => (input, None)
    }
}

fn scan_number(mut input: &str) -> (&str, Option<Token>) {
    let mut digits: Vec<char> = vec![];
    let mut has_dot = false;

    loop {
        let mut chars = input.chars();
        match chars.next() {
            Some(d @ '0'..='9') => {
                digits.push(d);
                input = chars.as_str();
            }
            Some('.') => {
                if has_dot {
                    break;
                }
                match chars.next() {
                    Some(d @ '0'..='9') => {
                        digits.push('.');
                        has_dot = true;
                        digits.push(d);
                        input = chars.as_str();
                    }
                    _ => break,
                }
            }
            _ => break,
        }
    }

    if digits.is_empty() {
        return (input, None);
    }
    if digits.last() == Some(&'.') {
        return (input, None);
    }
    if digits.first() == Some(&'.') {
        return (input, None);
    }

    (input, Some(Token::Number(digits.iter().collect::<String>().parse().unwrap())))
}

fn scan_identifier(mut input: &str) -> (&str, Option<Token>) {
    let mut identifier_name: Vec<char> = vec![];

    match input.chars().next() {
        Some(c @ ('a'..='z' | 'A'..='Z' | '_')) => {
            identifier_name.push(c);

            let mut chars = input.chars();
            chars.next();
            input = chars.as_str();
        }
        _ => return (input, None)
    }

    while let Some(c @ ('a'..='z' | 'A'..='Z' | '0'..='9' | '_')) = input.chars().next() {
        identifier_name.push(c);

        let mut chars = input.chars();
        chars.next();
        input = chars.as_str();
    }

    (input, Some(Token::Identifier(identifier_name.iter().collect())))
}


#[cfg(test)]
mod tests {
    use crate::lexer::scan_token;

    #[test]
    fn test_whitespace() {
        assert_eq!(scan_token(" "), ("", None));
        assert_eq!(scan_token("\t"), ("", None));
        assert_eq!(scan_token("\r"), ("", None));
        assert_eq!(scan_token("\n"), ("", None));
        assert_eq!(scan_token(" a"), ("a", None));
        assert_eq!(scan_token("\ta"), ("a", None));
        assert_eq!(scan_token("\ra"), ("a", None));
        assert_eq!(scan_token("\na"), ("a", None));
    }

    mod tokens {
        use crate::lexer::{PunctuatorKind, scan, Token};

        #[test]
        fn test_scan_tokens() {
            assert_eq!(
                scan("function(123)"),
                vec![
                    Token::Identifier("function".to_string()),
                    Token::Punctuator(PunctuatorKind::LeftParen),
                    Token::Number(123f64),
                    Token::Punctuator(PunctuatorKind::RightParen),
                ]
            );
        }
    }

    mod number {
        use crate::lexer::{scan_token, Token};

        #[test]
        fn test_number_decimal_int() {
            assert_eq!(scan_token("123"), ("", Some(Token::Number(123f64))));
            assert_eq!(scan_token("123a"), ("a", Some(Token::Number(123f64))));
        }

        #[test]
        fn test_number_decimal_int_starting_with_zero() {
            assert_eq!(scan_token("0"), ("", Some(Token::Number(0f64))));
            assert_eq!(scan_token("0a"), ("a", Some(Token::Number(0f64))));
            assert_eq!(scan_token("0123"), ("", Some(Token::Number(123f64))));
        }

        #[test]
        fn test_number_decimal_float() {
            assert_eq!(scan_token("123.456"), ("", Some(Token::Number(123.456f64))));
            assert_eq!(scan_token("123.456a"), ("a", Some(Token::Number(123.456f64))));
        }

        #[test]
        fn test_number_decimal_float_starting_with_zero() {
            assert_eq!(scan_token("0.456"), ("", Some(Token::Number(0.456f64))));
            assert_eq!(scan_token("0.456a"), ("a", Some(Token::Number(0.456f64))));
        }

        #[test]
        fn test_number_decimal_ending_with_dot() {
            assert_eq!(scan_token("123."), (".", Some(Token::Number(123f64))));
            assert_eq!(scan_token("0."), (".", Some(Token::Number(0f64))));
            assert_eq!(scan_token("123.a"), (".a", Some(Token::Number(123f64))));
            assert_eq!(scan_token("0.a"), (".a", Some(Token::Number(0f64))));
        }

        #[test]
        fn test_number_decimal_with_multiple_dots() {
            assert_eq!(scan_token("123.456.789"), (".789", Some(Token::Number(123.456f64))));
        }
    }

    mod punctuator {
        use crate::lexer::{PunctuatorKind, scan_token, Token};

        #[test]
        fn test_paren_open() {
            assert_eq!(scan_token("("), ("", Some(Token::Punctuator(PunctuatorKind::LeftParen))));
            assert_eq!(scan_token("(a"), ("a", Some(Token::Punctuator(PunctuatorKind::LeftParen))));
        }

        #[test]
        fn test_paren_close() {
            assert_eq!(scan_token(")"), ("", Some(Token::Punctuator(PunctuatorKind::RightParen))));
            assert_eq!(scan_token(")a"), ("a", Some(Token::Punctuator(PunctuatorKind::RightParen))));
        }

        #[test]
        fn test_brace_open() {
            assert_eq!(scan_token("{"), ("", Some(Token::Punctuator(PunctuatorKind::LeftBrace))));
            assert_eq!(scan_token("{a"), ("a", Some(Token::Punctuator(PunctuatorKind::LeftBrace))));
        }

        #[test]
        fn test_brace_close() {
            assert_eq!(scan_token("}"), ("", Some(Token::Punctuator(PunctuatorKind::RightBrace))));
            assert_eq!(scan_token("}a"), ("a", Some(Token::Punctuator(PunctuatorKind::RightBrace))));
        }

        #[test]
        fn test_semicolon() {
            assert_eq!(scan_token(";"), ("", Some(Token::Punctuator(PunctuatorKind::SemiColon))));
            assert_eq!(scan_token(";a"), ("a", Some(Token::Punctuator(PunctuatorKind::SemiColon))));
        }
    }
}