use crate::punctuator_kind::PunctuatorKind;
use crate::token::Token;

pub fn scan(input: &str) -> Result<Vec<Token>, String> {
    let mut chars = input.chars().collect::<Vec<char>>();
    let mut tokens = Vec::new();
    while !chars.is_empty() { 
        if let Some(token) = scan_token(&mut chars)? {
            tokens.push(token)
        }
    }
    Ok(tokens)
}

fn scan_token(chars: &mut Vec<char>) -> Result<Option<Token>, String> {
    match chars.first() {
        Some(' ' | '\n' | '\r' | '\t') => {
            chars.remove(0);
            Ok(None)
        }
        Some('+') => {
            chars.remove(0);
            Ok(Some(Token::Punctuator(PunctuatorKind::Plus)))
        }
        Some('-') => {
            chars.remove(0);
            Ok(Some(Token::Punctuator(PunctuatorKind::Minus)))
        }
        Some('*') => {
            chars.remove(0);
            Ok(Some(Token::Punctuator(PunctuatorKind::Multiply)))
        }
        Some('/') => {
            chars.remove(0);
            Ok(Some(Token::Punctuator(PunctuatorKind::Divide)))
        }
        Some('(') => {
            chars.remove(0);
            Ok(Some(Token::Punctuator(PunctuatorKind::LeftParen)))
        }
        Some(')') => {
            chars.remove(0);
            Ok(Some(Token::Punctuator(PunctuatorKind::RightParen)))
        }
        Some('{') => {
            chars.remove(0);
            Ok(Some(Token::Punctuator(PunctuatorKind::LeftBrace)))
        }
        Some('}') => {
            chars.remove(0);
            Ok(Some(Token::Punctuator(PunctuatorKind::RightBrace)))
        }
        Some(';') => {
            chars.remove(0);
            Ok(Some(Token::Punctuator(PunctuatorKind::SemiColon)))
        }
        Some(',') => {
            chars.remove(0);
            Ok(Some(Token::Punctuator(PunctuatorKind::Comma)))
        }
        Some('=') => {
            chars.remove(0);
            Ok(Some(Token::Punctuator(PunctuatorKind::Equal)))
        }
        Some('!') => {
            chars.remove(0);
            Ok(Some(Token::Punctuator(PunctuatorKind::LogicalNot)))
        }
        Some('&') => {
            if matches!(chars.get(1), Some('&')) {
                chars.remove(0);
                chars.remove(0);
                Ok(Some(Token::Punctuator(PunctuatorKind::LogicalAnd)))
            } else {
                Err("Unexpected character".to_string())
            }
        }
        Some('|') => {
            if matches!(chars.get(1), Some('|')) {
                chars.remove(0);
                chars.remove(0);
                Ok(Some(Token::Punctuator(PunctuatorKind::LogicalOr)))
            } else {
                Err("Unexpected character".to_string())
            }
        }
        Some('0'..='9') => scan_number(chars),
        Some('"') => scan_string(chars),
        Some('a'..='z' | 'A'..='Z' | '_') => scan_identifier(chars),
        None => Ok(None),
        _ => Err("Unexpected character".to_string())
    }
}

fn scan_number(chars: &mut Vec<char>) -> Result<Option<Token>, String> {
    let mut digits: Vec<char> = vec![];
    let mut has_dot = false;

    loop {
        match chars.first() {
            Some(&d @ '0'..='9') => {
                digits.push(d);
                chars.remove(0);
            }
            Some('.') => {
                if digits.is_empty() {
                    return Ok(None);
                }
                if has_dot {
                    break;
                }
                match chars.get(1) {
                    Some(&d @ '0'..='9') => {
                        digits.push('.');
                        has_dot = true;
                        digits.push(d);
                        chars.remove(0);
                        chars.remove(0);
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
        Ok(Some(Token::Number(digits.iter().collect::<String>().parse().unwrap())))
    }
}

fn scan_string(chars: &mut Vec<char>) -> Result<Option<Token>, String> {
    let mut string: Vec<char> = vec![];

    if !matches!(chars.first(), Some('"')) {
        return Ok(None);
    }
    chars.remove(0);

    loop {
        match chars.first() {
            Some('"') => {
                chars.remove(0);
                return Ok(Some(Token::String(string.iter().collect::<String>())));
            }
            Some(&c) => {
                string.push(c);
                chars.remove(0);
            }
            None => return Err("Unexpected end of input".to_string())
        }
    }
}

fn scan_identifier(chars: &mut Vec<char>) -> Result<Option<Token>, String> {
    let mut identifier_name: Vec<char> = vec![];

    match chars.first() {
        Some(&c @ ('a'..='z' | 'A'..='Z' | '_')) => {
            identifier_name.push(c);
            chars.remove(0);
        }
        _ => return Ok(None)
    }

    while let Some(&c @ ('a'..='z' | 'A'..='Z' | '0'..='9' | '_')) = chars.first() {
        identifier_name.push(c);
        chars.remove(0);
    }

    let name = identifier_name.iter().collect::<String>();

    match name.as_str() {
        "true" => Ok(Some(Token::Bool(true))),
        "false" => Ok(Some(Token::Bool(false))),
        _ => Ok(Some(Token::Identifier(name)))
    }
}


#[cfg(test)]
mod tests {
    use crate::lexer::scan_token;
    use crate::token::Token;

    fn assert(input: &str, expected_token: Result<Option<Token>, String>, expected_remaining: &str) {
        let mut chars = input.chars().collect();
        let token = scan_token(&mut chars);
        assert_eq!(token, expected_token);
        assert_eq!(chars.iter().collect::<String>(), expected_remaining);
    }

    #[test]
    fn test_whitespace() {
        assert(" ", Ok(None), "");
        assert("\t", Ok(None), "");
        assert("\r", Ok(None), "");
        assert("\n", Ok(None), "");
        assert(" a", Ok(None), "a");
        assert("\ta", Ok(None), "a");
        assert("\ra", Ok(None), "a");
        assert("\na", Ok(None), "a");
    }

    mod tokens {
        use crate::lexer::{PunctuatorKind, scan, Token};

        #[test]
        fn test_scan_tokens() {
            assert_eq!(
                scan("function(123)"),
                Ok(vec![
                    Token::Identifier("function".to_string()),
                    Token::Punctuator(PunctuatorKind::LeftParen),
                    Token::Number(123f64),
                    Token::Punctuator(PunctuatorKind::RightParen),
                ])
            );
        }
    }

    mod number {
        use crate::lexer::tests::assert;
        use crate::lexer::Token;

        #[test]
        fn test_number_decimal_int() {
            assert("123", Ok(Some(Token::Number(123f64))), "");
            assert("123a", Ok(Some(Token::Number(123f64))), "a");
        }

        #[test]
        fn test_number_decimal_int_starting_with_zero() {
            assert("0", Ok(Some(Token::Number(0f64))), "");
            assert("0a", Ok(Some(Token::Number(0f64))), "a");
            assert("0123", Ok(Some(Token::Number(123f64))), "");
        }

        #[test]
        fn test_number_decimal_float() {
            assert("123.456", Ok(Some(Token::Number(123.456f64))), "");
            assert("123.456a", Ok(Some(Token::Number(123.456f64))), "a");
        }

        #[test]
        fn test_number_decimal_float_starting_with_zero() {
            assert("0.456", Ok(Some(Token::Number(0.456f64))), "");
            assert("0.456a", Ok(Some(Token::Number(0.456f64))), "a");
        }

        #[test]
        fn test_number_decimal_ending_with_dot() {
            assert("123.", Ok(Some(Token::Number(123f64))), ".");
            assert("0.", Ok(Some(Token::Number(0f64))), ".");
            assert("123.a", Ok(Some(Token::Number(123f64))), ".a");
            assert("0.a", Ok(Some(Token::Number(0f64))), ".a");
        }

        #[test]
        fn test_number_decimal_with_multiple_dots() {
            assert("123.456.789", Ok(Some(Token::Number(123.456f64))), ".789");
        }
    }

    mod string {
        use crate::lexer::tests::assert;
        use crate::lexer::Token;

        #[test]
        fn string() {
            assert("\"123\"", Ok(Some(Token::String("123".to_string()))), "");
            assert("\"123\"a", Ok(Some(Token::String("123".to_string()))), "a");
        }

        #[test]
        fn empty_string() {
            assert("\"\"", Ok(Some(Token::String("".to_string()))), "");
            assert("\"\"a", Ok(Some(Token::String("".to_string()))), "a");
        }
    }

    mod punctuator {
        use crate::lexer::{PunctuatorKind, Token};
        use crate::lexer::tests::assert;

        #[test]
        fn left_paren() {
            assert("(", Ok(Some(Token::Punctuator(PunctuatorKind::LeftParen))), "");
            assert("(a", Ok(Some(Token::Punctuator(PunctuatorKind::LeftParen))), "a");
        }

        #[test]
        fn right_paren() {
            assert(")", Ok(Some(Token::Punctuator(PunctuatorKind::RightParen))), "");
            assert(")a", Ok(Some(Token::Punctuator(PunctuatorKind::RightParen))), "a");
        }

        #[test]
        fn left_brace() {
            assert("{", Ok(Some(Token::Punctuator(PunctuatorKind::LeftBrace))), "");
            assert("{a", Ok(Some(Token::Punctuator(PunctuatorKind::LeftBrace))), "a");
        }

        #[test]
        fn right_brace() {
            assert("}", Ok(Some(Token::Punctuator(PunctuatorKind::RightBrace))), "");
            assert("}a", Ok(Some(Token::Punctuator(PunctuatorKind::RightBrace))), "a");
        }

        #[test]
        fn semicolon() {
            assert(";", Ok(Some(Token::Punctuator(PunctuatorKind::SemiColon))), "");
            assert(";a", Ok(Some(Token::Punctuator(PunctuatorKind::SemiColon))), "a");
        }

        #[test]
        fn logical_not() {
            assert("!", Ok(Some(Token::Punctuator(PunctuatorKind::LogicalNot))), "");
            assert("!a", Ok(Some(Token::Punctuator(PunctuatorKind::LogicalNot))), "a");
        }

        #[test]
        fn logical_and() {
            assert("&&", Ok(Some(Token::Punctuator(PunctuatorKind::LogicalAnd))), "");
            assert("&&a", Ok(Some(Token::Punctuator(PunctuatorKind::LogicalAnd))), "a");
            assert("&&&", Ok(Some(Token::Punctuator(PunctuatorKind::LogicalAnd))), "&");
        }

        #[test]
        fn logical_or() {
            assert("||", Ok(Some(Token::Punctuator(PunctuatorKind::LogicalOr))), "");
            assert("||a", Ok(Some(Token::Punctuator(PunctuatorKind::LogicalOr))), "a");
            assert("|||", Ok(Some(Token::Punctuator(PunctuatorKind::LogicalOr))), "|");
        }
    }

    mod identifier {
        use crate::lexer::tests::assert;
        use crate::token::Token;

        #[test]
        fn bool_true() {
            assert("true", Ok(Some(Token::Bool(true))), "");
            assert("trueq", Ok(Some(Token::Identifier("trueq".to_string()))), "");
        }

        #[test]
        fn bool_false() {
            assert("false", Ok(Some(Token::Bool(false))), "");
            assert("falseq", Ok(Some(Token::Identifier("falseq".to_string()))), "");
        }
    }
}