use crate::char_iterator::CharIterator;
use crate::error::Error;
use crate::punctuation_kind::PunctuationKind;
use crate::token::Token;

pub fn scan(input: &str) -> Vec<Result<Token, Error>> {
    let mut chars = CharIterator::new(input);
    let mut tokens = Vec::new();

    loop {
        let token = scan_token(&mut chars);
        tokens.push(token);
        if matches!(tokens.last(), Some(Ok(Token::EndOfInput(..)))) {
            break;
        }
    };

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
            Ok(Token::line_terminator(position))
        }
        Some('?') => {
            chars.next();
            Ok(Token::punctuation(position, PunctuationKind::Question))
        }
        Some('+') => {
            chars.next();
            Ok(Token::punctuation(position, PunctuationKind::Plus))
        }
        Some('-') => {
            chars.next();
            Ok(Token::punctuation(position, PunctuationKind::Minus))
        }
        Some('*') => {
            chars.next();
            Ok(Token::punctuation(position, PunctuationKind::Asterisk))
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
                    Ok(Token::punctuation(position, PunctuationKind::Slash))
                }
            }
        }
        Some('(') => {
            chars.next();
            Ok(Token::punctuation(position, PunctuationKind::LeftParen))
        }
        Some(')') => {
            chars.next();
            Ok(Token::punctuation(position, PunctuationKind::RightParen))
        }
        Some('{') => {
            chars.next();
            Ok(Token::punctuation(position, PunctuationKind::LeftBrace))
        }
        Some('}') => {
            chars.next();
            Ok(Token::punctuation(position, PunctuationKind::RightBrace))
        }
        Some(';') => {
            chars.next();
            Ok(Token::punctuation(position, PunctuationKind::SemiColon))
        }
        Some(':') => {
            chars.next();
            Ok(Token::punctuation(position, PunctuationKind::Colon))
        }
        Some(',') => {
            chars.next();
            Ok(Token::punctuation(position, PunctuationKind::Comma))
        }
        Some('=') => {
            if matches!(chars.peek(1), Some('=')) {
                chars.next();
                chars.next();
                Ok(Token::punctuation(position, PunctuationKind::EqualEqual))
            } else {
                chars.next();
                Ok(Token::punctuation(position, PunctuationKind::Equal))
            }
        }
        Some('!') => {
            if matches!(chars.peek(1), Some('=')) {
                chars.next();
                chars.next();
                Ok(Token::punctuation(position, PunctuationKind::ExclamationEqual))
            } else {
                chars.next();
                Ok(Token::punctuation(position, PunctuationKind::Exclamation))
            }
        }
        Some('<') => {
            if matches!(chars.peek(1), Some('=')) {
                chars.next();
                chars.next();
                Ok(Token::punctuation(position, PunctuationKind::LeftChevronEqual))
            } else {
                chars.next();
                Ok(Token::punctuation(position, PunctuationKind::LeftChevron))
            }
        }
        Some('>') => {
            if matches!(chars.peek(1), Some('=')) {
                chars.next();
                chars.next();
                Ok(Token::punctuation(position, PunctuationKind::RightChevronEqual))
            } else {
                chars.next();
                Ok(Token::punctuation(position, PunctuationKind::RightChevron))
            }
        }
        Some('&') => {
            if matches!(chars.peek(1), Some('&')) {
                chars.next();
                chars.next();
                Ok(Token::punctuation(position, PunctuationKind::AndAnd))
            } else {
                chars.next();
                Err(Error::unexpected_token(position, "&"))
            }
        }
        Some('|') => {
            if matches!(chars.peek(1), Some('|')) {
                chars.next();
                chars.next();
                Ok(Token::punctuation(position, PunctuationKind::VerticalLineVerticalLine))
            } else {
                chars.next();
                Ok(Token::punctuation(position, PunctuationKind::VerticalLine))
            }
        }
        Some('.') => {
            chars.next();
            Ok(Token::punctuation(position, PunctuationKind::Dot))
        }
        Some('0'..='9') => scan_number(chars),
        Some('"') => scan_string(chars),
        Some('a'..='z' | 'A'..='Z' | '_') => scan_identifier(chars),
        Some(other) => {
            let other = *other;
            chars.next();
            Err(Error::unexpected_token(position, other))
        }
        None => Ok(Token::end_of_input(position)),
    }
}

fn scan_number(chars: &mut CharIterator) -> Result<Token, Error> {
    let mut digits = String::new();
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
        Err(Error::invalid_syntax(position, "invalid number format"))
    } else {
        Ok(Token::number(position, digits.clone(), digits.parse().unwrap()))
    }
}

fn scan_string(chars: &mut CharIterator) -> Result<Token, Error> {
    let mut value = String::new();
    let position = chars.get_position().clone();

    if !matches!(chars.peek(0), Some('"')) {
        return Err(Error::unexpected_token(position, "\""));
    }
    chars.next();

    loop {
        match chars.peek(0) {
            Some('"') => {
                chars.next();
                return Ok(Token::string(position, format!("\"{}\"", value), value));
            }
            Some(&c) => {
                value.push(c);
                chars.next();
            }
            None => return Err(Error::unexpected_token(chars.get_position().clone(), "\"")),
        }
    }
}

fn scan_identifier(chars: &mut CharIterator) -> Result<Token, Error> {
    let mut identifier_name = String::new();
    let position = chars.get_position().clone();

    match chars.peek(0) {
        Some(&c @ ('a'..='z' | 'A'..='Z' | '_')) => {
            identifier_name.push(c);
            chars.next();
        }
        _ => return Err(Error::invalid_syntax(position, "Invalid identifier name")),
    }

    while let Some(&c @ ('a'..='z' | 'A'..='Z' | '0'..='9' | '_')) = chars.peek(0) {
        identifier_name.push(c);
        chars.next();
    }

    match identifier_name.as_str() {
        "true" => Ok(Token::bool(position, "true".to_string(), true)),
        "false" => Ok(Token::bool(position, "false".to_string(), false)),
        _ => Ok(Token::identifier(position, identifier_name)),
    }
}

#[cfg(test)]
mod tests {
    use crate::error::Error;
    use crate::lexer::scan;
    use crate::punctuation_kind::PunctuationKind;
    use crate::token::Token;

    fn test(input: &str, expected: Vec<Result<Token, Error>>) {
        let tokens = scan(input);

        assert_eq!(tokens, expected);
    }

    #[test]
    fn skip_whitespace() {
        test(" ", vec![
            Ok(Token::end_of_input((0, 1))),
        ]);
    }

    #[test]
    fn skip_tab() {
        test("\t", vec![
            Ok(Token::end_of_input((0, 1))),
        ]);
    }

    #[test]
    fn skip_carriage_return() {
        test("\r", vec![
            Ok(Token::end_of_input((0, 1))),
        ]);
    }

    #[test]
    fn identifier() {
        test("a", vec![
            Ok(Token::identifier((0, 0), "a")),
            Ok(Token::end_of_input((0, 1))),
        ]);
    }

    #[test]
    fn identifier_after_whitespace() {
        test(" a", vec![
            Ok(Token::identifier((0, 1), "a")),
            Ok(Token::end_of_input((0, 2))),
        ]);
    }

    #[test]
    fn multiple_identifiers() {
        test("a b", vec![
            Ok(Token::identifier((0, 0), "a")),
            Ok(Token::identifier((0, 2), "b")),
            Ok(Token::end_of_input((0, 3))),
        ]);
    }

    #[test]
    fn integer() {
        test("123", vec![
            Ok(Token::number((0, 0), "123", 123f64)),
            Ok(Token::end_of_input((0, 3))),
        ]);
    }

    #[test]
    fn zero() {
        test("0", vec![
            Ok(Token::number((0, 0), "0", 0f64)),
            Ok(Token::end_of_input((0, 1))),
        ]);
    }

    #[test]
    fn integer_with_leading_zero() {
        test("0123", vec![
            Ok(Token::number((0, 0), "0123", 123f64)),
            Ok(Token::end_of_input((0, 4))),
        ]);
    }

    #[test]
    fn integer_with_trailing_dot() {
        test("123.", vec![
            Ok(Token::number((0, 0), "123", 123f64)),
            Ok(Token::punctuation((0, 3), PunctuationKind::Dot)),
            Ok(Token::end_of_input((0, 4))),
        ]);
    }

    #[test]
    fn integer_with_plus() {
        test("+123", vec![
            Ok(Token::punctuation((0, 0), PunctuationKind::Plus)),
            Ok(Token::number((0, 1), "123", 123f64)),
            Ok(Token::end_of_input((0, 4))),
        ]);
    }

    #[test]
    fn integer_with_minus() {
        test("-123", vec![
            Ok(Token::punctuation((0, 0), PunctuationKind::Minus)),
            Ok(Token::number((0, 1), "123", 123f64)),
            Ok(Token::end_of_input((0, 4))),
        ]);
    }

    #[test]
    fn float() {
        test("123.456", vec![
            Ok(Token::number((0, 0), "123.456", 123.456f64)),
            Ok(Token::end_of_input((0, 7))),
        ]);
    }

    #[test]
    fn float_less_than_1() {
        test("0.456", vec![
            Ok(Token::number((0, 0), "0.456", 0.456f64)),
            Ok(Token::end_of_input((0, 5))),
        ]);
    }

    #[test]
    fn float_starting_with_dot() {
        test(".456", vec![
            Ok(Token::punctuation((0, 0), PunctuationKind::Dot)),
            Ok(Token::number((0, 1), "456", 456f64)),
            Ok(Token::end_of_input((0, 4))),
        ]);
    }

    #[test]
    fn float_with_multiple_dots() {
        test("123.456.789", vec![
            Ok(Token::number((0, 0), "123.456", 123.456f64)),
            Ok(Token::punctuation((0, 7), PunctuationKind::Dot)),
            Ok(Token::number((0, 8), "789", 789f64)),
            Ok(Token::end_of_input((0, 11))),
        ]);
    }

    #[test]
    fn string() {
        test("\"ABC\"", vec![
            Ok(Token::string((0, 0), "\"ABC\"", "ABC")),
            Ok(Token::end_of_input((0, 5))),
        ]);
    }

    #[test]
    fn empty_string() {
        test("\"\"", vec![
            Ok(Token::string((0, 0), "\"\"", "")),
            Ok(Token::end_of_input((0, 2))),
        ]);
    }

    #[test]
    fn unterminated_string() {
        test("\"ABC", vec![
            Err(Error::unexpected_token((0, 4), "\"")),
            Ok(Token::end_of_input((0, 4))),
        ]);
    }

    #[test]
    fn bool_true() {
        test("true", vec![
            Ok(Token::bool((0, 0), "true", true)),
            Ok(Token::end_of_input((0, 4))),
        ]);
    }

    #[test]
    fn identifier_with_prefix_true() {
        test("trueabc", vec![
            Ok(Token::identifier((0, 0), "trueabc")),
            Ok(Token::end_of_input((0, 7))),
        ]);
    }

    #[test]
    fn bool_false() {
        test("false", vec![
            Ok(Token::bool((0, 0), "false", false)),
            Ok(Token::end_of_input((0, 5))),
        ]);
    }

    #[test]
    fn identifier_with_prefix_false() {
        test("falseabc", vec![
            Ok(Token::identifier((0, 0), "falseabc")),
            Ok(Token::end_of_input((0, 8))),
        ]);
    }

    #[test]
    fn invalid_token() {
        test("[@#$^&%]", vec![
            Err(Error::unexpected_token((0, 0), "[")),
            Err(Error::unexpected_token((0, 1), "@")),
            Err(Error::unexpected_token((0, 2), "#")),
            Err(Error::unexpected_token((0, 3), "$")),
            Err(Error::unexpected_token((0, 4), "^")),
            Err(Error::unexpected_token((0, 5), "&")),
            Err(Error::unexpected_token((0, 6), "%")),
            Err(Error::unexpected_token((0, 7), "]")),
            Ok(Token::end_of_input((0, 8))),
        ]);
    }
}