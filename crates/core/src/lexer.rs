use crate::char_iterator::CharIterator;
use crate::error::Error;
use crate::position::pos;
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
    let start = chars.get_position();

    match chars.peek(0) {
        Some('\n') => {
            chars.next();
            let range = start..chars.get_position();
            Ok(Token::line_terminator(range))
        }
        Some('?') => {
            chars.next();
            let range = start..chars.get_position();
            Ok(Token::punctuation(range, PunctuationKind::Question))
        }
        Some('+') => {
            chars.next();
            let range = start..chars.get_position();
            Ok(Token::punctuation(range, PunctuationKind::Plus))
        }
        Some('-') => {
            chars.next();
            let range = start..chars.get_position();
            Ok(Token::punctuation(range, PunctuationKind::Minus))
        }
        Some('*') => {
            chars.next();
            let range = start..chars.get_position();
            Ok(Token::punctuation(range, PunctuationKind::Asterisk))
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
                    let range = start..chars.get_position();
                    Ok(Token::punctuation(range, PunctuationKind::Slash))
                }
            }
        }
        Some('(') => {
            chars.next();
            let range = start..chars.get_position();
            Ok(Token::punctuation(range, PunctuationKind::LeftParen))
        }
        Some(')') => {
            chars.next();
            let range = start..chars.get_position();
            Ok(Token::punctuation(range, PunctuationKind::RightParen))
        }
        Some('{') => {
            chars.next();
            let range = start..chars.get_position();
            Ok(Token::punctuation(range, PunctuationKind::LeftBrace))
        }
        Some('}') => {
            chars.next();
            let range = start..chars.get_position();
            Ok(Token::punctuation(range, PunctuationKind::RightBrace))
        }
        Some(';') => {
            chars.next();
            let range = start..chars.get_position();
            Ok(Token::punctuation(range, PunctuationKind::SemiColon))
        }
        Some(':') => {
            chars.next();
            let range = start..chars.get_position();
            Ok(Token::punctuation(range, PunctuationKind::Colon))
        }
        Some(',') => {
            chars.next();
            let range = start..chars.get_position();
            Ok(Token::punctuation(range, PunctuationKind::Comma))
        }
        Some('=') => {
            if matches!(chars.peek(1), Some('=')) {
                chars.next();
                chars.next();
                let range = start..chars.get_position();
                Ok(Token::punctuation(range, PunctuationKind::EqualEqual))
            } else {
                chars.next();
                let range = start..chars.get_position();
                Ok(Token::punctuation(range, PunctuationKind::Equal))
            }
        }
        Some('!') => {
            if matches!(chars.peek(1), Some('=')) {
                chars.next();
                chars.next();
                let range = start..chars.get_position();
                Ok(Token::punctuation(range, PunctuationKind::ExclamationEqual))
            } else {
                chars.next();
                let range = start..chars.get_position();
                Ok(Token::punctuation(range, PunctuationKind::Exclamation))
            }
        }
        Some('<') => {
            if matches!(chars.peek(1), Some('=')) {
                chars.next();
                chars.next();
                let range = start..chars.get_position();
                Ok(Token::punctuation(range, PunctuationKind::LeftChevronEqual))
            } else {
                chars.next();
                let range = start..chars.get_position();
                Ok(Token::punctuation(range, PunctuationKind::LeftChevron))
            }
        }
        Some('>') => {
            if matches!(chars.peek(1), Some('=')) {
                chars.next();
                chars.next();
                let range = start..chars.get_position();
                Ok(Token::punctuation(range, PunctuationKind::RightChevronEqual))
            } else {
                chars.next();
                let range = start..chars.get_position();
                Ok(Token::punctuation(range, PunctuationKind::RightChevron))
            }
        }
        Some('&') => {
            if matches!(chars.peek(1), Some('&')) {
                chars.next();
                chars.next();
                let range = start..chars.get_position();
                Ok(Token::punctuation(range, PunctuationKind::AndAnd))
            } else {
                chars.next();
                let range = start..chars.get_position();
                Err(Error::unexpected_token(range, "&"))
            }
        }
        Some('|') => {
            if matches!(chars.peek(1), Some('|')) {
                chars.next();
                chars.next();
                let range = start..chars.get_position();
                Ok(Token::punctuation(range, PunctuationKind::VerticalLineVerticalLine))
            } else {
                chars.next();
                let range = start..chars.get_position();
                Ok(Token::punctuation(range, PunctuationKind::VerticalLine))
            }
        }
        Some('.') => {
            chars.next();
            let range = start..chars.get_position();
            Ok(Token::punctuation(range, PunctuationKind::Dot))
        }
        Some('0'..='9') => scan_number(chars),
        Some('"') => scan_string(chars),
        Some('a'..='z' | 'A'..='Z' | '_') => scan_identifier(chars),
        Some(other) => {
            let other = *other;
            chars.next();
            Err(Error::unexpected_token(start..chars.get_position(), other))
        }
        None => {
            Ok(Token::end_of_input(start..chars.get_position()))
        }
    }
}

fn scan_number(chars: &mut CharIterator) -> Result<Token, Error> {
    let mut digits = String::new();
    let mut has_dot = false;
    let start = chars.get_position().clone();

    loop {
        match chars.peek(0) {
            Some(&d @ '0'..='9') => {
                digits.push(d);
                chars.next();
            }
            Some('.') => {
                if digits.is_empty() {
                    let position = chars.get_position();
                    let range = position..pos(position.line, position.character + 1);
                    return Err(Error::unexpected_token(range, "."));
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
        Err(Error::invalid_syntax(start..start, "invalid number format"))
    } else {
        let range = start..chars.get_position();
        Ok(Token::number(range, digits.clone(), digits.parse().unwrap()))
    }
}

fn scan_string(chars: &mut CharIterator) -> Result<Token, Error> {
    let mut value = String::new();
    let start = chars.get_position().clone();

    if !matches!(chars.peek(0), Some('"')) {
        let range = start..pos(start.line, start.character + 1);
        return Err(Error::unexpected_token(range, "\""));
    }
    chars.next();

    loop {
        match chars.peek(0) {
            Some('"') => {
                chars.next();
                let range = start..chars.get_position();
                return Ok(Token::string(range, format!("\"{}\"", value), value));
            }
            Some(&c) => {
                value.push(c);
                chars.next();
            }
            None => {
                let range = chars.get_position()..chars.get_position();
                return Err(Error::unexpected_token(range, "\""));
            }
        }
    }
}

fn scan_identifier(chars: &mut CharIterator) -> Result<Token, Error> {
    let mut identifier_name = String::new();
    let start = chars.get_position().clone();

    match chars.peek(0) {
        Some(&c @ ('a'..='z' | 'A'..='Z' | '_')) => {
            identifier_name.push(c);
            chars.next();
        }
        _ => return Err(Error::invalid_syntax(start..start, "Invalid identifier name")),
    }

    while let Some(&c @ ('a'..='z' | 'A'..='Z' | '0'..='9' | '_')) = chars.peek(0) {
        identifier_name.push(c);
        chars.next();
    }

    let range = start..chars.get_position();

    match identifier_name.as_str() {
        "true" => Ok(Token::bool(range, "true".to_string(), true)),
        "false" => Ok(Token::bool(range, "false".to_string(), false)),
        _ => Ok(Token::identifier(range, identifier_name)),
    }
}

#[cfg(test)]
mod tests {
    use crate::error::Error;
    use crate::lexer::scan;
    use crate::position::pos;
    use crate::punctuation_kind::PunctuationKind;
    use crate::token::Token;

    fn test(input: &str, expected: Vec<Result<Token, Error>>) {
        let tokens = scan(input);

        assert_eq!(tokens, expected);
    }

    #[test]
    fn skip_whitespace() {
        test(" ", vec![
            Ok(Token::end_of_input(pos(0, 1)..pos(0, 1))),
        ]);
    }

    #[test]
    fn skip_tab() {
        test("\t", vec![
            Ok(Token::end_of_input(pos(0, 1)..pos(0, 1))),
        ]);
    }

    #[test]
    fn skip_carriage_return() {
        test("\r", vec![
            Ok(Token::end_of_input(pos(0, 1)..pos(0, 1))),
        ]);
    }

    #[test]
    fn identifier() {
        test("a", vec![
            Ok(Token::identifier(pos(0, 0)..pos(0, 1), "a")),
            Ok(Token::end_of_input(pos(0, 1)..pos(0, 1))),
        ]);
    }

    #[test]
    fn identifier_after_whitespace() {
        test(" a", vec![
            Ok(Token::identifier(pos(0, 1)..pos(0, 2), "a")),
            Ok(Token::end_of_input(pos(0, 2)..pos(0, 2))),
        ]);
    }

    #[test]
    fn multiple_identifiers() {
        test("a b", vec![
            Ok(Token::identifier(pos(0, 0)..pos(0, 1), "a")),
            Ok(Token::identifier(pos(0, 2)..pos(0, 3), "b")),
            Ok(Token::end_of_input(pos(0, 3)..pos(0, 3))),
        ]);
    }

    #[test]
    fn integer() {
        test("123", vec![
            Ok(Token::number(pos(0, 0)..pos(0, 3), "123", 123f64)),
            Ok(Token::end_of_input(pos(0, 3)..pos(0, 3))),
        ]);
    }

    #[test]
    fn zero() {
        test("0", vec![
            Ok(Token::number(pos(0, 0)..pos(0, 1), "0", 0f64)),
            Ok(Token::end_of_input(pos(0, 1)..pos(0, 1))),
        ]);
    }

    #[test]
    fn integer_with_leading_zero() {
        test("0123", vec![
            Ok(Token::number(pos(0, 0)..pos(0, 4), "0123", 123f64)),
            Ok(Token::end_of_input(pos(0, 4)..pos(0, 4))),
        ]);
    }

    #[test]
    fn integer_with_trailing_dot() {
        test("123.", vec![
            Ok(Token::number(pos(0, 0)..pos(0, 3), "123", 123f64)),
            Ok(Token::punctuation(pos(0, 3)..pos(0, 4), PunctuationKind::Dot)),
            Ok(Token::end_of_input(pos(0, 4)..pos(0, 4))),
        ]);
    }

    #[test]
    fn integer_with_plus() {
        test("+123", vec![
            Ok(Token::punctuation(pos(0, 0)..pos(0, 1), PunctuationKind::Plus)),
            Ok(Token::number(pos(0, 1)..pos(0, 4), "123", 123f64)),
            Ok(Token::end_of_input(pos(0, 4)..pos(0, 4))),
        ]);
    }

    #[test]
    fn integer_with_minus() {
        test("-123", vec![
            Ok(Token::punctuation(pos(0, 0)..pos(0, 1), PunctuationKind::Minus)),
            Ok(Token::number(pos(0, 1)..pos(0, 4), "123", 123f64)),
            Ok(Token::end_of_input(pos(0, 4)..pos(0, 4))),
        ]);
    }

    #[test]
    fn float() {
        test("123.456", vec![
            Ok(Token::number(pos(0, 0)..pos(0, 7), "123.456", 123.456f64)),
            Ok(Token::end_of_input(pos(0, 7)..pos(0, 7))),
        ]);
    }

    #[test]
    fn float_less_than_1() {
        test("0.456", vec![
            Ok(Token::number(pos(0, 0)..pos(0, 5), "0.456", 0.456f64)),
            Ok(Token::end_of_input(pos(0, 5)..pos(0, 5))),
        ]);
    }

    #[test]
    fn float_starting_with_dot() {
        test(".456", vec![
            Ok(Token::punctuation(pos(0, 0)..pos(0, 1), PunctuationKind::Dot)),
            Ok(Token::number(pos(0, 1)..pos(0, 4), "456", 456f64)),
            Ok(Token::end_of_input(pos(0, 4)..pos(0, 4))),
        ]);
    }

    #[test]
    fn float_with_multiple_dots() {
        test("123.456.789", vec![
            Ok(Token::number(pos(0, 0)..pos(0, 7), "123.456", 123.456f64)),
            Ok(Token::punctuation(pos(0, 7)..pos(0, 8), PunctuationKind::Dot)),
            Ok(Token::number(pos(0, 8)..pos(0, 11), "789", 789f64)),
            Ok(Token::end_of_input(pos(0, 11)..pos(0, 11))),
        ]);
    }

    #[test]
    fn string() {
        test("\"ABC\"", vec![
            Ok(Token::string(pos(0, 0)..pos(0, 5), "\"ABC\"", "ABC")),
            Ok(Token::end_of_input(pos(0, 5)..pos(0, 5))),
        ]);
    }

    #[test]
    fn empty_string() {
        test("\"\"", vec![
            Ok(Token::string(pos(0, 0)..pos(0, 2), "\"\"", "")),
            Ok(Token::end_of_input(pos(0, 2)..pos(0, 2))),
        ]);
    }

    #[test]
    fn unterminated_string() {
        test("\"ABC", vec![
            Err(Error::unexpected_token(pos(0, 4)..pos(0, 4), "\"")),
            Ok(Token::end_of_input(pos(0, 4)..pos(0, 4))),
        ]);
    }

    #[test]
    fn bool_true() {
        test("true", vec![
            Ok(Token::bool(pos(0, 0)..pos(0, 4), "true", true)),
            Ok(Token::end_of_input(pos(0, 4)..pos(0, 4))),
        ]);
    }

    #[test]
    fn identifier_with_prefix_true() {
        test("trueabc", vec![
            Ok(Token::identifier(pos(0, 0)..pos(0, 7), "trueabc")),
            Ok(Token::end_of_input(pos(0, 7)..pos(0, 7))),
        ]);
    }

    #[test]
    fn bool_false() {
        test("false", vec![
            Ok(Token::bool(pos(0, 0)..pos(0, 5), "false", false)),
            Ok(Token::end_of_input(pos(0, 5)..pos(0, 5))),
        ]);
    }

    #[test]
    fn identifier_with_prefix_false() {
        test("falseabc", vec![
            Ok(Token::identifier(pos(0, 0)..pos(0, 8), "falseabc")),
            Ok(Token::end_of_input(pos(0, 8)..pos(0, 8))),
        ]);
    }

    #[test]
    fn invalid_token() {
        test("[@#$^&%]", vec![
            Err(Error::unexpected_token(pos(0, 0)..pos(0, 1), "[")),
            Err(Error::unexpected_token(pos(0, 1)..pos(0, 2), "@")),
            Err(Error::unexpected_token(pos(0, 2)..pos(0, 3), "#")),
            Err(Error::unexpected_token(pos(0, 3)..pos(0, 4), "$")),
            Err(Error::unexpected_token(pos(0, 4)..pos(0, 5), "^")),
            Err(Error::unexpected_token(pos(0, 5)..pos(0, 6), "&")),
            Err(Error::unexpected_token(pos(0, 6)..pos(0, 7), "%")),
            Err(Error::unexpected_token(pos(0, 7)..pos(0, 8), "]")),
            Ok(Token::end_of_input(pos(0, 8)..pos(0, 8))),
        ]);
    }
}