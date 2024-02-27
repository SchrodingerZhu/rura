use winnow::Located;

use crate::{character, string, Constant};

#[test]
fn test_parse_string() {
    let mut input = Located::new(r#""Hello, world!""#);
    let result = string(&mut input);
    assert_eq!(result, Ok(Constant::Literal("Hello, world!".to_string())));
    assert_eq!(input.to_string(), "");
    let mut input_with_emoji = Located::new(r#""Hello, 🌍!""#);
    let result = string(&mut input_with_emoji);
    assert_eq!(result, Ok(Constant::Literal("Hello, 🌍!".to_string())));
    let mut input_with_escaped_emoji = Located::new(r#""Hello, \u{1F30D}!""#);
    let result = string(&mut input_with_escaped_emoji);
    assert_eq!(result, Ok(Constant::Literal("Hello, 🌍!".to_string())));
}

#[test]
fn test_parse_char() {
    let mut input = Located::new(r#"'a'"#);
    let result = character(&mut input);
    assert_eq!(result, Ok(Constant::Char('a')));
    assert_eq!(input.to_string(), "");
    let mut input_with_escaped_char = Located::new(r#"'\\'"#);
    let result = character(&mut input_with_escaped_char);
    assert_eq!(result, Ok(Constant::Char('\\')));
    assert_eq!(input_with_escaped_char.to_string(), "");
}
