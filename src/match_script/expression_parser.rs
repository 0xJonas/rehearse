use super::{MSType, Grapheme, Matcher};

use std::iter::Peekable;
use std::str::FromStr;
use std::fmt::{Display, Formatter, Error};

/// AST node for any identifier that may be used.
/// 
/// A Symbol is a string that only contains alphanumeric characters and underscores.
/// Symbols always start with an uppercase or lowercase letter.
type Symbol = String;

/// AST node for expressions that can be used as argument values in a `FunctionCall`.
#[derive(Clone, Debug, PartialEq)]
enum ArgumentExpression {
    Symbol(Symbol),
    Number(i64),
    FunctionCall(FunctionCall)
}

impl Display for ArgumentExpression {

    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            ArgumentExpression::Symbol(symbol) => symbol.fmt(f),
            ArgumentExpression::Number(number) => number.fmt(f),
            ArgumentExpression::FunctionCall(funcall) => funcall.fmt(f)
        }
    }
}

/// AST node for function call expressions.
/// 
/// A function call can take any number of key-value pairs as arguments,
/// as well as any number of text expressions to operate on.
#[derive(Clone, Debug, PartialEq)]
struct FunctionCall {
    name: Symbol,
    arguments: Vec<(Symbol, ArgumentExpression)>,
    texts: Vec<TextExpression>
}

impl Display for FunctionCall {

    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "`{}", self.name)?;
        if self.arguments.len() > 0 {
            write!(f, "[")?;
            for (name, arg_expr) in &self.arguments {
                write!(f, "{} = {}", name, arg_expr)?;
            }
            write!(f, "]")?;
        }

        if self.texts.len() > 0 {
            for text_expr in &self.texts {
                write!(f, "{{{}}}", text_expr)?;
            }
        }

        return Ok(());
    }
}

/// Single segment of a `TextExpression`, which can be either some text or a function call.
#[derive(Clone, Debug, PartialEq)]
enum TextExpressionSegment {
    String(String),
    FunctionCall(FunctionCall)
}

/// Escapes certain characters in a [str] so that it can be unambiguously parsed
/// by `parse_text_expression`.
/// 
/// This essentially amounts to adding a '`' before any `  as well as any unmatched
/// left or right brace.
fn escape_text(str: &str) -> String {
    let mut out = String::with_capacity(str.len() + 10);
    let mut brace_nesting = 0;
    let mut left_brace_positions = Vec::new();
    let mut right_brace_positions = Vec::new();
    for c in str.chars() {
        match c {
            '`' => {
                out.push('`');
                out.push('`');
            },
            '{' => {
                brace_nesting += 1;
                left_brace_positions.push(out.len());
                out.push('{');
            },
            '}' => {
                if brace_nesting > 0 {
                    brace_nesting -= 1;
                }
                right_brace_positions.push(out.len());
                out.push('}');
            },
            _ => out.push(c)
        }
    }

    if brace_nesting < 0 {
        // Escape the the first `brace_nesting` right braces. Start with the rightmost
        // of them, because adding the escape character will invalidate the following indices.
        for i in 0..-brace_nesting {
            let position = right_brace_positions[(-brace_nesting - 1 - i) as usize];
            out.insert(position, '`');
        }
    } else if brace_nesting > 0 {
        // Escape the the first `brace_nesting` left braces. Start with the rightmost
        // of them, because adding the escape character will invalidate the following indices.
        for i in 0..brace_nesting {
            let position = left_brace_positions[(brace_nesting - 1 - i) as usize];
            out.insert(position, '`');
        }
    }

    return out;
}

impl Display for TextExpressionSegment {

    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            TextExpressionSegment::String(str) => write!(f, "{}", escape_text(&str)),
            TextExpressionSegment::FunctionCall(funcall) => funcall.fmt(f)
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct TextExpression {
    segments: Vec<TextExpressionSegment>
}

impl Display for TextExpression {

    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        for segment in &self.segments {
            write!(f, "{}", segment)?;
        }
        return Ok(());
    }
}

/// Primary source of characters for the parsing functions.
/// 
/// This type offers some convenience methods for parsing as well as keeping
/// track of the current position in the input.
struct Cursor<I: Iterator<Item=char>> {
    iter: Peekable<I>,
    position: CursorPosition
}

/// A representation of a location in a file.
#[derive(Clone, Debug)]
struct CursorPosition {
    file: String,
    line: usize,
    col: usize
}

impl Display for CursorPosition {
    
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}:{},{}", self.file, self.line, self.col)
    }
}

/// Error returned when anything goes wrong during parsing.
#[derive(Debug)]
struct ParseError {
    message: String,
    context: CursorPosition
}

/// Helper function to construct an error message that lists all the
/// characters which were expected at the point that the error occurred.
fn build_error_message_from_chars(chars: &[char]) -> String {
    let char_list = chars.iter()
        .map(|c| format!("'{}'", c))
        .collect::<Vec<_>>()
        .join(", ");
    return format!("{} expected", char_list);
}

impl<I: Iterator<Item=char>> Cursor<I> {

    /// Returns the character that the Cursor currently points to,
    /// without advancing the Cursor.
    fn peek(&mut self) -> Option<&char> {
        self.iter.peek()
    }

    /// Advances the Cursor to the next character.
    fn advance(&mut self) -> () {
        self.position.col += 1;
        if let Some('\n') = self.iter.next() {
            self.position.col = 0;
            self.position.line += 1;
        }
    }

    /// If the current character is any of the ones contained in `chars`,
    /// advances the Cursor, otherwise returns an error.
    fn expect(&mut self, chars: &[char]) -> Result<(), ParseError> {
        if let Some(c) = self.peek() {
            if chars.contains(c) {
                // One of the expected chars was found
                self.advance();
                Ok(())
            } else {
                // A char that was not expected was found
                Err(ParseError::new(&build_error_message_from_chars(chars), self.get_position()))
            }
        } else if chars.len() == 0 {
            // Cursor is empty, but nothing was expected
            Ok(())
        } else {
            // Cursor is empty, but something was expected
            Err(ParseError::new(&build_error_message_from_chars(chars), self.get_position()))
        }
    }

    /// Returns a snapshot of the current position of the Cursor.
    fn get_position(&self) -> CursorPosition {
        self.position.clone()
    }
}

impl ParseError {

    fn new(message: &str, context: CursorPosition) -> ParseError {
        ParseError {
            message: String::from(message),
            context
        }
    }
}

/// Skips all whitespace characters up to the next non-whitespace character.
fn skip_whitespace<I: Iterator<Item=char>>(cursor: &mut Cursor<I>) -> () {
    while let Some(&c) = cursor.peek() {
        if c.is_ascii_whitespace() {
            cursor.advance();
        } else {
            break;
        }
    }
}

/// Parses a Symbol.
/// 
/// A Symbol starts with either an upper- or lowercase character and contains
/// only alphanumeric characters and underscores.
fn parse_symbol<I: Iterator<Item=char>>(cursor: &mut Cursor<I>) -> Result<Symbol, ParseError> {
    let mut symbol = String::new();
    while let Some(&c) = cursor.peek() {
        if c.is_ascii_alphanumeric() || c == '_' {
            cursor.advance();
            symbol.push(c);
        } else {
            break;
        }
    }

    return if symbol.len() > 0 {
        Ok(symbol)
    } else {
        Err(ParseError::new("Symbol expected", cursor.get_position()))
    }
}

/// Parses a number.
/// 
/// Numbers are signed 64-bit integers.
fn parse_number<I: Iterator<Item=char>>(cursor: &mut Cursor<I>) -> Result<i64, ParseError> {
    let mut number_str = String::new();

    // Read sign
    let sign = if let Some('-') = cursor.peek() {
        cursor.advance();
        -1
    } else {
        1
    };

    // Read digits
    while let Some(&c) = cursor.peek() {
        if c.is_ascii_digit() {
            cursor.advance();
            number_str.push(c);
        } else {
            break;
        }
    }

    return if number_str.len() > 0 {
        match i64::from_str(&number_str) {
            Ok(v) => Ok(v * sign),
            Err(e) => Err(ParseError::new(&format!("Error parsing {}: {}", number_str, e), cursor.get_position()))
        }
    } else {
        Err(ParseError::new("Number expected", cursor.get_position()))
    }
}

/// Reads a key-value pair representing an argument of a function call.
/// 
/// Arguments have the syntax `<argument> := <symbol> '=' <argument-expr>`.
/// ArgumentExpressions can be a symbol, a number or a function call: `<argument-expr> := <symbol> | <number> | <function-call>`.
fn parse_argument<I: Iterator<Item=char>>(cursor: &mut Cursor<I>) -> Result<(Symbol, ArgumentExpression), ParseError> {
    let argument_name = parse_symbol(cursor)?;

    skip_whitespace(cursor);
    cursor.expect(&['='])?;
    skip_whitespace(cursor);

    let argument_expr = match cursor.peek() {
        Some('`') => {
            cursor.advance();
            ArgumentExpression::FunctionCall(parse_function_call(cursor)?)
        },
        Some(&c) if c.is_ascii_digit() || c == '-' => ArgumentExpression::Number(parse_number(cursor)?),
        Some(&c) if c.is_ascii_alphabetic() || c == '_' => ArgumentExpression::Symbol(parse_symbol(cursor)?),
        _ => return Err(ParseError::new("Symbol, number or function call expected", cursor.get_position()))
    };

    return Ok((argument_name, argument_expr));
}

/// Parses a function call.
/// 
/// A function call can take any number of arguments as well as any number of text expressions to operate on.
/// The syntax for a function call is:
/// ```text
/// <function-call> := `<symbol> ('[' <argument> (',' <argument>)* ']')? ('{' <text-expr> '}')*
/// ```
fn parse_function_call<I: Iterator<Item=char>>(cursor: &mut Cursor<I>) -> Result<FunctionCall, ParseError> {
    // Parse name
    let name = parse_symbol(cursor)?;
    let mut arguments = Vec::new();

    // Parse arguments
    if let Some('[') = cursor.peek() {
        cursor.advance();
        loop {
            skip_whitespace(cursor);
            arguments.push(parse_argument(cursor)?);
            skip_whitespace(cursor);
            if let Some(',') = cursor.peek() {
                cursor.advance();
            } else {
                break;
            }
        }

        cursor.expect(&[']'])?;
    }

    // Parse texts
    let mut texts = Vec::new();
    while let Some('{') = cursor.peek() {
        cursor.advance();
        texts.push(parse_text_expression(cursor)?);
        cursor.expect(&['}'])?;
    }

    return Ok(FunctionCall { name, arguments, texts });
}

/// Parses a text expression.
/// 
/// TextExpressions are the root element of an expression. They can contain ordinary text or function calls.
/// Normal text can contain control characters, if they are escaped. To write any of '`', '{' or '}', precede them
/// with a '`' to create the character sequences '``', '`{' or '`}'. It is possible to write unescaped curly braces
/// if for each left curly brace there is a matching right curly brace. Finally, curly braces can also be escaped
/// with a '\', however, the backslash will also be part of the resulting text. Essentially, the sequences "\{" and "\}"
/// are treated as single characters. This is indended to be used in regular expressions.
fn parse_text_expression<I: Iterator<Item=char>>(cursor: &mut Cursor<I>) -> Result<TextExpression, ParseError> {
    let mut brace_nesting = 0;
    let mut segments = Vec::<TextExpressionSegment>::new();

    let mut current_string = String::new();

    while let Some(&c) = cursor.peek() {
        match c {
            '\\' => {
                current_string.push('\\');
                cursor.advance();

                let next = cursor.peek();
                if let Some('{') = next {
                    current_string.push('{');
                    cursor.advance();
                } else if let Some('}') = next {
                    current_string.push('}');
                    cursor.advance();
                }
            },
            '}' => if brace_nesting == 0 {
                    break;
                } else {
                    brace_nesting -= 1;
                    current_string.push('}');
                    cursor.advance();
                },
            '{' => {
                brace_nesting += 1;
                current_string.push('{');
                cursor.advance();
            },
            '`' => {
                cursor.advance();

                let next = cursor.peek();

                match next {
                    Some('`') | Some('{') | Some('}') => {
                        // Escaped character
                        current_string.push(*next.unwrap());
                        cursor.advance();
                    },
                    None => {
                        return Err(ParseError::new("Dangling ` character", cursor.get_position()));
                    },
                    _ => {
                        // Function call
                        if current_string.len() != 0 {
                            segments.push(TextExpressionSegment::String(current_string.clone()));
                        }
                        segments.push(TextExpressionSegment::FunctionCall(parse_function_call(cursor)?));
                        current_string.clear();
                    }
                }
            },
            _ => {
                // Normal character
                current_string.push(c);
                cursor.advance();
            }
        }
    }

    if brace_nesting != 0 {
        return Err(ParseError::new("Unmatched left curly brace", cursor.get_position()));
    }

    if current_string.len() != 0 {
        segments.push(TextExpressionSegment::String(current_string));
    }

    return Ok(TextExpression {
        segments
    })
}

#[cfg(test)]
mod tests {

    use super::*;

    use std::str::Chars;

    fn cursor_from_string<'a>(str: &'a str, name: &str) -> Cursor<Chars<'a>> {
        let position = CursorPosition {
            file: String::from(name),
            line: 0,
            col: 0
        };

        return Cursor {
            iter: str.chars().peekable(),
            position
        };
    }

    #[test]
    fn parse_function_call_without_arguments() {
        let mut cursor = cursor_from_string("`test", "parse_function_call_without_arguments");
        let expr = parse_text_expression(&mut cursor).unwrap();

        let expected = TextExpression {
            segments: vec![
                TextExpressionSegment::FunctionCall(
                    FunctionCall {
                        name: String::from("test"),
                        arguments: Vec::new(),
                        texts: Vec::new(),
                    }
                )
            ]
        };
        
        assert_eq!(expr, expected);
    }
    
    #[test]
    fn parse_function_call_with_arguments() {
        let mut cursor = cursor_from_string("`test[arg1 = something]", "parse_function_call_with_arguments");
        let expr = parse_text_expression(&mut cursor).unwrap();

        let expected = TextExpression {
            segments: vec![
                TextExpressionSegment::FunctionCall(
                    FunctionCall {
                        name: String::from("test"),
                        arguments: vec![(String::from("arg1"), ArgumentExpression::Symbol(String::from("something")))],
                        texts: Vec::new(),
                    }
                )
            ]
        };
        
        assert_eq!(expr, expected);
    }

    #[test]
    fn parse_function_call_with_texts() {
        let mut cursor = cursor_from_string("`test{This is a test}", "parse_function_call_with_texts");
        let expr = parse_text_expression(&mut cursor).unwrap();

        let expected = TextExpression {
            segments: vec![
                TextExpressionSegment::FunctionCall(
                    FunctionCall {
                        name: String::from("test"),
                        arguments: Vec::new(),
                        texts: vec![TextExpression { segments: vec![TextExpressionSegment::String(String::from("This is a test"))] }],
                    }
                )
            ]
        };

        assert_eq!(expr, expected);
    }
    
    #[test]
    fn parse_function_call_with_arguments_and_texts() {
        let mut cursor = cursor_from_string("`test[arg1 = something]{This is a test}", "parse_function_call_with_arguments_and_texts");
        let expr = parse_text_expression(&mut cursor).unwrap();
        
        let expected = TextExpression {
            segments: vec![
                TextExpressionSegment::FunctionCall(
                    FunctionCall {
                        name: String::from("test"),
                        arguments: vec![(String::from("arg1"), ArgumentExpression::Symbol(String::from("something")))],
                        texts: vec![TextExpression { segments: vec![TextExpressionSegment::String(String::from("This is a test"))] }],
                    }
                )
            ]
        };

        assert_eq!(expr, expected);
    }

    #[test]
    fn parse_function_call_with_different_argument_types() {
        let mut cursor = cursor_from_string("`test[arg1 = something, arg2 = 5, arg3 = -10, arg4 = `test2{Hello}]", "parse_function_call_with_different_argument_types");
        let expr = parse_text_expression(&mut cursor).unwrap();

        let expected = TextExpression {
            segments: vec![
                TextExpressionSegment::FunctionCall(
                    FunctionCall {
                        name: String::from("test"),
                        arguments: vec![
                            (String::from("arg1"), ArgumentExpression::Symbol(String::from("something"))),
                            (String::from("arg2"), ArgumentExpression::Number(5)),
                            (String::from("arg3"), ArgumentExpression::Number(-10)),
                            (String::from("arg4"), ArgumentExpression::FunctionCall(FunctionCall {
                                name: String::from("test2"),
                                arguments: Vec::new(),
                                texts: vec![TextExpression { segments: vec![TextExpressionSegment::String(String::from("Hello"))] }]
                            })),
                        ],
                        texts: Vec::new(),
                    }
                )
            ]
        };
        
        assert_eq!(expr, expected);
    }

    #[test]
    fn parse_text() {
        let mut cursor = cursor_from_string("Hello, World", "parse_text");
        let expr = parse_text_expression(&mut cursor).unwrap();

        let expected = TextExpression {
            segments: vec![
                TextExpressionSegment::String(String::from("Hello, World"))
            ]
        };

        assert_eq!(expr, expected);
    }

    #[test]
    fn parse_text_with_escape_character() {
        let mut cursor = cursor_from_string("{}{{}}`{`}\\{\\}", "parse_text_with_escape_character");
        let expr = parse_text_expression(&mut cursor).unwrap();

        let expected = TextExpression {
            segments: vec![
                TextExpressionSegment::String(String::from("{}{{}}{}\\{\\}"))
            ]
        };
        
        assert_eq!(expr, expected);
    }

    #[test]
    fn parse_text_with_function_call() {
        let mut cursor = cursor_from_string("Hello `func{World}", "parse_text_with_function_call");
        let expr = parse_text_expression(&mut cursor).unwrap();

        let expected = TextExpression {
            segments: vec![
                TextExpressionSegment::String(String::from("Hello ")),
                TextExpressionSegment::FunctionCall(FunctionCall {
                    name: String::from("func"),
                    arguments: Vec::new(),
                    texts: vec![TextExpression { segments: vec![TextExpressionSegment::String(String::from("World"))] }]
                })
            ]
        };
        
        assert_eq!(expr, expected);
    }
}
