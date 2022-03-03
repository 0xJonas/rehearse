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
struct Cursor<'a> {
    content: &'a str,
    iter: Peekable<std::str::Chars<'a>>,
    position: CursorPosition
}

/// A representation of a location in a file.
#[derive(Clone, Debug)]
struct CursorPosition {
    file: String,
    line: usize,
    col: usize,
    offset: usize,
    line_offset: usize
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
    context: String,
    position: CursorPosition
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

impl<'a> Cursor<'a> {

    fn new<'b>(content: &'b str, file: &str) -> Cursor<'b> {
        Cursor {
            content,
            iter: content.chars().peekable(),
            position: CursorPosition {
                file: String::from(file),
                line: 0,
                col: 0,
                offset: 0,
                line_offset: 0
            }
        }
    }

    /// Returns the character that the Cursor currently points to,
    /// without advancing the Cursor.
    fn peek(&mut self) -> Option<char> {
        self.iter.peek().map(|c| *c)
    }

    /// Advances the Cursor to the next character.
    fn advance(&mut self) -> () {
        if let Some(&c) = self.iter.peek() {
            self.position.col += 1;
            self.position.offset += c.len_utf8();
            if let Some('\n') = self.iter.next() {
                self.position.col = 0;
                self.position.line += 1;
                self.position.line_offset = self.position.offset;
            }
        }
    }

    /// If the current character is any of the ones contained in `chars`,
    /// advances the Cursor, otherwise returns an error.
    fn expect(&mut self, chars: &[char]) -> Result<(), ParseError> {
        if let Some(c) = self.peek() {
            if chars.contains(&c) {
                // One of the expected chars was found
                self.advance();
                Ok(())
            } else {
                // A char that was not expected was found
                Err(ParseError::new(&build_error_message_from_chars(chars), self))
            }
        } else if chars.len() == 0 {
            // Cursor is empty, but nothing was expected
            Ok(())
        } else {
            // Cursor is empty, but something was expected
            Err(ParseError::new(&build_error_message_from_chars(chars), self))
        }
    }

    /// Returns a snapshot of the current position of the Cursor.
    fn get_position(&self) -> CursorPosition {
        self.position.clone()
    }
}

impl ParseError {

    fn new(message: &str, cursor: &Cursor) -> ParseError {
        let line_len: usize = cursor
            .content[cursor.position.line_offset..]
            .chars()
            .take_while(|&c| c != '\n')
            .map(|c| c.len_utf8())
            .sum();
        ParseError {
            message: String::from(message),
            position: cursor.get_position(),
            context: String::from(&cursor.content[cursor.position.line_offset .. (cursor.position.line_offset + line_len)])
        }
    }

    fn new_at(message: &str, cursor: &Cursor, position: CursorPosition) -> ParseError {
        let line_len: usize = cursor
            .content[position.line_offset..]
            .chars()
            .take_while(|&c| c != '\n')
            .map(|c| c.len_utf8())
            .sum();
        ParseError {
            message: String::from(message),
            context: String::from(&cursor.content[position.line_offset .. (position.line_offset + line_len)]),
            position
        }
    }
}

/// Skips all whitespace characters up to the next non-whitespace character.
fn skip_whitespace(cursor: &mut Cursor) -> () {
    while let Some(c) = cursor.peek() {
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
fn parse_symbol(cursor: &mut Cursor) -> Result<Symbol, ParseError> {
    let mut symbol = String::new();
    while let Some(c) = cursor.peek() {
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
        Err(ParseError::new("Symbol expected", cursor))
    }
}

/// Parses a number.
/// 
/// Numbers are signed 64-bit integers.
fn parse_number(cursor: &mut Cursor) -> Result<i64, ParseError> {
    let mut number_str = String::new();

    // Read sign
    let sign = if let Some('-') = cursor.peek() {
        cursor.advance();
        -1
    } else {
        1
    };

    // Read digits
    while let Some(c) = cursor.peek() {
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
            Err(e) => Err(ParseError::new(&format!("Error parsing {}: {}", number_str, e), cursor))
        }
    } else {
        Err(ParseError::new("Number expected", cursor))
    }
}

/// Reads a key-value pair representing an argument of a function call.
/// 
/// Arguments have the syntax `<argument> := <symbol> '=' <argument-expr>`.
/// ArgumentExpressions can be a symbol, a number or a function call: `<argument-expr> := <symbol> | <number> | <function-call>`.
fn parse_argument(cursor: &mut Cursor) -> Result<(Symbol, ArgumentExpression), ParseError> {
    let argument_name = parse_symbol(cursor)?;

    skip_whitespace(cursor);
    cursor.expect(&['='])?;
    skip_whitespace(cursor);

    let argument_expr = match cursor.peek() {
        Some('`') => {
            cursor.advance();
            ArgumentExpression::FunctionCall(parse_function_call(cursor)?)
        },
        Some(c) if c.is_ascii_digit() || c == '-' => ArgumentExpression::Number(parse_number(cursor)?),
        Some(c) if c.is_ascii_alphabetic() || c == '_' => ArgumentExpression::Symbol(parse_symbol(cursor)?),
        _ => return Err(ParseError::new("Symbol, number or function call expected", cursor))
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
fn parse_function_call(cursor: &mut Cursor) -> Result<FunctionCall, ParseError> {
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
fn parse_text_expression(cursor: &mut Cursor) -> Result<TextExpression, ParseError> {
    let mut brace_nesting = 0;
    let mut segments = Vec::<TextExpressionSegment>::new();

    let mut current_string = String::new();
    let mut last_left_brace = cursor.get_position();

    while let Some(c) = cursor.peek() {
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
                last_left_brace = cursor.get_position();
                cursor.advance();
            },
            '`' => {
                let backtick_pos = cursor.get_position();
                cursor.advance();

                let next = cursor.peek();

                match next {
                    Some('`') | Some('{') | Some('}') => {
                        // Escaped character
                        current_string.push(next.unwrap());
                        cursor.advance();
                    },
                    None => {
                        return Err(ParseError::new_at("Dangling ` character", cursor, backtick_pos));
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
        return Err(ParseError::new_at("Unmatched left curly brace", cursor, last_left_brace));
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

    #[test]
    fn parse_function_call_without_arguments() {
        let mut cursor = Cursor::new("`test", "parse_function_call_without_arguments");
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
        let mut cursor = Cursor::new("`test[arg1 = something]", "parse_function_call_with_arguments");
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
        let mut cursor = Cursor::new("`test{This is a test}", "parse_function_call_with_texts");
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
        let mut cursor = Cursor::new("`test[arg1 = something]{This is a test}", "parse_function_call_with_arguments_and_texts");
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
        let mut cursor = Cursor::new("`test[arg1 = something, arg2 = 5, arg3 = -10, arg4 = `test2{Hello}]", "parse_function_call_with_different_argument_types");
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
        let mut cursor = Cursor::new("Hello, World", "parse_text");
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
        let mut cursor = Cursor::new("{}{{}}`{`}\\{\\}", "parse_text_with_escape_character");
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
        let mut cursor = Cursor::new("Hello `func{World}", "parse_text_with_function_call");
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

    #[test]
    fn unexpected_character() {
        let mut cursor = Cursor::new("`func[arg=val{Test}", "unexpected_character");
        let err = parse_text_expression(&mut cursor).unwrap_err();

        assert_eq!(err.message, "']' expected");
        assert_eq!(err.context, "`func[arg=val{Test}");
        assert_eq!(err.position.file, "unexpected_character");
        assert_eq!(err.position.line, 0);
        assert_eq!(err.position.col, 13);
    }

    #[test]
    fn empty_argument_name() {
        let mut cursor = Cursor::new("`func[=1]", "empty_argument_name");
        let err = parse_text_expression(&mut cursor).unwrap_err();

        assert_eq!(err.message, "Symbol expected");
        assert_eq!(err.context, "`func[=1]");
        assert_eq!(err.position.file, "empty_argument_name");
        assert_eq!(err.position.line, 0);
        assert_eq!(err.position.col, 6);
    }

    #[test]
    fn dangling_backtick() {
        let mut cursor = Cursor::new("there -> `", "dangling_backtick");
        let err = parse_text_expression(&mut cursor).unwrap_err();

        assert_eq!(err.message, "Dangling ` character");
        assert_eq!(err.context, "there -> `");
        assert_eq!(err.position.file, "dangling_backtick");
        assert_eq!(err.position.line, 0);
        assert_eq!(err.position.col, 9);
    }

    #[test]
    fn unmatched_brace() {
        let mut cursor = Cursor::new("there -> {  Hi", "unmatched_brace");
        let err = parse_text_expression(&mut cursor).unwrap_err();

        assert_eq!(err.message, "Unmatched left curly brace");
        assert_eq!(err.context, "there -> {  Hi");
        assert_eq!(err.position.file, "unmatched_brace");
        assert_eq!(err.position.line, 0);
        assert_eq!(err.position.col, 9);
    }

    // Error message contains correct line
    #[test]
    fn parse_error_contains_the_correct_context() {
        let mut cursor = Cursor::new("Good line\nBad line`", "parse_error_contains_the_correct_context");
        let err = parse_text_expression(&mut cursor).unwrap_err();

        assert_eq!(err.message, "Dangling ` character");
        assert_eq!(err.context, "Bad line`");
        assert_eq!(err.position.file, "parse_error_contains_the_correct_context");
        assert_eq!(err.position.line, 1);
        assert_eq!(err.position.col, 8);
    }
}
