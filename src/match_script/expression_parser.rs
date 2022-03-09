use super::{CursorPosition, ParseErrorVariant, ParseError};

use std::iter::Peekable;
use std::str::FromStr;
use std::fmt::{Display, Formatter, Error};

/// AST node for any identifier that may be used.
/// 
/// A Symbol is a string that only contains alphanumeric characters and underscores.
/// Symbols always start with an uppercase or lowercase letter.
pub type Symbol = String;

/// AST node for expressions that can be used as argument values in a `FunctionCall`.
#[derive(Debug, PartialEq)]
pub enum ArgumentExpression {
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
#[derive(Debug, PartialEq)]
pub struct FunctionCall {
    pub name: Symbol,
    pub arguments: Vec<(Symbol, ArgumentExpression)>,
    pub texts: Vec<TextExpression>,
    pub position: CursorPosition
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
#[derive(Debug, PartialEq)]
pub enum TextExpressionSegment {
    String(String),
    FunctionCall(FunctionCall)
}

/// Escapes certain characters in a [str] so that it can be unambiguously parsed
/// by `parse_text_expression`.
/// 
/// This essentially amounts to adding a '`' before any '`' as well as any unmatched
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
            TextExpressionSegment::String(str) => write!(f, "{}", escape_text(str)),
            TextExpressionSegment::FunctionCall(funcall) => funcall.fmt(f)
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct TextExpression {
    pub segments: Vec<TextExpressionSegment>,
    pub position: CursorPosition
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

fn find_next_newline(text: &str) -> usize {
    text.chars()
        .take_while(|c| *c != '\n')
        .map(|c| c.len_utf8())
        .sum()
}

impl<'a> Cursor<'a> {

    fn new<'b>(content: &'b str) -> Cursor<'b> {
        Cursor {
            content,
            iter: content.chars().peekable(),
            position: CursorPosition {
                line: 0,
                col: 0,
                offset: 0
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
                Err(ParseError { position: self.get_position().to_owned(), variant: ParseErrorVariant::CharsExpected(Vec::from(chars)) })
            }
        } else if chars.len() == 0 {
            // Cursor is empty, but nothing was expected
            Ok(())
        } else {
            // Cursor is empty, but something was expected
            Err(ParseError { position: self.get_position().to_owned(), variant: ParseErrorVariant::CharsExpected(Vec::from(chars)) })
        }
    }

    /// Returns a snapshot of the current position of the Cursor.
    fn get_position(&self) -> CursorPosition {
        self.position.clone()
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
        Err(ParseError { position: cursor.get_position().to_owned(), variant: ParseErrorVariant::TokenExpected("symbol".to_owned()) })
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
            Err(e) => Err(ParseError {
                position: cursor.get_position().to_owned(),
                variant: ParseErrorVariant::External("Could not parse number".to_owned(), Box::new(e)) 
            })
        }
    } else {
        Err(ParseError { position: cursor.get_position().to_owned(), variant: ParseErrorVariant::TokenExpected("number".to_owned()) })
    }
}

/// Reads a key-value pair representing an argument of a function call.
/// 
/// Arguments have the syntax `<argument> := <symbol> '=' <argument-expr>`.
/// ArgumentExpressions can be a symbol, a number or a function call: `<argument-expr> := <symbol> | <number> | <function-call>`.
fn parse_argument<'i>(cursor: &mut Cursor<'i>) -> Result<(Symbol, ArgumentExpression), ParseError> {
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
        _ => return Err( ParseError {
            position: cursor.get_position().to_owned(),
            variant: ParseErrorVariant::TokenExpected("symbol, number of function call".to_owned()) 
        })
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
fn parse_function_call<'i>(cursor: &mut Cursor<'i>) -> Result<FunctionCall, ParseError> {
    let mut position = cursor.get_position();
    // A function call starts at the ` character, but that is already parsed at this point.
    // In theory, there should always be at least one character to go back to,
    // but just to be safe, we wrap it in an 'if'
    if position.col > 0 {
        position.col -= 1;
    }
    if position.offset > 0 {
        position.offset -= 1;
    }
    
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

    return Ok(FunctionCall { name, arguments, texts, position });
}

/// Parses a text expression.
/// 
/// TextExpressions are the root element of an expression. They can contain ordinary text or function calls.
/// Normal text can contain control characters, if they are escaped. To write any of '`', '{' or '}', precede them
/// with a '`' to create the character sequences '``', '`{' or '`}'. It is possible to write unescaped curly braces
/// if for each left curly brace there is a matching right curly brace. Finally, curly braces can also be escaped
/// with a '\', however, the backslash will also be part of the resulting text. Essentially, the sequences "\{" and "\}"
/// are treated as single characters. This is indended to be used in regular expressions.
fn parse_text_expression<'i>(cursor: &mut Cursor<'i>) -> Result<TextExpression, ParseError> {
    let position = cursor.get_position();
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
                println!("{:?}", backtick_pos);

                let next = cursor.peek();

                match next {
                    Some('`') | Some('{') | Some('}') => {
                        // Escaped character
                        current_string.push(next.unwrap());
                        cursor.advance();
                    },
                    None => {
                        return Err(ParseError { position: backtick_pos, variant: ParseErrorVariant::DanglingBacktick });
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
        return Err(ParseError { position: last_left_brace.to_owned(), variant: ParseErrorVariant::UnmatchedBrace });
    }

    if current_string.len() != 0 {
        segments.push(TextExpressionSegment::String(current_string));
    }

    return Ok(TextExpression { segments, position })
}

#[cfg(test)]
mod tests {

    use crate::match_script::{CursorPosition, ParseError};
    use super::*;

    #[test]
    fn parse_function_call_without_arguments() {
        let mut cursor = Cursor::new("`test");
        let expr = parse_text_expression(&mut cursor).unwrap();

        let expected = TextExpression {
            segments: vec![
                TextExpressionSegment::FunctionCall(
                    FunctionCall {
                        name: "test".to_owned(),
                        arguments: Vec::new(),
                        texts: Vec::new(),
                        position: CursorPosition { line: 0, col: 0, offset: 0 }
                    }
                )
            ],
            position: CursorPosition { line: 0, col: 0, offset: 0 }
        };
        
        assert_eq!(expr, expected);
    }
    
    #[test]
    fn parse_function_call_with_arguments() {
        let mut cursor = Cursor::new("`test[arg1 = something]");
        let expr = parse_text_expression(&mut cursor).unwrap();

        let expected = TextExpression {
            segments: vec![
                TextExpressionSegment::FunctionCall(
                    FunctionCall {
                        name: "test".to_owned(),
                        arguments: vec![("arg1".to_owned(), ArgumentExpression::Symbol("something".to_owned()))],
                        texts: Vec::new(),
                        position: CursorPosition { line: 0, col: 0, offset: 0 }
                    }
                )
            ],
            position: CursorPosition { line: 0, col: 0, offset: 0 }
        };
        
        assert_eq!(expr, expected);
    }

    #[test]
    fn parse_function_call_with_texts() {
        let mut cursor = Cursor::new("`test{This is a test}");
        let expr = parse_text_expression(&mut cursor).unwrap();

        let expected = TextExpression {
            segments: vec![
                TextExpressionSegment::FunctionCall(
                    FunctionCall {
                        name: "test".to_owned(),
                        arguments: Vec::new(),
                        texts: vec![TextExpression {
                            segments: vec![TextExpressionSegment::String("This is a test".to_owned())],
                            position: CursorPosition { line: 0, col: 6, offset: 6 }
                        }],
                        position: CursorPosition { line: 0, col: 0, offset: 0 }
                    }
                )
            ],
            position: CursorPosition { line: 0, col: 0, offset: 0 }
        };

        assert_eq!(expr, expected);
    }
    
    #[test]
    fn parse_function_call_with_arguments_and_texts() {
        let mut cursor = Cursor::new("`test[arg1 = something]{This is a test}");
        let expr = parse_text_expression(&mut cursor).unwrap();
        
        let expected = TextExpression {
            segments: vec![
                TextExpressionSegment::FunctionCall(
                    FunctionCall {
                        name: String::from("test"),
                        arguments: vec![(String::from("arg1"), ArgumentExpression::Symbol("something".to_owned()))],
                        texts: vec![TextExpression {
                            segments: vec![TextExpressionSegment::String("This is a test".to_owned())],
                            position: CursorPosition { line: 0, col: 24, offset: 24 }
                        }],
                        position: CursorPosition { line: 0, col: 0, offset: 0 }
                    }
                )
            ],
            position: CursorPosition { line: 0, col: 0, offset: 0 }
        };

        assert_eq!(expr, expected);
    }

    #[test]
    fn parse_function_call_with_different_argument_types() {
        let mut cursor = Cursor::new("`test[arg1 = something, arg2 = 5, arg3 = -10, arg4 = `test2{Hello}]");
        let expr = parse_text_expression(&mut cursor).unwrap();

        let expected = TextExpression {
            segments: vec![
                TextExpressionSegment::FunctionCall(
                    FunctionCall {
                        name: "test".to_owned(),
                        arguments: vec![
                            ("arg1".to_owned(), ArgumentExpression::Symbol("something".to_owned())),
                            ("arg2".to_owned(), ArgumentExpression::Number(5)),
                            ("arg3".to_owned(), ArgumentExpression::Number(-10)),
                            ("arg4".to_owned(), ArgumentExpression::FunctionCall(FunctionCall {
                                name: "test2".to_owned(),
                                arguments: Vec::new(),
                                texts: vec![TextExpression {
                                    segments: vec![TextExpressionSegment::String("Hello".to_owned())],
                                    position: CursorPosition { line: 0, col: 60, offset: 60 }
                                }],
                                position: CursorPosition { line: 0, col: 53, offset: 53 }
                            })),
                        ],
                        texts: Vec::new(),
                        position: CursorPosition { line: 0, col: 0, offset: 0 }
                    }
                )
            ],
            position: CursorPosition { line: 0, col: 0, offset: 0 }
        };
        
        assert_eq!(expr, expected);
    }

    #[test]
    fn parse_text() {
        let mut cursor = Cursor::new("Hello, World");
        let expr = parse_text_expression(&mut cursor).unwrap();

        let expected = TextExpression {
            segments: vec![
                TextExpressionSegment::String("Hello, World".to_owned())
            ],
            position: CursorPosition { line: 0, col: 0, offset: 0 }
        };

        assert_eq!(expr, expected);
    }

    #[test]
    fn parse_text_with_escape_character() {
        let mut cursor = Cursor::new("{}{{}}`{`}\\{\\}");
        let expr = parse_text_expression(&mut cursor).unwrap();

        let expected = TextExpression {
            segments: vec![
                TextExpressionSegment::String("{}{{}}{}\\{\\}".to_owned())
            ],
            position: CursorPosition { line: 0, col: 0, offset: 0 }
        };
        
        assert_eq!(expr, expected);
    }

    #[test]
    fn parse_text_with_function_call() {
        let mut cursor = Cursor::new("Hello `func{World}");
        let expr = parse_text_expression(&mut cursor).unwrap();

        let expected = TextExpression {
            segments: vec![
                TextExpressionSegment::String("Hello ".to_owned()),
                TextExpressionSegment::FunctionCall(FunctionCall {
                    name: String::from("func"),
                    arguments: Vec::new(),
                    texts: vec![TextExpression {
                        segments: vec![TextExpressionSegment::String("World".to_owned())],
                        position: CursorPosition { line: 0, col: 12, offset: 12 }
                    }],
                    position: CursorPosition { line: 0, col: 6, offset: 6 }
                })
            ],
            position: CursorPosition { line: 0, col: 0, offset: 0 }
        };
        
        assert_eq!(expr, expected);
    }

    #[test]
    fn unexpected_character() {
        let mut cursor = Cursor::new("`func[arg=val{Test}");
        if let ParseError { variant: ParseErrorVariant::CharsExpected(chars), position } = parse_text_expression(&mut cursor).unwrap_err() {
            assert_eq!(chars.len(), 1);
            assert_eq!(chars[0], ']');
            assert_eq!(position.line, 0);
            assert_eq!(position.col, 13);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn empty_argument_name() {
        let mut cursor = Cursor::new("`func[=1]");
        if let ParseError { variant: ParseErrorVariant::TokenExpected(token), position } = parse_text_expression(&mut cursor).unwrap_err() {
            assert_eq!(token, "symbol");
            assert_eq!(position.line, 0);
            assert_eq!(position.col, 6);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn dangling_backtick() {
        let mut cursor = Cursor::new("there -> `");
        if let ParseError { variant: ParseErrorVariant::DanglingBacktick, position } = parse_text_expression(&mut cursor).unwrap_err() {
            assert_eq!(position.line, 0);
            assert_eq!(position.col, 9);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn unmatched_brace() {
        let mut cursor = Cursor::new("there -> {  Hi");
        if let ParseError { variant: ParseErrorVariant::UnmatchedBrace, position } = parse_text_expression(&mut cursor).unwrap_err() {
            assert_eq!(position.line, 0);
            assert_eq!(position.col, 9);
        } else {
            assert!(false);
        }
    }

    // Error message contains correct line
    #[test]
    fn parse_error_contains_the_correct_context() {
        let mut cursor = Cursor::new("Good line\nBad line`");
        if let ParseError { variant: ParseErrorVariant::DanglingBacktick, position } = parse_text_expression(&mut cursor).unwrap_err() {
            assert_eq!(position.line, 1);
            assert_eq!(position.col, 8);
        } else {
            assert!(false);
        }
    }
}
