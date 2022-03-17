mod proto_parser;
mod expression_parser;
mod functions;

use expression_parser::Symbol;

use tokio::io::AsyncReadExt;
use encoding_rs::Encoding;

use std::fmt::{Display, Formatter};
use std::error::Error;
use std::collections::HashMap;

use crate::cli::{TermColor, Terminal, write_blanks};

use self::expression_parser::parse_expression;
use self::functions::{add_standard_functions, compile_expression};
use self::proto_parser::{ProtoGraphemeSource, ProtoGrapheme};

/// Compiled version of ArgumentExpression. The only variant changed is the function call.
enum CompiledArgument {
    Symbol(Symbol),
    Number(i64),
    FunctionCall(Box<dyn Matcher>)
}

/// Type for a function that takes the arguments to a function call and creates a Matcher.
type MatcherFunction = dyn Fn(&CursorPosition, &Vec<(Symbol, CompiledArgument)>, &Vec<Box<dyn Matcher>>) -> Result<Box<dyn Matcher>, ParseError>;

/// Context in which all MatchScript definitions are stored
pub struct MSContext {
    functions: HashMap<Symbol, &'static MatcherFunction>
}

impl MSContext {

    fn new() -> MSContext {
        MSContext {
            functions: HashMap::new()
        }
    }
}

/// A Matcher takes a string slice as input and returns a shorter slice according
/// to its individual rules.
pub trait Matcher: std::fmt::Debug {
    /// Returns a shorter version of `input`, that matches this Matcher's rules.
    /// If no match is found in the input, None is returned instead.
    fn find_match<'a>(&self, input: &'a str) -> Option<&'a str>;

    /// If a Matcher will always match a known string, then return
    /// that string, otherwise return None.
    fn get_static_match(&self) -> Option<&str>;
}

pub enum Grapheme {
    Char(char),
    Matcher(Box<dyn Matcher>)
}

/// A representation of a location in a file.
#[derive(Clone, Debug, PartialEq)]
pub struct CursorPosition {
    line: usize,
    col: usize
}

impl CursorPosition {

    pub fn new() -> CursorPosition {
        CursorPosition {
            line: 0,
            col: 0
        }
    }

    pub fn add_char(&mut self, c: char) -> &mut Self {
        if c == '\n' {
            self.line += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
        return self;
    }

    pub fn add_string(&mut self, str: &str) -> &mut Self {
        str.chars().for_each(|c| { self.add_char(c); });
        return self;
    }

    pub fn add_position(&mut self, other: &CursorPosition) -> &mut Self {
        self.line += other.line;
        if other.line == 0 {
            self.col += other.col;
        } else {
            self.col = other.col;
        }

        return self;
    }
}

#[derive(Debug)]
enum ParseErrorVariant {
    // Parsing errors

    /// A specific character was expected, but another one turned up
    CharsExpected(Vec<char>),

    /// A specific token was expected, but another one turned up
    TokenExpected(String),

    /// A dangling backtick is an incomplete escape sequence
    DanglingBacktick,

    /// The end of input was encountered when there were still unmatched braces
    UnmatchedBrace,

    // Compilation errors

    /// An unknown function was called
    FunctionNotFound(String),

    /// A function got an unknown parameter
    BadParameter(String, String),

    /// A function got an invalid value for a parameter
    BadArgument(String, String),

    /// A function received the wrong number of texts
    BadTextCount(String, usize, usize),

    /// A function required that a text section is known at parse time, but
    /// that requirement was not met.
    StaticMatchRequired,

    // Miscellaneous errors

    /// The encoding of the input had an error
    Encoding(Vec<u8>, &'static Encoding),

    /// An IO-Error occurred
    IO(std::io::Error),

    /// The error was caused by another error
    External(String, Box<dyn Error>)
}

/// Helper function to construct an error message that lists all the
/// characters which were expected at the point that the error occurred.
fn build_char_list(chars: &[char]) -> String {
    return chars.iter()
        .map(|c| format!("'{}'", c))
        .collect::<Vec<_>>()
        .join(", ");
}

impl Display for ParseErrorVariant {

    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::CharsExpected(chars) => write!(f, "Syntax error: {} expected", build_char_list(&chars)),
            Self::TokenExpected(token) => write!(f, "Syntax error: {} expected", token),
            Self::DanglingBacktick => write!(f, "Dangling ` character"),
            Self::UnmatchedBrace => write!(f, "Unmatched left curly brace"),
            Self::FunctionNotFound(function_name) => write!(f, "No function '{}' found", function_name),
            Self::BadParameter(function_name, param_name) => write!(f, "Function '{}' does not take a parameter called '{}'", function_name, param_name),
            Self::BadArgument(param_name, hint) => write!(f, "Invalid value for parameter '{}'. {}", param_name, hint),
            Self::BadTextCount(function_name, expected, actual) => write!(f, "Function '{}' expects {} text sections but got {}", function_name, expected, actual),
            Self::StaticMatchRequired => write!(f, "Text must be known at compile time"),
            Self::Encoding(bytes, encoding) => write!(f, "Could not decode {:?} as valid {}", bytes, encoding.name()),
            Self::IO(error) => write!(f, "IO error: {}", error),
            Self::External(message, error) => write!(f, "{}: {}", message, error),
        }
    }
}

fn get_usable_context<'a>(context: &'a str, position: &CursorPosition, max_length: usize) -> Option<(&'a str, usize)> {
    // Find the correct line in the context
    let mut line_num = position.line;
    let mut line_start = 0;
    for c in context.chars() {
        if line_num <= 0 {
            break;
        }
        line_start += c.len_utf8();
        if c == '\n' {
            line_num -= 1;
        }
    }

    if line_num > 0 {
        // Not enough lines in the context
        return None;
    }

    let mut line_end = line_start;
    for c in context[line_start..].chars() {
        line_end += c.len_utf8();
        if c == '\n' {
            break;
        }
    }

    let line = &context[line_start..line_end];

    if line.len() <= max_length {
        // If the line is shorter than max_length, return the whole line
        return Some((line, 0));
    } else {
        // Otherwise return max_length characters around the col
        let half_max_length = max_length / 2;
        if position.col < half_max_length {
            return Some((&line[..max_length], 0));
        } else if position.col >= line.len() - half_max_length {
            let offset = line.len() - half_max_length;
            return Some((&line[offset..], offset));
        } else {
            let offset = position.col - half_max_length;
            return Some((&line[offset..(offset + max_length)], offset));
        }
    }
}

/// Error returned when anything goes wrong during parsing.
#[derive(Debug)]
pub struct ParseError {
    variant: ParseErrorVariant,
    position: CursorPosition,
    context: Option<String>,
    context_position: Option<CursorPosition>,
    input_name: Option<String>
}

impl ParseError {

    fn new(position: CursorPosition, variant: ParseErrorVariant) -> ParseError {
        ParseError {
            position, 
            variant,
            context: None,
            context_position: None,
            input_name: None
        }
    }

    pub fn get_position(&self) -> &CursorPosition {
        &self.position
    }

    fn get_variant(&self) -> &ParseErrorVariant {
        &self.variant
    }

    fn set_context(&mut self, context_position: CursorPosition, context: &str) -> () {
        self.context_position = Some(context_position);
        self.context = Some(context.to_owned());
    }

    fn set_input_name(&mut self, input_name: &str) -> () {
        self.input_name = Some(input_name.to_owned());
    }

    pub fn write<T: Terminal>(&self, terminal: &mut T) -> std::io::Result<()> {
        // Error: Syntax error: ',' expected
        // ╭── ./examples/hello-world/stderr.expected:5:1 ─
        // │
        // │     Hello Error!
        // │         ^
        // ╰──

        terminal.reset_color()?;

        // Error message
        terminal.set_color_fg(&TermColor::Error)?;
        terminal.write("Error: ")?;
        terminal.set_color_fg(&TermColor::Highlight)?;
        terminal.write(format!("{}\n", self.variant))?;

        let position = match &self.context_position {
            Some(context_position) => {
                let mut position = context_position.clone();
                position.add_position(&self.position);
                position
            },
            None => self.position.clone()
        };

        let max_length = terminal.size()?.0;
        let context = self.context.as_ref().and_then(|c| get_usable_context(&c, &self.position, max_length));

        terminal.set_color_fg(&TermColor::Error)?;
        match (context, &self.input_name) {
            (Some((line, offset)), Some(input_name)) => {
                terminal.write(format!("╭── {}:{}:{} ───\n", input_name, position.line + 1, position.col + 1))?;
                terminal.write("│\n│     ")?;
                terminal.reset_color()?;
                terminal.write(line)?;
                terminal.set_color_fg(&TermColor::Error)?;
                terminal.write("\n│     ")?;
                write_blanks(terminal, self.position.col - offset)?;
                terminal.set_color_fg(&TermColor::Highlight)?;
                terminal.write("^\n")?;
                terminal.set_color_fg(&TermColor::Error)?;
                terminal.write("╰──\n\n")?;
            },
            (None, Some(input_name)) => {
                terminal.write(format!("─── {}:{}:{} ───\n\n", input_name, position.line + 1, position.col + 1))?;
            }
            (Some((line, offset)), None) => {
                terminal.write(format!("╭── On line {}, col {} ───\n", position.line + 1, position.col + 1))?;
                terminal.write("│\n│     ")?;
                terminal.reset_color()?;
                terminal.write(line)?;
                terminal.set_color_fg(&TermColor::Error)?;
                terminal.write("\n│     ")?;
                write_blanks(terminal, self.position.col - offset)?;
                terminal.set_color_fg(&TermColor::Highlight)?;
                terminal.write("^\n")?;
                terminal.set_color_fg(&TermColor::Error)?;
                terminal.write("╰──\n\n")?;
            },
            (None, None) => {
                terminal.write(format!("─── On line {}, col {} ───\n\n", position.line + 1, position.col + 1))?;
            }
        }

        Ok(())
    }
}

pub struct GraphemeSource<R: AsyncReadExt + Unpin> {
    proto_grapheme_source: ProtoGraphemeSource<R>,
    buffer: Vec<ProtoGrapheme>,
    context: MSContext
}

impl<R: AsyncReadExt + Unpin> GraphemeSource<R> {

    pub fn new(proto_grapheme_source: ProtoGraphemeSource<R>, buffer_size: usize) -> GraphemeSource<R> {
        let mut buffer = Vec::with_capacity(buffer_size);
        buffer.resize(buffer_size, ProtoGrapheme::Char('\0'));
        let mut context = MSContext::new();
        add_standard_functions(&mut context);
        return GraphemeSource {
            proto_grapheme_source,
            buffer,
            context
        }
    }

    pub async fn read_graphemes(&mut self, out_buffer: &mut [Grapheme]) -> Result<usize, ParseError> {
        let mut graphemes_read_total = 0;
        while graphemes_read_total < out_buffer.len() {
            let read_stop = self.buffer.len().min(out_buffer.len() - graphemes_read_total);
            let graphemes_read = match self.proto_grapheme_source.read_proto_graphemes(&mut self.buffer[..read_stop]).await {
                Ok(graphemes_read) => graphemes_read,
                Err(mut err) => {
                    err.set_input_name(&self.proto_grapheme_source.get_input_name());
                    return Err(err);
                }
            };

            for (proto_grapheme, grapheme) in self.buffer[..read_stop].iter().zip(out_buffer[graphemes_read_total..].iter_mut()) {
                *grapheme = match proto_grapheme {
                    // Char grapheme
                    ProtoGrapheme::Char(c) => Grapheme::Char(*c),

                    // Expression grapheme
                    ProtoGrapheme::ProtoExpression(expr) => {
                        let context = &expr.expr;
                        let context_position = &expr.position;

                        // Parse
                        let parsed_expr = match parse_expression(&expr.expr) {
                            Ok(parsed_expr) => parsed_expr,
                            Err(mut err) => {
                                err.set_context(context_position.to_owned(), context);
                                err.set_input_name(&self.proto_grapheme_source.get_input_name());
                                return Err(err);
                            }
                        };

                        // Compile
                        let matcher = match compile_expression(&self.context, &parsed_expr) {
                            Ok(matcher) => matcher,
                            Err(mut err) => {
                                err.set_context(context_position.to_owned(), context);
                                err.set_input_name(&self.proto_grapheme_source.get_input_name());
                                return Err(err);
                            }
                        };

                        Grapheme::Matcher(matcher)
                    }
                }
            }

            graphemes_read_total += graphemes_read;
            if graphemes_read < read_stop {
                // End of input was reached
                return Ok(graphemes_read_total);
            }
        }
        return Ok(graphemes_read_total);
    }
}

#[cfg(test)]
mod tests {

    use super::{ParseError, CursorPosition, ParseErrorVariant};
    use crate::cli::StringTerminal;

    #[test]
    fn parse_error_diagnostic_with_name_with_context() -> () {
        let mut error = ParseError::new(CursorPosition { line: 0, col: 2 }, ParseErrorVariant::FunctionNotFound("test".to_owned()));
        error.set_context(CursorPosition { line: 5, col: 5 }, "0123456789");
        error.set_input_name("parse_error_diagnostic_with_name_with_context");

        let mut string_terminal = StringTerminal::new((80, 40));
        error.write(&mut string_terminal).unwrap();

        let expected = 
r"Error: No function 'test' found
╭── parse_error_diagnostic_with_name_with_context:6:8 ───
│
│     0123456789
│       ^
╰──

";      
        assert_eq!(expected, string_terminal.get_buffer());
    }

    #[test]
    fn parse_error_diagnostic_with_name_without_context() -> () {
        let mut error = ParseError::new(CursorPosition { line: 0, col: 2 }, ParseErrorVariant::FunctionNotFound("test".to_owned()));
        error.set_input_name("parse_error_diagnostic_with_name_with_context");

        let mut string_terminal = StringTerminal::new((80, 40));
        error.write(&mut string_terminal).unwrap();

        let expected = 
r"Error: No function 'test' found
─── parse_error_diagnostic_with_name_with_context:1:3 ───

";
        assert_eq!(expected, string_terminal.get_buffer());
    }

    #[test]
    fn parse_error_diagnostic_without_name_with_context() -> () {
        let mut error = ParseError::new(CursorPosition { line: 0, col: 2 }, ParseErrorVariant::FunctionNotFound("test".to_owned()));
        error.set_context(CursorPosition { line: 5, col: 5 }, "0123456789");

        let mut string_terminal = StringTerminal::new((80, 40));
        error.write(&mut string_terminal).unwrap();

        let expected = 
r"Error: No function 'test' found
╭── On line 6, col 8 ───
│
│     0123456789
│       ^
╰──

";
        assert_eq!(expected, string_terminal.get_buffer());
    }

    #[test]
    fn parse_error_diagnostic_without_name_without_context() -> () {
        let error = ParseError::new(CursorPosition { line: 0, col: 2 }, ParseErrorVariant::FunctionNotFound("test".to_owned()));

        let mut string_terminal = StringTerminal::new((80, 40));
        error.write(&mut string_terminal).unwrap();

        let expected = 
r"Error: No function 'test' found
─── On line 6, col 8 ───

";
        assert_eq!(expected, string_terminal.get_buffer());
    }

    // Test GraphemeSource without errors

    // Test GraphemeSource with decoding error
    // Test GraphemeSource with parsing error
    // Test GraphemeSource with compiler error
}
