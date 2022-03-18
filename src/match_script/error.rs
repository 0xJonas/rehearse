use crate::cli::{TermColor, Terminal, write_blanks};
use encoding_rs::Encoding;

use std::fmt::{Display, Formatter};
use std::error::Error;

/// A representation of a location in a file.
#[derive(Clone, Debug, PartialEq)]
pub struct CursorPosition {
    pub line: usize,
    pub col: usize
}

impl CursorPosition {

    /// Creates a new CursorPosition at line 0, col 0.
    pub fn new() -> CursorPosition {
        CursorPosition {
            line: 0,
            col: 0
        }
    }

    /// Adds a single character to this `CursorPosition`,
    /// changing the line and col counters according to the character.
    pub fn add_char(&mut self, c: char) -> &mut Self {
        if c == '\n' {
            self.line += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
        return self;
    }

    /// Adds several characters to this `CursorPosition`, changing the
    /// line and col counters accordingly.
    pub fn add_string(&mut self, str: &str) -> &mut Self {
        str.chars().for_each(|c| { self.add_char(c); });
        return self;
    }

    /// Adds another CursorPosition to this one. This operation is not
    /// commutative. This is used to combine multiple CursorPositions when
    /// computing the final position for a ParseError with context.
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

/// The possible kinds of errors that can occur when reading, parsing and compiling
/// MatchScript files.
#[derive(Debug)]
pub enum ParseErrorVariant {
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
/// 
/// A ParseError is required to contain at least a ParseErrorVariant describing
/// the specific error that occurred, and a CursorPosition giving the location
/// in some input data. ParseErrors can optionally include a context with an associated
/// CursorPosition, which is a part of the input data in which the occurred. If a
/// context is given, the position of the error is formed by adding the error's
/// CursorPosition to the CursorPosition of the context.
/// 
/// An error can also optionally include a name for the input data, usually a file name.
#[derive(Debug)]
pub struct ParseError {
    variant: ParseErrorVariant,
    position: CursorPosition,
    context: Option<String>,
    context_position: Option<CursorPosition>,
    input_name: Option<String>
}

impl ParseError {

    /// Creates a new ParseError from a `position` and a `variant`.
    pub fn new(position: CursorPosition, variant: ParseErrorVariant) -> ParseError {
        ParseError {
            position, 
            variant,
            context: None,
            context_position: None,
            input_name: None
        }
    }

    /// Returns the position of the `ParseError`. This is the position that was
    /// supplied with the `new` function. If this `ParseError` does not have a context
    /// set, this is the final position of the error. If it does have a context, then
    /// the final position must be optained by adding the position returned by `get_position`,
    /// to the one returned by `get_context`.
    pub fn get_position(&self) -> &CursorPosition {
        &self.position
    }

    /// Returns the `ParseErrorVariant` which represents the specific error that occurred.
    pub fn get_variant(&self) -> &ParseErrorVariant {
        &self.variant
    }

    /// Sets the context for the error. The context should mirror the part of the input
    /// data in which the error occurred. The `context_position` should be the position
    /// of the start of the context in the complete input data.
    /// 
    /// Once a context is set, it can be overridden, but not cleared.
    pub fn set_context(&mut self, context_position: CursorPosition, context: &str) -> () {
        self.context_position = Some(context_position);
        self.context = Some(context.to_owned());
    }

    /// Returns the context of this `ParseError` along with its position, or None
    /// if no context was set.
    pub fn get_context(&self) -> Option<(&CursorPosition, &str)> {
        match (&self.context_position, &self.context) {
            (Some(cp), Some(c)) => Some((cp, c.as_str())),
            _ => None
        }
    }

    /// Sets the name of the input data. For example, if the input
    /// comes from a file, this should be the filename.
    pub fn set_input_name(&mut self, input_name: &str) -> () {
        self.input_name = Some(input_name.to_owned());
    }

    /// Returns the name of the input data, or None if no name
    /// was set.
    pub fn get_input_name(&self) -> Option<&str> {
        self.input_name
            .as_ref()
            .map(|s| s.as_str())
    }

    /// Writes a diagnostic to the given `terminal`, containing the information currently
    /// stored in this `ParseError`. The format of the output changes depending on what information
    /// is actually contained in the error.
    pub fn write<T: Terminal>(&self, terminal: &mut T) -> std::io::Result<()> {
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
─── On line 1, col 3 ───

";
        assert_eq!(expected, string_terminal.get_buffer());
    }
}
