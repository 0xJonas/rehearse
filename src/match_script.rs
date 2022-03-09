mod proto_parser;
mod expression_parser;
// mod functions;

// use tokio::io::AsyncReadExt;

use std::fmt::{Display, Formatter};
use std::error::Error;

/// A Matcher takes a string slice as input and returns a shorter slice according
/// to its individual rules.
trait Matcher {
    /// Returns a shorter version of `input`, that matches this Matcher's rules.
    /// If no match is found in the input, None is returned instead.
    fn find_match<'a>(&self, input: &'a str) -> Option<&'a str>;

    /// If a Matcher will always match a known string, then return
    /// that string, otherwise return None.
    fn get_static_match(&self) -> Option<&str>;
}

enum Grapheme {
    Char(char),
    Matcher(Box<dyn Matcher>)
}

/// A representation of a location in a file.
#[derive(Clone, Debug, PartialEq)]
pub struct CursorPosition {
    line: usize,
    col: usize,
    offset: usize
}

#[derive(Debug)]
enum ParseErrorVariant {
    /// A specific character was expected, but another one turned up
    CharsExpected(Vec<char>),

    /// A specific token was expected, but another one turned up
    TokenExpected(String),

    /// A dangling backtick is an incomplete escape sequence
    DanglingBacktick,

    /// The end of input was encountered when there were still unmatched braces
    UnmatchedBrace,

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
            Self::External(message, error) => write!(f, "{}: {}", message, error),
        }
    }
}

/// Error returned when anything goes wrong during parsing.
#[derive(Debug)]
pub struct ParseError {
    variant: ParseErrorVariant,
    position: CursorPosition
}

// async fn read_graphemes<R: AsyncReadExt + Unpin>(source: &mut R, buffer: &mut [Grapheme]) -> Result<usize, ()> {

// }
