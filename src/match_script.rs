mod proto_parser;
mod expression_parser;
mod functions;

use expression_parser::Symbol;

use tokio::io::AsyncReadExt;
use encoding_rs::Encoding;

use std::fmt::{Display, Formatter};
use std::error::Error;
use std::collections::HashMap;
use std::path::PathBuf;

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

/// Error returned when anything goes wrong during parsing.
#[derive(Debug)]
pub struct ParseError {
    variant: ParseErrorVariant,
    position: CursorPosition,
    context: Option<String>,
    context_position: Option<CursorPosition>,
    file: Option<PathBuf>
}

impl ParseError {

    fn new(position: CursorPosition, variant: ParseErrorVariant) -> ParseError {
        ParseError {
            position, 
            variant,
            context: None,
            context_position: None,
            file: None
        }
    }

    pub fn get_position(&self) -> &CursorPosition {
        &self.position
    }

    fn get_variant(&self) -> &ParseErrorVariant {
        &self.variant
    }

    fn set_context(&mut self, context_position: CursorPosition, context: String) -> () {
        self.context_position = Some(context_position);
        self.context = Some(context);
    }

    fn get_context(&self) -> Option<(&CursorPosition, &String)> {
        if self.context.is_some() && self.context_position.is_some() {
            Some((self.context_position.as_ref().unwrap(), self.context.as_ref().unwrap()))
        } else {
            None
        }
    }

    fn set_file(&mut self, file: PathBuf) -> () {
        self.file = Some(file);
    }

    fn get_file(&self) -> Option<&PathBuf> {
        self.file.as_ref()
    }
}
