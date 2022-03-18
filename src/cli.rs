use log::{trace};
use crossterm::QueueableCommand;
use crossterm::style::{Print, SetForegroundColor, SetBackgroundColor, Color};

use std::collections::HashMap;
use std::io::Write;
use std::fmt::Display;
use std::sync::Arc;

/// Indices into the color palette for terminal colors
#[derive(Debug, Clone, Copy)]
pub enum TermColor {
    /// Text highlight color
    Highlight,

    /// Warning Color
    Warning,

    /// Error Color
    Error,

    /// Foreground color for added text in a diff view
    DiffAdded,

    /// Background color for added text in a diff view
    DiffAddedBg,

    /// Foreground color for deleted text in a diff view
    DiffDeleted,

    /// Background color for deleted text in a diff view
    DiffDeletedBg
}

/// Color modes for AnsiTerminals
#[derive(Debug, Clone, Copy)]
pub enum TermColorMode {
    /// Do not use color. In this mode, all calls to color-related functions
    /// do nothing.
    None,

    /// Use the colors from the 8-color palette.
    C8,

    /// Use the colors from the 256-color palette.
    C256
}

/// Color palette when using 256 Ansi-color mode.
const PALETTE_256: [Color; 7] = [
    Color::AnsiValue(39),   // Highlight: rgb(0,175,255)
    Color::AnsiValue(221),  // Warning: rgb(255, 215, 95)
    Color::AnsiValue(196),  // Error: rgb(255, 0, 0)
    Color::AnsiValue(48),   // DiffAdded: rgb(0,255,135)
    Color::AnsiValue(23),   // DiffAddedBg: rgb(0,95,95)
    Color::AnsiValue(205),  // DiffDeleted: rgb(255,95,175)
    Color::AnsiValue(53),   // DiffDeletedBg: rgb(95,0,95)
];

/// Color palette when using 8-color + intensity mode
const PALETTE_8: [Color; 7] = [
    Color::Cyan,        // Highlight
    Color::Yellow,      // Warning
    Color::Red,         // Error
    Color::Green,       // DiffAdded
    Color::DarkGreen,   // DiffAddedBg
    Color::Red,         // DiffDeleted
    Color::DarkRed      // DiffDeletedBg
];

/// Represents a Terminal
pub trait Terminal {

    /// Returns the current size of the terminal as (columns, rows)
    fn size(&self) -> std::io::Result<(usize, usize)>;

    /// Sets the terminal's foreground color.
    fn set_color_fg(&mut self, color: &TermColor) -> std::io::Result<()>;

    /// Sets the terminal's background color
    fn set_color_bg(&mut self, color: &TermColor) -> std::io::Result<()>;

    /// Resets the terminal colors to their default values
    fn reset_color(&mut self) -> std::io::Result<()>;

    /// Writes text to the internal buffer. This text is not displayed immediately
    /// but only after a call to flush().
    fn write<D: Display>(&mut self, buf: D) -> std::io::Result<()>;

    /// Flushes the internal buffer by writing its contents to the terminal.
    /// This uses stdout.
    fn flush(&mut self) -> std::io::Result<()>;
}

/// Represents a terminal that may or may not be able to display color.
#[derive(Clone)]
pub struct ColorTerminal {
    stdout: Arc<std::io::Stdout>,
    color_mode: TermColorMode
}

impl ColorTerminal {

    /// Creates a new ColorTerminal with the specified TermColorMode.
    /// This function does not check whether the supplied TermColorMode
    /// is actually supported.
    pub fn new(color_mode: TermColorMode) -> ColorTerminal {
        ColorTerminal {
            stdout: Arc::new(std::io::stdout()),
            color_mode
        }
    }

    /// Utility function to return a color from a color palette by using
    /// a TermColor as an index.
    fn get_color_from_palette(palette: &[Color; 7], color: &TermColor) -> Color {
        match color {
            TermColor::Highlight => palette[0],
            TermColor::Warning => palette[1],
            TermColor::Error => palette[2],
            TermColor::DiffAdded => palette[3],
            TermColor::DiffAddedBg => palette[4],
            TermColor::DiffDeleted => palette[5],
            TermColor::DiffDeletedBg => palette[6]
        }
    }
}

impl Terminal for ColorTerminal {
    
    fn size(&self) -> std::io::Result<(usize, usize)> {
        let (w, h) = crossterm::terminal::size()?;
        Ok((w as usize, h as usize))
    }

    fn set_color_fg(&mut self, color: &TermColor) -> std::io::Result<()> {
        let palette_color = match self.color_mode {
            TermColorMode::None => return Ok(()),
            TermColorMode::C256 => ColorTerminal::get_color_from_palette(&PALETTE_256, color),
            TermColorMode::C8 => ColorTerminal::get_color_from_palette(&PALETTE_8, color)
        };

        match self.stdout.lock().queue(SetForegroundColor(palette_color)) {
            Ok(_) => Ok(()),
            Err(e) => Err(e)
        }
    }

    fn set_color_bg(&mut self, color: &TermColor) -> std::io::Result<()> {
        let palette_color = match self.color_mode {
            TermColorMode::None => return Ok(()),
            TermColorMode::C256 => ColorTerminal::get_color_from_palette(&PALETTE_256, color),
            TermColorMode::C8 => ColorTerminal::get_color_from_palette(&PALETTE_8, color)
        };

        match self.stdout.lock().queue(SetBackgroundColor(palette_color)) {
            Ok(_) => Ok(()),
            Err(e) => Err(e)
        }
    }

    fn reset_color(&mut self) -> std::io::Result<()> {
        let mut stdout_lock = self.stdout.lock();
        if let Err(e) = stdout_lock.queue(SetForegroundColor(Color::Reset)) {
            return Err(e);
        }
        if let Err(e) = stdout_lock.queue(SetBackgroundColor(Color::Reset)) {
            return Err(e);
        }
        return Ok(());
    }

    fn write<D: Display>(&mut self, buf: D) -> std::io::Result<()> {
        match self.stdout.lock().queue(Print(buf)) {
            Ok(_) => Ok(()),
            Err(e) => Err(e)
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.stdout.lock().flush()
    }
}

impl Drop for ColorTerminal {

    /// Reset the terminal colors when the terminal is dropped.
    fn drop(&mut self) {
        self.reset_color().unwrap_or(());   // Ignore result
    }
}

/// Represents a Terminal that collects the output into a String.
/// This struct is intended to be used for testing purposes only as is lacks
/// several features from the ColorTerminal.
/// 
/// Specifically, a StringTerminal cannot display color and does not buffer
/// the output.
pub struct StringTerminal {
    buffer: String,
    size: (usize, usize)
}

impl StringTerminal {

    /// Creates a new StringTerminal with the given (columns, rows) `size`.
    pub fn new(size: (usize, usize)) -> StringTerminal {
        StringTerminal {
            buffer: String::new(),
            size
        }
    }

    pub fn get_buffer(&self) -> &str {
        &self.buffer
    }
}

impl Terminal for StringTerminal {

    /// Returns the size that was set when creating the StringTerminal
    fn size(&self) -> std::io::Result<(usize, usize)> {
        Ok(self.size)
    }

    /// Does nothing
    fn set_color_fg(&mut self, _color: &TermColor) -> std::io::Result<()> { Ok(()) }

    /// Does nothing
    fn set_color_bg(&mut self, _color: &TermColor) -> std::io::Result<()> { Ok(()) }

    /// Does nothing
    fn reset_color(&mut self) -> std::io::Result<()> { Ok(()) }

    /// Appends the given text to the internal String. This is done immediately
    /// and not just after a call to flush().
    fn write<D: Display>(&mut self, buf: D) -> std::io::Result<()> {
        self.buffer.push_str(&format!("{}", buf));
        Ok(())
    }

    /// Does nothing
    fn flush(&mut self) -> std::io::Result<()> { Ok(()) }
}

/// Writes a number of spaces to a Terminal.
/// 
/// # Examples
/// 
/// ```
/// # use rehearse::cli::{StringTerminal, write_blanks};
/// let mut terminal = StringTerminal::new((80, 40));
/// write_blanks(&mut terminal, 10);
/// assert_eq!(terminal.get_buffer(), "          ");
/// ```
pub fn write_blanks<T: Terminal>(terminal: &mut T, len: usize) -> std::io::Result<()> {
    let mut blanks = String::with_capacity(len);
    for _ in 0..len {
        blanks.push(' ');
    }
    terminal.write(&blanks[..])?;
    return Ok(());
}

/// Wraps a text into individual lines with a given maximum length.
/// The text is wrapped according to the following rules:
/// * If the text contains a newline character, it is wrapped at that character
/// * If a line would be longer than the allowed maximum length, it is wrapped at the last space character
/// * If the line does not contain a space character, it is wrapped at the maximum line length, even if there
/// is no space character at that position.
fn wrap_lines(text: &str, max_len: usize) -> Vec<&str> {
    let mut out = Vec::new();
    let mut start_pos = 0;
    let mut current_pos = 0;
    let mut last_space = 0;
    let mut current_line_len = 0;

    for c in text.chars() {
        if c == '\n' {
            out.push(&text[start_pos..current_pos]);
            start_pos = current_pos + 1;    // Skip newline
            current_line_len = 0;
        } else if current_line_len >= max_len {
            if last_space <= start_pos {
                out.push(&text[start_pos..current_pos]);
                start_pos = current_pos;
            } else {
                out.push(&text[start_pos..last_space]);
                start_pos = last_space + 1; // Skip space
            }
            current_line_len = 0;
        } else if c == ' ' {
            last_space = current_pos;
        }

        current_pos += c.len_utf8();
        current_line_len += 1;
    }
    out.push(&text[start_pos..]);

    return out;
}

/// A single argument for an ArgTemplate
pub struct Arg {
    short: Option<String>,
    long: String,
    description: String,
    parameter: Option<String>
}

impl Arg {

    /// Creates a new Arg with the given `long` name and `description`.
    pub fn new(long: &str, description: &str) -> Arg {
        Arg {
            short: None,
            long: String::from(long),
            description: String::from(description),
            parameter: None
        }
    }

    /// Sets the `short` name for an Arg.
    pub fn short(mut self, short: &str) -> Arg {
        self.short = Some(String::from(short));
        return self;
    }

    /// Sets the name of the Arg's `parameter`.
    pub fn parameter(mut self, parameter: &str) -> Arg {
        self.parameter = Some(String::from(parameter));
        return self;
    }
}

/// The result of a successful call to `parse_args`.
/// This struct contains the parsed arguments as well as any remaining file arguments.
pub struct ArgResult {
    args: HashMap<String, String>,
    rest: Vec<String>
}

impl ArgResult {

    fn new() -> Self {
        ArgResult {
            args: HashMap::new(),
            rest: Vec::new()
        }
    }

    /// Returns the value parsed for the given argument name.
    /// If the argument does not take a parameter, Some("") is returned.
    /// If the argument does take a parameter, but none was given, None is returned.
    pub fn get_argument<'a>(&'a self, arg: &str) -> Option<&'a str> {
        match self.args.get(&String::from(arg)) {
            Some(val) => Some(val as &str),
            None => None
        }
    }

    /// Gets a vector containing all arguments that where not parsed by the ArgTemplate.
    pub fn get_rest(&self) -> &Vec<String> {
        &self.rest
    }
}

/// Description of the arguments which an application might accept.
pub struct ArgTemplate {
    args: Vec<Arg>,
    usage_header: String,
    usage_trailer: String
}

/// Error returned in case of an unsuccessful call to parse_args().
/// The String member contains an informative message on what went wrong.
#[derive(Debug)]
pub struct ArgError(pub String);

impl ArgTemplate {

    /// Creates a new ArgTemplate.
    pub fn new() -> Self {
        Self {
            args: Vec::new(),
            usage_header: String::from(""),
            usage_trailer: String::from("")
        }
    }

    /// Sets text that will be printed before the argument list, by call to write_usage()
    pub fn set_usage_header(mut self, header: &str) -> Self {
        self.usage_header = String::from(header);
        return self;
    }

    /// Sets text that will be printed after the argument list, by call to write_usage()
    pub fn set_usage_trailer(mut self, trailer: &str) -> Self {
        self.usage_trailer = String::from(trailer);
        return self;
    }

    /// Adds an argument to the ArgTemplate.
    pub fn add_argument(mut self, arg: Arg) -> Self {
        self.args.push(arg);
        return self;
    }

    /// Computes the maximum length of the short and long argument names. This
    /// is used as a helper function when printing the usage.
    /// 
    /// The first value is the maximum length of the short argument names.
    /// The second value is the maximum length of the long argument names concatenated
    /// with a space, concatened with the parameter names.
    fn compute_argument_lengths(&self) -> (usize, usize) {
        let max = |a, b| if a > b { a } else { b };

        self.args.iter()
            .map(|arg| {
                let short_len = arg.short.as_ref().map_or(0, |s| s.len());
                let long_len = arg.long.len();
                let param_len = arg.parameter.as_ref().map_or(0, |s| s.len() + 1);
                (short_len, long_len + param_len)
            })
            .fold((0, 0), |(short_acc, long_acc), (short_len, long_len)| 
                (max(short_acc, short_len), max(long_acc, long_len)))
    }

    /// Writes a message containing the descriptions of the arguements
    /// to the given writer.
    /// 
    /// # Examples
    /// 
    /// ```
    /// # use rehearse::cli::{StringTerminal, Arg, ArgTemplate, ArgResult};
    /// let mut terminal = StringTerminal::new((80, 40));
    /// let arg_template = ArgTemplate::new()
    ///     .set_usage_header("This is the usage header")
    ///     .add_argument(Arg::new("--arg1", "The first argument").short("-1"))
    ///     .add_argument(Arg::new("--arg2", "The second argument").parameter("<thing>"))
    ///     .add_argument(Arg::new("--arg3", "The second argument with a very long description that is really verbose and spans multiple lines and goes on and on and on...").parameter("<thing>"))
    ///     .set_usage_trailer("This is the usage trailer");
    /// let reference = "\
    /// This is the usage header
    ///
    /// USAGE:
    ///     my_prog [OPTIONS] files...
    ///
    /// OPTIONS:
    ///     -1, --arg1         The first argument
    ///         --arg2 <thing> The second argument
    ///         --arg3 <thing> The second argument with a very long description that is
    ///                        really verbose and spans multiple lines and goes on and
    ///                        on and on...
    ///
    /// This is the usage trailer
    /// ";
    /// 
    /// arg_template.write_usage(&mut terminal, "my_prog").unwrap();
    /// assert_eq!(terminal.get_buffer(), reference);
    /// ```
    pub fn write_usage<T: Terminal>(&self, terminal: &mut T, program_name: &str) -> std::io::Result<()> {
        trace!("Printing help text...");
        
        // Write usage header
        terminal.reset_color()?;
        terminal.write(format!("{}\n\n", self.usage_header))?;
        
        let (term_width, _) = terminal.size()?;
        let term_len = term_width as usize;
        let indentation = 4;
        
        terminal.write("USAGE:\n")?;
        write_blanks(terminal, indentation)?;
        terminal.write(format!("{} [OPTIONS] files...\n\n", program_name))?;
        terminal.write("OPTIONS:\n")?;

        let (short_len, long_len) = self.compute_argument_lengths();
        let description_start_col_raw = indentation + short_len + 2 + long_len + 1;
        let description_start_col = if description_start_col_raw > term_len / 2 {
            term_len / 2
        } else {
            description_start_col_raw
        };

        // Write actual arguments
        for arg in self.args.iter() {
            // Indentation
            write_blanks(terminal, indentation)?;

            terminal.set_color_fg(&TermColor::Highlight)?;

            // Short argument name
            match &arg.short {
                Some(str) => {
                    terminal.write(format!("{},", str))?;
                    write_blanks(terminal, short_len - str.len() + 1)?;
                },
                None => {
                    write_blanks(terminal, short_len + 2)?;
                }
            }

            // Long argument name
            terminal.write(&arg.long[..])?;

            // Parameter
            match &arg.parameter {
                Some(str) => {
                    terminal.write(format!(" {}", str))?;
                    write_blanks(terminal, long_len - (arg.long.len() + 1 + str.len()) + 1)?;
                },
                None => {
                    write_blanks(terminal, long_len - arg.long.len() + 1)?;
                }
            }

            terminal.reset_color()?;

            // Description
            let description_lines = wrap_lines(&arg.description, term_len - description_start_col);
            
            // Write first line of description
            let current_col = indentation + short_len + 2 + arg.long.len() + 1;
            if current_col >= description_start_col {
                // If the current column is greater than the intended starting column for
                // the argument description, then start on the next line.
                terminal.write("\n")?;
                write_blanks(terminal, description_start_col)?;
            }
            terminal.write(format!("{}\n", description_lines.first().unwrap()))?;

            // Write remaining lines of description
            for line in description_lines.iter().skip(1) {
                write_blanks(terminal, description_start_col)?;
                terminal.write(format!("{}\n", line))?;
            }
        }

        // Write usage trailer
        terminal.reset_color()?;
        terminal.write(format!("\n{}\n", self.usage_trailer))?;

        terminal.flush()?;
        return Ok(());
    }

    /// Parses an iterator of Strings according to the ArgTemplate.
    /// Returns a Result containing either the parsed arguments in an
    /// ArgResult, or an ArgError if the sequence of arguments could
    /// not be parsed.
    /// 
    /// Currently, the only way an ArgError can be returned if the last
    /// item in the sequence is an argument expecting a parameter.
    /// 
    /// # Examples
    /// 
    /// ```
    /// # use rehearse::cli::{Arg, ArgTemplate, ArgResult};
    /// let arg_template = ArgTemplate::new()
    ///     .add_argument(Arg::new("--arg1", "The first argument"))
    ///     .add_argument(Arg::new("--arg2", "The second argument").parameter("<thing>"))
    ///     .add_argument(Arg::new("--arg3", "The third argument").short("-3"))
    ///     .add_argument(Arg::new("--arg4", "The fourth argument").short("-4").parameter("<thing>"));
    /// 
    /// let test_args = vec!["my_prog", "--arg2", "x", "-3", "something1", "-4", "y", "something2"];
    /// 
    /// let res = arg_template.parse_args(test_args).unwrap();
    /// assert!(res.get_argument("--arg1").is_none());
    /// assert_eq!(res.get_argument("--arg2").unwrap(), "x");
    /// assert_eq!(res.get_argument("--arg3").unwrap(), "");
    /// assert_eq!(res.get_argument("--arg4").unwrap(), "y");
    /// assert_eq!(res.get_rest(), &vec![String::from("something1"), String::from("something2")]);
    /// ```
    pub fn parse_args<T>(&self, args_iter: T) -> Result<ArgResult, ArgError>
    where
        T: IntoIterator,
        T::Item: AsRef<str>
    {
        let mut iter = args_iter.into_iter().skip(1);
        let mut out = ArgResult::new();

        // Iterate over the arguments
        while let Some(arg_str) = iter.next() {
            // Helper function to check whether a string matches an Arg
            let matches_arg = |arg: &&Arg| {
                return arg.long == arg_str.as_ref() ||
                arg.short.as_ref().map_or(false, |s| s == arg_str.as_ref())
            };

            if let Some(arg) = self.args.iter().find(matches_arg) {
                // Argument found
                if arg.parameter.is_some() {
                    // Argument required parameter
                    if let Some(param) = iter.next() {
                        // Parameter found
                        out.args.insert(arg.long.clone(), String::from(param.as_ref()));
                    } else {
                        // Parameter missing
                        return Err(ArgError(format!("Missing required argument for {}", arg.long)));
                    }
                } else {
                    // Argument does not require parameter, insert empty string
                    out.args.insert(arg.long.clone(), String::new());
                }
            } else {
                // No argument found
                out.rest.push(String::from(arg_str.as_ref()));
            }
        }
        
        return Ok(out);
    }
}
