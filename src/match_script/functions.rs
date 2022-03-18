//! This module compiles the AST generated in match_script::expression_parser
//! into the Matchers that will be used in the diff algorithm.
//! 
//! To create a Matcher from a FunctionCall, the MatcherFunction of that function is called
//! with the arguments given in the call. This will return a new Matcher that will correspond
//! to the given set of arguments.

use crate::match_script::{
    expression_parser::{
        Symbol,
        TextExpression,
        TextExpressionSegment,
        FunctionCall,
        ArgumentExpression
    },
    error::{ParseError, ParseErrorVariant, CursorPosition}
};

use regex::Regex;

use std::collections::HashMap;

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

    pub fn new() -> MSContext {
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

const FUN_ID_REGEX: &'static str = "regex";

/// Represents a MatchScript function signature
struct FunctionSignature<'a> {
    name: &'static str,
    params: &'a [&'a str],
    num_texts: usize
}

/// Verifies that a set of arguments and text sections corresponds to a given FunctionSignature.
/// Returns an appropriate error if the signature does not match.
fn verify_function_signature(
    signature: &FunctionSignature,
    position: &CursorPosition,
    args: &Vec<(Symbol, CompiledArgument)>,
    texts: &Vec<Box<dyn Matcher>>
)-> Result<(), ParseError> {
    // Check the number of text sections
    if texts.len() != signature.num_texts {
        return Err(ParseError::new(
            position.to_owned(),
            ParseErrorVariant::BadTextCount(signature.name.to_owned(), signature.num_texts, texts.len())
        ));
    }

    // Check each argument
    'arg_loop: for (symbol, _) in args {
        for &param in signature.params {
            if param == symbol {
                continue 'arg_loop;
            }
            return Err(ParseError::new(
                position.to_owned(),
                ParseErrorVariant::BadParameter(signature.name.to_owned(), symbol.to_owned())
            ));
        }
    }

    return Ok(());
}

/// Matcher that matches a constant String
#[derive(Debug)]
struct StringMatcher {
    match_string: String
}

impl Matcher for StringMatcher {

    fn find_match<'a>(&self, input: &'a str) -> Option<&'a str> {
        if input.len() < self.match_string.len() {
            return None;
        }

        for (a, b) in input.chars().zip(self.match_string.chars()) {
            if a != b {
                return None;
            }
        }

        return Some(&input[..self.match_string.len()]);
    }

    fn get_static_match(&self) -> Option<&str> {
        return Some(&self.match_string);
    }
}

/// Matcher that matches a regular expression.
/// The regex must be known at compile time.
#[derive(Debug)]
struct RegexMatcher {
    regex: Regex
}

/// MatcherFunction that constructs a RegexMatcher.
fn build_regex_matcher(
    position: &CursorPosition,
    args: &Vec<(Symbol, CompiledArgument)>,
    texts: &Vec<Box<dyn Matcher>>
) -> Result<Box<dyn Matcher>, ParseError> {
    verify_function_signature(&FunctionSignature { name: FUN_ID_REGEX, params: &[], num_texts: 1 }, &position, args, texts)?;

    return match texts[0].get_static_match() {
        // The regex crate does not (to my knowledge) have dedicated functions
        // to match a string from the start. Adding "\A" to the front of the regex
        // does the same thing.
        // TODO: What if there already is an "\A" at the start?
        Some(text) => match Regex::new(&["\\A", text].concat()) {
            Ok(regex) => Ok(Box::new(RegexMatcher { regex })),
            Err(err) => Err(ParseError::new(
                position.to_owned(),
                ParseErrorVariant::External(format!("Could not parse {} as a regular expression", text), Box::new(err))
            ))
        },
        None => return Err(ParseError::new(position.to_owned(), ParseErrorVariant::StaticMatchRequired))
    };
}

impl Matcher for RegexMatcher {

    fn find_match<'a>(&self, input: &'a str) -> Option<&'a str> {
        self.regex.find(input).map(|m| m.as_str())
    }

    fn get_static_match(&self) -> Option<&str> {
        None
    }
}

/// Matcher for a TextExpression. This is essentially a sequence of StringMatchers
/// and other Matchers, which are matched in order of ocurrence. If all contained Matchers
/// have a static match, then so does this Matcher.
#[derive(Debug)]
struct TextExpressionMatcher {
    matchers: Vec<Box<dyn Matcher>>,
    static_match: Option<String>
}

impl Matcher for TextExpressionMatcher {

    fn find_match<'a>(&self, input: &'a str) -> Option<&'a str> {
        let mut offset = 0;

        for matcher in &self.matchers {
            match matcher.find_match(&input[offset..]) {
                Some(m) => offset += m.len(),
                None => return None
            }
        }

        return Some(&input[..offset]);
    }

    fn get_static_match(&self) -> Option<&str> {
        self.static_match.as_ref().map(|s| s.as_str())
    }
}

/// Compiles a FunctionCall into the corresponding Matcher.
fn compile_function_call(context: &MSContext, funcall: &FunctionCall) -> Result<Box<dyn Matcher>, ParseError> {
    // Lookup function name
    let matcher_function = match context.functions.get(&funcall.name) {
        Some(fun) => fun,
        None => return Err(ParseError::new(
            funcall.position.to_owned(),
            ParseErrorVariant::FunctionNotFound(funcall.name.to_owned())
        ))
    };

    // Compile arguments
    let mut args = Vec::with_capacity(funcall.arguments.len());
    for (symbol, arg) in &funcall.arguments {
        match arg {
            ArgumentExpression::Symbol(v) => args.push((symbol.to_owned(), CompiledArgument::Symbol(v.to_owned()))),
            ArgumentExpression::Number(v) => args.push((symbol.to_owned(), CompiledArgument::Number(*v))),
            ArgumentExpression::FunctionCall(funcall) => args.push((symbol.to_owned(), CompiledArgument::FunctionCall(compile_function_call(context, &funcall)?)))
        }
    }

    // Compile text sections
    let mut texts: Vec<Box<dyn Matcher>> = Vec::with_capacity(funcall.texts.len());
    for text in &funcall.texts {
        texts.push(Box::new(compile_text_expression(context, text)?));
    }

    // Compile function call
    return Ok(matcher_function(&funcall.position, &args, &texts)?);
}

/// Compiles a TextExpression into a TextExpressionMatcher.
fn compile_text_expression(context: &MSContext, expr: &TextExpression) -> Result<TextExpressionMatcher, ParseError> {
    // Compile matchers
    let mut matchers: Vec<Box<dyn Matcher>> = Vec::with_capacity(expr.segments.len());
    for segment in &expr.segments {
        match segment {
            TextExpressionSegment::FunctionCall(funcall) => matchers.push(compile_function_call(context, &funcall)?),
            TextExpressionSegment::String(string) => matchers.push(Box::new(StringMatcher { match_string: string.to_owned() }))
        }
    }

    // Generate static match if possible
    let static_matches: Vec<Option<&str>> = matchers.iter().map(|m| m.get_static_match()).collect();
    let has_static_match = static_matches.iter().all(|o| o.is_some());
    let static_match = if has_static_match {
        Some(
            static_matches.iter()
                .map(|o| o.unwrap())
                .collect::<Vec<_>>()
                .concat()
        )
    } else {
        None
    };

    return Ok(TextExpressionMatcher { matchers, static_match });
}

pub fn add_standard_functions(context: &mut MSContext) -> () {
    context.functions.insert(FUN_ID_REGEX.to_owned(), &build_regex_matcher);
}

pub fn compile_expression(context: &MSContext, expr: &TextExpression) -> Result<Box<dyn Matcher>, ParseError> {
    let compiled_expression = compile_text_expression(context, expr)?;
    Ok(Box::new(compiled_expression))
}

#[cfg(test)]
mod tests {

    use super::{MSContext, compile_expression, add_standard_functions};
    use crate::match_script::{
        error::ParseErrorVariant,
        expression_parser::parse_expression
    };

    #[test]
    fn text_expression_compiles() {
        let expr = parse_expression("hello").unwrap();
        let mut context = MSContext::new();
        add_standard_functions(&mut context);
        let _ = compile_expression(&context, &expr).unwrap();
    }

    #[test]
    fn function_call_compiles() {
        let expr = parse_expression("`regex{hello}").unwrap();
        let mut context = MSContext::new();
        add_standard_functions(&mut context);
        let _ = compile_expression(&context, &expr).unwrap();
    }

    #[test]
    fn function_call_with_bad_text_count_is_rejected() {
        let expr = parse_expression("`regex{hello}{world}").unwrap();
        let mut context = MSContext::new();
        add_standard_functions(&mut context);
        let error = compile_expression(&context, &expr).unwrap_err();
        match error.get_variant() {
            ParseErrorVariant::BadTextCount(name, expected, actual) => {
                assert_eq!(name, "regex");
                assert_eq!(*expected, 1);
                assert_eq!(*actual, 2);
            },
            _ => assert!(false)
        }
        let position = error.get_position();
        assert_eq!(position.line, 0);
        assert_eq!(position.col, 0);
    }

    #[test]
    fn unknown_function_call_is_rejected() {
        let expr = parse_expression("`does_not_exist[arg=hi]{hello}{world}").unwrap();
        let mut context = MSContext::new();
        add_standard_functions(&mut context);
        let error = compile_expression(&context, &expr).unwrap_err();
        match error.get_variant() {
            ParseErrorVariant::FunctionNotFound(name) => assert_eq!(name, "does_not_exist"),
            _ => assert!(false)
        }
        let position = error.get_position();
        assert_eq!(position.line, 0);
        assert_eq!(position.col, 0);
    }

    #[test]
    fn string_matcher() {
        let expr = parse_expression("hello").unwrap();
        let mut context = MSContext::new();
        add_standard_functions(&mut context);
        let compiled = compile_expression(&context, &expr).unwrap();

        assert_eq!(compiled.get_static_match(), Some("hello"));
        assert_eq!(compiled.find_match("hello world"), Some("hello"));
        assert_eq!(compiled.find_match("howdy world"), None);
    }

    #[test]
    fn regex_matcher() {
        let expr = parse_expression("`regex{Wo+ho+!}").unwrap();
        let mut context = MSContext::new();
        add_standard_functions(&mut context);
        let compiled = compile_expression(&context, &expr).unwrap();

        assert_eq!(compiled.get_static_match(), None);
        assert_eq!(compiled.find_match("Woohoo! hi"), Some("Woohoo!"));
        assert_eq!(compiled.find_match("Weeeee!"), None);
    }

    #[test]
    fn nested_regexes_are_not_possible() {
        let expr = parse_expression("`regex{`regex{error}}").unwrap();
        let mut context = MSContext::new();
        add_standard_functions(&mut context);
        let error = compile_expression(&context, &expr).unwrap_err();
        match error.get_variant() {
            ParseErrorVariant::StaticMatchRequired => {},
            _ => assert!(false)
        }
        let position = error.get_position();
        assert_eq!(position.line, 0);
        assert_eq!(position.col, 0);
    }
}
