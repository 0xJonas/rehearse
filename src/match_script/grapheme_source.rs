use crate::match_script::{
    proto_grapheme_source::{ProtoGrapheme, ProtoGraphemeSource},
    functions::{MSContext, Matcher, add_standard_functions, compile_expression},
    expression_parser::parse_expression,
    error::ParseError
};
use tokio::io::AsyncReadExt;

/// A Grapheme represents a single atomic part of a MatchScript file.
/// It can either by a single character or a Matcher that matches
/// a sequence of characters.
pub enum Grapheme {
    Char(char),
    Matcher(Box<dyn Matcher>)
}

/// A GraphemeSource compiles the output of a ProtoGraphemeSource into
/// a sequence of Graphemes. A GraphemeSource also maintains the
/// MSContext used to compile the ProtoGraphemes.
pub struct GraphemeSource<R: AsyncReadExt + Unpin> {
    proto_grapheme_source: ProtoGraphemeSource<R>,
    buffer: Vec<ProtoGrapheme>,
    context: MSContext
}

impl<R: AsyncReadExt + Unpin> GraphemeSource<R> {

    /// Creates a new GraphemeSource from the given ProtoGraphemeSource, with a given `buffer_size`
    /// for the internal read buffer.
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

    /// Returns true is all Graphemes from this GraphemeSource have been read.
    pub fn is_end_of_input(&self) -> bool {
        self.proto_grapheme_source.is_end_of_input()
    }

    /// Reads Graphemes into the given `out_buffer`.
    pub async fn read_graphemes(&mut self, out_buffer: &mut [Grapheme]) -> Result<usize, ParseError> {
        let mut graphemes_read_total = 0;
        while graphemes_read_total < out_buffer.len() {

            // Read ProtoGraphemes
            let read_stop = self.buffer.len().min(out_buffer.len() - graphemes_read_total);
            let graphemes_read = match self.proto_grapheme_source.read_proto_graphemes(&mut self.buffer[..read_stop]).await {
                Ok(graphemes_read) => graphemes_read,
                Err(mut err) => {
                    err.set_input_name(&self.proto_grapheme_source.get_input_name());
                    return Err(err);
                }
            };

            // Parse and compile the ProtoGraphemes
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

    use std::io::Cursor;

    use encoding_rs::UTF_8;

    use super::{GraphemeSource, Grapheme};
    use crate::match_script::{char_source::CharSource, proto_grapheme_source::ProtoGraphemeSource};
    use crate::match_script::error::ParseErrorVariant;

    #[tokio::test]
    async fn grapheme_source_without_error() -> () {
        let input = "Hello {|World|}{|`regex{!+}|}";
        let cursor = Cursor::new(input.to_owned());
        let char_source = CharSource::new(UTF_8, cursor, "grapheme_source_without_error", 1024);
        let proto_grapheme_source = ProtoGraphemeSource::new(char_source, "", 1024);
        let mut grapheme_source = GraphemeSource::new(proto_grapheme_source, 1024);
        let mut output: Vec<Grapheme> = Vec::with_capacity(10);
        output.resize_with(10, || Grapheme::Char('\0'));

        let graphemes_read = grapheme_source.read_graphemes(&mut output[..]).await.unwrap();
        assert_eq!(graphemes_read, 8);
        assert!(grapheme_source.is_end_of_input());
        match output[0] {
            Grapheme::Char(c) => assert_eq!(c, 'H'),
            _ => assert!(false)
        }
        match &output[6] {
            Grapheme::Matcher(m) => assert_eq!(m.get_static_match(), Some("World")),
            _ => assert!(false)
        }
        match &output[7] {
            Grapheme::Matcher(m) => assert_eq!(m.get_static_match(), None),
            _ => assert!(false)
        }
    }

    #[tokio::test]
    async fn grapheme_source_with_decoding_error() -> () {
        let input = b"Hello {|World|}{|`regex{!+}|}\xff";
        let cursor = Cursor::new(input.to_owned());
        let char_source = CharSource::new(UTF_8, cursor, "grapheme_source_with_decoding_error", 1024);
        let proto_grapheme_source = ProtoGraphemeSource::new(char_source, "", 1024);
        let mut grapheme_source = GraphemeSource::new(proto_grapheme_source, 1024);
        let mut output: Vec<Grapheme> = Vec::with_capacity(10);
        output.resize_with(10, || Grapheme::Char('\0'));

        let error = grapheme_source.read_graphemes(&mut output[..]).await.unwrap_err();
        match &error.get_variant() {
            ParseErrorVariant::Encoding(bytes, encoding) => {
                assert_eq!(bytes, &vec![255]);
                assert_eq!(encoding, &UTF_8);
            },
            _ => assert!(false)
        }
        assert_eq!(error.get_input_name(), Some("grapheme_source_with_decoding_error"));
        assert_eq!(error.get_position().line, 0);
        assert_eq!(error.get_position().col, 29);
    }

    #[tokio::test]
    async fn grapheme_source_with_parsing_error() -> () {
        let input = "Hello\n{|World{|}\n{|`regex{!+}|}";
        let cursor = Cursor::new(input.to_owned());
        let char_source = CharSource::new(UTF_8, cursor, "grapheme_source_with_parsing_error", 1024);
        let proto_grapheme_source = ProtoGraphemeSource::new(char_source, "", 1024);
        let mut grapheme_source = GraphemeSource::new(proto_grapheme_source, 1024);
        let mut output: Vec<Grapheme> = Vec::with_capacity(10);
        output.resize_with(10, || Grapheme::Char('\0'));

        let error = grapheme_source.read_graphemes(&mut output[..]).await.unwrap_err();
        match &error.get_variant() {
            ParseErrorVariant::UnmatchedBrace => (),
            _ => assert!(false)
        }
        assert_eq!(error.get_input_name(), Some("grapheme_source_with_parsing_error"));
        assert_eq!(error.get_position().line, 0);
        assert_eq!(error.get_position().col, 5);
        match &error.get_context() {
            &Some((context_position, context)) => {
                assert_eq!(context_position.line, 1);
                assert_eq!(context_position.col, 2);
                assert_eq!(context, "World{");
            },
            _ => assert!(false)
        }
    }

    #[tokio::test]
    async fn grapheme_source_with_compile_error() -> () {
        let input = "Hello\n{|World|}\n{|`what_func|}";
        let cursor = Cursor::new(input.to_owned());
        let char_source = CharSource::new(UTF_8, cursor, "grapheme_source_with_compile_error", 1024);
        let proto_grapheme_source = ProtoGraphemeSource::new(char_source, "", 1024);
        let mut grapheme_source = GraphemeSource::new(proto_grapheme_source, 1024);
        let mut output: Vec<Grapheme> = Vec::with_capacity(10);
        output.resize_with(10, || Grapheme::Char('\0'));

        let error = grapheme_source.read_graphemes(&mut output[..]).await.unwrap_err();
        match &error.get_variant() {
            ParseErrorVariant::FunctionNotFound(fun) => assert_eq!(fun, "what_func"),
            _ => assert!(false)
        }
        assert_eq!(error.get_input_name(), Some("grapheme_source_with_compile_error"));
        assert_eq!(error.get_position().line, 0);
        assert_eq!(error.get_position().col, 0);
        match &error.get_context() {
            &Some((context_position, context)) => {
                assert_eq!(context_position.line, 2);
                assert_eq!(context_position.col, 2);
                assert_eq!(context, "`what_func");
            },
            _ => assert!(false)
        }
    }
}
