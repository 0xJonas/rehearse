use crate::match_script::error::{ParseError, CursorPosition};
use crate::match_script::char_source::CharSource;

use tokio::io::AsyncReadExt;

use std::iter::FromIterator;

const LEXEME_LIMIT: usize = 4096;            // 4 KiB, must be smaller than BUFFER_REFILL_AMOUNT in char_source.rs

#[derive(Clone, Debug, PartialEq)]
pub struct ProtoExpression {
    pub expr: String,
    pub position: CursorPosition
}

/// Early stage for a Grapheme, which is either
/// a character or an unparsed expression.
#[derive(Clone, Debug, PartialEq)]
pub enum ProtoGrapheme {
    Char(char),
    ProtoExpression(Box<ProtoExpression>)
}

/// Iterator-like object which creates the first stage of Graphemes from a data source.
pub struct ProtoGraphemeSource<R: AsyncReadExt + Unpin> {
    buffer: Vec<char>,
    buffer_pos: usize,
    buffer_content_len: usize,
    delimiter_tag: Vec<char>,
    source: CharSource<R>,
    position: CursorPosition
}

impl<R: AsyncReadExt + Unpin> ProtoGraphemeSource<R> {

    /// Creates a new `ProtoGraphemeIterator` from the given `AsyncCharSource`,
    /// which parses using `delimiter_tag` and has an internal buffer size of `char_buffer_size`.
    pub fn new(source: CharSource<R>, delimiter_tag: &str, char_buffer_size: usize) -> ProtoGraphemeSource<R> {
        let mut buffer = Vec::with_capacity(char_buffer_size);
        buffer.resize(char_buffer_size, '\0');
        ProtoGraphemeSource {
            buffer,
            buffer_pos: 0,
            buffer_content_len: 0,
            delimiter_tag: delimiter_tag.chars().collect(),
            source,
            position: CursorPosition::new()
        }
    }

    pub fn get_input_name(&self) -> &str {
        &self.source.get_input_name()
    }

    /// Refills the internal buffer by discarding already read data and
    /// filling the remaining space with new data.
    /// 
    /// This function resets `buffer_pos` to 0.
    async fn top_up_buffer(&mut self) -> Result<(), ParseError> {
        // Move remaining content in buffer to the beginning
        let tail_len = self.buffer_content_len - self.buffer_pos;
        self.buffer.copy_within(self.buffer_pos.. , 0);
        self.buffer_pos = 0;

        self.buffer_content_len = tail_len + self.source.read_chars(&mut self.buffer[tail_len..]).await?;

        return Ok(());
    }

    /// Returns true if the char at the current `buffer_offset` matches
    /// the given `char`, false otherwise. Also returns false if the
    /// position to be inspected is outside the contents of the buffer.
    fn match_char(&self, offset: usize, c: char) -> bool {
        if self.buffer_pos + offset < self.buffer_content_len {
            self.buffer[self.buffer_pos + offset] == c
        } else {
            false
        }
    }

    /// Attempts to read a ProtoExpression from the underlying CharSource. It is expected
    /// that the current buffer_pos points to the left brace of the expression.
    fn read_proto_expression(&mut self) -> Option<ProtoExpression> {
        let mut offset = 0;

        if !self.match_char(offset, '{') {
            return None;
        }
        offset += 1;

        // match left delimiter tag
        for d in &self.delimiter_tag {
            if !self.match_char(offset, *d) {
                return None;
            }
            offset += 1;
        }

        // match left pipe
        if !self.match_char(offset, '|') {
            return None;
        }
        offset += 1;

        let expression_start_offset = offset;
        'outer: while offset < LEXEME_LIMIT {
            let expression_end_offset = offset;

            // match right pipe
            if !self.match_char(offset, '|') {
                offset += 1;
                continue;
            }
            offset += 1;

            // match left delimiter tag
            for d in &self.delimiter_tag {
                if !self.match_char(offset, *d) {
                    continue 'outer;
                }
                offset += 1;
            }

            // match right brace
            if !self.match_char(offset, '}') {
                continue;
            }
            offset += 1;

            let expression_start = self.buffer_pos + expression_start_offset;
            let expression_end = self.buffer_pos + expression_end_offset;
            let expr = String::from_iter(&self.buffer[expression_start .. expression_end]);

            // Create CursorPosition for the new ProtoExpression
            let delimiter_tag_str = &self.delimiter_tag.iter().collect::<String>();
            let mut expr_position = self.position.clone();
            expr_position.add_char('{').add_string(&delimiter_tag_str).add_char('|');

            // Update the Source's CursorPosition
            self.buffer_pos += offset;
            self.position.add_char('{').add_string(&delimiter_tag_str).add_char('|');
            self.position.add_string(&expr);
            self.position.add_char('|').add_string(&delimiter_tag_str).add_char('}');

            return Some(ProtoExpression { expr, position: expr_position });
        }

        // LEXEME_LIMIT was reached
        return None;
    }

    /// Reads ProtoGraphemes from the underlying CharSource into the given `buffer`. Returns the number
    /// of ProtoGraphemes read. If less ProtoGraphemes were read than the given buffer is long, it means 
    /// that the underlying CharSource has reached its end of input.
    pub async fn read_proto_graphemes(&mut self, out_buffer: &mut [ProtoGrapheme]) -> Result<usize, ParseError> {
        let mut proto_graphemes_read = 0;
        
        for out_entry in out_buffer {
            // Refresh buffer if it has been read completely
            if self.buffer_pos >= self.buffer_content_len {
                self.top_up_buffer().await?;
            }
            
            if self.buffer_pos < self.buffer_content_len {
                if self.buffer[self.buffer_pos] == '{' {
                    if self.buffer_pos + LEXEME_LIMIT >= self.buffer_content_len {
                        self.top_up_buffer().await?;
                    }

                    match self.read_proto_expression() {
                        Some(expr) => {
                            *out_entry = ProtoGrapheme::ProtoExpression(Box::new(expr));
                        },
                        None => {
                            *out_entry = ProtoGrapheme::Char('{');
                            self.position.add_char('{');
                            self.buffer_pos += 1;
                        }
                    }
                } else {
                    *out_entry = ProtoGrapheme::Char(self.buffer[self.buffer_pos]);
                    self.position.add_char(self.buffer[self.buffer_pos]);
                    self.buffer_pos += 1;
                }

                proto_graphemes_read += 1;
            } else {
                break;
            }
        }
        return Ok(proto_graphemes_read);
    }
}

#[cfg(test)]
mod tests {

    use super::{CharSource, ProtoExpression, ProtoGrapheme, ProtoGraphemeSource};
    use crate::match_script::error::CursorPosition;
    use quickcheck::{Arbitrary, Gen};
    use quickcheck_macros::quickcheck;
    use encoding_rs::UTF_8;
    use tokio::runtime::Builder;

    use std::io::Cursor;

    impl Arbitrary for ProtoGrapheme {

        fn arbitrary(g: &mut Gen) -> ProtoGrapheme {
            match g.choose(&[0, 1]) {
                Some(0) => ProtoGrapheme::Char(char::arbitrary(g)),
                Some(1) => {
                    let mut expr = String::arbitrary(g);
                    expr.retain(|c| c != '{' && c != '}' && c != '|');
                    ProtoGrapheme::ProtoExpression(Box::new(ProtoExpression {
                        expr,
                        position: CursorPosition::new() // We don't know any actual position here and we also don't care
                    }))
                },
                _ => unreachable!()
            }
        }

        fn shrink(&self) -> Box<dyn Iterator<Item=Self>> {
            match &self {
                ProtoGrapheme::Char(c) => Box::new(c.shrink().map(|c| ProtoGrapheme::Char(c))),
                ProtoGrapheme::ProtoExpression(expr) => Box::new(
                    expr.expr.shrink().map(|new_expr| ProtoGrapheme::ProtoExpression(Box::new(ProtoExpression { expr: new_expr, position: CursorPosition::new() })))
                )
            }
        }
    }

    fn serialize_and_fill_positions(proto_graphemes: &mut [ProtoGrapheme], delimiter_tag: &str) -> String {
        let mut position = CursorPosition::new();
        let mut out = String::new();
        
        for proto_grapheme in proto_graphemes {
            match proto_grapheme {
                ProtoGrapheme::Char(c) => {
                    position.add_char(*c);
                    out.push(*c);
                },
                ProtoGrapheme::ProtoExpression(expr) => {
                    let mut expr_position = position.clone();
                    expr_position.add_char('{').add_string(delimiter_tag).add_char('|');
                    expr.position = expr_position;

                    let tag = format!("{{{}|{}|{}}}", delimiter_tag, expr.expr, delimiter_tag);
                    out.push_str(&tag);
                    position.add_string(&tag);
                }
            }
        }
        return out;
    }

    #[quickcheck]
    fn serializing_a_proto_grapheme_iterator_yields_the_original_string(mut proto_graphemes: Vec<ProtoGrapheme>, delimiter_tag: String) -> bool {
        let input = serialize_and_fill_positions(&mut proto_graphemes, &delimiter_tag);
        if input.len() > 100000 {
            // Skip if the input is too long. This keeps the test from taking forever.
            return true;
        }

        let cursor = Cursor::new(input);
        let char_source = CharSource::new(UTF_8, cursor, "serializing_a_proto_grapheme_iterator_yields_the_original_string", 100000 * 4);
        let mut proto_grapheme_source = ProtoGraphemeSource::new(char_source, &delimiter_tag, 100000);
        let mut output = Vec::with_capacity(proto_graphemes.len());
        output.resize(proto_graphemes.len(), ProtoGrapheme::Char('\0'));

        let rt = Builder::new_current_thread().build().unwrap();
        return rt.block_on(async {
            proto_grapheme_source.read_proto_graphemes(&mut output[..]).await.unwrap();
            // println!("Delimiter: {:?}\nInput:  {:?}\nOutput: {:?}", delimiter_tag, proto_graphemes, output);
            if proto_graphemes == output {
                // println!("OK");
                true
            } else {
                // println!("ERROR");
                false
            }
        });
    }
}
