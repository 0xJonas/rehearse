use encoding_rs::{Encoding, Decoder, DecoderResult};
use tokio::io::AsyncReadExt;

use crate::match_script::{Grapheme, CharSource, ProtoGraphemeSource, GraphemeSource, ParseError, CursorPosition, ParseErrorVariant};

const READ_BUFFER_SIZE: usize = 1024;

#[derive(Clone, Copy, PartialEq)]
struct InputCheckpoint(usize, usize);

#[derive(PartialEq)]
enum DiffAction {
    // The start of a diff
    Start,

    // A character was matched (no-op, used if the end of the buffer was hit)
    Match,

    // A character was added compared to the reference
    Add,

    // A character was deleted compared to the reference
    Delete,

    // An expression from the reference did not produce a match
    DeleteExpr
}

#[derive(PartialEq)]
struct DiffNode {
    offset_ref: u64,
    offset_test: u64,
    parent: u32,
    deviation: i32,
    action: DiffAction
}

/// Auxiliary type to manage the parameters for a Differ.
pub struct DifferParams<'ref_name, 'test_name, 'delimiter_tag, R: AsyncReadExt + Unpin> {
    pub ref_input: R,
    pub ref_encoding: &'static Encoding,
    pub ref_name: &'ref_name str,

    pub test_input: R,
    pub test_encoding: &'static Encoding,
    pub test_name: &'test_name str,

    pub delimiter_tag: &'delimiter_tag str,
    pub buffer_size: usize
}

/// Object which manages the reference and test inputs for the Differ.
struct DiffInput<R: AsyncReadExt + Unpin> {
    /// Source from which to read the reference Graphemes.
    grapheme_source: GraphemeSource<R>,

    /// Checkpoints for mapping the Graphemes in `ref_buffer` to byte offsets in the reference input.
    ref_checkpoints: Vec<InputCheckpoint>,

    /// Buffer for the reference Graphemes
    ref_buffer: Vec<Grapheme>,

    /// Number of chars in `ref_buffer` which are marked as processed.
    ref_processed_mark: usize,


    /// Name for the test input. Used for error reporting.
    test_name: String,

    /// Decoder for the test input.
    test_decoder: Decoder,

    /// Checkpoints for mapping the chars in `test_buffer` to byte offsets in the test input.
    test_checkpoints: Vec<InputCheckpoint>,

    /// Buffer for the test chars.
    test_buffer: String,

    /// Char indices for `test_buffer` as returned by `.char_indices()`.
    test_char_indices: Vec<usize>,

    /// Undecoded strip of the last data block passed to `push_test_data(...)`.
    test_buffer_strip: Vec<u8>,

    /// Number of chars in `test_buffer` which are marked as processed.
    test_processed_mark: usize,

    /// CursorPosition which points into the test input. Used for error reporting.
    test_position: CursorPosition,

    /// Total number of bytes read from the test input so far.
    test_byte_offset: usize,

    /// Total number of chars read from the test input so far.
    test_char_offset: usize
}

impl<R: AsyncReadExt + Unpin> DiffInput<R> {

    /// Creates a new `DiffInput` from the given `DifferParams`.
    fn new<'a, 'b, 'c, X: AsyncReadExt + Unpin>(params: DifferParams<'a, 'b, 'c, X>) -> DiffInput<X> {
        let char_source = CharSource::new(params.ref_encoding, params.ref_input, params.ref_name, READ_BUFFER_SIZE);
        let proto_grapheme_source = ProtoGraphemeSource::new(char_source, params.delimiter_tag, READ_BUFFER_SIZE);

        return DiffInput {
            grapheme_source: GraphemeSource::new(proto_grapheme_source, READ_BUFFER_SIZE),
            ref_checkpoints: Vec::new(),
            ref_buffer: Vec::with_capacity(params.buffer_size * 2),
            ref_processed_mark: 0,

            test_name: params.test_name.to_owned(),
            test_decoder: params.test_encoding.new_decoder(),
            test_checkpoints: Vec::new(),
            test_buffer: String::with_capacity(params.buffer_size * 2),
            test_char_indices: Vec::with_capacity(params.buffer_size * 2),
            test_buffer_strip: Vec::with_capacity(16),
            test_processed_mark: 0,
            test_position: CursorPosition::new(),

            test_byte_offset: 0,
            test_char_offset: 0
        };
    }

    /// Marks the given number of chars in the reference and test buffers as processed.
    fn mark_as_processed(&mut self, ref_chars: usize, test_chars: usize) -> () {
        self.ref_processed_mark = ref_chars;
        self.test_processed_mark = test_chars;
    }

    /// Discards all characters in the reference and test buffers which are marked as processed
    /// and shifts the remaining data to the front of the buffers.
    fn discard_processed_data(&mut self) -> () {
        // Move unprocessed data to the front
        // This removes a contiguous chunk at the beginning of the Vecs.
        let _ = self.ref_buffer.drain(..self.ref_processed_mark);
        let _ = self.test_buffer.drain(..self.test_char_indices[self.test_processed_mark]);
        let _ = self.test_char_indices.drain(..self.test_processed_mark);
        self.ref_processed_mark = 0;
        self.test_processed_mark = 0;
    }

    /// Sets the size of the reference buffer and reads as many Graphemes from the reference
    /// input as needed to fill the newly allocated spaces.
    async fn top_up_ref_buffer(&mut self, new_len: usize) -> Result<(), ParseError> {
        let old_len = self.ref_buffer.len();
        let mut graphemes_read = 0;
        self.ref_buffer.resize_with(new_len, || Grapheme::Char('\0'));

        while old_len + graphemes_read < self.ref_buffer.len() && !self.grapheme_source.is_end_of_input() {
            // Record checkpoint
            let (byte_offset, char_offset) = self.grapheme_source.get_input_checkpoint();
            if self.ref_checkpoints.last().cloned() != Some(InputCheckpoint(byte_offset, char_offset)) {
                self.ref_checkpoints.push(InputCheckpoint(byte_offset, char_offset));
            }

            let start_offset = old_len + graphemes_read;
            let end_offset = usize::min(start_offset + READ_BUFFER_SIZE, self.ref_buffer.len());
            graphemes_read += self.grapheme_source.read_graphemes(&mut self.ref_buffer[start_offset..end_offset]).await?;
        }

        return Ok(());
    }

    /// Helper function to decode the strip left from a previous call to `push_test_data`
    /// by using the next block of data. Returns the number of bytes decoded from the new block.
    /// (This does not include the bytes already in the strip)
    fn decode_strip(&mut self, data: &[u8]) -> Result<usize, ParseError> {
        let test_buffer_prev_len = self.test_buffer.len();

        // Pad current strip with parts of the new data to make it at least four characters long.
        let strip_len = self.test_buffer_strip.len();
        let padding_len = usize::min(4 - strip_len, data.len());
        data[..padding_len].iter().for_each(|&b| self.test_buffer_strip.push(b));

        // Make sure that at least 4 characters fit the buffer + 1 byte extra to not
        // hit the panic-branch.
        if self.test_buffer.capacity() - self.test_buffer.len() < 17 {
            self.test_buffer.reserve(17);
        }

        let (res, mut bytes_decoded) = self.test_decoder.decode_to_string_without_replacement(&self.test_buffer_strip, &mut self.test_buffer, false);

        // Handle result
        match res {
            DecoderResult::OutputFull => panic!("Output for decoding the strip was full"),
            DecoderResult::Malformed(bad_bytes, _) => if bytes_decoded - bad_bytes as usize >= strip_len {
                bytes_decoded -= bad_bytes as usize + strip_len
            } else {
                // Encoding error
                self.test_position.add_string(&self.test_buffer[test_buffer_prev_len..]);
                let bad_range_start = bytes_decoded - bad_bytes as usize;
                let bad_range_end = bytes_decoded;

                // Construct error object
                let mut err = ParseError::new(
                    self.test_position.clone(),
                    ParseErrorVariant::Encoding((&self.test_buffer_strip[bad_range_start .. bad_range_end]).to_owned(), self.test_decoder.encoding())
                );
                err.set_input_name(&self.test_name);

                return Err(err);
            },
            DecoderResult::InputEmpty => bytes_decoded -= strip_len
        };

        // Update offsets
        self.test_byte_offset += bytes_decoded;
        self.test_char_offset += self.test_buffer[test_buffer_prev_len..].chars().count();
        self.test_position.add_string(&self.test_buffer[test_buffer_prev_len..]);

        return Ok(bytes_decoded);
    }

    /// Pushes a block of data into the test buffer, increasing its size if necessary.
    /// If the end of the `data` does not fall on a char boundary, the undecoded bytes are saved
    /// and decoded in a subsequent call.
    fn push_test_data(&mut self, data: &[u8]) -> Result<(), ParseError> {
        let mut data_offset = self.decode_strip(data)?;

        // Decode remaining data
        while data_offset < data.len() - 3 {
            if self.test_buffer.capacity() - self.test_buffer.len() < 4 {
                // encoding_rs requires at least 4 bytes of free space or the function will
                // get stuck.
                self.test_buffer.reserve(4);
            }
            let test_buffer_prev_len = self.test_buffer.len();
            let read_stop = usize::min(data_offset + READ_BUFFER_SIZE, data.len());
            let (res, bytes_decoded) = self.test_decoder.decode_to_string_without_replacement(
                &data[data_offset..read_stop],
                &mut self.test_buffer,
                false
            );
            data_offset += bytes_decoded;

            // Handle result
            match res {
                DecoderResult::Malformed(bad_bytes, _) => {
                    data_offset -= bad_bytes as usize;
                    if data_offset <= read_stop - 4 {
                        // Decoding error
                        // TODO add context
                        self.test_position.add_string(&self.test_buffer[test_buffer_prev_len..]);

                        let mut err = ParseError::new(
                            self.test_position.clone(),
                            ParseErrorVariant::Encoding((data[data_offset - bad_bytes as usize .. data_offset]).to_owned(), self.test_decoder.encoding())
                        );
                        err.set_input_name(&self.test_name);
                        return Err(err);
                    }
                },
                DecoderResult::OutputFull => {
                    self.test_buffer.reserve(self.test_decoder.max_utf8_buffer_length_without_replacement(data.len() - data_offset).unwrap());
                },
                DecoderResult::InputEmpty => ()
            }

            // Generate char indices
            let test_char_indices_prev_len = self.test_char_indices.len();
            for (i, _) in self.test_buffer[test_buffer_prev_len..].char_indices() {
                self.test_char_indices.push(i);
            }

            self.test_position.add_string(&self.test_buffer[test_buffer_prev_len..]);

            // Record checkpoint
            self.test_byte_offset += bytes_decoded;
            self.test_char_offset += self.test_char_indices.len() - test_char_indices_prev_len;
            if self.test_checkpoints.last().cloned() != Some(InputCheckpoint(self.test_byte_offset, self.test_char_offset)) {
                self.test_checkpoints.push(InputCheckpoint(self.test_byte_offset, self.test_char_offset));
            }
        }

        // Save new strip
        self.test_buffer_strip.clear();
        data[data_offset..].iter().for_each(|&b| self.test_buffer_strip.push(b));

        return Ok(());
    }
}

#[cfg(test)]
mod tests {

    use std::io::Cursor;

    use encoding_rs::UTF_8;

    use crate::match_script::{ParseErrorVariant, Grapheme};

    use super::{DiffInput, DifferParams};

    fn build_utf_8_input(buffer_size: usize, name: &str, ref_input: &str, test_input: &str) -> DiffInput<Cursor<String>> {
        let ref_cursor = Cursor::new(ref_input.to_owned());
        let test_cursor = Cursor::new(test_input.to_owned());

        let params = DifferParams {
            ref_input: ref_cursor,
            ref_encoding: UTF_8,
            ref_name: name,

            test_input: test_cursor,
            test_encoding: UTF_8,
            test_name: name,

            delimiter_tag: "",
            buffer_size
        };

        return DiffInput::<Cursor<String>>::new(params);
    }

    fn build_empty_utf_8_input() -> DiffInput<Cursor<String>> {
        build_utf_8_input(1024, "empty", "", "")
    }

    fn match_graphemes(graphemes: &[Grapheme], input: &str) -> bool {
        input.chars()
            .zip(graphemes.iter())
            .all(|(t, g)| match g {
                &Grapheme::Char(c) => t == c,
                _ => false
            })
    }

    #[test]
    fn test_push_test_data() -> () {
        let test_input = "test test test test test test test test";
        let mut input = build_empty_utf_8_input();

        input.push_test_data(&test_input.as_bytes()[..20]).unwrap();
        assert_eq!(input.test_buffer, &test_input[..20]);
        assert_eq!(input.test_position.col, 20);

        input.push_test_data(&test_input.as_bytes()[20..]).unwrap();
        assert_eq!(input.test_buffer, test_input);
        assert_eq!(input.test_position.col, 39);
    }

    #[test]
    fn test_push_test_data_multibyte_char() -> () {
        let test_input = "test test test test🧪test test test test";
        let mut input = build_empty_utf_8_input();

        input.push_test_data(&test_input.as_bytes()[..20]).unwrap();
        assert_eq!(input.test_buffer, &test_input[..19]);

        input.push_test_data(&test_input.as_bytes()[20..]).unwrap();
        assert_eq!(input.test_buffer, test_input);
    }

    #[test]
    fn test_push_test_data_encoding_error() -> () {
        let test_input = b"test test test test\xfftest test test test";
        let mut input = build_empty_utf_8_input();

        input.push_test_data(&test_input[..20]).unwrap();
        assert_eq!(input.test_buffer.as_bytes(), &test_input[..19]);

        let error = input.push_test_data(&test_input[20..]).unwrap_err();
        if let ParseErrorVariant::Encoding(bad_data, encoding) = error.get_variant() {
            assert_eq!(bad_data, &vec![255_u8]);
            assert_eq!(encoding, &UTF_8);
            let position = error.get_position();
            assert_eq!(position.col, 19);
            assert_eq!(position.line, 0);
        } else {
            assert!(false);
        }
    }

    #[tokio::test]
    async fn test_top_up_ref_buffer() -> () {
        let ref_input =  "ref ref ref ref ref ref ref ref ref ref";
        let mut input = build_utf_8_input(1024, "test_push_test_data", ref_input, "");

        input.top_up_ref_buffer(20).await.unwrap();
        assert!(match_graphemes(&input.ref_buffer[..20], ref_input));
        assert_eq!(input.ref_buffer.len(), 20);

        input.top_up_ref_buffer(39).await.unwrap();
        assert!(match_graphemes(&input.ref_buffer, ref_input));
    }

    #[tokio::test]
    async fn test_discard_processed_data() -> () {
        let ref_input =  "ref ref ref ref ref ref ref ref ref ref";
        let test_input = "test test🧪test test test test test test";
        let mut input = build_utf_8_input(1024, "test_push_test_data", ref_input, test_input);

        input.push_test_data(test_input[..29].as_bytes()).unwrap();
        input.top_up_ref_buffer(25).await.unwrap();

        assert_eq!(input.test_buffer.len(), 29);
        assert_eq!(input.ref_buffer.len(), 25);

        input.mark_as_processed(10, 10);
        input.discard_processed_data();

        assert_eq!(input.test_buffer, &test_input[13..29]);
        assert!(match_graphemes(&input.ref_buffer, &ref_input[10..25]));
    }
}
