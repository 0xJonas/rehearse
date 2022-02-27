use tokio::io::AsyncReadExt;
use encoding_rs::{UTF_8, Decoder, DecoderResult};

use std::iter::FromIterator;
use std::path::Path;

const BUFFER_REFILL_AMOUNT: usize = 1 << 20; // 1 MiB
const LEXEME_LIMIT: usize = 4096;            // 4 KiB, must be smaller than BUFFER_REFILL_AMOUNT

/// Reads until either the supplied buffer is full, or EOI is reached.
async fn read_to_buffer<R: AsyncReadExt + Unpin>(buffer: &mut [u8], input: &mut R) -> std::io::Result<usize> {
    let mut total_bytes_read = 0;
    loop {
        let bytes_read = input.read(&mut buffer[total_bytes_read..]).await?;
        total_bytes_read += bytes_read;

        if bytes_read == 0 {
            // End of input
            return Ok(total_bytes_read);
        } else if total_bytes_read == buffer.len() {
            // Buffer full
            return Ok(total_bytes_read);
        }
    }
}

/// Data source which decodes an input and returns UTF-8 characters.
struct AsyncCharSource<R: AsyncReadExt + Unpin> {
    read_buffer: Vec<u8>,
    decode_strip_len: usize,
    decoder: Decoder,
    input: R
}

impl<R: AsyncReadExt + Unpin> AsyncCharSource<R> {

    /// Creates a new `AsyncCharSource` from the given `input`, using a `decoder` and a read buffer
    /// of size `buffer_size`.
    fn new(decoder: Decoder, input: R, buffer_size: usize) -> AsyncCharSource<R> {
        let mut read_buffer = Vec::with_capacity(buffer_size);
        read_buffer.resize(buffer_size, 0);
        AsyncCharSource {
            read_buffer: read_buffer,
            decode_strip_len: 0,
            decoder: decoder,
            input: input
        }
    }

    /// Appends characters from the source to the given `buffer`. Returns `true`
    /// if the source has been depleted by the call.
    /// 
    /// This function will append as many characters as the buffer can hold
    /// without reallocating (i.e. until `buffer.capacity()` is reached), or until
    /// the source is depleted.
    async fn append_chars(&mut self, buffer: &mut Vec<char>) -> std::io::Result<bool> {
        let read_buffer_len = self.read_buffer.len();
        let iterations = (buffer.capacity() + read_buffer_len - 1) / read_buffer_len;
        let mut utf8_buffer = String::new();
        let mut end_of_input = false;

        for i in 0..iterations {
            // Read new data
            let read_stop = read_buffer_len.min(buffer.capacity() - i * read_buffer_len);
            let bytes_read = read_to_buffer(&mut self.read_buffer[self.decode_strip_len..read_stop], &mut self.input).await?;
            let data_length = self.decode_strip_len + bytes_read;
            end_of_input = data_length < read_stop;

            // Decode new data
            utf8_buffer.clear();
            utf8_buffer.reserve(data_length);
            let (res, bytes_decoded) = self.decoder.decode_to_string_without_replacement(&self.read_buffer[..data_length], &mut utf8_buffer, end_of_input);
            println!("data_length: {}, bytes_decoded: {}", data_length, bytes_decoded);

            match res {
                DecoderResult::Malformed(bad_bytes, _) => {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        format!(
                            "Could not decode {:?} as valid {}",
                            // The range has to be like this, because the malformed sequence counts as
                            // part of the decoded bytes, even though it was not actually decoded.
                            Vec::from(&self.read_buffer[bytes_decoded - bad_bytes as usize .. bytes_decoded]),
                            self.decoder.encoding().name()
                        )
                    ))
                },
                DecoderResult::OutputFull if end_of_input && bytes_decoded < data_length => {
                    // If the malformed sequence is at the end of the input and the utf8_buffer
                    // is almost full, encoding_rs does not return a 'Malformed' result.
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        format!(
                            "Could not decode {:?} as valid {}",
                            Vec::from(&self.read_buffer[bytes_decoded..data_length]),
                            self.decoder.encoding().name()
                        )
                    ))
                },
                _ => {}
            }

            // Write decode strip to the struct
            self.read_buffer.copy_within(bytes_decoded..data_length, 0);
            self.decode_strip_len = data_length - bytes_decoded;

            // Write new characters to the buffer
            for c in utf8_buffer.chars() {
                buffer.push(c);
            }
            utf8_buffer.clear();

            if end_of_input {
                break;
            }
        }

        return Ok(end_of_input);
    }
}

/// Early stage for a Grapheme, which is either
/// a character or an unparsed expression.
#[derive(Clone, Debug, PartialEq, Eq)]
enum ProtoGrapheme {
    Char(char),
    ProtoExpression(String)
}

/// Iterator-like object which creates the first stage of Graphemes from a data source.
struct ProtoGraphemeIterator<R: AsyncReadExt + Unpin> {
    buffer: Vec<char>,
    buffer_pos: usize,
    delimiter_tag: Vec<char>,
    source: AsyncCharSource<R>,
    end_of_input: bool
}

impl<R: AsyncReadExt + Unpin> ProtoGraphemeIterator<R> {

    /// Creates a new `ProtoGraphemeIterator` from the given `AsyncCharSource`,
    /// which parses using `delimiter_tag` and has an internal buffer size of `char_buffer_size`.
    fn new(source: AsyncCharSource<R>, delimiter_tag: &str, char_buffer_size: usize) -> ProtoGraphemeIterator<R> {
        ProtoGraphemeIterator {
            buffer: Vec::with_capacity(char_buffer_size),
            buffer_pos: 0,
            delimiter_tag: delimiter_tag.chars().collect(),
            source: source,
            end_of_input: false
        }
    }

    /// Refills the internal buffer by discarding already read data and
    /// filling the remaining space with new data.
    /// 
    /// This function resets `buffer_pos` to 0.
    async fn top_up_buffer(&mut self) -> std::io::Result<()> {
        // Move remaining content in buffer to the beginning
        let tail_len = self.buffer.len() - self.buffer_pos;
        self.buffer.copy_within(self.buffer_pos.. , 0);
        self.buffer.truncate(tail_len);
        self.buffer_pos = 0;

        self.end_of_input = self.source.append_chars(&mut self.buffer).await?;

        return Ok(());
    }

    /// Returns the char that is `offset` ahead of the iterator's current pointer or None if
    /// if the offset would reach past the end of the internal buffer.
    /// 
    /// This function should be used instead of using `buffer_pos` directly, because
    /// `buffer_pos` can be changed by other functions.
    fn peek_char(&mut self, offset: usize) -> Option<char> {
        if self.buffer_pos + offset < self.buffer.len() {
            let out = self.buffer[self.buffer_pos + offset];
            return Some(out);
        } else {
            return None;
        }
    }

    /// Utility function that returns the supplied Option<char> and advances the iterator's
    /// pointer if the contents of the Option is Some(...).
    fn advance_pos_and_return(&mut self, opt_char: Option<char>) -> std::io::Result<Option<ProtoGrapheme>> {
        match opt_char {
            Some(t) => {
                self.buffer_pos += 1;
                return Ok(Some(ProtoGrapheme::Char(t)));
            },
            None => Ok(None)
        }
    }

    /// Returns the next Protographeme from this iterator, Ok(None) if the 
    /// iterator is depleted, or an error if the next ProtoGrapheme could not be read.
    async fn next(&mut self) -> std::io::Result<Option<ProtoGrapheme>> {
        fn match_char(opt_char: Option<char>, c: char) -> bool {
            if let Some(t) = opt_char {
                t == c
            } else {
                false
            }
        }

        // Refresh buffer if it has been read completely
        if self.buffer_pos >= self.buffer.len() && !self.end_of_input {
            self.top_up_buffer().await?;
        }

        let mut offset = 0;

        // match left brace
        let first_char = self.peek_char(offset);
        if !match_char(first_char, '{') {
            return self.advance_pos_and_return(first_char);
        }

        // Make sure a maximum-length lexeme can fit into the read buffer
        if self.buffer_pos + LEXEME_LIMIT > self.buffer.len() && !self.end_of_input {
            self.top_up_buffer().await?;
        }

        offset += 1;

        // match left delimiter tag
        for d in self.delimiter_tag.clone() {
            let current_char = self.peek_char(offset);
            offset += 1;
            if !match_char(current_char, d) {
                return self.advance_pos_and_return(first_char);
            }
        }

        // match left pipe
        let current_char = self.peek_char(offset);
        if !match_char(current_char, '|') {
            return self.advance_pos_and_return(first_char);
        }
        offset += 1;

        let expression_start_offset = offset;
        'outer: while offset < LEXEME_LIMIT {
            let expression_end_offset = offset;

            // match right pipe
            let current_char = self.peek_char(offset);
            offset += 1;
            if !match_char(current_char, '|') {
                continue;
            }

            // match left delimiter tag
            for d in self.delimiter_tag.clone() {
                let current_char = self.peek_char(offset);
                offset += 1;
                if !match_char(current_char, d) {
                    continue 'outer;
                }
            }

            // match right brace
            let current_char = self.peek_char(offset);
            offset += 1;
            if !match_char(current_char, '}') {
                continue;
            }

            let expression_start = self.buffer_pos + expression_start_offset;
            let expression_end = self.buffer_pos + expression_end_offset;
            self.buffer_pos += offset;
            return Ok(Some(ProtoGrapheme::ProtoExpression(String::from_iter(&self.buffer[expression_start .. expression_end]))));
        }

        return self.advance_pos_and_return(first_char);
    }
}

/// Creates an iterator-like object which represents the first stage of parsing the
/// artifact file at `path`, using the given `delimiter_tag` to denote special expressions
/// in the file.
fn read_proto_graphemes(path: &Path, delimiter_tag: &str) -> std::io::Result<ProtoGraphemeIterator<tokio::fs::File>> {
    let input = tokio::fs::File::from_std(std::fs::File::open(path)?);
    let source = AsyncCharSource::new(UTF_8.new_decoder(), input, BUFFER_REFILL_AMOUNT * 4);

    Ok(ProtoGraphemeIterator::new(source, delimiter_tag, BUFFER_REFILL_AMOUNT))
}

#[cfg(test)]
mod tests {

    use super::{BUFFER_REFILL_AMOUNT, AsyncCharSource, ProtoGrapheme, ProtoGraphemeIterator};
    use quickcheck::{Arbitrary, Gen};
    use quickcheck_macros::quickcheck;
    use encoding_rs::{UTF_8};
    use tokio::runtime::{Builder};

    use std::io::{Cursor, ErrorKind};

    #[tokio::test]
    async fn char_source_returns_correct_chars() {
        let input = Cursor::new("Test1234");
        let mut source = AsyncCharSource::new(UTF_8.new_decoder(), input, 10000);
        let mut buffer = Vec::<char>::with_capacity(100);
        assert_eq!(source.append_chars(&mut buffer).await.unwrap(), true);
        assert_eq!(buffer, vec!['T', 'e', 's', 't', '1', '2', '3', '4']);
    }

    #[tokio::test]
    async fn char_source_works_with_multiple_calls() {
        let input = Cursor::new("Test1234");
        let mut source = AsyncCharSource::new(UTF_8.new_decoder(), input, 10000);
        let mut buffer = Vec::<char>::with_capacity(4);

        assert_eq!(source.append_chars(&mut buffer).await.unwrap(), false);
        assert_eq!(buffer, vec!['T', 'e', 's', 't']);

        buffer.clear();
        assert_eq!(source.append_chars(&mut buffer).await.unwrap(), false);
        assert_eq!(buffer, vec!['1', '2', '3', '4']);

        buffer.clear();
        assert_eq!(source.append_chars(&mut buffer).await.unwrap(), true);
        assert!(buffer.is_empty());
    }

    #[tokio::test]
    async fn char_source_works_with_multibyte_chars() {
        let input = Cursor::new("Test 🧪");
        let mut source = AsyncCharSource::new(UTF_8.new_decoder(), input, 10000);
        let mut buffer = Vec::<char>::with_capacity(100);

        assert_eq!(source.append_chars(&mut buffer).await.unwrap(), true);
        assert_eq!(buffer, vec!['T', 'e', 's', 't', ' ', '🧪']);
    }

    #[tokio::test]
    async fn char_source_works_with_buffer_sizes_not_aligned_to_char_boundaries() {
        let input = Cursor::new("Test 🧪");
        let mut source = AsyncCharSource::new(UTF_8.new_decoder(), input, 4);
        let mut buffer = Vec::<char>::with_capacity(100);

        assert_eq!(source.append_chars(&mut buffer).await.unwrap(), true);
        assert_eq!(buffer, vec!['T', 'e', 's', 't', ' ', '🧪']);
    }

    #[tokio::test]
    async fn char_source_rejects_malformed_bytes_at_the_end() {
        let input = Cursor::new(b"Error \xff");
        let mut source = AsyncCharSource::new(UTF_8.new_decoder(), input, 10000);
        let mut buffer = Vec::<char>::with_capacity(100);

        let err = source.append_chars(&mut buffer).await.unwrap_err();
        assert_eq!(err.kind(), ErrorKind::InvalidData);
        assert_eq!(format!("{}", err), "Could not decode [255] as valid UTF-8");
    }

    #[tokio::test]
    async fn char_source_rejects_malformed_bytes_in_the_middle() {
        let input = Cursor::new(b"Error \xff More stuff");
        let mut source = AsyncCharSource::new(UTF_8.new_decoder(), input, 10000);
        let mut buffer = Vec::<char>::with_capacity(100);

        let err = source.append_chars(&mut buffer).await.unwrap_err();
        assert_eq!(err.kind(), ErrorKind::InvalidData);
        assert_eq!(format!("{}", err), "Could not decode [255] as valid UTF-8");
    }

    impl Arbitrary for ProtoGrapheme {

        fn arbitrary(g: &mut Gen) -> ProtoGrapheme {
            match g.choose(&[0, 1]) {
                Some(0) => ProtoGrapheme::Char(char::arbitrary(g)),
                Some(1) => {
                    let mut content = String::arbitrary(g);
                    content.retain(|c| c != '{' && c != '}' && c != '|');
                    ProtoGrapheme::ProtoExpression(content)
                },
                _ => unreachable!()
            }
        }
    }

    fn serialize_protographemes(protographemes: &[ProtoGrapheme], delimiter_tag: &str) -> String {
        protographemes.iter()
            .map(|pg| match pg {
                ProtoGrapheme::Char(c) => String::from(*c),
                ProtoGrapheme::ProtoExpression(str) => format!("{{{}|{}|{}}}", delimiter_tag, str, delimiter_tag)
            })
            .collect::<String>()
    }

    #[quickcheck]
    fn serializing_a_protographeme_iterator_yields_the_original_string(protographemes: Vec<ProtoGrapheme>, delimiter_tag: String) -> bool {
        let input = serialize_protographemes(&protographemes, &delimiter_tag);
        let cursor = Cursor::new(input);
        let source = AsyncCharSource::new(UTF_8.new_decoder(), cursor, BUFFER_REFILL_AMOUNT * 4);
        let mut iterator = ProtoGraphemeIterator::new(source, &delimiter_tag, BUFFER_REFILL_AMOUNT);
        let mut output = Vec::with_capacity(protographemes.len());

        let rt = Builder::new_current_thread().build().unwrap();
        return rt.block_on(async {
            while let Ok(Some(pg)) = iterator.next().await {
                output.push(pg);
            }
            protographemes == output
        });
    }
}
