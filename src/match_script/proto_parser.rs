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
struct CharSource<R: AsyncReadExt + Unpin> {
    read_buffer: Vec<u8>,
    decode_strip_len: usize,
    decoder: Decoder,
    input: R,
    end_of_input: bool
}

impl<R: AsyncReadExt + Unpin> CharSource<R> {

    /// Creates a new `AsyncCharSource` from the given `input`, using a `decoder` and a read buffer
    /// of size `buffer_size`.
    fn new(decoder: Decoder, input: R, buffer_size: usize) -> CharSource<R> {
        let mut read_buffer = Vec::with_capacity(buffer_size);
        read_buffer.resize(buffer_size, 0);
        CharSource {
            read_buffer,
            decode_strip_len: 0,
            decoder,
            input,
            end_of_input: false
        }
    }

    /// Appends characters from the source to the given `buffer`. Returns `true`
    /// if the source has been depleted by the call.
    /// 
    /// This function will append as many characters as the buffer can hold
    /// without reallocating (i.e. until `buffer.capacity()` is reached), or until
    /// the source is depleted.
    async fn read_chars(&mut self, buffer: &mut [char]) -> std::io::Result<usize> {
        if self.end_of_input {
            // if the end of input was reached in a previous call, the decoder will be invalid now,
            // so return early to prevent a panic.
            return Ok(0);
        }

        let read_buffer_len = self.read_buffer.len();
        let iterations = (buffer.len() + read_buffer_len - 1) / read_buffer_len;
        let mut utf8_buffer = String::new();
        let mut buffer_offset = 0;

        for i in 0..iterations {
            // Read new data
            let read_stop = read_buffer_len.min(buffer.len() - i * read_buffer_len);
            let bytes_read = read_to_buffer(&mut self.read_buffer[self.decode_strip_len..read_stop], &mut self.input).await?;
            let data_length = self.decode_strip_len + bytes_read;
            self.end_of_input = data_length < read_stop;

            // Decode new data
            utf8_buffer.clear();
            utf8_buffer.reserve(data_length);
            let (res, bytes_decoded) = self.decoder.decode_to_string_without_replacement(&self.read_buffer[..data_length], &mut utf8_buffer, self.end_of_input);

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
                DecoderResult::OutputFull if self.end_of_input && bytes_decoded < data_length => {
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
                buffer[buffer_offset] = c;
                buffer_offset += 1;
            }
            utf8_buffer.clear();

            if self.end_of_input {
                break;
            }
        }

        return Ok(buffer_offset);
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
struct ProtoGraphemeSource<R: AsyncReadExt + Unpin> {
    buffer: Vec<char>,
    buffer_pos: usize,
    buffer_content_len: usize,
    delimiter_tag: Vec<char>,
    source: CharSource<R>
}

impl<R: AsyncReadExt + Unpin> ProtoGraphemeSource<R> {

    /// Creates a new `ProtoGraphemeIterator` from the given `AsyncCharSource`,
    /// which parses using `delimiter_tag` and has an internal buffer size of `char_buffer_size`.
    fn new(source: CharSource<R>, delimiter_tag: &str, char_buffer_size: usize) -> ProtoGraphemeSource<R> {
        let mut buffer = Vec::with_capacity(char_buffer_size);
        buffer.resize(char_buffer_size, '\0');
        ProtoGraphemeSource {
            buffer,
            buffer_pos: 0,
            buffer_content_len: 0,
            delimiter_tag: delimiter_tag.chars().collect(),
            source
        }
    }

    /// Refills the internal buffer by discarding already read data and
    /// filling the remaining space with new data.
    /// 
    /// This function resets `buffer_pos` to 0.
    async fn top_up_buffer(&mut self) -> std::io::Result<()> {
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

    /// Attempts a ProtoExpression from the underlying CharSource. It is expected
    /// that the current buffer_pos points to the left brace of the expression.
    /// The return value will either be a ProtoGrapheme::ProtoExpression or None.
    fn read_proto_expression(&mut self) -> Option<ProtoGrapheme> {
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
            self.buffer_pos += offset;
            return Some(ProtoGrapheme::ProtoExpression(String::from_iter(&self.buffer[expression_start .. expression_end])));
        }

        // LEXEME_LIMIT was reached
        return None;
    }

    /// Reads ProtoGraphemes from the underlying CharSource into the given `buffer`. Returns the number
    /// of ProtoGraphemes read. If less ProtoGraphemes were read than the given buffer is long, it means 
    /// that the underlying CharSource has reached its end of input.
    pub async fn read_proto_graphemes(&mut self, buffer: &mut [ProtoGrapheme]) -> std::io::Result<usize> {
        let mut proto_graphemes_read = 0;
        
        while proto_graphemes_read < buffer.len() {
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
                            buffer[proto_graphemes_read] = expr;
                        },
                        None => {
                            buffer[proto_graphemes_read] = ProtoGrapheme::Char('{');
                            self.buffer_pos += 1;
                        }
                    }
                } else {
                    buffer[proto_graphemes_read] = ProtoGrapheme::Char(self.buffer[self.buffer_pos]);
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

    use super::{CharSource, ProtoGrapheme, ProtoGraphemeSource};
    use quickcheck::{Arbitrary, Gen};
    use quickcheck_macros::quickcheck;
    use encoding_rs::UTF_8;
    use tokio::runtime::Builder;

    use std::io::{Cursor, ErrorKind};

    #[tokio::test]
    async fn char_source_returns_correct_chars() {
        let input = Cursor::new("Test1234");
        let mut source = CharSource::new(UTF_8.new_decoder(), input, 10000);
        let mut buffer = ['\0'; 100];
        assert_eq!(source.read_chars(&mut buffer).await.unwrap(), 8);
        assert_eq!(&buffer[..8], vec!['T', 'e', 's', 't', '1', '2', '3', '4']);
    }

    #[tokio::test]
    async fn char_source_works_with_multiple_calls() {
        let input = Cursor::new("Test1234");
        let mut source = CharSource::new(UTF_8.new_decoder(), input, 10000);
        let mut buffer = ['\0'; 4];

        assert_eq!(source.read_chars(&mut buffer).await.unwrap(), 4);
        assert_eq!(&buffer[..4], vec!['T', 'e', 's', 't']);

        assert_eq!(source.read_chars(&mut buffer).await.unwrap(), 4);
        assert_eq!(&buffer[..4], vec!['1', '2', '3', '4']);

        assert_eq!(source.read_chars(&mut buffer).await.unwrap(), 0);
    }

    #[tokio::test]
    async fn char_source_works_with_multibyte_chars() {
        let input = Cursor::new("Test 🧪");
        let mut source = CharSource::new(UTF_8.new_decoder(), input, 10000);
        let mut buffer = ['\0'; 100];

        assert_eq!(source.read_chars(&mut buffer).await.unwrap(), 6);
        assert_eq!(&buffer[..6], vec!['T', 'e', 's', 't', ' ', '🧪']);
    }

    #[tokio::test]
    async fn char_source_works_with_buffer_sizes_not_aligned_to_char_boundaries() {
        let input = Cursor::new("Test 🧪");
        let mut source = CharSource::new(UTF_8.new_decoder(), input, 4);
        let mut buffer = ['\0'; 100];

        assert_eq!(source.read_chars(&mut buffer).await.unwrap(), 6);
        assert_eq!(&buffer[..6], vec!['T', 'e', 's', 't', ' ', '🧪']);
    }

    #[tokio::test]
    async fn char_source_rejects_malformed_bytes_at_the_end() {
        let input = Cursor::new(b"Error \xff");
        let mut source = CharSource::new(UTF_8.new_decoder(), input, 10000);
        let mut buffer = ['\0'; 100];

        let err = source.read_chars(&mut buffer).await.unwrap_err();
        assert_eq!(err.kind(), ErrorKind::InvalidData);
        assert_eq!(format!("{}", err), "Could not decode [255] as valid UTF-8");
    }

    #[tokio::test]
    async fn char_source_rejects_malformed_bytes_in_the_middle() {
        let input = Cursor::new(b"Error \xff More stuff");
        let mut source = CharSource::new(UTF_8.new_decoder(), input, 10000);
        let mut buffer = ['\0'; 100];

        let err = source.read_chars(&mut buffer).await.unwrap_err();
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

        fn shrink(&self) -> Box<dyn Iterator<Item=Self>> {
            match &self {
                ProtoGrapheme::Char(c) => Box::new(c.shrink().map(|c| ProtoGrapheme::Char(c))),
                ProtoGrapheme::ProtoExpression(expr) => Box::new(expr.shrink().map(|expr| ProtoGrapheme::ProtoExpression(expr)))
            }
        }
    }

    fn serialize_proto_graphemes(proto_graphemes: &[ProtoGrapheme], delimiter_tag: &str) -> String {
        proto_graphemes.iter()
            .map(|pg| match pg {
                ProtoGrapheme::Char(c) => String::from(*c),
                ProtoGrapheme::ProtoExpression(str) => format!("{{{}|{}|{}}}", delimiter_tag, str, delimiter_tag)
            })
            .collect::<String>()
    }

    #[quickcheck]
    fn serializing_a_proto_grapheme_iterator_yields_the_original_string(proto_graphemes: Vec<ProtoGrapheme>, delimiter_tag: String) -> bool {
        let input = serialize_proto_graphemes(&proto_graphemes, &delimiter_tag);
        if input.len() > 100000 {
            // Skip if the input is too long. This keeps the test from taking forever.
            return true;
        }

        let cursor = Cursor::new(input);
        let char_source = CharSource::new(UTF_8.new_decoder(), cursor, 100000 * 4);
        let mut proto_grapheme_source = ProtoGraphemeSource::new(char_source, &delimiter_tag, 100000);
        let mut output = Vec::with_capacity(proto_graphemes.len());
        output.resize(proto_graphemes.len(), ProtoGrapheme::Char('\0'));

        let rt = Builder::new_current_thread().build().unwrap();
        return rt.block_on(async {
            proto_grapheme_source.read_proto_graphemes(&mut output[..]).await.unwrap();
            proto_graphemes == output
        });
    }
}
