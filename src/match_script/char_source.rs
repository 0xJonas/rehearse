use crate::match_script::error::{ParseError, ParseErrorVariant, CursorPosition};

use tokio::io::AsyncReadExt;
use encoding_rs::{Encoding, Decoder, DecoderResult};

const BUFFER_REFILL_AMOUNT: usize = 8192;

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
pub struct CharSource<R: AsyncReadExt + Unpin> {
    read_buffer: Vec<u8>,
    decode_strip_len: usize,
    decoder: Decoder,
    input: R,
    input_name: String,
    position: CursorPosition,
    end_of_input: bool
}

impl<R: AsyncReadExt + Unpin> CharSource<R> {

    /// Creates a new `AsyncCharSource` from the given `input`, using a `decoder` and a read buffer
    /// of size `buffer_size`.
    pub fn new(encoding: &'static Encoding, input: R, input_name: &str, buffer_size: usize) -> CharSource<R> {
        let mut read_buffer = Vec::with_capacity(buffer_size);
        read_buffer.resize(buffer_size, 0);
        CharSource {
            read_buffer,
            decode_strip_len: 0,
            decoder: encoding.new_decoder(),
            input,
            input_name: input_name.to_owned(),
            position: CursorPosition::new(),
            end_of_input: false
        }
    }

    pub fn get_input_name(&self) -> &str {
        &self.input_name
    }

    /// Appends characters from the source to the given `buffer`. Returns `true`
    /// if the source has been depleted by the call.
    /// 
    /// This function will append as many characters as the buffer can hold
    /// without reallocating (i.e. until `buffer.capacity()` is reached), or until
    /// the source is depleted.
    pub async fn read_chars(&mut self, buffer: &mut [char]) -> Result<usize, ParseError> {
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
            let bytes_read = match read_to_buffer(&mut self.read_buffer[self.decode_strip_len..read_stop], &mut self.input).await {
                Ok(bytes_read) => bytes_read,
                Err(err) => {
                    return Err(ParseError::new(self.position.clone(), ParseErrorVariant::IO(err)));
                }
            };
            let data_length = self.decode_strip_len + bytes_read;
            self.end_of_input = data_length < read_stop;

            // Decode new data
            utf8_buffer.clear();
            utf8_buffer.reserve(data_length);
            let (res, bytes_decoded) = self.decoder.decode_to_string_without_replacement(&self.read_buffer[..data_length], &mut utf8_buffer, self.end_of_input);

            // Handle potential error
            match res {
                DecoderResult::Malformed(bad_bytes, _) => {
                    self.position.add_string(&utf8_buffer);
                    // The range has to be like this, because the malformed sequence counts as
                    // part of the decoded bytes, even though it was not actually decoded.
                    return Err(ParseError::new(
                        self.position.clone(),
                        ParseErrorVariant::Encoding((&self.read_buffer[bytes_decoded - bad_bytes as usize .. bytes_decoded]).to_owned(), self.decoder.encoding())
                    ));
                },
                DecoderResult::OutputFull if self.end_of_input && bytes_decoded < data_length => {
                    self.position.add_string(&utf8_buffer);
                    // If the malformed sequence is at the end of the input and the utf8_buffer
                    // is almost full, encoding_rs does not return a 'Malformed' result.
                    return Err(ParseError::new(
                        self.position.clone(),
                        ParseErrorVariant::Encoding((&self.read_buffer[bytes_decoded..data_length]).to_owned(), self.decoder.encoding())
                    ));
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
            self.position.add_string(&utf8_buffer);
            utf8_buffer.clear();

            if self.end_of_input {
                break;
            }
        }

        return Ok(buffer_offset);
    }
}

#[cfg(test)]
mod tests {

    use super::CharSource;
    use crate::match_script::error::ParseErrorVariant;

    use encoding_rs::UTF_8;

    use std::io::Cursor;

    #[tokio::test]
    async fn char_source_returns_correct_chars() {
        let input = Cursor::new("Test1234");
        let mut source = CharSource::new(UTF_8, input, "char_source_returns_correct_chars", 10000);
        let mut buffer = ['\0'; 100];
        assert_eq!(source.read_chars(&mut buffer).await.unwrap(), 8);
        assert_eq!(&buffer[..8], vec!['T', 'e', 's', 't', '1', '2', '3', '4']);
    }

    #[tokio::test]
    async fn char_source_works_with_multiple_calls() {
        let input = Cursor::new("Test1234");
        let mut source = CharSource::new(UTF_8, input, "char_source_works_with_multiple_calls", 10000);
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
        let mut source = CharSource::new(UTF_8, input, "char_source_works_with_multibyte_chars", 10000);
        let mut buffer = ['\0'; 100];

        assert_eq!(source.read_chars(&mut buffer).await.unwrap(), 6);
        assert_eq!(&buffer[..6], vec!['T', 'e', 's', 't', ' ', '🧪']);
    }

    #[tokio::test]
    async fn char_source_works_with_buffer_sizes_not_aligned_to_char_boundaries() {
        let input = Cursor::new("Test 🧪");
        let mut source = CharSource::new(UTF_8, input, "char_source_works_with_buffer_sizes_not_aligned_to_char_boundaries", 4);
        let mut buffer = ['\0'; 100];

        assert_eq!(source.read_chars(&mut buffer).await.unwrap(), 6);
        assert_eq!(&buffer[..6], vec!['T', 'e', 's', 't', ' ', '🧪']);
    }

    #[tokio::test]
    async fn char_source_rejects_malformed_bytes_at_the_end() {
        let input = Cursor::new(b"Error \xff");
        let mut source = CharSource::new(UTF_8, input, "char_source_rejects_malformed_bytes_at_the_end", 10000);
        let mut buffer = ['\0'; 100];

        let error = source.read_chars(&mut buffer).await.unwrap_err();
        match error.get_variant() {
            ParseErrorVariant::Encoding(bytes, encoding) => {
                assert_eq!(bytes, &[255]);
                assert_eq!(*encoding, UTF_8);
            },
            _ => assert!(false)
        }
        let position = error.get_position();
        assert_eq!(position.line, 0);
        assert_eq!(position.col, 6);
    }

    #[tokio::test]
    async fn char_source_rejects_malformed_bytes_in_the_middle() {
        let input = Cursor::new(b"Error \xff More stuff");
        let mut source = CharSource::new(UTF_8, input, "char_source_rejects_malformed_bytes_in_the_middle", 10000);
        let mut buffer = ['\0'; 100];

        let error = source.read_chars(&mut buffer).await.unwrap_err();
        match error.get_variant() {
            ParseErrorVariant::Encoding(bytes, encoding) => {
                assert_eq!(bytes, &[255]);
                assert_eq!(*encoding, UTF_8);
            },
            _ => assert!(false)
        }
        let position = error.get_position();
        assert_eq!(position.line, 0);
        assert_eq!(position.col, 6);
    }
}
