use std::io::SeekFrom;

use crate::match_script::{
    InputCheckpoint,
    error::{ParseError, ParseErrorVariant, CursorPosition}
};

use tokio::io::{AsyncReadExt, AsyncSeekExt};
use encoding_rs::{Encoding, Decoder, DecoderResult};

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
pub struct CharSource<R: AsyncReadExt + AsyncSeekExt + Unpin> {
    read_buffer: Vec<u8>,
    read_buffer_offset: usize,
    read_buffer_content_len: usize,
    decoder: Decoder,
    input: R,
    input_name: String,
    position: CursorPosition,
    byte_offset: usize,
    char_offset: usize,
    input_exhausted: bool
}

impl<R: AsyncReadExt + AsyncSeekExt + Unpin> CharSource<R> {

    /// Creates a new `AsyncCharSource` from the given `input`, using a `decoder` and a read buffer
    /// of size `buffer_size`.
    pub fn new(encoding: &'static Encoding, input: R, input_name: &str, buffer_size: usize) -> CharSource<R> {
        let mut read_buffer = Vec::with_capacity(buffer_size);
        read_buffer.resize(buffer_size, 0);
        CharSource {
            read_buffer,
            read_buffer_offset: 0,
            read_buffer_content_len: 0,
            decoder: encoding.new_decoder(),
            input,
            input_name: input_name.to_owned(),
            position: CursorPosition::new(),
            byte_offset: 0,
            char_offset: 0,
            input_exhausted: false
        }
    }

    /// Returns the name of the input from which this CharSource is reading.
    pub fn get_input_name(&self) -> &str {
        &self.input_name
    }

    /// Gets the offsets up to which data has been read by the CharSource.
    pub fn get_input_checkpoint(&self) -> InputCheckpoint {
        InputCheckpoint {
            byte_offset: self.byte_offset,
            char_offset: self.char_offset,
            position: self.position.clone()
        }
    }

    /// Returns true if all chars from this CharSource have been read.
    pub fn is_end_of_input(&self) -> bool {
        self.input_exhausted && self.read_buffer_offset >= self.read_buffer_content_len
    }

    async fn top_up_buffer(&mut self) -> std::io::Result<()> {
        let tail_len = self.read_buffer_content_len - self.read_buffer_offset;
        self.read_buffer.copy_within(self.read_buffer_offset.., 0);
        let bytes_read = read_to_buffer(&mut self.read_buffer[tail_len..], &mut self.input).await?;
        self.input_exhausted = bytes_read < self.read_buffer.len() - tail_len;
        self.read_buffer_offset = 0;
        self.read_buffer_content_len = tail_len + bytes_read;
        Ok(())
    }

    /// Reads chars from the underlying source into `out_buffer`. Returns the number of characters read.
    pub async fn read_chars(&mut self, out_buffer: &mut [char]) -> Result<usize, ParseError> {
        let out_buffer_len = out_buffer.len();
        assert!(out_buffer_len <= self.read_buffer.len());

        if self.is_end_of_input() || out_buffer_len < 4 {
            // encode_rs does not like buffers shorter than 4 bytes.
            // Even though the out_buffer contains characters instead of bytes,
            // at some points it is approximated that 1 byte == 1 char.
            return Ok(0);
        }

        let mut utf8_buffer = String::with_capacity(out_buffer_len);
        let mut out_buffer_offset = 0;

        // Read new data
        if self.read_buffer_content_len - self.read_buffer_offset < out_buffer_len {
            if let Err(err) = self.top_up_buffer().await {
                return Err(ParseError::new(self.position.clone(), ParseErrorVariant::IO(err)));
            }
        }

        // Decode new data
        let (res, bytes_decoded) = self.decoder.decode_to_string_without_replacement(
            &self.read_buffer[self.read_buffer_offset..self.read_buffer_content_len],
            &mut utf8_buffer,
            self.input_exhausted
        );

        // Handle potential error
        if let DecoderResult::Malformed(bad_bytes, _) = res {
            self.position.add_string(&utf8_buffer);
            // The range has to be like this, because the malformed sequence counts as
            // part of the decoded bytes, even though it was not actually decoded.
            return Err(ParseError::new(
                self.position.clone(),
                ParseErrorVariant::Encoding((&self.read_buffer[bytes_decoded - bad_bytes as usize .. bytes_decoded]).to_owned(), self.decoder.encoding())
            ));
        }

        // Write new characters to the buffer
        for c in utf8_buffer.chars() {
            out_buffer[out_buffer_offset] = c;
            out_buffer_offset += 1;
        }
        self.position.add_string(&utf8_buffer);

        // Update offsets
        self.read_buffer_offset += bytes_decoded;
        self.byte_offset += bytes_decoded;
        self.char_offset += out_buffer_offset;

        return Ok(out_buffer_offset);
    }

    /// Rewinds or forwards the CharSource to a previously recorded `checkpoint`.
    pub async fn seek(&mut self, checkpoint: &InputCheckpoint) -> std::io::Result<()> {
        let new_pos = self.input.seek(SeekFrom::Start(checkpoint.byte_offset as u64)).await?;
        assert_eq!(new_pos, checkpoint.byte_offset as u64);

        self.read_buffer_offset = 0;
        self.read_buffer_content_len = 0;
        self.decoder = self.decoder.encoding().new_decoder();
        self.position = checkpoint.position.to_owned();
        self.byte_offset = checkpoint.byte_offset;
        self.char_offset = checkpoint.char_offset;
        self.input_exhausted = false;

        return Ok(());
    }
}

#[cfg(test)]
mod tests {

    use super::CharSource;
    use crate::match_script::{error::ParseErrorVariant, CursorPosition, InputCheckpoint};

    use encoding_rs::UTF_8;

    use std::io::Cursor;

    #[tokio::test]
    async fn char_source_returns_correct_chars() {
        let input = Cursor::new("Test1234");
        let mut source = CharSource::new(UTF_8, input, "char_source_returns_correct_chars", 10000);
        let mut buffer = ['\0'; 100];
        assert_eq!(source.read_chars(&mut buffer).await.unwrap(), 8);
        assert_eq!(&buffer[..8], vec!['T', 'e', 's', 't', '1', '2', '3', '4']);
        assert_eq!(source.get_input_checkpoint(), InputCheckpoint { byte_offset: 8, char_offset: 8, position: CursorPosition { line: 0, col: 8 }});
    }

    #[tokio::test]
    async fn char_source_works_with_multiple_calls() {
        let input = Cursor::new("Test1234");
        let mut source = CharSource::new(UTF_8, input, "char_source_works_with_multiple_calls", 10000);
        let mut buffer = ['\0'; 4];

        assert_eq!(source.read_chars(&mut buffer).await.unwrap(), 4);
        assert_eq!(&buffer[..4], vec!['T', 'e', 's', 't']);
        assert_eq!(source.get_input_checkpoint(), InputCheckpoint { byte_offset: 4, char_offset: 4, position: CursorPosition { line: 0, col: 4 }});
        assert!(!source.is_end_of_input());

        assert_eq!(source.read_chars(&mut buffer).await.unwrap(), 4);
        assert_eq!(&buffer[..4], vec!['1', '2', '3', '4']);
        assert_eq!(source.get_input_checkpoint(), InputCheckpoint { byte_offset: 8, char_offset: 8, position: CursorPosition { line: 0, col: 8 }});
        assert!(source.is_end_of_input());

        assert_eq!(source.read_chars(&mut buffer).await.unwrap(), 0);
    }

    #[tokio::test]
    async fn char_source_works_with_multibyte_chars() {
        let input = Cursor::new("Test 🧪");
        let mut source = CharSource::new(UTF_8, input, "char_source_works_with_multibyte_chars", 10000);
        let mut buffer = ['\0'; 100];

        assert_eq!(source.read_chars(&mut buffer).await.unwrap(), 6);
        assert_eq!(&buffer[..6], vec!['T', 'e', 's', 't', ' ', '🧪']);
        assert_eq!(source.get_input_checkpoint(), InputCheckpoint { byte_offset: 9, char_offset: 6, position: CursorPosition { line: 0, col: 6 }});
    }

    #[tokio::test]
    async fn char_source_works_with_multibyte_chars_with_multiple_calls() {
        let input = Cursor::new("Test 🧪");
        let mut source = CharSource::new(UTF_8, input, "char_source_works_with_multibyte_chars_with_multiple_calls", 6);
        let mut buffer = ['\0'; 6];

        assert_eq!(source.read_chars(&mut buffer).await.unwrap(), 5);
        assert_eq!(&buffer[..5], vec!['T', 'e', 's', 't', ' ']);
        assert_eq!(source.read_chars(&mut buffer).await.unwrap(), 1);
        assert_eq!(&buffer[..1], vec!['🧪']);
        assert_eq!(source.get_input_checkpoint(), InputCheckpoint { byte_offset: 9, char_offset: 6, position: CursorPosition { line: 0, col: 6 }});
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

    #[tokio::test]
    async fn char_source_can_seek_to_checkpoints() {
        let input = Cursor::new("Test 🧪 0123456789");
        let mut source = CharSource::new(UTF_8, input, "char_source_works_with_multibyte_chars_with_multiple_calls", 10000);
        let mut buffer = ['\0'; 10];

        assert_eq!(source.read_chars(&mut buffer).await.unwrap(), 7);
        assert_eq!(&buffer[..7], vec!['T', 'e', 's', 't', ' ', '🧪', ' ']);
        let checkpoint = source.get_input_checkpoint();
        assert_eq!(checkpoint, InputCheckpoint { byte_offset: 10, char_offset: 7, position: CursorPosition { line: 0, col: 7 }});

        assert_eq!(source.read_chars(&mut buffer).await.unwrap(), 10);
        assert_eq!(&buffer[..10], vec!['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']);
        assert!(source.is_end_of_input());

        source.seek(&checkpoint).await.unwrap();

        assert!(!source.is_end_of_input());
        assert_eq!(source.read_chars(&mut buffer).await.unwrap(), 10);
        assert_eq!(&buffer[..10], vec!['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']);
        assert!(source.is_end_of_input());
    }
}
