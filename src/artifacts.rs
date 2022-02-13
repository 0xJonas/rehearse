use tokio::io::AsyncReadExt;
use encoding_rs::{UTF_8, Decoder};

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

struct AsyncCharSource<R: AsyncReadExt + Unpin> {
    /// Remaining undecoded characters from the last call to refill_buffer
    decode_strip: Vec<u8>,
    decoder: Decoder,
    input: R
}

impl<R: AsyncReadExt + Unpin> AsyncCharSource<R> {

    fn new(decoder: Decoder, input: R) -> AsyncCharSource<R> {
        AsyncCharSource {
            decode_strip: Vec::with_capacity(4),
            decoder: decoder,
            input: input
        }
    }

    async fn append_chars(&mut self, buffer: &mut Vec<char>) -> std::io::Result<bool> {
        // Read new data
        let byte_buffer_size = buffer.capacity() * 4;
        let mut byte_buffer = Vec::<u8>::with_capacity(byte_buffer_size);
        byte_buffer.append(&mut self.decode_strip);
        byte_buffer.resize(byte_buffer_size, 0);
        let bytes_read = read_to_buffer(&mut byte_buffer[..], &mut self.input).await?;
        let end_of_input = bytes_read < byte_buffer_size;

        // Decode new data
        let mut utf8_buffer = String::with_capacity(bytes_read);
        let (res, bytes_decoded) = self.decoder.decode_to_string_without_replacement(&byte_buffer, &mut utf8_buffer, bytes_read != BUFFER_REFILL_AMOUNT);

        // Return an error if the data could not be decoded
        if let encoding_rs::DecoderResult::Malformed(bad_bytes, _) = res {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!(
                    "Could not decode {:?} as valid {}",
                    Vec::from(&byte_buffer[bytes_decoded .. bytes_decoded + bad_bytes as usize]),
                    self.decoder.encoding().name()
                )
            ))
        }

        // Write decode strip to the struct
        for b in &byte_buffer[bytes_decoded..] {
            self.decode_strip.push(*b);
        }
        
        // Write new characters to the buffer
        for c in utf8_buffer.chars() {
            buffer.push(c);
        }

        // Cannot use ut8_buffer.len() here, because that would return the length in bytes,
        // and not the number of characters.
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

struct ProtoGraphemeIterator<R: AsyncReadExt + Unpin> {
    buffer: Vec<char>,
    buffer_pos: usize,
    delimiter_tag: Vec<char>,
    source: AsyncCharSource<R>,
}

impl<R: AsyncReadExt + Unpin> ProtoGraphemeIterator<R> {

    fn new(source: AsyncCharSource<R>, delimiter_tag: &str) -> ProtoGraphemeIterator<R> {
        ProtoGraphemeIterator {
            buffer: Vec::with_capacity(BUFFER_REFILL_AMOUNT),
            buffer_pos: 0,
            delimiter_tag: delimiter_tag.chars().collect(),
            source: source
        }
    }

    async fn refill_buffer(&mut self) -> std::io::Result<bool> {
        // Move remaining content in buffer to the beginning
        let tail_len = self.buffer.len() - self.buffer_pos;
        self.buffer.copy_within(self.buffer_pos.. , 0);
        self.buffer.truncate(tail_len);
        self.buffer_pos = 0;

        let end_of_input = self.source.append_chars(&mut self.buffer).await?;

        return Ok(end_of_input);
    }

    async fn peek_char(&mut self, offset: usize) -> std::io::Result<Option<char>> {
        if self.buffer_pos + offset >= self.buffer.len() {
            self.refill_buffer().await?;
        }

        if self.buffer_pos + offset < self.buffer.len() {
            let out = self.buffer[self.buffer_pos + offset];
            return Ok(Some(out));
        } else {
            return Ok(None);
        }
    }

    fn advance_pos_and_return(&mut self, opt_char: Option<char>) -> std::io::Result<Option<ProtoGrapheme>> {
        match opt_char {
            Some(t) => {
                self.buffer_pos += 1;
                return Ok(Some(ProtoGrapheme::Char(t)));
            },
            None => Ok(None)
        }
    }

    async fn next(&mut self) -> std::io::Result<Option<ProtoGrapheme>> {
        fn match_char(opt_char: Option<char>, c: char) -> bool {
            if let Some(t) = opt_char {
                t == c
            } else {
                false
            }
        }

        let mut offset = 0;

        // match left brace
        let first_char = self.peek_char(offset).await?;
        if !match_char(first_char, '{') {
            return self.advance_pos_and_return(first_char);
        }
        offset += 1;

        // match left delimiter tag
        for d in self.delimiter_tag.clone() {
            let current_char = self.peek_char(offset).await?;
            offset += 1;
            if !match_char(current_char, d) {
                return self.advance_pos_and_return(first_char);
            }
        }

        // match left pipe
        let current_char = self.peek_char(offset).await?;
        if !match_char(current_char, '|') {
            return self.advance_pos_and_return(first_char);
        }
        offset += 1;

        let expression_start_offset = offset;
        'outer: while offset < LEXEME_LIMIT {
            let expression_end_offset = offset;

            // match right pipe
            let current_char = self.peek_char(offset).await?;
            offset += 1;
            if !match_char(current_char, '|') {
                continue;
            }

            // match left delimiter tag
            for d in self.delimiter_tag.clone() {
                let current_char = self.peek_char(offset).await?;
                offset += 1;
                if !match_char(current_char, d) {
                    continue 'outer;
                }
            }

            // match right brace
            let current_char = self.peek_char(offset).await?;
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

fn read_proto_graphemes(path: &Path, delimiter_tag: &str) -> std::io::Result<ProtoGraphemeIterator<tokio::fs::File>> {
    let input = tokio::fs::File::from_std(std::fs::File::open(path)?);
    let source = AsyncCharSource::new(UTF_8.new_decoder(), input);

    Ok(ProtoGraphemeIterator::new(source, delimiter_tag))
}

#[cfg(test)]
mod tests {

    use super::{AsyncCharSource, ProtoGrapheme, ProtoGraphemeIterator};
    use quickcheck::{Arbitrary, Gen};
    use quickcheck_macros::quickcheck;
    use encoding_rs::{UTF_8};
    use tokio::runtime::{Builder};

    use std::io::Cursor;

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
        let source = AsyncCharSource::new(UTF_8.new_decoder(), cursor);
        let mut iterator = ProtoGraphemeIterator::new(source, &delimiter_tag);
        let mut output = Vec::with_capacity(protographemes.len());

        let rt = Builder::new_current_thread().build().unwrap();
        return rt.block_on(async {
            while let Ok(Some(pg)) = iterator.next().await {
                output.push(pg);
            }
            println!("!");
            protographemes == output
        });
    }
}
