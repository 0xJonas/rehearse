mod char_source;
mod proto_grapheme_source;
mod grapheme_source;
mod expression_parser;
mod functions;
mod error;

pub use char_source::CharSource;
pub use proto_grapheme_source::ProtoGraphemeSource;
pub use grapheme_source::{GraphemeSource, Grapheme};
pub use error::{ParseError, ParseErrorVariant, CursorPosition};

#[derive(Debug, Clone, PartialEq)]
pub struct InputCheckpoint {
    pub byte_offset: usize,
    pub char_offset: usize,
    pub position: CursorPosition
}
