mod proto_parser;
mod expression_parser;

trait Matcher {
    fn find_match(&self, input: &str) -> Option<&str>;
}

enum MSType {
    String(String),
    Matcher(Box<dyn Matcher>)
}

enum Grapheme {
    Char(char),
    Expression(MSType)
}
