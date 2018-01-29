pub mod stream;
pub mod combinators;

pub use stream::Stream;

#[derive(Debug)]
pub enum ParseError {
    EOF,
    NotReady,
    SyntaxError,
}
impl ParseError {
    pub fn is_recoverable(&self) -> bool {
        match self {
            &ParseError::EOF | &ParseError::SyntaxError => true,
            &ParseError::NotReady => false,
        }
    }
}

pub type ParseResult<T> = Result<T, ParseError>;

pub trait Parser<O, S: Stream + ?Sized> {
    fn parse(&mut self, stream: &mut S) -> ParseResult<O>;
}
pub trait LookaheadParser<O, S: Stream + ?Sized>: Parser<O, S> {
    fn parse_lookahead<Alt>(&mut self, stream: &mut S, alt: &mut Alt) -> ParseResult<O>
    where
        Alt: Parser<O, S> + ?Sized,
        Self: Sized;
    fn parse_lookahead_dyn(&mut self, stream: &mut S, alt: &mut Parser<O, S>) -> ParseResult<O>;
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
