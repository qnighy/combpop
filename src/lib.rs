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

pub trait Parser<S: Stream<Item = Self::Input> + ?Sized> {
    type Input;
    type Output;
    fn parse(&mut self, stream: &mut S) -> ParseResult<Self::Output>;
}
pub trait LookaheadParser<S: Stream<Item = Self::Input> + ?Sized>: Parser<S> {
    fn parse_lookahead(&mut self, stream: &mut S) -> ParseResult<Option<Self::Output>>;
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
