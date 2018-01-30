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
    fn parse_lookahead<Alt>(&mut self, stream: &mut S, alt: &mut Alt) -> ParseResult<Self::Output>
    where
        Alt: Parser<S, Input = Self::Input, Output = Self::Output> + ?Sized,
        Self: Sized;
    fn parse_lookahead_dyn(
        &mut self,
        stream: &mut S,
        alt: &mut Parser<S, Input = Self::Input, Output = Self::Output>,
    ) -> ParseResult<Self::Output>;
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
