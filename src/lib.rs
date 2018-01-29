pub mod stream;

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

pub trait Parser<O, S: Stream> {
    fn parse(&mut self, stream: &mut S) -> ParseResult<O>;
}
pub trait LookaheadParser<O, S: Stream>: Parser<O, S> {
    fn parse_lookahead<Alt>(&mut self, stream: &mut S, alt: &mut Alt) -> ParseResult<O>
    where
        Alt: Parser<O, S> + ?Sized,
        Self: Sized;
    fn parse_lookahead_dyn(&mut self, stream: &mut S, alt: &mut Parser<O, S>) -> ParseResult<O>;
}

pub struct Token;
impl<S: Stream> Parser<S::Item, S> for Token {
    fn parse(&mut self, stream: &mut S) -> ParseResult<S::Item> {
        stream.next()
    }
}
impl<S: Stream> LookaheadParser<S::Item, S> for Token {
    fn parse_lookahead<Alt>(&mut self, stream: &mut S, alt: &mut Alt) -> ParseResult<S::Item>
    where
        Alt: Parser<S::Item, S> + ?Sized,
    {
        let pos = stream.mark();
        match stream.next() {
            Ok(x) => {
                stream.commit();
                Ok(x)
            }
            Err(e) => if e.is_recoverable() {
                stream.rollback(pos);
                alt.parse(stream)
            } else {
                stream.commit();
                Err(e)
            },
        }
    }
    fn parse_lookahead_dyn(
        &mut self,
        stream: &mut S,
        alt: &mut Parser<S::Item, S>,
    ) -> ParseResult<S::Item> {
        self.parse_lookahead(stream, alt)
    }
}

pub struct Concat<P0, P1>(P0, P1);
impl<X0, X1, P0, P1, S: Stream> Parser<(X0, X1), S> for Concat<P0, P1>
where
    P0: Parser<X0, S>,
    P1: Parser<X1, S>,
{
    fn parse(&mut self, stream: &mut S) -> ParseResult<(X0, X1)> {
        Ok((self.0.parse(stream)?, self.1.parse(stream)?))
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
