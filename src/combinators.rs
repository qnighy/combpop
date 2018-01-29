use {LookaheadParser, ParseResult, Parser, Stream};

pub fn any_token() -> AnyToken {
    AnyToken
}

pub struct AnyToken;

impl<S: Stream + ?Sized> Parser<S::Item, S> for AnyToken {
    fn parse(&mut self, stream: &mut S) -> ParseResult<S::Item> {
        stream.next()
    }
}

impl<S: Stream + ?Sized> LookaheadParser<S::Item, S> for AnyToken {
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

pub fn concat<P0, P1>(p0: P0, p1: P1) -> Concat<P0, P1> {
    Concat(p0, p1)
}

pub struct Concat<P0, P1>(P0, P1);

impl<X0, X1, P0, P1, S: Stream + ?Sized> Parser<(X0, X1), S> for Concat<P0, P1>
where
    P0: Parser<X0, S>,
    P1: Parser<X1, S>,
{
    fn parse(&mut self, stream: &mut S) -> ParseResult<(X0, X1)> {
        Ok((self.0.parse(stream)?, self.1.parse(stream)?))
    }
}
