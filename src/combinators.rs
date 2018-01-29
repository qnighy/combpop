use {LookaheadParser, ParseError, ParseResult, Parser, Stream};

fn parse_lookahead_all<O, S: Stream + ?Sized, P, Alt>(
    parser: &mut P,
    stream: &mut S,
    alt: &mut Alt,
) -> ParseResult<O>
where
    P: Parser<O, S> + ?Sized,
    Alt: Parser<O, S> + ?Sized,
{
    let pos = stream.mark();
    match parser.parse(stream) {
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
        parse_lookahead_all(self, stream, alt)
    }
    fn parse_lookahead_dyn(
        &mut self,
        stream: &mut S,
        alt: &mut Parser<S::Item, S>,
    ) -> ParseResult<S::Item> {
        self.parse_lookahead(stream, alt)
    }
}

pub fn token<I, F: FnMut(&I) -> bool>(f: F) -> Token<F> {
    Token(f)
}

pub struct Token<F>(F);

impl<S: Stream + ?Sized, F: FnMut(&S::Item) -> bool> Parser<S::Item, S> for Token<F> {
    fn parse(&mut self, stream: &mut S) -> ParseResult<S::Item> {
        let x = stream.next()?;
        if (self.0)(&x) {
            Ok(x)
        } else {
            Err(ParseError::SyntaxError)
        }
    }
}

impl<S: Stream + ?Sized, F: FnMut(&S::Item) -> bool> LookaheadParser<S::Item, S> for Token<F> {
    fn parse_lookahead<Alt>(&mut self, stream: &mut S, alt: &mut Alt) -> ParseResult<S::Item>
    where
        Alt: Parser<S::Item, S> + ?Sized,
    {
        parse_lookahead_all(self, stream, alt)
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
