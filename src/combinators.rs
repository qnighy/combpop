use std::marker::PhantomData;
use {LookaheadParser, ParseError, ParseResult, Parser, Stream};

fn parse_lookahead_all<I, O, S: Stream<Item = I> + ?Sized, P, Alt>(
    parser: &mut P,
    stream: &mut S,
    alt: &mut Alt,
) -> ParseResult<P::Output>
where
    P: Parser<S, Input = I, Output = O> + ?Sized,
    Alt: Parser<S, Input = I, Output = O> + ?Sized,
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

pub fn any_token<I>() -> AnyToken<I> {
    AnyToken(PhantomData)
}

pub struct AnyToken<I>(PhantomData<fn(I)>);

impl<I, S: Stream<Item = I> + ?Sized> Parser<S> for AnyToken<I> {
    type Input = I;
    type Output = I;
    fn parse(&mut self, stream: &mut S) -> ParseResult<I> {
        stream.next()
    }
}

impl<I, S: Stream<Item = I> + ?Sized> LookaheadParser<S> for AnyToken<I> {
    fn parse_lookahead<Alt>(&mut self, stream: &mut S, alt: &mut Alt) -> ParseResult<I>
    where
        Alt: Parser<S, Input = I, Output = I> + ?Sized,
    {
        parse_lookahead_all(self, stream, alt)
    }
    fn parse_lookahead_dyn(
        &mut self,
        stream: &mut S,
        alt: &mut Parser<S, Input = I, Output = I>,
    ) -> ParseResult<I> {
        self.parse_lookahead(stream, alt)
    }
}

pub fn token<I, F: FnMut(&I) -> bool>(f: F) -> Token<I, F> {
    Token(f, PhantomData)
}

pub struct Token<I, F: FnMut(&I) -> bool>(F, PhantomData<fn(I)>);

impl<I, S: Stream<Item = I> + ?Sized, F: FnMut(&I) -> bool> Parser<S> for Token<I, F> {
    type Input = I;
    type Output = I;
    fn parse(&mut self, stream: &mut S) -> ParseResult<I> {
        let x = stream.next()?;
        if (self.0)(&x) {
            Ok(x)
        } else {
            Err(ParseError::SyntaxError)
        }
    }
}

impl<I, S: Stream<Item = I> + ?Sized, F: FnMut(&I) -> bool> LookaheadParser<S> for Token<I, F> {
    fn parse_lookahead<Alt>(&mut self, stream: &mut S, alt: &mut Alt) -> ParseResult<I>
    where
        Alt: Parser<S, Input = I, Output = I> + ?Sized,
    {
        parse_lookahead_all(self, stream, alt)
    }
    fn parse_lookahead_dyn(
        &mut self,
        stream: &mut S,
        alt: &mut Parser<S, Input = I, Output = I>,
    ) -> ParseResult<I> {
        self.parse_lookahead(stream, alt)
    }
}

pub fn concat<P0, P1>(p0: P0, p1: P1) -> Concat<P0, P1> {
    Concat(p0, p1)
}

pub struct Concat<P0, P1>(P0, P1);

impl<I, P0, P1, S: Stream<Item = I> + ?Sized> Parser<S> for Concat<P0, P1>
where
    P0: Parser<S, Input = I>,
    P1: Parser<S, Input = I>,
{
    type Input = I;
    type Output = (P0::Output, P1::Output);
    fn parse(&mut self, stream: &mut S) -> ParseResult<Self::Output> {
        Ok((self.0.parse(stream)?, self.1.parse(stream)?))
    }
}
