use std::marker::PhantomData;
use {LookaheadParser, ParseError, ParseResult, Parser, Stream};
use stream::stream_transaction;

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
    fn parse_lookahead(&mut self, stream: &mut S) -> ParseResult<Option<I>> {
        stream_transaction(stream, |stream| self.parse(stream))
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
    fn parse_lookahead(&mut self, stream: &mut S) -> ParseResult<Option<I>> {
        stream_transaction(stream, |stream| self.parse(stream))
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
