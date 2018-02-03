use std::marker::PhantomData;
use {Consume, LookaheadParser, ParseError, ParseResult, Parser, ParserBase, Stream};
use stream::stream_transaction;

pub fn any_token<I: Clone>() -> AnyToken<I> {
    AnyToken(PhantomData)
}

pub struct AnyToken<I: Clone>(PhantomData<fn(I)>);

impl<I: Clone> ParserBase for AnyToken<I> {
    type Input = I;
    type Output = I;
    fn nonempty() -> bool
    where
        Self: Sized,
    {
        true
    }
}
impl<I: Clone, S: Stream<Item = I> + ?Sized> Parser<S> for AnyToken<I> {
    fn parse(&mut self, stream: &mut S) -> ParseResult<(I, Consume)> {
        match stream.lookahead(1) {
            Ok(()) => Ok((stream.get(0).clone(), Consume::Consumed)),
            Err(ParseError::EOF) => Err(ParseError::SyntaxError),
            Err(e) => Err(e),
        }
    }
}

impl<I: Clone, S: Stream<Item = I> + ?Sized> LookaheadParser<S> for AnyToken<I> {
    fn parse_lookahead(&mut self, stream: &mut S) -> ParseResult<Option<I>> {
        stream_transaction(stream, |stream| self.parse(stream).map(|(x, _)| x))
    }
}

pub fn token<I: Clone, F: FnMut(&I) -> bool>(f: F) -> Token<I, F> {
    Token(f, PhantomData)
}

pub struct Token<I: Clone, F: FnMut(&I) -> bool>(F, PhantomData<fn(I)>);

impl<I: Clone, F: FnMut(&I) -> bool> ParserBase for Token<I, F> {
    type Input = I;
    type Output = I;
    fn nonempty() -> bool
    where
        Self: Sized,
    {
        true
    }
}
impl<I: Clone, S: Stream<Item = I> + ?Sized, F: FnMut(&I) -> bool> Parser<S> for Token<I, F> {
    fn parse(&mut self, stream: &mut S) -> ParseResult<(I, Consume)> {
        match stream.lookahead(1) {
            Ok(()) => {
                let x = stream.get(0);
                if (self.0)(x) {
                    Ok((x.clone(), Consume::Consumed))
                } else {
                    Err(ParseError::SyntaxError)
                }
            }
            Err(ParseError::EOF) => Err(ParseError::SyntaxError),
            Err(e) => Err(e),
        }
    }
}

impl<I: Clone, S: Stream<Item = I> + ?Sized, F: FnMut(&I) -> bool> LookaheadParser<S>
    for Token<I, F> {
    fn parse_lookahead(&mut self, stream: &mut S) -> ParseResult<Option<I>> {
        stream_transaction(stream, |stream| self.parse(stream).map(|(x, _)| x))
    }
}

pub fn concat<I, P>(p: P) -> Concat<I, P>
where
    Concat<I, P>: ParserBase,
{
    Concat(p, PhantomData)
}

pub struct Concat<I, P>(P, PhantomData<fn(I)>);

macro_rules! define_concat_parser {
    ($($var:ident : $ty:ident,)*) => {
        impl<I, $($ty,)*> ParserBase for Concat<I, ($($ty,)*)>
        where
            $($ty: ParserBase<Input = I>,)*
        {
            type Input = I;
            type Output = ($($ty::Output,)*);
            fn nonempty() -> bool
            where
                Self: Sized,
            {
                false $(|| $ty::nonempty())*
            }
        }
        impl<I, $($ty,)* S: Stream<Item = I> + ?Sized> Parser<S> for Concat<I, ($($ty,)*)>
        where
            $($ty: Parser<S, Input = I>,)*
        {
            #[allow(unused_variables)]
            fn parse(&mut self, stream: &mut S) -> ParseResult<(Self::Output, Consume)> {
                let ($(ref mut $var,)*) = self.0;
                $(let $var = $var.parse(stream)?;)*
                Ok((($($var.0,)*), Consume::Empty $(| $var.1)*))
            }
        }
    };
}

macro_rules! define_concat_lookahead_parser {
    () => {};
    ($var0:ident : $ty0:ident, $($var:ident : $ty:ident,)*) => {
        impl<I, $ty0, $($ty,)* S: Stream<Item = I> + ?Sized>
            LookaheadParser<S> for Concat<I, ($ty0, $($ty,)*)>
        where
            $ty0: LookaheadParser<S, Input = I>,
            $($ty: Parser<S, Input = I>,)*
        {
            #[allow(unused_variables)]
            fn parse_lookahead(&mut self, stream: &mut S) -> ParseResult<Option<Self::Output>> {
                let (ref mut $var0, $(ref mut $var,)*) = self.0;
                Ok(if let Some(x) = $var0.parse_lookahead(stream)? {
                    Some((x, $($var.parse(stream)?.0,)*))
                } else {
                    None
                })
            }
        }
    };
}

macro_rules! define_concat_parsers {
    (($($var:ident : $ty:ident,)*), ()) => {
        define_concat_parser!($($var : $ty,)*);
        define_concat_lookahead_parser!($($var : $ty,)*);
    };
    (($($var:ident : $ty:ident,)*), ($var2:ident : $ty2:ident, $($var3:ident : $ty3:ident,)*)) => {
        define_concat_parser!($($var : $ty,)*);
        define_concat_lookahead_parser!($($var : $ty,)*);
        define_concat_parsers!(($($var : $ty,)* $var2 : $ty2,), ($($var3 : $ty3,)*));
    };
    ($($var:ident : $ty:ident,)*) => {
        define_concat_parsers!((), ($($var : $ty,)*));
    };
}

define_concat_parsers!(
    p0: P0,
    p1: P1,
    p2: P2,
    p3: P3,
    p4: P4,
    p5: P5,
    p6: P6,
    p7: P7,
    p8: P8,
    p9: P9,
    p10: P10,
    p11: P11,
);
