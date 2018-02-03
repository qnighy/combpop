use std::marker::PhantomData;
use {Consume, ParseError, ParseResult, Parser, ParserBase, Stream};

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
    fn parse_lookahead(&mut self, stream: &mut S) -> ParseResult<Option<(I, Consume)>> {
        match stream.lookahead(1) {
            Ok(()) => {
                let x = stream.get(0).clone();
                stream.advance(1);
                Ok(Some((x, Consume::Consumed)))
            }
            Err(ParseError::EOF) => Ok(None),
            Err(e) => Err(e),
        }
    }
    fn emit_expectations(&mut self, _stream: &mut S) {
        // TODO
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
    fn parse_lookahead(&mut self, stream: &mut S) -> ParseResult<Option<(I, Consume)>> {
        match stream.lookahead(1) {
            Ok(()) => {
                if (self.0)(stream.get(0)) {
                    let x = stream.get(0).clone();
                    stream.advance(1);
                    Ok(Some((x, Consume::Consumed)))
                } else {
                    Ok(None)
                }
            }
            Err(ParseError::EOF) => Ok(None),
            Err(e) => Err(e),
        }
    }
    fn emit_expectations(&mut self, _stream: &mut S) {
        // TODO
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
            fn no_backtrack() -> bool
            where
                Self: Sized,
            {
                true $(&& $ty::no_backtrack())*
            }
        }
        impl<I, $($ty,)* S: Stream<Item = I> + ?Sized> Parser<S> for Concat<I, ($($ty,)*)>
        where
            $($ty: Parser<S, Input = I>,)*
        {
            #[allow(unused_variables)]
            fn parse_consume(&mut self, stream: &mut S) -> ParseResult<(Self::Output, Consume)> {
                let ($(ref mut $var,)*) = self.0;
                let consumed = Consume::Empty;
                concat_parse_consume!{(), ($($var : $ty,)*), stream, consumed}
            }
            #[allow(unused_variables)]
            fn parse_lookahead(&mut self, stream: &mut S)
                    -> ParseResult<Option<(Self::Output, Consume)>> {
                let ($(ref mut $var,)*) = self.0;
                let consumed = Consume::Empty;
                let lazy_emit = true;
                concat_parse_lookahead!{(), ($($var : $ty,)*), stream, consumed, lazy_emit}
            }
            #[allow(unused_variables)]
            fn emit_expectations(&mut self, stream: &mut S) {
                let ($(ref mut $var,)*) = self.0;
                $(
                    if $ty::nonempty() {
                        $var.emit_expectations(stream);
                        return;
                    }
                    if !$ty::no_backtrack() {
                        // Eager emit; exit.
                        return;
                    }
                    $var.emit_expectations(stream);
                )*
            }
        }
    };
}

macro_rules! concat_parse_consume {
    (($($var:ident : $ty:ident,)*), (), $stream:ident, $consumed:ident) => {
        Ok((($($var,)*), $consumed))
    };
    (($($var:ident : $ty:ident,)*), ($var2:ident : $ty2:ident, $($var3:ident : $ty3:ident,)*),
    $stream:ident, $consumed:ident) => {
        if $ty2::nonempty() {
            let $var2 = $var2.parse($stream)?;
            $(
                let $var3 = $var3.parse($stream)?;
            )*
            return Ok((($($var,)* $var2, $($var3,)*), Consume::Consumed));
        }
        let ($var2, $consumed) = {
            let (x, c) = $var2.parse_consume($stream)?;
            (x, $consumed | c)
        };
        concat_parse_consume!{
            ($($var : $ty,)* $var2 : $ty2,), ($($var3 : $ty3,)*), $stream, $consumed
        }
    };
}

macro_rules! concat_parse_lookahead {
    (($($var:ident : $ty:ident,)*), (), $stream:ident, $consumed:ident, $lazy_emit:ident) => {
        Ok(Some((($($var,)*), $consumed)))
    };
    (($($var:ident : $ty:ident,)*), ($var2:ident : $ty2:ident, $($var3:ident : $ty3:ident,)*),
    $stream:ident, $consumed:ident, $lazy_emit:ident) => {
        if $ty2::nonempty() {
            let $var2 = if let Some((x, _)) = $var2.parse_lookahead($stream)? {
                x
            } else {
                if !$lazy_emit {
                    $var2.emit_expectations($stream);
                }
                return Ok(None)
            };
            $(
                let $var3 = $var3.parse($stream)?;
            )*
            return Ok(Some((($($var,)* $var2, $($var3,)*), Consume::Consumed)));
        }
        let $lazy_emit = $lazy_emit && $ty2::no_backtrack();
        let ($var2, $consumed) = if let Some((x, c)) = $var2.parse_lookahead($stream)? {
            if ($consumed | c) == Consume::Empty && !$lazy_emit {
                $var2.emit_expectations($stream);
            }
            (x, $consumed | c)
        } else if $consumed == Consume::Consumed {
            return Err(ParseError::SyntaxError)
        } else {
            if !$lazy_emit {
                $var2.emit_expectations($stream);
            }
            return Ok(None)
        };
        concat_parse_lookahead!{
            ($($var : $ty,)* $var2 : $ty2,), ($($var3 : $ty3,)*), $stream, $consumed, $lazy_emit
        }
    };
}

macro_rules! define_concat_parsers {
    (($($var:ident : $ty:ident,)*), ()) => {
        define_concat_parser!($($var : $ty,)*);
    };
    (($($var:ident : $ty:ident,)*), ($var2:ident : $ty2:ident, $($var3:ident : $ty3:ident,)*)) => {
        define_concat_parser!($($var : $ty,)*);
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
