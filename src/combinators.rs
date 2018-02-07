use std::marker::PhantomData;
use {Consume, ParseError, ParseResult, Parser, ParserBase, Stream};

pub fn any_token<I: Clone>() -> AnyToken<I> {
    AnyToken(PhantomData)
}

pub struct AnyToken<I: Clone>(PhantomData<fn(I)>);

impl<I: Clone> ParserBase for AnyToken<I> {
    type Input = I;
    type Output = I;
}
impl<I: Clone, S: Stream<Item = I> + ?Sized> Parser<S> for AnyToken<I> {
    delegate_parser!(&mut token(|_| true));
}

pub fn token<I: Clone, F: FnMut(&I) -> bool>(f: F) -> Token<I, F> {
    Token(f, PhantomData)
}

pub struct Token<I: Clone, F: FnMut(&I) -> bool>(F, PhantomData<fn(I)>);

impl<I: Clone, F: FnMut(&I) -> bool> ParserBase for Token<I, F> {
    type Input = I;
    type Output = I;
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

pub(crate) fn and_then<P0, P1, F>(p0: P0, f: F) -> AndThen<P0, P1, F>
where
    P0: ParserBase,
    P1: ParserBase<Input = P0::Input>,
    F: FnMut(P0::Output) -> P1,
{
    AndThen(p0, f)
}

pub struct AndThen<P0, P1, F>(P0, F)
where
    P0: ParserBase,
    P1: ParserBase<Input = P0::Input>,
    F: FnMut(P0::Output) -> P1;

impl<P0, P1, F> ParserBase for AndThen<P0, P1, F>
where
    P0: ParserBase,
    P1: ParserBase<Input = P0::Input>,
    F: FnMut(P0::Output) -> P1,
{
    type Input = P0::Input;
    type Output = P1::Output;
    fn emptiable() -> bool
    where
        Self: Sized,
    {
        P0::emptiable() && P1::emptiable()
    }
}

impl<S, P0, P1, F> Parser<S> for AndThen<P0, P1, F>
where
    S: Stream<Item = P0::Input>,
    P0: Parser<S>,
    P1: Parser<S, Input = P0::Input>,
    F: FnMut(P0::Output) -> P1,
{
    fn parse_lookahead(&mut self, stream: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        let AndThen(ref mut p0, ref mut f) = *self;
        let (x, consumed) = if let Some((x, c)) = p0.parse_lookahead(stream)? {
            (x, c)
        } else {
            return Ok(None);
        };
        let mut p1 = f(x);
        if let Some((y, c)) = p1.parse_lookahead(stream)? {
            Ok(Some((y, consumed | c)))
        } else if consumed == Consume::Empty {
            Ok(None)
        } else {
            Err(ParseError::SyntaxError)
        }
    }
    fn emit_expectations(&mut self, stream: &mut S) {
        let AndThen(ref mut p0, _) = *self;
        p0.emit_expectations(stream);
        if P0::emptiable() {
            // FIXME
            // p1.emit_expectations(stream);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use stream::SliceStream;

    #[test]
    fn test_any_token() {
        let mut p = any_token();
        assert_eq!(p.parse(&mut SliceStream::new(b"hoge")).unwrap(), b'h');
        assert!(p.parse(&mut SliceStream::new(b"")).is_err());
    }
}
