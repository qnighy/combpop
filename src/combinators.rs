use std::marker::PhantomData;
use {Consume, ParseError, ParseResult, Parser, ParserBase, ParserMut, ParserOnce, Stream};

pub fn any_token<I: Clone>() -> AnyToken<I> {
    AnyToken(PhantomData)
}

pub struct AnyToken<I: Clone>(PhantomData<fn(I)>);

impl<I: Clone> ParserBase for AnyToken<I> {
    type Input = I;
    type Output = I;
}
impl<I: Clone, S: Stream<Item = I> + ?Sized> ParserOnce<S> for AnyToken<I> {
    delegate_parser_once!(token(|_| true));
}
impl<I: Clone, S: Stream<Item = I> + ?Sized> ParserMut<S> for AnyToken<I> {
    delegate_parser_mut!(&mut token(|_| true));
}
impl<I: Clone, S: Stream<Item = I> + ?Sized> Parser<S> for AnyToken<I> {
    delegate_parser!(&token(|_| true));
}

pub fn token_once<I: Clone, F: FnOnce(&I) -> bool>(f: F) -> Token<I, F> {
    Token(f, PhantomData)
}

pub fn token_mut<I: Clone, F: FnMut(&I) -> bool>(f: F) -> Token<I, F> {
    Token(f, PhantomData)
}

pub fn token<I: Clone, F: Fn(&I) -> bool>(f: F) -> Token<I, F> {
    Token(f, PhantomData)
}

pub struct Token<I: Clone, F: FnOnce(&I) -> bool>(F, PhantomData<fn(I)>);

impl<I: Clone, F: FnOnce(&I) -> bool> ParserBase for Token<I, F> {
    type Input = I;
    type Output = I;
}
impl<I: Clone, S: Stream<Item = I> + ?Sized, F: FnOnce(&I) -> bool> ParserOnce<S> for Token<I, F> {
    fn parse_lookahead_once(self, stream: &mut S) -> ParseResult<Option<(I, Consume)>> {
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
}
impl<I: Clone, S: Stream<Item = I> + ?Sized, F: FnMut(&I) -> bool> ParserMut<S> for Token<I, F> {
    fn parse_lookahead_mut(&mut self, stream: &mut S) -> ParseResult<Option<(I, Consume)>> {
        ParserOnce::parse_lookahead_once(token_mut(&mut self.0), stream)
    }
    fn emit_expectations_mut(&mut self, _stream: &mut S) {
        // TODO
    }
}
impl<I: Clone, S: Stream<Item = I> + ?Sized, F: Fn(&I) -> bool> Parser<S> for Token<I, F> {
    fn parse_lookahead(&self, stream: &mut S) -> ParseResult<Option<(I, Consume)>> {
        ParserOnce::parse_lookahead_once(token(&self.0), stream)
    }
    fn emit_expectations(&self, _stream: &mut S) {
        // TODO
    }
}

pub fn eof<I>() -> Eof<I> {
    Eof(PhantomData)
}

pub struct Eof<I>(PhantomData<fn(I)>);

impl<I> ParserBase for Eof<I> {
    type Input = I;
    type Output = ();
    fn emptiable() -> bool {
        true
    }
}
impl<I, S: Stream<Item = I> + ?Sized> ParserOnce<S> for Eof<I> {
    fn parse_lookahead_once(self, stream: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        match stream.lookahead(1) {
            Ok(()) => Ok(None),
            Err(ParseError::EOF) => Ok(Some(((), Consume::Empty))),
            Err(e) => Err(e),
        }
    }
}
impl<I, S: Stream<Item = I> + ?Sized> ParserMut<S> for Eof<I> {
    fn parse_lookahead_mut(
        &mut self,
        stream: &mut S,
    ) -> ParseResult<Option<(Self::Output, Consume)>> {
        ParserOnce::parse_lookahead_once(eof(), stream)
    }
    fn emit_expectations_mut(&mut self, _stream: &mut S) {
        // TODO
    }
}
impl<I, S: Stream<Item = I> + ?Sized> Parser<S> for Eof<I> {
    fn parse_lookahead(&self, stream: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        ParserOnce::parse_lookahead_once(eof(), stream)
    }
    fn emit_expectations(&self, _stream: &mut S) {
        // TODO
    }
}

pub(crate) fn map_once<O, P, F>(p: P, f: F) -> Map<O, P, F>
where
    P: ParserBase,
    F: FnOnce(P::Output) -> O,
{
    Map(p, f)
}

pub(crate) fn map_mut<O, P, F>(p: P, f: F) -> Map<O, P, F>
where
    P: ParserBase,
    F: FnMut(P::Output) -> O,
{
    Map(p, f)
}

pub(crate) fn map<O, P, F>(p: P, f: F) -> Map<O, P, F>
where
    P: ParserBase,
    F: Fn(P::Output) -> O,
{
    Map(p, f)
}

pub struct Map<O, P, F>(P, F)
where
    P: ParserBase,
    F: FnOnce(P::Output) -> O;

impl<O, P, F> ParserBase for Map<O, P, F>
where
    P: ParserBase,
    F: FnOnce(P::Output) -> O,
{
    type Input = P::Input;
    type Output = O;
    fn emptiable() -> bool {
        P::emptiable()
    }
}

impl<S, O, P, F> ParserOnce<S> for Map<O, P, F>
where
    S: Stream<Item = P::Input>,
    P: ParserOnce<S>,
    F: FnOnce(P::Output) -> O,
{
    fn parse_lookahead_once(self, stream: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        let Map(p, f) = self;
        if let Some((x, c)) = p.parse_lookahead_once(stream)? {
            Ok(Some((f(x), c)))
        } else {
            Ok(None)
        }
    }
}

impl<S, O, P, F> ParserMut<S> for Map<O, P, F>
where
    S: Stream<Item = P::Input>,
    P: ParserMut<S>,
    F: FnMut(P::Output) -> O,
{
    fn parse_lookahead_mut(
        &mut self,
        stream: &mut S,
    ) -> ParseResult<Option<(Self::Output, Consume)>> {
        let Map(ref mut p, ref mut f) = *self;
        if let Some((x, c)) = p.parse_lookahead_mut(stream)? {
            Ok(Some((f(x), c)))
        } else {
            Ok(None)
        }
    }
    fn emit_expectations_mut(&mut self, stream: &mut S) {
        let Map(ref mut p, _) = *self;
        p.emit_expectations_mut(stream);
    }
}

impl<S, O, P, F> Parser<S> for Map<O, P, F>
where
    S: Stream<Item = P::Input>,
    P: Parser<S>,
    F: Fn(P::Output) -> O,
{
    fn parse_lookahead(&self, stream: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        let Map(ref p, ref f) = *self;
        if let Some((x, c)) = p.parse_lookahead(stream)? {
            Ok(Some((f(x), c)))
        } else {
            Ok(None)
        }
    }
    fn emit_expectations(&self, stream: &mut S) {
        let Map(ref p, _) = *self;
        p.emit_expectations(stream);
    }
}

pub(crate) fn and_then_once<P0, P1, F>(p0: P0, f: F) -> AndThen<P0, P1, F>
where
    P0: ParserBase,
    P1: ParserBase<Input = P0::Input>,
    F: FnOnce(P0::Output) -> P1,
{
    AndThen(p0, f)
}

pub(crate) fn and_then_mut<P0, P1, F>(p0: P0, f: F) -> AndThen<P0, P1, F>
where
    P0: ParserBase,
    P1: ParserBase<Input = P0::Input>,
    F: FnMut(P0::Output) -> P1,
{
    AndThen(p0, f)
}

pub(crate) fn and_then<P0, P1, F>(p0: P0, f: F) -> AndThen<P0, P1, F>
where
    P0: ParserBase,
    P1: ParserBase<Input = P0::Input>,
    F: Fn(P0::Output) -> P1,
{
    AndThen(p0, f)
}

pub struct AndThen<P0, P1, F>(P0, F)
where
    P0: ParserBase,
    P1: ParserBase<Input = P0::Input>,
    F: FnOnce(P0::Output) -> P1;

impl<P0, P1, F> ParserBase for AndThen<P0, P1, F>
where
    P0: ParserBase,
    P1: ParserBase<Input = P0::Input>,
    F: FnOnce(P0::Output) -> P1,
{
    type Input = P0::Input;
    type Output = P1::Output;
    fn emptiable() -> bool {
        P0::emptiable() && P1::emptiable()
    }
}

impl<S, P0, P1, F> ParserOnce<S> for AndThen<P0, P1, F>
where
    S: Stream<Item = P0::Input>,
    P0: ParserOnce<S>,
    P1: ParserOnce<S, Input = P0::Input>,
    F: FnOnce(P0::Output) -> P1,
{
    fn parse_lookahead_once(self, stream: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        let AndThen(p0, f) = self;
        let (x, consumed) = if let Some((x, c)) = p0.parse_lookahead_once(stream)? {
            (x, c)
        } else {
            return Ok(None);
        };
        let p1 = f(x);
        if let Some((y, c)) = p1.parse_lookahead_once(stream)? {
            Ok(Some((y, consumed | c)))
        } else if consumed == Consume::Empty {
            Ok(None)
        } else {
            Err(ParseError::SyntaxError)
        }
    }
}

impl<S, P0, P1, F> ParserMut<S> for AndThen<P0, P1, F>
where
    S: Stream<Item = P0::Input>,
    P0: ParserMut<S>,
    P1: ParserOnce<S, Input = P0::Input>,
    F: FnMut(P0::Output) -> P1,
{
    fn parse_lookahead_mut(
        &mut self,
        stream: &mut S,
    ) -> ParseResult<Option<(Self::Output, Consume)>> {
        let AndThen(ref mut p0, ref mut f) = *self;
        let (x, consumed) = if let Some((x, c)) = p0.parse_lookahead_mut(stream)? {
            (x, c)
        } else {
            return Ok(None);
        };
        let p1 = f(x);
        if let Some((y, c)) = p1.parse_lookahead_once(stream)? {
            Ok(Some((y, consumed | c)))
        } else if consumed == Consume::Empty {
            Ok(None)
        } else {
            Err(ParseError::SyntaxError)
        }
    }
    fn emit_expectations_mut(&mut self, stream: &mut S) {
        let AndThen(ref mut p0, _) = *self;
        p0.emit_expectations_mut(stream);
        if P0::emptiable() {
            // FIXME
            // p1.emit_expectations_mut(stream);
        }
    }
}

impl<S, P0, P1, F> Parser<S> for AndThen<P0, P1, F>
where
    S: Stream<Item = P0::Input>,
    P0: Parser<S>,
    P1: ParserOnce<S, Input = P0::Input>,
    F: Fn(P0::Output) -> P1,
{
    fn parse_lookahead(&self, stream: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        let AndThen(ref p0, ref f) = *self;
        let (x, consumed) = if let Some((x, c)) = p0.parse_lookahead(stream)? {
            (x, c)
        } else {
            return Ok(None);
        };
        let p1 = f(x);
        if let Some((y, c)) = p1.parse_lookahead_once(stream)? {
            Ok(Some((y, consumed | c)))
        } else if consumed == Consume::Empty {
            Ok(None)
        } else {
            Err(ParseError::SyntaxError)
        }
    }
    fn emit_expectations(&self, stream: &mut S) {
        let AndThen(ref p0, _) = *self;
        p0.emit_expectations(stream);
        if P0::emptiable() {
            // FIXME
            // p1.emit_expectations(stream);
        }
    }
}

pub fn empty<I>() -> Empty<I> {
    Empty(PhantomData)
}

pub struct Empty<I>(PhantomData<fn(I)>);

impl<I> ParserBase for Empty<I> {
    type Input = I;
    type Output = ();
    fn emptiable() -> bool {
        true
    }
}

impl<S, I> ParserOnce<S> for Empty<I>
where
    S: Stream<Item = I>,
{
    fn parse_lookahead_once(self, _: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        Ok(Some(((), Consume::Empty)))
    }
}

impl<S, I> ParserMut<S> for Empty<I>
where
    S: Stream<Item = I>,
{
    fn parse_lookahead_mut(&mut self, _: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        Ok(Some(((), Consume::Empty)))
    }
    fn emit_expectations_mut(&mut self, _: &mut S) {}
}

impl<S, I> Parser<S> for Empty<I>
where
    S: Stream<Item = I>,
{
    fn parse_lookahead(&self, _: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        Ok(Some(((), Consume::Empty)))
    }
    fn emit_expectations(&self, _: &mut S) {}
}

pub(crate) fn concat2<P0, P1>(p0: P0, p1: P1) -> Concat2<P0, P1>
where
    P0: ParserBase,
    P1: ParserBase<Input = P0::Input>,
{
    Concat2(p0, p1)
}

pub struct Concat2<P0, P1>(P0, P1)
where
    P0: ParserBase,
    P1: ParserBase<Input = P0::Input>;

impl<P0, P1> ParserBase for Concat2<P0, P1>
where
    P0: ParserBase,
    P1: ParserBase<Input = P0::Input>,
{
    type Input = P0::Input;
    type Output = (P0::Output, P1::Output);
    fn emptiable() -> bool {
        P0::emptiable() && P1::emptiable()
    }
}

impl<S, P0, P1> ParserOnce<S> for Concat2<P0, P1>
where
    S: Stream<Item = P0::Input>,
    P0: ParserOnce<S>,
    P1: ParserOnce<S, Input = P0::Input>,
{
    fn parse_lookahead_once(self, stream: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        let Concat2(p0, p1) = self;
        let (x, consumed) = if let Some((x, c)) = p0.parse_lookahead_once(stream)? {
            (x, c)
        } else {
            return Ok(None);
        };
        if let Some((y, c)) = p1.parse_lookahead_once(stream)? {
            Ok(Some(((x, y), consumed | c)))
        } else if consumed == Consume::Empty {
            Ok(None)
        } else {
            Err(ParseError::SyntaxError)
        }
    }
}

impl<S, P0, P1> ParserMut<S> for Concat2<P0, P1>
where
    S: Stream<Item = P0::Input>,
    P0: ParserMut<S>,
    P1: ParserMut<S, Input = P0::Input>,
{
    fn parse_lookahead_mut(
        &mut self,
        stream: &mut S,
    ) -> ParseResult<Option<(Self::Output, Consume)>> {
        let Concat2(ref mut p0, ref mut p1) = *self;
        let (x, consumed) = if let Some((x, c)) = p0.parse_lookahead_mut(stream)? {
            (x, c)
        } else {
            return Ok(None);
        };
        if let Some((y, c)) = p1.parse_lookahead_mut(stream)? {
            Ok(Some(((x, y), consumed | c)))
        } else if consumed == Consume::Empty {
            Ok(None)
        } else {
            Err(ParseError::SyntaxError)
        }
    }
    fn emit_expectations_mut(&mut self, stream: &mut S) {
        let Concat2(ref mut p0, ref mut p1) = *self;
        p0.emit_expectations_mut(stream);
        if P0::emptiable() {
            p1.emit_expectations_mut(stream);
        }
    }
}

impl<S, P0, P1> Parser<S> for Concat2<P0, P1>
where
    S: Stream<Item = P0::Input>,
    P0: Parser<S>,
    P1: Parser<S, Input = P0::Input>,
{
    fn parse_lookahead(&self, stream: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        let Concat2(ref p0, ref p1) = *self;
        let (x, consumed) = if let Some((x, c)) = p0.parse_lookahead(stream)? {
            (x, c)
        } else {
            return Ok(None);
        };
        if let Some((y, c)) = p1.parse_lookahead(stream)? {
            Ok(Some(((x, y), consumed | c)))
        } else if consumed == Consume::Empty {
            Ok(None)
        } else {
            Err(ParseError::SyntaxError)
        }
    }
    fn emit_expectations(&self, stream: &mut S) {
        let Concat2(ref p0, ref p1) = *self;
        p0.emit_expectations(stream);
        if P0::emptiable() {
            p1.emit_expectations(stream);
        }
    }
}

pub fn fail<I, O>() -> Fail<I, O> {
    Fail(PhantomData)
}
pub struct Fail<I, O>(PhantomData<fn(I) -> O>);
impl<I, O> ParserBase for Fail<I, O> {
    type Input = I;
    type Output = O;
    fn emptiable() -> bool {
        false
    }
}
impl<S: Stream<Item = I>, I, O> ParserOnce<S> for Fail<I, O> {
    fn parse_lookahead_once(self, _: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        Ok(None)
    }
}
impl<S: Stream<Item = I>, I, O> ParserMut<S> for Fail<I, O> {
    fn parse_lookahead_mut(&mut self, _: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        Ok(None)
    }
    fn emit_expectations_mut(&mut self, _: &mut S) {}
}
impl<S: Stream<Item = I>, I, O> Parser<S> for Fail<I, O> {
    fn parse_lookahead(&self, _: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        Ok(None)
    }
    fn emit_expectations(&self, _: &mut S) {}
}

pub(crate) fn choice2<P0, P1>(p0: P0, p1: P1) -> Choice2<P0, P1>
where
    P0: ParserBase,
    P1: ParserBase<Input = P0::Input, Output = P0::Output>,
{
    Choice2(p0, p1)
}
pub struct Choice2<P0, P1>(P0, P1)
where
    P0: ParserBase,
    P1: ParserBase<Input = P0::Input, Output = P0::Output>;
impl<P0, P1> ParserBase for Choice2<P0, P1>
where
    P0: ParserBase,
    P1: ParserBase<Input = P0::Input, Output = P0::Output>,
{
    type Input = P0::Input;
    type Output = P0::Output;
    fn emptiable() -> bool {
        P0::emptiable() || P1::emptiable()
    }
}
impl<S: Stream<Item = P0::Input>, P0, P1> ParserOnce<S> for Choice2<P0, P1>
where
    P0: ParserOnce<S>,
    P1: ParserOnce<S, Input = P0::Input, Output = P0::Output>,
{
    fn parse_lookahead_once(self, stream: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        let Choice2(p0, p1) = self;
        if let Some((x, c)) = p0.parse_lookahead_once(stream)? {
            Ok(Some((x, c)))
        } else {
            p1.parse_lookahead_once(stream)
        }
    }
}
impl<S: Stream<Item = P0::Input>, P0, P1> ParserMut<S> for Choice2<P0, P1>
where
    P0: ParserMut<S>,
    P1: ParserMut<S, Input = P0::Input, Output = P0::Output>,
{
    fn parse_lookahead_mut(
        &mut self,
        stream: &mut S,
    ) -> ParseResult<Option<(Self::Output, Consume)>> {
        let Choice2(ref mut p0, ref mut p1) = *self;
        if let Some((x, c)) = p0.parse_lookahead_mut(stream)? {
            Ok(Some((x, c)))
        } else {
            p1.parse_lookahead_mut(stream)
        }
    }
    fn emit_expectations_mut(&mut self, stream: &mut S) {
        let Choice2(ref mut p0, ref mut p1) = *self;
        p0.emit_expectations_mut(stream);
        p1.emit_expectations_mut(stream);
    }
}
impl<S: Stream<Item = P0::Input>, P0, P1> Parser<S> for Choice2<P0, P1>
where
    P0: Parser<S>,
    P1: Parser<S, Input = P0::Input, Output = P0::Output>,
{
    fn parse_lookahead(&self, stream: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        let Choice2(ref p0, ref p1) = *self;
        if let Some((x, c)) = p0.parse_lookahead(stream)? {
            Ok(Some((x, c)))
        } else {
            p1.parse_lookahead(stream)
        }
    }
    fn emit_expectations(&self, stream: &mut S) {
        let Choice2(ref p0, ref p1) = *self;
        p0.emit_expectations(stream);
        p1.emit_expectations(stream);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use stream::SliceStream;

    #[test]
    fn test_any_token() {
        let p = any_token();
        assert_eq!(p.parse(&mut SliceStream::new(b"hoge")).unwrap(), b'h');
        assert!(p.parse(&mut SliceStream::new(b"")).is_err());
    }
}
