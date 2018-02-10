use std::marker::PhantomData;
use {Consume, ParseError, ParseResult, Parser, ParserBase, ParserMut, ParserOnce, Stream};

parser_alias! {
    #[struct = AnyToken]
    #[marker = fn(I)]
    #[type_alias = TokenIf<I, fn(&I) -> bool>]
    pub fn any_token<I>() -> impl Parser<Input = I, Output = I>
    where [
        I: [Clone],
    ] [] []
    {
        token_if(|_| true)
    }
}

pub fn token_if_once<I: Clone, F: FnOnce(&I) -> bool>(f: F) -> TokenIf<I, F> {
    TokenIf(f, PhantomData)
}

pub fn token_if_mut<I: Clone, F: FnMut(&I) -> bool>(f: F) -> TokenIf<I, F> {
    TokenIf(f, PhantomData)
}

pub fn token_if<I: Clone, F: Fn(&I) -> bool>(f: F) -> TokenIf<I, F> {
    TokenIf(f, PhantomData)
}

pub struct TokenIf<I: Clone, F: FnOnce(&I) -> bool>(F, PhantomData<fn(I)>);

impl<I: Clone, F: FnOnce(&I) -> bool> ParserBase for TokenIf<I, F> {
    type Input = I;
    type Output = I;
}
impl<I: Clone, S: Stream<Item = I> + ?Sized, F: FnOnce(&I) -> bool> ParserOnce<S>
    for TokenIf<I, F> {
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
    fn emit_expectations(&self, _stream: &mut S) {
        // TODO: "a token"
    }
}
impl<I: Clone, S: Stream<Item = I> + ?Sized, F: FnMut(&I) -> bool> ParserMut<S> for TokenIf<I, F> {
    fn parse_lookahead_mut(&mut self, stream: &mut S) -> ParseResult<Option<(I, Consume)>> {
        ParserOnce::parse_lookahead_once(TokenIf(&mut self.0, PhantomData), stream)
    }
}
impl<I: Clone, S: Stream<Item = I> + ?Sized, F: Fn(&I) -> bool> Parser<S> for TokenIf<I, F> {
    fn parse_lookahead(&self, stream: &mut S) -> ParseResult<Option<(I, Consume)>> {
        ParserOnce::parse_lookahead_once(TokenIf(&self.0, PhantomData), stream)
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
    fn emit_expectations(&self, _stream: &mut S) {
        // TODO: "EOF"
    }
}
impl<I, S: Stream<Item = I> + ?Sized> ParserMut<S> for Eof<I> {
    fn parse_lookahead_mut(
        &mut self,
        stream: &mut S,
    ) -> ParseResult<Option<(Self::Output, Consume)>> {
        ParserOnce::parse_lookahead_once(Eof(PhantomData), stream)
    }
}
impl<I, S: Stream<Item = I> + ?Sized> Parser<S> for Eof<I> {
    fn parse_lookahead(&self, stream: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        ParserOnce::parse_lookahead_once(Eof(PhantomData), stream)
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
    S: Stream<Item = P::Input> + ?Sized,
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
    fn emit_expectations(&self, stream: &mut S) {
        let Map(ref p, _) = *self;
        p.emit_expectations(stream);
    }
}

impl<S, O, P, F> ParserMut<S> for Map<O, P, F>
where
    S: Stream<Item = P::Input> + ?Sized,
    P: ParserMut<S>,
    F: FnMut(P::Output) -> O,
{
    fn parse_lookahead_mut(
        &mut self,
        stream: &mut S,
    ) -> ParseResult<Option<(Self::Output, Consume)>> {
        ParserOnce::parse_lookahead_once(Map(&mut self.0, &mut self.1), stream)
    }
}

impl<S, O, P, F> Parser<S> for Map<O, P, F>
where
    S: Stream<Item = P::Input> + ?Sized,
    P: Parser<S>,
    F: Fn(P::Output) -> O,
{
    fn parse_lookahead(&self, stream: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        ParserOnce::parse_lookahead_once(Map(&self.0, &self.1), stream)
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
    S: Stream<Item = P0::Input> + ?Sized,
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
    fn emit_expectations(&self, stream: &mut S) {
        let AndThen(ref p0, _) = *self;
        p0.emit_expectations(stream);
        if P0::emptiable() {
            // FIXME
            // p1.emit_expectations(stream);
        }
    }
}

impl<S, P0, P1, F> ParserMut<S> for AndThen<P0, P1, F>
where
    S: Stream<Item = P0::Input> + ?Sized,
    P0: ParserMut<S>,
    P1: ParserOnce<S, Input = P0::Input>,
    F: FnMut(P0::Output) -> P1,
{
    fn parse_lookahead_mut(
        &mut self,
        stream: &mut S,
    ) -> ParseResult<Option<(Self::Output, Consume)>> {
        ParserOnce::parse_lookahead_once(AndThen(&mut self.0, &mut self.1), stream)
    }
}

impl<S, P0, P1, F> Parser<S> for AndThen<P0, P1, F>
where
    S: Stream<Item = P0::Input> + ?Sized,
    P0: Parser<S>,
    P1: ParserOnce<S, Input = P0::Input>,
    F: Fn(P0::Output) -> P1,
{
    fn parse_lookahead(&self, stream: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        ParserOnce::parse_lookahead_once(AndThen(&self.0, &self.1), stream)
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
    S: Stream<Item = I> + ?Sized,
{
    fn parse_lookahead_once(self, _: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        Ok(Some(((), Consume::Empty)))
    }
    fn emit_expectations(&self, _: &mut S) {}
}

impl<S, I> ParserMut<S> for Empty<I>
where
    S: Stream<Item = I> + ?Sized,
{
    fn parse_lookahead_mut(
        &mut self,
        stream: &mut S,
    ) -> ParseResult<Option<(Self::Output, Consume)>> {
        ParserOnce::parse_lookahead_once(Empty(PhantomData), stream)
    }
}

impl<S, I> Parser<S> for Empty<I>
where
    S: Stream<Item = I> + ?Sized,
{
    fn parse_lookahead(&self, stream: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        ParserOnce::parse_lookahead_once(Empty(PhantomData), stream)
    }
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
    S: Stream<Item = P0::Input> + ?Sized,
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
    fn emit_expectations(&self, stream: &mut S) {
        let Concat2(ref p0, ref p1) = *self;
        p0.emit_expectations(stream);
        if P0::emptiable() {
            p1.emit_expectations(stream);
        }
    }
}

impl<S, P0, P1> ParserMut<S> for Concat2<P0, P1>
where
    S: Stream<Item = P0::Input> + ?Sized,
    P0: ParserMut<S>,
    P1: ParserMut<S, Input = P0::Input>,
{
    fn parse_lookahead_mut(
        &mut self,
        stream: &mut S,
    ) -> ParseResult<Option<(Self::Output, Consume)>> {
        ParserOnce::parse_lookahead_once(Concat2(&mut self.0, &mut self.1), stream)
    }
}

impl<S, P0, P1> Parser<S> for Concat2<P0, P1>
where
    S: Stream<Item = P0::Input> + ?Sized,
    P0: Parser<S>,
    P1: Parser<S, Input = P0::Input>,
{
    fn parse_lookahead(&self, stream: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        ParserOnce::parse_lookahead_once(Concat2(&self.0, &self.1), stream)
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
impl<S: Stream<Item = I> + ?Sized, I, O> ParserOnce<S> for Fail<I, O> {
    fn parse_lookahead_once(self, _: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        Ok(None)
    }
    fn emit_expectations(&self, _: &mut S) {}
}
impl<S: Stream<Item = I> + ?Sized, I, O> ParserMut<S> for Fail<I, O> {
    fn parse_lookahead_mut(
        &mut self,
        stream: &mut S,
    ) -> ParseResult<Option<(Self::Output, Consume)>> {
        ParserOnce::parse_lookahead_once(Fail(PhantomData), stream)
    }
}
impl<S: Stream<Item = I> + ?Sized, I, O> Parser<S> for Fail<I, O> {
    fn parse_lookahead(&self, stream: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        ParserOnce::parse_lookahead_once(Fail(PhantomData), stream)
    }
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
impl<S: Stream<Item = P0::Input> + ?Sized, P0, P1> ParserOnce<S> for Choice2<P0, P1>
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
    fn emit_expectations(&self, stream: &mut S) {
        let Choice2(ref p0, ref p1) = *self;
        p0.emit_expectations(stream);
        p1.emit_expectations(stream);
    }
}
impl<S: Stream<Item = P0::Input> + ?Sized, P0, P1> ParserMut<S> for Choice2<P0, P1>
where
    P0: ParserMut<S>,
    P1: ParserMut<S, Input = P0::Input, Output = P0::Output>,
{
    fn parse_lookahead_mut(
        &mut self,
        stream: &mut S,
    ) -> ParseResult<Option<(Self::Output, Consume)>> {
        ParserOnce::parse_lookahead_once(Choice2(&mut self.0, &mut self.1), stream)
    }
}
impl<S: Stream<Item = P0::Input> + ?Sized, P0, P1> Parser<S> for Choice2<P0, P1>
where
    P0: Parser<S>,
    P1: Parser<S, Input = P0::Input, Output = P0::Output>,
{
    fn parse_lookahead(&self, stream: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        ParserOnce::parse_lookahead_once(Choice2(&self.0, &self.1), stream)
    }
}

pub(crate) fn assert_once<P, F>(p: P, f: F) -> Assert<P, F>
where
    P: ParserBase,
    F: FnOnce(&P::Output) -> bool,
{
    Assert(p, f)
}
pub(crate) fn assert_mut<P, F>(p: P, f: F) -> Assert<P, F>
where
    P: ParserBase,
    F: FnMut(&P::Output) -> bool,
{
    Assert(p, f)
}
pub(crate) fn assert<P, F>(p: P, f: F) -> Assert<P, F>
where
    P: ParserBase,
    F: Fn(&P::Output) -> bool,
{
    Assert(p, f)
}
pub struct Assert<P, F>(P, F)
where
    P: ParserBase,
    F: FnOnce(&P::Output) -> bool;
impl<P, F> ParserBase for Assert<P, F>
where
    P: ParserBase,
    F: FnOnce(&P::Output) -> bool,
{
    type Input = P::Input;
    type Output = P::Output;
    fn emptiable() -> bool {
        P::emptiable()
    }
}
impl<S: Stream<Item = P::Input>, P, F> ParserOnce<S> for Assert<P, F>
where
    P: ParserOnce<S>,
    F: FnOnce(&P::Output) -> bool,
{
    fn parse_lookahead_once(self, stream: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        let Assert(p, f) = self;
        if let Some((x, c)) = p.parse_lookahead_once(stream)? {
            if f(&x) {
                Ok(Some((x, c)))
            } else if c == Consume::Empty {
                Ok(None)
            } else {
                Err(ParseError::SyntaxError)
            }
        } else {
            Ok(None)
        }
    }
    fn emit_expectations(&self, stream: &mut S) {
        let Assert(ref p, _) = *self;
        p.emit_expectations(stream);
    }
}
impl<S: Stream<Item = P::Input>, P, F> ParserMut<S> for Assert<P, F>
where
    P: ParserMut<S>,
    F: FnMut(&P::Output) -> bool,
{
    fn parse_lookahead_mut(
        &mut self,
        stream: &mut S,
    ) -> ParseResult<Option<(Self::Output, Consume)>> {
        ParserOnce::parse_lookahead_once(Assert(&mut self.0, &mut self.1), stream)
    }
}
impl<S: Stream<Item = P::Input>, P, F> Parser<S> for Assert<P, F>
where
    P: Parser<S>,
    F: Fn(&P::Output) -> bool,
{
    fn parse_lookahead(&self, stream: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        ParserOnce::parse_lookahead_once(Assert(&self.0, &self.1), stream)
    }
}

parser_alias! {
    #[struct = SkipLeft]
    #[marker = ()]
    #[type_alias = Map<P1::Output, Concat2<P0, P1>, fn((P0::Output, P1::Output)) -> P1::Output>]
    pub fn skip_left<P0, P1>(p0: P0, p1: P1)
        -> impl Parser<Input = P0::Input, Output = P1::Output>
    where [] [
        P0: ParserBase<>,
        P1: ParserBase<Input = P0::Input>,
    ] []
    {
        p0.concat(p1).map(|(_, y)| y)
    }
}
parser_alias! {
    #[struct = SkipRight]
    #[marker = ()]
    #[type_alias = Map<P0::Output, Concat2<P0, P1>, fn((P0::Output, P1::Output)) -> P0::Output>]
    pub fn skip_right<P0, P1>(p0: P0, p1: P1)
        -> impl Parser<Input = P0::Input, Output = P0::Output>
    where [] [
        P0: ParserBase<>,
        P1: ParserBase<Input = P0::Input>,
    ] []
    {
        p0.concat(p1).map(|(x, _)| x)
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
