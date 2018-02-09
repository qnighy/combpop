use std::marker::PhantomData;
use std::iter::FromIterator;
use {Consume, ParseError, ParseResult, Parser, ParserBase, ParserMut, ParserOnce, Stream};

pub trait ParserIteratorBase {
    type Input;
    type Element;
    fn emptiable() -> bool
    where
        Self: Sized,
    {
        false
    }

    fn collect<V: FromIterator<Self::Element>>(self) -> Collect<Self, V>
    where
        Self: Sized,
    {
        collect(self)
    }
}
pub trait ParserIteratorMut<S: Stream<Item = Self::Input> + ?Sized>
    : ParserIteratorBase {
    type State;
    fn begin(&self) -> Self::State;
    fn next_mut(
        &mut self,
        stream: &mut S,
        state: &mut Self::State,
    ) -> ParseResult<Option<(Option<Self::Element>, Consume)>>;
    fn emit_expectations(&self, stream: &mut S);
}
pub trait ParserIterator<S: Stream<Item = Self::Input> + ?Sized>
    : ParserIteratorMut<S> {
    fn next(
        &self,
        stream: &mut S,
        state: &mut Self::State,
    ) -> ParseResult<Option<(Option<Self::Element>, Consume)>>;
}

pub(crate) fn many<P: ParserBase>(p: P) -> Many<P> {
    Many(p)
}
pub struct Many<P: ParserBase>(P);
impl<P> ParserIteratorBase for Many<P>
where
    P: ParserBase,
{
    type Input = P::Input;
    type Element = P::Output;
    fn emptiable() -> bool {
        P::emptiable()
    }
}
impl<S, P> ParserIteratorMut<S> for Many<P>
where
    S: Stream<Item = P::Input> + ?Sized,
    P: ParserMut<S>,
{
    type State = ();
    fn begin(&self) -> () {
        ()
    }
    fn next_mut(
        &mut self,
        stream: &mut S,
        _: &mut Self::State,
    ) -> ParseResult<Option<(Option<Self::Element>, Consume)>> {
        if let Some((x, c)) = self.0.parse_lookahead_mut(stream)? {
            Ok(Some((Some(x), c)))
        } else {
            Ok(Some((None, Consume::Empty)))
        }
    }
    fn emit_expectations(&self, stream: &mut S) {
        self.0.emit_expectations(stream);
    }
}
impl<S, P> ParserIterator<S> for Many<P>
where
    S: Stream<Item = P::Input> + ?Sized,
    P: Parser<S>,
{
    fn next(
        &self,
        stream: &mut S,
        _: &mut Self::State,
    ) -> ParseResult<Option<(Option<Self::Element>, Consume)>> {
        if let Some((x, c)) = self.0.parse_lookahead(stream)? {
            Ok(Some((Some(x), c)))
        } else {
            Ok(Some((None, Consume::Empty)))
        }
    }
}

pub(crate) fn many1<P: ParserBase>(p: P) -> Many1<P> {
    Many1(p)
}
pub struct Many1<P: ParserBase>(P);
impl<P> ParserIteratorBase for Many1<P>
where
    P: ParserBase,
{
    type Input = P::Input;
    type Element = P::Output;
    fn emptiable() -> bool {
        P::emptiable()
    }
}
impl<S, P> ParserIteratorMut<S> for Many1<P>
where
    S: Stream<Item = P::Input> + ?Sized,
    P: ParserMut<S>,
{
    type State = usize;
    fn begin(&self) -> usize {
        0
    }
    fn next_mut(
        &mut self,
        stream: &mut S,
        state: &mut Self::State,
    ) -> ParseResult<Option<(Option<Self::Element>, Consume)>> {
        if let Some((x, c)) = self.0.parse_lookahead_mut(stream)? {
            *state += 1;
            Ok(Some((Some(x), c)))
        } else if *state == 0 {
            Ok(None)
        } else {
            Ok(Some((None, Consume::Empty)))
        }
    }
    fn emit_expectations(&self, stream: &mut S) {
        self.0.emit_expectations(stream);
    }
}
impl<S, P> ParserIterator<S> for Many1<P>
where
    S: Stream<Item = P::Input> + ?Sized,
    P: Parser<S>,
{
    fn next(
        &self,
        stream: &mut S,
        state: &mut Self::State,
    ) -> ParseResult<Option<(Option<Self::Element>, Consume)>> {
        if let Some((x, c)) = self.0.parse_lookahead(stream)? {
            *state += 1;
            Ok(Some((Some(x), c)))
        } else if *state == 0 {
            Ok(None)
        } else {
            Ok(Some((None, Consume::Empty)))
        }
    }
}

pub(crate) fn collect<P, V>(p: P) -> Collect<P, V>
where
    P: ParserIteratorBase,
    V: FromIterator<P::Element>,
{
    Collect(p, PhantomData)
}
pub struct Collect<P, V>(P, PhantomData<fn() -> V>)
where
    P: ParserIteratorBase,
    V: FromIterator<P::Element>;
impl<P, V> ParserBase for Collect<P, V>
where
    P: ParserIteratorBase,
    V: FromIterator<P::Element>,
{
    type Input = P::Input;
    type Output = V;
    fn emptiable() -> bool {
        P::emptiable()
    }
}
impl<S, P, V> ParserOnce<S> for Collect<P, V>
where
    S: Stream<Item = P::Input> + ?Sized,
    P: ParserIteratorMut<S>,
    V: FromIterator<P::Element>,
{
    fn parse_lookahead_once(
        mut self,
        stream: &mut S,
    ) -> ParseResult<Option<(Self::Output, Consume)>> {
        ParserMut::parse_lookahead_mut(&mut self, stream)
    }
    fn emit_expectations(&self, stream: &mut S) {
        self.0.emit_expectations(stream);
    }
}
impl<S, P, V> ParserMut<S> for Collect<P, V>
where
    S: Stream<Item = P::Input> + ?Sized,
    P: ParserIteratorMut<S>,
    V: FromIterator<P::Element>,
{
    fn parse_lookahead_mut(
        &mut self,
        stream: &mut S,
    ) -> ParseResult<Option<(Self::Output, Consume)>> {
        let state = self.0.begin();
        let mut iter = CollectorMut {
            parser: &mut self.0,
            stream,
            state,
            consume: Consume::Empty,
        };
        let x: ParseResult<Option<Self::Output>> = iter.by_ref().collect();
        x.map(|x| x.map(|x| (x, iter.consume)))
    }
}
impl<S, P, V> Parser<S> for Collect<P, V>
where
    S: Stream<Item = P::Input> + ?Sized,
    P: ParserIterator<S>,
    V: FromIterator<P::Element>,
{
    fn parse_lookahead(&self, stream: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        let state = self.0.begin();
        let mut iter = Collector {
            parser: &self.0,
            stream,
            state,
            consume: Consume::Empty,
        };
        let x: ParseResult<Option<Self::Output>> = iter.by_ref().collect();
        x.map(|x| x.map(|x| (x, iter.consume)))
    }
}

struct CollectorMut<'a, S, P>
where
    S: Stream<Item = P::Input> + ?Sized + 'a,
    P: ParserIteratorMut<S> + 'a,
{
    stream: &'a mut S,
    parser: &'a mut P,
    state: P::State,
    consume: Consume,
}
impl<'a, S, P> Iterator for CollectorMut<'a, S, P>
where
    S: Stream<Item = P::Input> + ?Sized + 'a,
    P: ParserIteratorMut<S> + 'a,
{
    type Item = ParseResult<Option<P::Element>>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self.parser.next_mut(self.stream, &mut self.state) {
            Ok(Some((Some(x), c))) => {
                self.consume |= c;
                Some(Ok(Some(x)))
            }
            Ok(Some((None, _))) => None,
            Ok(None) => if self.consume == Consume::Empty {
                Some(Ok(None))
            } else {
                Some(Err(ParseError::SyntaxError))
            },
            Err(e) => Some(Err(e)),
        }
    }
}

struct Collector<'a, S, P>
where
    S: Stream<Item = P::Input> + ?Sized + 'a,
    P: ParserIterator<S> + 'a,
{
    stream: &'a mut S,
    parser: &'a P,
    state: P::State,
    consume: Consume,
}
impl<'a, S, P> Iterator for Collector<'a, S, P>
where
    S: Stream<Item = P::Input> + ?Sized + 'a,
    P: ParserIterator<S> + 'a,
{
    type Item = ParseResult<Option<P::Element>>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self.parser.next(self.stream, &mut self.state) {
            Ok(Some((Some(x), c))) => {
                self.consume |= c;
                Some(Ok(Some(x)))
            }
            Ok(Some((None, _))) => None,
            Ok(None) => if self.consume == Consume::Empty {
                Some(Ok(None))
            } else {
                Some(Err(ParseError::SyntaxError))
            },
            Err(e) => Some(Err(e)),
        }
    }
}
