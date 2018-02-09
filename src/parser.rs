use Stream;
use combinators;
use iter;

#[derive(Debug)]
pub enum ParseError {
    EOF,
    NotReady,
    SyntaxError,
}
impl ParseError {
    pub fn is_recoverable(&self) -> bool {
        match self {
            &ParseError::EOF | &ParseError::SyntaxError => true,
            &ParseError::NotReady => false,
        }
    }
}

pub type ParseResult<T> = Result<T, ParseError>;

/// A flag to indicate whether a parser consumed a token or not.
///
/// # Example
///
/// ```
/// # extern crate combpop;
/// # use combpop::Consume;
/// # fn main() {
/// assert_eq!(Consume::Empty | Consume::Consumed, Consume::Consumed);
/// assert_eq!(Consume::Empty & Consume::Consumed, Consume::Empty);
/// # }
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Consume {
    Consumed,
    Empty,
}
impl Default for Consume {
    fn default() -> Self {
        Consume::Consumed
    }
}
impl ::std::ops::BitAnd for Consume {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self {
        match self {
            Consume::Consumed => rhs,
            Consume::Empty => Consume::Empty,
        }
    }
}
impl ::std::ops::BitAndAssign for Consume {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = *self & rhs;
    }
}
impl ::std::ops::BitOr for Consume {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        match self {
            Consume::Consumed => Consume::Consumed,
            Consume::Empty => rhs,
        }
    }
}
impl ::std::ops::BitOrAssign for Consume {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

/// Stream-independent properties of parsers and parser-building methods.
///
/// See `Parser` for the actual parsing functionality. See `combinators`, `iter`, and `bytes` for
/// other parser-building functions.
pub trait ParserBase {
    /// The input type of the parser. `u8` for byte-eating parsers.
    type Input;
    /// The output type which this monadic parser produces.
    type Output;
    /// Whether the parser "usually" accepts the empty sequence. Only used in `emit_expectations`.
    fn emptiable() -> bool
    where
        Self: Sized,
    {
        false
    }

    /// Converts a semantic value after parsing. Similar to `ParserBase::map`, but accepts `FnOnce`
    /// closures.
    fn map_once<O, F>(self, f: F) -> combinators::Map<O, Self, F>
    where
        Self: Sized,
        F: FnOnce(Self::Output) -> O,
    {
        combinators::map_once(self, f)
    }

    /// Converts a semantic value after parsing. Similar to `ParserBase::map`, but accepts `FnMut`
    /// closures.
    fn map_mut<O, F>(self, f: F) -> combinators::Map<O, Self, F>
    where
        Self: Sized,
        F: FnMut(Self::Output) -> O,
    {
        combinators::map_mut(self, f)
    }

    /// Converts a semantic value after parsing.
    fn map<O, F>(self, f: F) -> combinators::Map<O, Self, F>
    where
        Self: Sized,
        F: Fn(Self::Output) -> O,
    {
        combinators::map(self, f)
    }

    /// Monadic operator: run another parser `f(x)` after parsing. Similar to
    /// `ParserBase::and_then`, but accepts `FnOnce` closures.
    fn and_then_once<P, F>(self, f: F) -> combinators::AndThen<Self, P, F>
    where
        Self: Sized,
        P: ParserBase<Input = Self::Input>,
        F: FnOnce(Self::Output) -> P,
    {
        combinators::and_then_once(self, f)
    }

    /// Monadic operator: run another parser `f(x)` after parsing. Similar to
    /// `ParserBase::and_then`, but accepts `FnMut` closures.
    fn and_then_mut<P, F>(self, f: F) -> combinators::AndThen<Self, P, F>
    where
        Self: Sized,
        P: ParserBase<Input = Self::Input>,
        F: FnMut(Self::Output) -> P,
    {
        combinators::and_then_mut(self, f)
    }

    /// Monadic operator: run another parser `f(x)` after parsing.
    ///
    /// # Restrictions
    ///
    /// Applicative construction (`concat` + `map`) is recommended than `and_then`. Since the
    /// value-dependency of the monadic construction, it may prevent useful optimizations. For
    /// example, `AndThen::emit_expectations` cannot emit expectations from the latter parser.
    fn and_then<P, F>(self, f: F) -> combinators::AndThen<Self, P, F>
    where
        Self: Sized,
        P: ParserBase<Input = Self::Input>,
        F: Fn(Self::Output) -> P,
    {
        combinators::and_then(self, f)
    }

    /// Applicative operator: run another parser `p` after parsing. The result is a pair of the
    /// result of the parsers.
    fn concat<P>(self, p: P) -> combinators::Concat2<Self, P>
    where
        Self: Sized,
        P: ParserBase<Input = Self::Input>,
    {
        combinators::concat2(self, p)
    }

    /// If the parser failed without consumption, try another parser.
    fn or<P>(self, p: P) -> combinators::Choice2<Self, P>
    where
        Self: Sized,
        P: ParserBase<Input = Self::Input, Output = Self::Output>,
    {
        combinators::choice2(self, p)
    }

    /// Returns a `ParserIteratorBase` that collects zero or more objects from this parser.
    fn many(self) -> iter::Many<Self>
    where
        Self: Sized,
    {
        iter::many(self)
    }

    /// Returns a `ParserIteratorBase` that collects one or more objects from this parser.
    fn many1(self) -> iter::Many1<Self>
    where
        Self: Sized,
    {
        iter::many1(self)
    }

    /// Equivalent to `self.concat(p).map(|(_, y)| y)`.
    fn skip_left<P>(self, p: P) -> combinators::SkipLeft<Self, P>
    where
        Self: Sized,
        P: ParserBase<Input = Self::Input>,
    {
        combinators::skip_left(self, p)
    }

    /// Equivalent to `self.concat(p).map(|(x, _)| x)`.
    fn skip_right<P>(self, p: P) -> combinators::SkipRight<Self, P>
    where
        Self: Sized,
        P: ParserBase<Input = Self::Input>,
    {
        combinators::skip_right(self, p)
    }
}
pub trait ParserOnce<S: Stream<Item = Self::Input> + ?Sized>: ParserBase {
    fn parse_lookahead_once(self, stream: &mut S) -> ParseResult<Option<(Self::Output, Consume)>>
    where
        Self: Sized;
    fn emit_expectations(&self, stream: &mut S);
}
pub trait ParserMut<S: Stream<Item = Self::Input> + ?Sized>: ParserOnce<S> {
    fn parse_mut(&mut self, stream: &mut S) -> ParseResult<Self::Output> {
        Ok(self.parse_consume_mut(stream)?.0)
    }
    fn parse_consume_mut(&mut self, stream: &mut S) -> ParseResult<(Self::Output, Consume)> {
        if let Some(x) = self.parse_lookahead_mut(stream)? {
            Ok(x)
        } else {
            self.emit_expectations(stream);
            Err(ParseError::SyntaxError)
        }
    }
    fn parse_lookahead_mut(
        &mut self,
        stream: &mut S,
    ) -> ParseResult<Option<(Self::Output, Consume)>>;
}
pub trait Parser<S: Stream<Item = Self::Input> + ?Sized>: ParserMut<S> {
    fn parse(&self, stream: &mut S) -> ParseResult<Self::Output> {
        Ok(self.parse_consume(stream)?.0)
    }
    fn parse_consume(&self, stream: &mut S) -> ParseResult<(Self::Output, Consume)> {
        if let Some(x) = self.parse_lookahead(stream)? {
            Ok(x)
        } else {
            self.emit_expectations(stream);
            Err(ParseError::SyntaxError)
        }
    }
    fn parse_lookahead(&self, stream: &mut S) -> ParseResult<Option<(Self::Output, Consume)>>;
}

macro_rules! delegate_parser_once {
    ($this:expr) => {
        fn parse_lookahead_once(self, stream: &mut S)
            -> ParseResult<Option<(Self::Output, Consume)>>
        where
            Self: Sized,
        {
            ParserOnce::parse_lookahead_once($this, stream)
        }
    }
}
macro_rules! delegate_parser_mut {
    ($this:expr) => {
        fn parse_mut(&mut self, stream: &mut S) -> ParseResult<Self::Output> {
            ParserMut::parse_mut($this, stream)
        }
        fn parse_consume_mut(&mut self, stream: &mut S) -> ParseResult<(Self::Output, Consume)> {
            ParserMut::parse_consume_mut($this, stream)
        }
        fn parse_lookahead_mut(&mut self, stream: &mut S)
            -> ParseResult<Option<(Self::Output, Consume)>> {
            ParserMut::parse_lookahead_mut($this, stream)
        }
    }
}
macro_rules! delegate_parser {
    ($this:expr) => {
        fn parse(&self, stream: &mut S) -> ParseResult<Self::Output> {
            Parser::parse($this, stream)
        }
        fn parse_consume(&self, stream: &mut S) -> ParseResult<(Self::Output, Consume)> {
            Parser::parse_consume($this, stream)
        }
        fn parse_lookahead(&self, stream: &mut S)
            -> ParseResult<Option<(Self::Output, Consume)>> {
            Parser::parse_lookahead($this, stream)
        }
    }
}
