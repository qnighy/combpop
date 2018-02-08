use Stream;
use combinators;

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

pub trait ParserBase {
    type Input;
    type Output;
    fn emptiable() -> bool
    where
        Self: Sized,
    {
        false
    }

    fn map_once<O, F>(self, f: F) -> combinators::Map<O, Self, F>
    where
        Self: Sized,
        F: FnOnce(Self::Output) -> O,
    {
        combinators::map_once(self, f)
    }

    fn map_mut<O, F>(self, f: F) -> combinators::Map<O, Self, F>
    where
        Self: Sized,
        F: FnMut(Self::Output) -> O,
    {
        combinators::map_mut(self, f)
    }

    fn map<O, F>(self, f: F) -> combinators::Map<O, Self, F>
    where
        Self: Sized,
        F: Fn(Self::Output) -> O,
    {
        combinators::map(self, f)
    }

    fn and_then_once<P, F>(self, f: F) -> combinators::AndThen<Self, P, F>
    where
        Self: Sized,
        P: ParserBase<Input = Self::Input>,
        F: FnOnce(Self::Output) -> P,
    {
        combinators::and_then_once(self, f)
    }

    fn and_then_mut<P, F>(self, f: F) -> combinators::AndThen<Self, P, F>
    where
        Self: Sized,
        P: ParserBase<Input = Self::Input>,
        F: FnMut(Self::Output) -> P,
    {
        combinators::and_then_mut(self, f)
    }

    fn and_then<P, F>(self, f: F) -> combinators::AndThen<Self, P, F>
    where
        Self: Sized,
        P: ParserBase<Input = Self::Input>,
        F: Fn(Self::Output) -> P,
    {
        combinators::and_then(self, f)
    }

    fn concat<P>(self, p: P) -> combinators::Concat2<Self, P>
    where
        Self: Sized,
        P: ParserBase<Input = Self::Input>,
    {
        combinators::concat2(self, p)
    }

    fn or<P>(self, p: P) -> combinators::Choice2<Self, P>
    where
        Self: Sized,
        P: ParserBase<Input = Self::Input, Output = Self::Output>,
    {
        combinators::choice2(self, p)
    }
}
pub trait ParserOnce<S: Stream<Item = Self::Input> + ?Sized>: ParserBase {
    fn parse_lookahead_once(self, stream: &mut S) -> ParseResult<Option<(Self::Output, Consume)>>
    where
        Self: Sized;
}
pub trait ParserMut<S: Stream<Item = Self::Input> + ?Sized>: ParserOnce<S> {
    fn parse_mut(&mut self, stream: &mut S) -> ParseResult<Self::Output> {
        Ok(self.parse_consume_mut(stream)?.0)
    }
    fn parse_consume_mut(&mut self, stream: &mut S) -> ParseResult<(Self::Output, Consume)> {
        if let Some(x) = self.parse_lookahead_mut(stream)? {
            Ok(x)
        } else {
            self.emit_expectations_mut(stream);
            Err(ParseError::SyntaxError)
        }
    }
    fn parse_lookahead_mut(
        &mut self,
        stream: &mut S,
    ) -> ParseResult<Option<(Self::Output, Consume)>>;
    fn emit_expectations_mut(&mut self, stream: &mut S);
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
    fn emit_expectations(&self, stream: &mut S);
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
        fn emit_expectations_mut(&mut self, stream: &mut S) {
            ParserMut::emit_expectations_mut($this, stream);
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
        fn emit_expectations(&self, stream: &mut S) {
            Parser::emit_expectations($this, stream);
        }
    }
}
