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
    fn nonempty() -> bool
    where
        Self: Sized,
    {
        false
    }
    fn no_backtrack() -> bool
    where
        Self: Sized,
    {
        false
    }

    fn and_then<P, F>(self, f: F) -> combinators::AndThen<Self, P, F>
    where
        Self: Sized,
        P: ParserBase<Input = Self::Input>,
        F: FnMut(Self::Output) -> P,
    {
        combinators::and_then(self, f)
    }
}
pub trait Parser<S: Stream<Item = Self::Input> + ?Sized>: ParserBase {
    fn parse(&mut self, stream: &mut S) -> ParseResult<Self::Output> {
        Ok(self.parse_consume(stream)?.0)
    }
    fn parse_consume(&mut self, stream: &mut S) -> ParseResult<(Self::Output, Consume)> {
        if let Some(x) = self.parse_lookahead(stream)? {
            Ok(x)
        } else {
            self.emit_expectations(stream);
            Err(ParseError::SyntaxError)
        }
    }
    fn parse_lookahead(&mut self, stream: &mut S) -> ParseResult<Option<(Self::Output, Consume)>>;
    fn emit_expectations(&mut self, stream: &mut S);
}

macro_rules! delegate_parser {
    ($this:expr) => {
        fn parse(&mut self, stream: &mut S) -> ParseResult<Self::Output> {
            Parser::parse($this, stream)
        }
        fn parse_consume(&mut self, stream: &mut S) -> ParseResult<(Self::Output, Consume)> {
            Parser::parse_consume($this, stream)
        }
        fn parse_lookahead(&mut self, stream: &mut S)
            -> ParseResult<Option<(Self::Output, Consume)>> {
            Parser::parse_lookahead($this, stream)
        }
        fn emit_expectations(&mut self, stream: &mut S) {
            Parser::emit_expectations($this, stream)
        }
    }
}
