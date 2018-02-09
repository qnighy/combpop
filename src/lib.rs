//! **Combpop** is a type-based parser combinator library.

pub mod stream;
#[macro_use]
pub mod parser;
pub mod combinators;
pub mod iter;
pub mod byte;

pub use stream::Stream;
pub use parser::{Consume, ParseError, ParseResult, Parser, ParserBase, ParserMut, ParserOnce};
pub use iter::{ParserIterator, ParserIteratorBase, ParserIteratorMut};

#[macro_export]
macro_rules! parser {
    (
        pub fn $name:ident () -> $struct:ident<Input = $Input:ident, Output = $Output:ident>
        $body:block
    ) => {
        pub fn $name() -> $struct {
            $struct(::std::marker::PhantomData)
        }
        pub struct $struct(::std::marker::PhantomData<()>);
        impl ParserBase for $struct {
            type Input = $Input;
            type Output = $Output;
        }
        impl<S: Stream<Item = $Input>> ParserOnce<S> for $struct {
            fn parse_lookahead_once(self, stream: &mut S)
                -> ParseResult<Option<(Self::Output, Consume)>>
            where
                Self: Sized,
            {
                let parser = $body;
                ParserOnce::parse_lookahead_once(parser, stream)
            }
            fn emit_expectations(&self, stream: &mut S) {
                let parser = $body;
                ParserOnce::emit_expectations(&parser, stream);
            }
        }
        impl<S: Stream<Item = $Input>> ParserMut<S> for $struct {
            fn parse_lookahead_mut(&mut self, stream: &mut S)
                -> ParseResult<Option<(Self::Output, Consume)>>
            {
                let mut parser = $body;
                ParserMut::parse_lookahead_mut(&mut parser, stream)
            }
        }
        impl<S: Stream<Item = $Input>> Parser<S> for $struct {
            fn parse_lookahead(&self, stream: &mut S)
                -> ParseResult<Option<(Self::Output, Consume)>>
            {
                let parser = $body;
                Parser::parse_lookahead(&parser, stream)
            }
        }
    }
}

#[cfg(test)]
mod tests;
