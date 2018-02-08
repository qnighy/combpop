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
