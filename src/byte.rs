use std::char;
use std::marker::PhantomData;
use {Consume, ParseError, ParseResult, Parser, ParserBase, Stream};
use combinators::{any_token, AnyToken};

pub fn any_byte() -> AnyToken<u8> {
    any_token()
}

pub fn any_char() -> AnyChar {
    AnyChar(PhantomData)
}

pub struct AnyChar(PhantomData<()>);

impl ParserBase for AnyChar {
    type Input = u8;
    type Output = char;
}
impl<S: Stream<Item = u8> + ?Sized> Parser<S> for AnyChar {
    delegate_parser!(&mut char(|_| true));
}

pub fn char<F: FnMut(char) -> bool>(f: F) -> Char<F> {
    Char(f)
}

pub struct Char<F: FnMut(char) -> bool>(F);

impl<F: FnMut(char) -> bool> ParserBase for Char<F> {
    type Input = u8;
    type Output = char;
}
impl<F: FnMut(char) -> bool, S: Stream<Item = u8> + ?Sized> Parser<S> for Char<F> {
    fn parse_lookahead(&mut self, stream: &mut S) -> ParseResult<Option<(char, Consume)>> {
        let b0 = match stream.lookahead(1) {
            Ok(()) => *stream.get(0),
            Err(ParseError::EOF) => return Ok(None),
            Err(e) => return Err(e),
        };
        macro_rules! get {
            ($i:expr, $min:expr, $max:expr) => {{
                let b = match stream.lookahead($i + 1) {
                    Ok(()) => *stream.get($i),
                    Err(ParseError::EOF) => return Ok(None),
                    Err(e) => return Err(e),
                };
                if !($min <= b && b < $max) {
                    return Err(ParseError::SyntaxError)
                };
                b
            }};
        }
        let (c, n) = if b0 < 0x80 {
            (b0 as char, 1)
        } else if b0 < 0xC2 {
            return Ok(None);
        } else if b0 < 0xE0 {
            let b0 = b0 as u32;
            let b1 = get!(1, 0x80, 0xC0) as u32;
            let c = ((b0 & 0x1F) << 6) | (b1 & 0x3F);
            (unsafe { char::from_u32_unchecked(c) }, 2)
        } else if b0 < 0xF0 {
            let b1lim = if b0 == 0xE0 { 0xA0 } else { 0x80 };
            let b0 = b0 as u32;
            let b1 = get!(1, b1lim, 0xC0) as u32;
            // Reject upper/lower surrogates (U+D800 to U+DFFF)
            if b0 == 0xED && b1 >= 0xA0 {
                return Ok(None);
            }
            let b2 = get!(2, 0x80, 0xC0) as u32;
            let c = ((b0 & 0x0F) << 12) | ((b1 & 0x3F) << 6) | (b2 & 0x3F);
            (unsafe { char::from_u32_unchecked(c) }, 3)
        } else if b0 < 0xF8 {
            let b1lim = if b0 == 0xF0 { 0x90 } else { 0x80 };
            let b0 = b0 as u32;
            let b1 = get!(1, b1lim, 0xC0) as u32;
            let b2 = get!(2, 0x80, 0xC0) as u32;
            let b3 = get!(3, 0x80, 0xC0) as u32;
            let c = ((b0 & 0x07) << 18) | ((b1 & 0x3F) << 12) | ((b2 & 0x3F) << 6) | (b3 & 0x3F);
            (unsafe { char::from_u32_unchecked(c) }, 4)
        } else {
            return Ok(None);
        };
        if !(self.0)(c) {
            return Ok(None);
        }
        stream.advance(n);
        Ok(Some((c, Consume::Consumed)))
    }
    fn emit_expectations(&mut self, _stream: &mut S) {
        // TODO
    }
}
