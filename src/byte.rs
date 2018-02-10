use std::char;
use {Consume, ParseError, ParseResult, Parser, ParserBase, ParserMut, ParserOnce, Stream};
use combinators::{any_token, token_if_once, AnyToken, TokenIf};

parser_alias! {
    #[struct = AnyByte]
    #[marker = ()]
    #[type_alias = AnyToken<u8>]
    pub fn any_byte<>() -> impl Parser<Input = u8, Output = u8>
    where [] [] []
    {
        any_token::<u8>()
    }
}

parser_alias! {
    #[struct = AnyChar]
    #[marker = ()]
    #[type_alias = CharIf<fn(char) -> bool>]
    pub fn any_char<>() -> impl Parser<Input = u8, Output = char>
    where [] [] []
    {
        char_if(|_| true)
    }
}

pub fn char_if_once<F: FnOnce(char) -> bool>(f: F) -> CharIf<F> {
    CharIf(f)
}

pub fn char_if_mut<F: FnMut(char) -> bool>(f: F) -> CharIf<F> {
    CharIf(f)
}

pub fn char_if<F: Fn(char) -> bool>(f: F) -> CharIf<F> {
    CharIf(f)
}

pub struct CharIf<F: FnOnce(char) -> bool>(F);

impl<F: FnOnce(char) -> bool> ParserBase for CharIf<F> {
    type Input = u8;
    type Output = char;
}
impl<F: FnOnce(char) -> bool, S: Stream<Item = u8> + ?Sized> ParserOnce<S> for CharIf<F> {
    fn parse_lookahead_once(self, stream: &mut S) -> ParseResult<Option<(char, Consume)>> {
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
    fn emit_expectations(&self, _stream: &mut S) {
        // TODO: "a char"
    }
}
impl<F: FnMut(char) -> bool, S: Stream<Item = u8> + ?Sized> ParserMut<S> for CharIf<F> {
    fn parse_lookahead_mut(
        &mut self,
        stream: &mut S,
    ) -> ParseResult<Option<(Self::Output, Consume)>> {
        ParserOnce::parse_lookahead_once(CharIf(&mut self.0), stream)
    }
}
impl<F: Fn(char) -> bool, S: Stream<Item = u8> + ?Sized> Parser<S> for CharIf<F> {
    fn parse_lookahead(&self, stream: &mut S) -> ParseResult<Option<(Self::Output, Consume)>> {
        ParserOnce::parse_lookahead_once(CharIf(&self.0), stream)
    }
}

parser_alias! {
    #[struct = AsciiIf]
    #[marker = ()]
    #[type_alias = TokenIf<u8, fn(&u8) -> bool>]
    pub fn ascii_if_once<F>(f: F) -> impl Parser<Input = u8, Output = u8>
    where [] [] [
        F: FnOnce(u8) -> bool,
    ]
    {
        token_if_once(move |&b| b < 0x80 && f(b))
    }
}
pub fn ascii_if_mut<F>(f: F) -> AsciiIf<F>
where
    F: FnMut(u8) -> bool,
{
    ascii_if_once(f)
}
pub fn ascii_if<F>(f: F) -> AsciiIf<F>
where
    F: Fn(u8) -> bool,
{
    ascii_if_once(f)
}

parser_alias! {
    #[struct = Alpha]
    #[marker = ()]
    #[type_alias = CharIf<fn(char) -> bool>]
    pub fn alpha<>() -> impl Parser<Input = u8, Output = char>
    where [] [] []
    {
        char_if(char::is_alphabetic)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use stream::SliceStream;
    use ParserIteratorBase;

    #[test]
    fn test_alpha() {
        let p = alpha().many1().collect::<String>();
        assert_eq!(p.parse(&mut SliceStream::new(b"hoge")).unwrap(), "hoge");
        assert_eq!(
            p.parse(&mut SliceStream::new(b"hoge fuga")).unwrap(),
            "hoge"
        );
        assert!(p.parse(&mut SliceStream::new(b" hoge")).is_err());
        assert!(p.parse(&mut SliceStream::new(b"")).is_err());
    }
}
