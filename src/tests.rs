use super::*;
use stream::SliceStream;

pub mod json {
    use ::*;
    use stream::SliceStream;
    use either::Either;
    use byte::char_if;
    parser! {
        pub fn json_string() -> JsonString<Input = u8, Output = String> {
            let u16escape = || {
                let hexchar = || {
                    char_if(|x| '0' <= x && x <= '9').map(|x| x as u32 - 0x30)
                        .or(char_if(|x| 'A' <= x && x <= 'F').map(|x| x as u32 - 0x41 + 10))
                        .or(char_if(|x| 'a' <= x && x <= 'f').map(|x| x as u32 - 0x61 + 10))
                };
                char_if(|x| x == 'u')
                .concat(hexchar())
                .concat(hexchar())
                .concat(hexchar())
                .concat(hexchar())
                .map(|((((_, x0), x1), x2), x3)| (x0 << 12) | (x1 << 8) | (x2 << 4) | x3)
            };
            let u16escape_2 = u16escape();
            let unicode_escape = u16escape_2
                .assert(|&x| !(0xDC00 <= x && x < 0xE000))
                .and_then(move |x| {
                    if 0xD800 <= x && x < 0xDC00 {
                        Either::Left(
                            char_if(|x| x == '\\')
                                .skip_left(u16escape())
                                .assert(|&y| 0xDC00 <= y && y < 0xE000)
                                .map(move |y| (x - 0xD800) * 0x400 + (y - 0xDC00) + 0x10000)
                        )
                    } else {
                        Either::Right(combinators::empty().map(move |_| x))
                    }
                });
            let escaped =
                char_if(|x| x == '"' || x == '\\' || x == '/')
                .or(char_if(|x| x == 'b').map(|_| '\x08'))
                .or(char_if(|x| x == 'f').map(|_| '\x0C'))
                .or(char_if(|x| x == 'n').map(|_| '\n'))
                .or(char_if(|x| x == 'r').map(|_| '\r'))
                .or(char_if(|x| x == 't').map(|_| '\t'))
                .or(unicode_escape.map(|x| unsafe { ::std::char::from_u32_unchecked(x) }));
            let escaped = char_if(|x| x == '\\').skip_left(escaped);
            let unescaped = char_if(|x| x >= ' ' && x != '"');
            char_if(|x| x == '"')
                .skip_left(escaped.or(unescaped).many().collect::<String>())
                .skip_right(char_if(|x| x == '"'))
        }
    }

    #[test]
    fn test_json_string() {
        let p = json_string();
        assert_eq!(p.parse(&mut SliceStream::new(b"\"hoge\"")).unwrap(), "hoge");
        assert_eq!(
            p.parse(&mut SliceStream::new("\"ho𐐷ge\"".as_bytes()))
                .unwrap(),
            "ho𐐷ge"
        );
        assert_eq!(
            p.parse(&mut SliceStream::new(b"\"ho\\uD801\\uDC37ge\""))
                .unwrap(),
            "ho𐐷ge"
        );
        assert_eq!(
            p.parse(&mut SliceStream::new(b"\"ho\\rge\"")).unwrap(),
            "ho\rge"
        );
        assert_eq!(
            p.parse(&mut SliceStream::new(b"\"ho\\u000Dge\"")).unwrap(),
            "ho\rge"
        );
        assert_eq!(
            p.parse(&mut SliceStream::new(b"\"ho\\rge\" ")).unwrap(),
            "ho\rge"
        );
        assert_eq!(p.parse(&mut SliceStream::new(b"\"\"")).unwrap(), "");
        assert!(p.parse(&mut SliceStream::new(b"\"ho\\rge")).is_err());
        assert!(p.parse(&mut SliceStream::new(b" \"hoge\"")).is_err());
    }
}

parser! {
    pub fn ident() -> Ident<Input = u8, Output = String> {
        byte::alpha().many1().collect::<String>()
    }
}

#[test]
fn test_ident() {
    let p = ident();
    assert_eq!(p.parse(&mut SliceStream::new(b"hoge")).unwrap(), "hoge");
    assert_eq!(
        p.parse(&mut SliceStream::new(b"hoge fuga")).unwrap(),
        "hoge"
    );
    assert!(p.parse(&mut SliceStream::new(b" hoge")).is_err());
    assert!(p.parse(&mut SliceStream::new(b"")).is_err());
}
