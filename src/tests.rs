use super::*;
use stream::SliceStream;

pub mod json {
    use ::*;
    use stream::SliceStream;
    parser! {
        pub fn json_string() -> JsonString<Input = u8, Output = String> {
            // let hexchar = || {
            //     byte::char(|x| '0' <= x && x <= '9').map(|x| x as u32 - 0x30)
            //         .or(byte::char(|x| 'A' <= x && x <= 'F').map(|x| x as u32 - 0x41 + 10))
            //         .or(byte::char(|x| 'a' <= x && x <= 'f').map(|x| x as u32 - 0x61 + 10))
            // };
            // let u16escape = || {
            //     byte::char(|x| x == 'u')
            //     .concat(hexchar())
            //     .concat(hexchar())
            //     .concat(hexchar())
            //     .concat(hexchar())
            //     .map(|((((_, x0), x1), x2), x3)| (x0 << 12) | (x1 << 8) | (x2 << 4) | x3)
            // };
            // let unicode_escape = || {
            //     u16escape().and_then(|x| {
            //
            //     })
            // };
            // TODO: \uXXXX
            let escaped =
                byte::char(|x| x == '"' || x == '\\' || x == '/')
                .or(byte::char(|x| x == 'b').map(|_| '\x08'))
                .or(byte::char(|x| x == 'f').map(|_| '\x0C'))
                .or(byte::char(|x| x == 'n').map(|_| '\n'))
                .or(byte::char(|x| x == 'r').map(|_| '\r'))
                .or(byte::char(|x| x == 't').map(|_| '\t'));
            let escaped = byte::char(|x| x == '\\').concat(escaped).map(|(_, x)| x);
            byte::char(|x| x == '"')
                .concat(escaped.or(byte::char(|x| x != '"')).many().collect::<String>())
                .concat(byte::char(|x| x == '"'))
                .map(|((_, x), _)| x)
        }
    }

    #[test]
    fn test_json_string() {
        let p = json_string();
        assert_eq!(p.parse(&mut SliceStream::new(b"\"hoge\"")).unwrap(), "hoge");
        assert_eq!(
            p.parse(&mut SliceStream::new(b"\"ho\rge\"")).unwrap(),
            "ho\rge"
        );
        assert_eq!(
            p.parse(&mut SliceStream::new(b"\"ho\\rge\"")).unwrap(),
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
