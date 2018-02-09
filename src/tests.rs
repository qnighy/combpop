use super::*;
use stream::SliceStream;

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
