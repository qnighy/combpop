use {ParseError, ParseResult};

pub trait Stream {
    type Item;
    fn next(&mut self) -> ParseResult<Self::Item>;
    fn mark(&mut self) -> u64;
    fn rollback(&mut self, pos: u64);
    fn commit(&mut self);
}

pub fn stream_transaction<R, S: Stream + ?Sized, F: FnOnce(&mut S) -> ParseResult<R>>(
    stream: &mut S,
    f: F,
) -> ParseResult<Option<R>> {
    let pos = stream.mark();
    match f(stream) {
        Ok(x) => {
            stream.commit();
            Ok(Some(x))
        }
        Err(e) => if e.is_recoverable() {
            stream.rollback(pos);
            Ok(None)
        } else {
            stream.commit();
            Err(e)
        },
    }
}

pub struct SliceStream<'a, T: 'a> {
    slice: &'a [T],
    position: usize,
    ready: bool,
}

impl<'a, T: 'a> SliceStream<'a, T> {
    pub fn new(slice: &'a [T]) -> Self {
        SliceStream {
            slice,
            position: 0,
            ready: true,
        }
    }
}

impl<'a, T: Copy + 'a> Stream for SliceStream<'a, T> {
    type Item = T;
    fn next(&mut self) -> ParseResult<Self::Item> {
        if self.position < self.slice.len() {
            let val = self.slice[self.position];
            self.position += 1;
            Ok(val)
        } else if self.ready {
            Err(ParseError::EOF)
        } else {
            Err(ParseError::NotReady)
        }
    }
    fn mark(&mut self) -> u64 {
        self.position as u64
    }
    fn rollback(&mut self, pos: u64) {
        self.position = pos as usize;
    }
    fn commit(&mut self) {}
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_slice_stream() {
        let mut s = SliceStream::new(b"Hello");
        assert_eq!(s.next().unwrap(), b'H');
        assert_eq!(s.mark(), 1);
        assert_eq!(s.next().unwrap(), b'e');
        assert_eq!(s.next().unwrap(), b'l');
        assert_eq!(s.mark(), 3);
        assert_eq!(s.next().unwrap(), b'l');
        s.commit();
        s.rollback(1);
        assert_eq!(s.next().unwrap(), b'e');
        assert_eq!(s.next().unwrap(), b'l');
        assert_eq!(s.next().unwrap(), b'l');
        assert_eq!(s.next().unwrap(), b'o');
        assert!(s.next().is_err());
    }
}
