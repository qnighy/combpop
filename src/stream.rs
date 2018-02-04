use {ParseError, ParseResult};

pub trait Stream {
    type Item;
    fn lookahead(&mut self, len: usize) -> ParseResult<()>;
    fn get(&self, idx: usize) -> &Self::Item;
    fn advance(&mut self, len: usize);
    fn mark(&mut self) -> StreamMark;
    fn rollback(&mut self, pos: StreamMark);
    fn commit(&mut self);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StreamMark {
    pub position: u64,
    pub depth: usize,
}

pub fn stream_transaction<R, S: Stream + ?Sized, F: FnOnce(&mut S) -> ParseResult<R>>(
    stream: &mut S,
    f: F,
) -> ParseResult<Option<R>> {
    let mark = stream.mark();
    match f(stream) {
        Ok(x) => {
            stream.commit();
            Ok(Some(x))
        }
        Err(e) => if e.is_recoverable() {
            stream.rollback(mark);
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
    fn lookahead(&mut self, len: usize) -> ParseResult<()> {
        if self.position + len <= self.slice.len() {
            Ok(())
        } else if self.ready {
            Err(ParseError::EOF)
        } else {
            Err(ParseError::NotReady)
        }
    }
    fn get(&self, idx: usize) -> &Self::Item {
        &self.slice[self.position + idx]
    }
    fn advance(&mut self, len: usize) {
        assert!(self.position + len <= self.slice.len());
        self.position += len;
    }
    fn mark(&mut self) -> StreamMark {
        StreamMark {
            position: self.position as u64,
            depth: 0,
        }
    }
    fn rollback(&mut self, mark: StreamMark) {
        self.position = mark.position as usize;
    }
    fn commit(&mut self) {}
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_slice_stream() {
        let mut s = SliceStream::new(b"Hello"); // [0] -- 0
        s.lookahead(2).unwrap(); // [0] -- 2
        assert_eq!(s.get(1), &b'e');
        assert_eq!(s.get(0), &b'H');
        s.advance(1); // [1] -- 2
        let mark0 = s.mark();
        assert_eq!(mark0.position, 1); // [1, 1] -- 2
        s.lookahead(1).unwrap(); // [1, 1] -- 2
        assert_eq!(s.get(0), &b'e');
        s.lookahead(2).unwrap(); // [1, 1] -- 3
        assert_eq!(s.get(1), &b'l');
        s.advance(2); // [1, 3] -- 3
        let mark1 = s.mark();
        assert_eq!(mark1.position, 3); // [1, 3, 3] -- 3
        s.lookahead(1).unwrap(); // [1, 3, 3] -- 4
        assert_eq!(s.get(0), &b'l');
        s.commit(); // [1, 3] -- 4
        s.rollback(mark0); // [1] -- 4
        s.lookahead(4).unwrap(); // [1] -- 5
        assert_eq!(s.get(0), &b'e');
        assert_eq!(s.get(1), &b'l');
        assert_eq!(s.get(2), &b'l');
        s.advance(3); // [4] -- 5
        assert_eq!(s.get(0), &b'o');
        assert!(s.lookahead(2).is_err());
        s.lookahead(1).unwrap(); // [4] -- 5
        s.advance(1); // [5] -- 5
        assert!(s.lookahead(1).is_err());
    }
}
