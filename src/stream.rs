use {ParseError, ParseResult};

/// `Stream` is a stream of tokens with lookahead/rollback ability.
///
/// # Semantics
///
/// A **stream** is thought to be a set of the following data:
///
/// - The sequence of **tokens** `S`
/// - The monotone sequence of **marks** `M`
/// - The **current position** `i`, which is not less than any marks
/// - The **lookahead position** `j`, which is not less than the current position
///
/// The actual streams have to remember **only the part** of the tokens, due to the contract
/// described in the `Stream::get` method below.
pub trait Stream {
    /// The type of the token. Note that `Stream::get` actually returns a *reference* to `I`.
    type Item;
    /// Tries to push the lookahead position forward.
    ///
    /// - If `i + len <= j`, then it does nothing.
    /// - If `i + len > j` and it hit an I/O error, then it returns `Err`.
    /// - If `i + len > j` and it hit an EOF, then it returns `Err(ParseError::EOF)`.
    /// - Otherwise, it updates `j` to `i + len` and returns `Ok`.
    fn lookahead(&mut self, len: usize) -> ParseResult<()>;
    /// Retrieves the looked-ahead token.
    ///
    /// The caller must ensure that `i + idx < j`. Otherwise it may panic.
    fn get(&self, idx: usize) -> &Self::Item;
    /// Advances the current position.
    ///
    /// The caller must ensure that `i + idx < j`. Otherwise it may panic.
    fn advance(&mut self, len: usize);
    /// Pushes the current position to the mark stack `M`.
    fn mark(&mut self) -> StreamMark;
    /// Pops `i` from the mark stack `M`.
    ///
    /// `pos` argument must match to the corresponding `Stream::mark` call.
    fn rollback(&mut self, pos: StreamMark);
    /// Pops a value from the mark stack `M` and discard it.
    ///
    /// The mark stack `M` must not be empty.
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
