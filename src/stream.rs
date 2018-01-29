use ParseResult;

pub trait Stream {
    type Item;
    fn next(&mut self) -> ParseResult<Self::Item>;
    fn mark(&mut self) -> u64;
    fn rollback(&mut self, pos: u64);
    fn commit(&mut self);
}
