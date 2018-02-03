combpop: a type-based parser combinator for Rust

Aim

- Monadic parser with consumed/empty and ok/error modes, as in Parsec
- Support both on-memory streams and on-demand streams
- Consistent semantics

Current Status

- Still in a design phase
- There are example `Stream` and `Parser` implementations, but the interfaces are still unstable.
