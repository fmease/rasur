<h1 align="center">——— rasur ———</h1>

<p align="center">A hobby parser for Rust source code</p>

### Stability

This project is under heavy development and it is not recommended for use outside of experimental non-production software.
It has no stability guarantees whatsoever; anything may change without notice.

### Description

Most prominently, this project features a Rust source code lexer+parser that's written from scratch and exposed via a library and a CLI.
Barring certain exceptions, it fully conforms with the parser of `rustc@main`; it understands editions and every unstable and internal syntactic construct.

It's a personal study and a means to find bugs in the Rust compiler `rustc` and in the Rust Reference.
It has already found bugs, most of which have been fixed. E.g., [rust#152499], [rust#152820], [rust#155073] and [rust#155698].

> [!IMPORTANT]
> It is considered a bug if `rasur@master` doesn't conform with a new `rustc@main` within 18h after its release excluding known preexisting incongruities. In general, mismatches between rasur and rustc are only allowed if the root issue is considered to be an "upstream" bug (i.e., a bug in rustc or in the Rust language) that's "niche" or has upstream fixes that are known to be merged "soon".

Regarding conformance, `rasur@6405756` (2026-04-30) and `rustc@c935696dd -Zparse-crate-root-only` (nightly 2026-04-29) have the same exit status for 25,407 out of 25,412 files (99.98%) in directory `tests/` in repository `rust-lang/rust@f53b654a888` (2026-04-30) for all editions (2015–future).
4 of these 5 mismatches are "spurious" since rustc doesn't perform certain Unicode checks under that zee flag; `rustc -Zcrate-attr='cfg(false)'` on the other hand agrees with `rasur` / `rasur --gatekeep` here.
The single non-spurious non-conformance (`tests/ui/parser/const-block-items/pub.rs`) is intentionally ignored for now since
it's considered to be an upstream bug. Of course, comparing exit statuses isn't the best measure, comparing ASTs would be better. That is currently only done manually.

[rust#152499]: https://github.com/rust-lang/rust/issues/152499
[rust#152820]: https://github.com/rust-lang/rust/issues/152820
[rust#155073]: https://github.com/rust-lang/rust/issues/155073
[rust#155698]: https://github.com/rust-lang/rust/pull/155698

### Non-Binding Plans

* perform Unicode NFC-normalization for identifiers
* experiment with further improving certain parsing "primitives" by rendering them more "principled"; this concerns areas like "prefix matching" (esp. "qualifiers"), token jointness, lookahead & backtracking
* experiment with better error recovery, CSTs and incremental parsing

### License

Except as otherwise noted, the contents of this repository are licensed under the [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0).
Files may include or may be accompanied by explicit license notices.
