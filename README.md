<h1 align="center">——— rasur ———</h1>

<p align="center">A hobby parser for Rust source code</p>

## Stability

This project is under heavy development and it is not recommended for use outside of experimental non-production software.
It has no stability guarantees whatsoever, anything may change without notice.

## Description

This is a personal study.
I aim to achieve and continuously maintain near 100% parity with `rustc`'s parser (incl. unstable and internal syntax).
My main goal is to write a more principled parser and find bugs in `rustc` and/or the Rust Reference this way.

I might use this in various future projects (like a minimizer, an auto-formatter, a static syntax
highlighter for the web emitting HTML+CSS) and I might experiment with better error recovery, CSTs,
incremental parsing, etc. in the future.

## License

Except as otherwise noted, the contents of this repository are licensed under the [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0).
Files may include or may be accompanied by explicit license notices.
