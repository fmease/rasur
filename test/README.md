# Tests

## Unit Tests

They are located in `src/lib/parser/test.rs`, not here in `test/`.

* they are mostly for edge cases for now but that'll slowly change
* they consist of positive and negative behavior test cases
* they work with the AST nodes and error 'codes'

## Per-Feature Smoke Tests

The test cases are located in `test/smoke/feature/`.

* the directory is meant to be fed to `etc/check.rs`
* that script compares the exit status of `rasur` and `rustc` per file
* the default edition is Rust 2015; pass `-e, --edition <EDITION>` to override;
  `--edition all` or `-eall` makes the script test each file under all editions
* the test cases do contain a custom test directive / parameter / configuration
  language (prefixed by `//:`) for setting edition-aware test expectations;
  however, these annotations aren't interpreted yet by the script
* they mainly consist of positive behavior tests
* they allow us to detect the removal of unstable syntax;
  such removals often implies test removals in r-l/r, so we wouldn't be able to
  if that happened when running rasur against r-l/r's test suite
