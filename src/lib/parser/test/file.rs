use super::{Frontmatter, FullFile, parse_file, parse_file_full, t};
use crate::{
    ast,
    edition::Edition::*,
    error::{Error, InvalidScalarPlace},
};
use deref as r;

#[test]
fn empty() {
    t!(parse_file, Rust2015, "", Ok(ast::File { attrs: r!([]), items: r!([]) }));
}

// FIXME: Test shebang!

// We only permit ASCII spaces and tabs in (the padding of) frontmatter infostrings & trailers.
// However, due to CRLF→LF normalization, we automatically also permit CR before the line break.
#[test]
fn frontmatter_crlf() {
    // See also <https://github.com/fmease/rasur/issues/15>.

    t!(
        parse_file_full,
        Rust2015,
        "---\t\r\n---\t\r\n",
        Ok(FullFile {
            shebang: None,
            frontmatter: Some(Frontmatter { infostring: "", content: "" }),
            ..
        })
    );
}

#[test]
fn frontmatter_cr() {
    // CR isn't "horizontal whitespace" and therefore forbidden inside infostrings.
    t!(parse_file, Rust2015, "--- \r \n---", Err(r!([Error::InvalidFrontmatterInfostring(_)])));

    // CR isn't "horizontal whitespace" and therefore forbidden inside trailers.
    t!(parse_file, Rust2015, "---\n--- \r ", Err(r!([Error::InvalidFrontmatterTrailer(_)])));

    // "Stray" CRs inside the frontmatter body are explicitly forbidden.
    t!(
        parse_file,
        Rust2015,
        "---\n(\r)\n---",
        Err(r!([Error::InvalidScalar('\r', InvalidScalarPlace::FrontmatterBody, _)]))
    );
}
