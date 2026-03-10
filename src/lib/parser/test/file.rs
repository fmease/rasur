use super::{parse_file, t};
use crate::{
    ast,
    edition::Edition::*,
    error::{Error, InvalidScalarPlace},
    span::Spanned,
};
use deref as r;

#[test]
fn empty() {
    t!(
        parse_file,
        Rust2015,
        "",
        Ok(ast::File { shebang: None, frontmatter: None, attrs: r!([]), items: r!([]), span: _ })
    );
}

// FIXME: Test shebang!

// We only permit ASCII spaces and tabs in (the padding of) frontmatter infostrings & trailers.
// However, due to CRLF→LF normalization, we automatically also permit CR before the line break.
#[test]
fn frontmatter_crlf() {
    // See also <https://github.com/fmease/rasur/issues/15>.

    t!(
        parse_file,
        Rust2015,
        "---\t\r\n---\t\r\n",
        Ok(ast::File {
            shebang: None,
            frontmatter: Some(ast::Frontmatter {
                infostring: Spanned { bare: "", .. },
                content: Spanned { bare: "", .. },
                ..
            }),
            ..
        })
    );
}

#[test]
fn frontmatter_cr() {
    // CR isn't "horizontal whitespace" and therefore forbidden inside infostrings.
    t!(parse_file, Rust2015, "--- \r \n---", Err(r!([Error::InvalidFrontmatterInfostring(_)])),);

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
