use super::{
    Cutter, is_horizontal_whitespace, is_ident_middle, is_ident_start, is_whitespace, lex,
};
use crate::{
    Edition,
    error::{Buffer as ErrorBuffer, Error, InvalidScalarPlace},
    span::{ByteIndex, Span},
    token::TokenKind,
};
use std::borrow::Cow;

pub fn normalize(source: &str) -> Cow<'_, str> {
    const BOM: char = '\u{FEFF}';
    let source = source.strip_prefix(BOM).unwrap_or(source);
    if source.contains('\r') { source.replace("\r\n", "\n").into() } else { source.into() }
}

pub fn strip_shebang(source: &str, offset: &mut ByteIndex, edition: Edition) -> Option<Span> {
    let suffix = source.strip_prefix("#!")?;

    let errors = ErrorBuffer::sealed();
    for token in lex(suffix, *offset, edition, &errors) {
        match token.kind {
            TokenKind::Trivia => {}
            TokenKind::OpenSquareBracket => return None,
            _ => break,
        }
    }

    let mut cutter = Cutter::new(source, *offset);
    let start = cutter.index();
    cutter.advance_while(|char| char != '\n');

    *offset = cutter.index();
    Some(cutter.span(start))
}

pub fn strip_frontmatter(
    source: &str,
    offset: &mut ByteIndex,
    errors: &ErrorBuffer,
) -> Option<Frontmatter> {
    let mut cutter = Cutter::new(source, *offset);

    let mut start = *offset;
    let mut line_start = true;
    let mut leading_dashes = 0;

    while let Some((index, char)) = cutter.advance() {
        if char == '-' && line_start {
            leading_dashes += 1;
            start = index;
            break;
        }
        if !is_whitespace(char) {
            return None;
        }
        line_start = char == '\n';
    }

    while let Some('-') = cutter.peek() {
        leading_dashes += 1;
        cutter.advance();
    }

    if leading_dashes < 3 {
        return None;
    }

    if leading_dashes > 255 {
        errors.add(Error::FrontmatterOpeningTooLarge(cutter.span(start)));
    }

    let infostring = {
        cutter.advance_while(is_horizontal_whitespace);
        let start = cutter.index();

        if cutter.peek().is_some_and(is_ident_start) {
            cutter.advance();
            cutter.advance_while(|char| char == '-' || char == '.' || is_ident_middle(char));
        }

        cutter.advance_while(is_horizontal_whitespace);

        let valid = cutter.peek().is_none_or(|char| char == '\n');
        let mut end = cutter.index();

        while let Some((_, char)) = cutter.advance() {
            if char == '\n' {
                break;
            }
            if !is_horizontal_whitespace(char) {
                end = cutter.index();
            }
        }

        let span = Span::new(start, end);

        if !valid {
            errors.add(Error::InvalidFrontmatterInfostring(span));
        }

        span
    };

    let mut content = Span::from(cutter.index());
    let mut line_start = true;
    let mut trailing_dashes = 0;
    let mut terminated = false;

    while let Some((index, char)) = cutter.advance() {
        if char == '-' && (line_start || trailing_dashes > 0) {
            trailing_dashes += 1;
            if trailing_dashes == leading_dashes {
                terminated = true;
                break;
            }
        }

        if char == '\r' {
            errors.add(Error::InvalidScalar(
                char,
                InvalidScalarPlace::FrontmatterBody,
                cutter.span(index),
            ));
        }

        line_start = char == '\n';
        if line_start {
            content.end = cutter.index();
            trailing_dashes = 0;
        }
    }

    let span = cutter.span(start);

    // The trailer.
    {
        cutter.advance_while(is_horizontal_whitespace);
        let start = cutter.index();

        let valid = cutter.peek().is_none_or(|char| char == '\n');
        let mut end = cutter.index();

        while let Some(char) = cutter.peek() {
            if char == '\n' {
                break;
            }
            cutter.advance();
            if !is_horizontal_whitespace(char) {
                end = cutter.index();
            }
        }

        if !valid {
            // FIXME: Emit a custom message if trailing_dashes > leading_dashes.
            errors.add(Error::InvalidFrontmatterTrailer(Span::new(start, end)));
        }
    }

    if !terminated {
        errors.add(Error::UnterminatedFrontmatter(span));
    }

    *offset = cutter.index();
    Some(Frontmatter { infostring, content, span })
}

#[derive(Clone, Copy)]
pub struct Frontmatter {
    pub infostring: Span,
    pub content: Span,
    pub span: Span,
}
