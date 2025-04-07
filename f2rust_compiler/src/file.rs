//! Parse a FORTRAN source file into a sequence of `grammar::Statement`.
//! This handles fixed-form and (sort of) free-form files.

use std::{
    fs::File,
    io::{BufRead, BufReader},
    path::Path,
};

use anyhow::{Result, bail};
use log::warn;

use crate::grammar;

#[derive(Clone, PartialEq, Eq)]
pub struct SourceLoc {
    pub file: String,
    pub line: usize,
}

impl std::fmt::Display for SourceLoc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.file, self.line)
    }
}

impl std::fmt::Debug for SourceLoc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

#[allow(dead_code)] // statement labels are not used
#[derive(Debug)]
enum Line {
    Blank(SourceLoc),
    Comment(SourceLoc, String),
    Statement(SourceLoc, u32, String),
}

// Remove all blanks, except ones inside character constants.
// Also convert to uppercase, because a few source files have some sneaky lowercase.
// (We don't support format specifiers, which would need to preserve blanks too)
pub fn remove_blanks(s: &str) -> String {
    let mut r = String::new();

    let mut quoted = false;
    for c in s.chars() {
        if quoted {
            r.push(c);
        } else if c != ' ' {
            r.push(c.to_ascii_uppercase());
        }
        if c == '\'' {
            quoted = !quoted;
        }
    }

    r
}

// Remove Fortran 90 style comments, i.e. anything after `!`
fn remove_f90_comments(s: &str) -> String {
    let mut r = String::new();

    let mut quoted = false;
    for c in s.chars() {
        if quoted {
            r.push(c);
        } else if c == '!' {
            return r;
        } else {
            r.push(c.to_ascii_uppercase());
        }
        if c == '\'' {
            quoted = !quoted;
        }
    }

    r
}

// Do the whitespace-sensitive parsing (labels, comments, etc)
pub fn parse_fixed(path: &Path) -> Result<Vec<(SourceLoc, grammar::Statement)>> {
    let file = BufReader::new(File::open(path)?);

    let parent = path
        .parent()
        .and_then(|p| p.file_name())
        .and_then(|f| f.to_str());

    let filename = format!(
        "{}/{}",
        parent.unwrap_or("."),
        path.file_name().unwrap().to_string_lossy()
    );

    let mut lines = Vec::new();
    for (line_no, line) in file.lines().enumerate() {
        let line = line?;
        let loc = SourceLoc {
            file: filename.clone(),
            line: line_no + 1,
        };

        // Required by support/convbt.f, since it has a too-long comment line
        let line = line.trim_ascii_end();

        if line.len() > 72 {
            warn!("{loc}: line exceeds 72 characters");
        }

        if line.chars().all(|c| c == ' ') {
            lines.push(Line::Blank(loc));
        } else if let Some(comment) = line.strip_prefix(|c| matches!(c, 'C' | 'c' | '*')) {
            lines.push(Line::Comment(loc, comment.to_owned()));
        } else if matches!(line.chars().nth(5), Some(' ') | Some('0')) {
            let label_str: String = line.chars().take(5).collect();
            let label = if label_str == "     " {
                0
            } else if let Ok(n) = label_str.parse::<u32>() {
                assert!(n != 0, "label must be non-zero");
                n
            } else {
                bail!("label must contain digits: {} - \"{}\"", loc, label_str);
            };
            lines.push(Line::Statement(loc, label, line.to_owned()));
        } else if let Some(rest) = line.strip_prefix("     ") {
            assert!(
                !matches!(rest.chars().next(), Some(' ') | Some('0')),
                "continuation line must not have 0/blank in column 6"
            );
            'FIND_INITIAL: {
                for prev in lines.iter_mut().rev() {
                    if let Line::Statement(_, _, l) = prev {
                        l.push(' ');
                        l.push_str(rest[1..].trim_ascii());
                        break 'FIND_INITIAL;
                    }
                }
                bail!("continuation line must follow an initial line: {}", loc);
            }
        } else {
            bail!("unrecognized line: {} - {}", loc, line);
        }
    }

    parse_lines(path, lines, parse_fixed)
}

// A very crude approximation of Fortran 90 free form; just a quick hack for tests.
// TODO: implement this properly
pub fn parse_free(path: &Path) -> Result<Vec<(SourceLoc, grammar::Statement)>> {
    let file = BufReader::new(File::open(path)?);

    let parent = path
        .parent()
        .and_then(|p| p.file_name())
        .and_then(|f| f.to_str());

    let filename = format!(
        "{}/{}",
        parent.unwrap_or("."),
        path.file_name().unwrap().to_string_lossy()
    );

    let mut lines = Vec::new();
    for (line_no, line) in file.lines().enumerate() {
        let line = line?;
        let loc = SourceLoc {
            file: filename.clone(),
            line: line_no + 1,
        };

        let line = remove_f90_comments(&line);

        if line.chars().all(|c| c == ' ') {
            lines.push(Line::Blank(loc));
        } else {
            lines.push(Line::Statement(loc, 0, line.to_owned()));
        }
    }

    parse_lines(path, lines, parse_free)
}

// After the fixed/free form processing, parse each line.
// Also handle INCLUDE, by recursively parsing the included file.
// (Please don't have any cyclic INCLUDEs.)
fn parse_lines<F>(
    path: &Path,
    lines: Vec<Line>,
    parse_fxx: F,
) -> Result<Vec<(SourceLoc, grammar::Statement)>>
where
    F: Fn(&Path) -> Result<Vec<(SourceLoc, grammar::Statement)>>,
{
    lines
        .into_iter()
        .map(|l| match l {
            Line::Blank(loc) => Ok((loc, grammar::Statement::Blank)),
            Line::Comment(loc, c) => Ok((loc, grammar::Statement::Comment(c))),
            Line::Statement(loc, _, s) => {
                let s = remove_blanks(&s);
                match grammar::fortran_parser::statement(&s) {
                    Ok(p) => {
                        if let grammar::Statement::Include(inc_file, _) = p {
                            Ok((
                                loc,
                                grammar::Statement::Include(
                                    inc_file.clone(),
                                    parse_fxx(&path.parent().unwrap().join(inc_file))?,
                                ),
                            ))
                        } else {
                            Ok((loc, p))
                        }
                    }
                    Err(e) => {
                        bail!("{loc}: parse error: \"{s}\": {e}");
                    }
                }
            }
        })
        .collect::<Result<Vec<_>>>()
}
