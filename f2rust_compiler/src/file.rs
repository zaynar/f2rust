//! Parse a FORTRAN source file into a sequence of `grammar::Statement`.
//! This handles fixed-form and (sort of) free-form files.

use std::{
    fs::File,
    io::{BufRead, BufReader},
    path::Path,
};

use crate::grammar;
use crate::grammar::Statement;
use anyhow::{Result, bail};
use log::warn;
use relative_path::RelativePath;

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

    let mut quoted = None;
    for c in s.chars() {
        if quoted.is_some() {
            r.push(c);
        } else if c != ' ' {
            r.push(c.to_ascii_uppercase());
        }
        if quoted == Some(c) {
            quoted = None;
        } else if quoted.is_none() && (c == '\'' || c == '"') {
            quoted = Some(c);
        }
    }

    r
}

// Remove Fortran 90 style comments, i.e. anything after `!`
fn remove_f90_comments(s: &str) -> String {
    let mut r = String::new();

    let mut quoted = None;
    for c in s.chars() {
        if quoted.is_some() {
            r.push(c);
        } else if c == '!' {
            return r;
        } else {
            r.push(c.to_ascii_uppercase());
        }
        if quoted == Some(c) {
            quoted = None;
        } else if quoted.is_none() && (c == '\'' || c == '"') {
            quoted = Some(c);
        }
    }

    r
}

// Do the whitespace-sensitive parsing (labels, comments, etc)
pub fn parse_fixed(
    path: &RelativePath,
    root: &Path,
) -> Result<Vec<(SourceLoc, grammar::Statement)>> {
    let file = BufReader::new(File::open(path.to_path(root))?);

    let mut lines = Vec::new();
    for (line_no, line) in file.lines().enumerate() {
        let line = line?;
        let loc = SourceLoc {
            file: path.to_string(),
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
                assert_ne!(n, 0, "label must be non-zero");
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

    parse_lines(path, root, lines, parse_fixed)
}

// A very crude approximation of Fortran 90 free form; just a quick hack for tests.
// TODO: implement this properly
pub fn parse_free(
    path: &RelativePath,
    root: &Path,
) -> Result<Vec<(SourceLoc, grammar::Statement)>> {
    let file = BufReader::new(File::open(path.to_path(root))?);

    let mut lines = Vec::new();
    for (line_no, line) in file.lines().enumerate() {
        let line = line?;
        let loc = SourceLoc {
            file: path.to_string(),
            line: line_no + 1,
        };

        let line = remove_f90_comments(&line);

        if line.chars().all(|c| c == ' ') {
            lines.push(Line::Blank(loc));
        } else {
            lines.push(Line::Statement(loc, 0, line.to_owned()));
        }
    }

    parse_lines(path, root, lines, parse_free)
}

// After the fixed/free form processing, parse each line.
// Also handle INCLUDE, by recursively parsing the included file.
// (Please don't have any cyclic INCLUDEs.)
fn parse_lines<F>(
    path: &RelativePath,
    root: &Path,
    lines: Vec<Line>,
    parse_fxx: F,
) -> Result<Vec<(SourceLoc, grammar::Statement)>>
where
    F: Fn(&RelativePath, &Path) -> Result<Vec<(SourceLoc, grammar::Statement)>>,
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
                                    parse_fxx(&path.parent().unwrap().join(inc_file), root)?,
                                ),
                            ))
                        } else {
                            Ok((loc, p))
                        }
                    }
                    Err(e) => {
                        bail!("{loc} parse error: \"{s}\": {e}");
                    }
                }
            }
        })
        .collect::<Result<Vec<_>>>()
}

/// A source file may contain 1+ program units (PROGRAM/SUBROUTINE/FUNCTION).
/// Split them up, and try to keep comments associated with the correct program unit.
pub fn split_program_units(
    source: Vec<(SourceLoc, grammar::Statement)>,
) -> Vec<Vec<(SourceLoc, grammar::Statement)>> {
    let mut progs = Vec::new();
    let mut prog = Vec::new();
    let mut comments = Vec::new();

    for (loc, line) in source.into_iter() {
        match line {
            Statement::Comment(..) | Statement::Blank => {
                // Collect comments, so we can decide later whether to attach them to
                // the preceding or following program unit
                comments.push((loc, line));
            }
            Statement::End => {
                prog.append(&mut comments);
                prog.push((loc, line));

                // End the current program unit, prepare for a new one
                progs.push(prog.clone());
                prog.clear();
            }
            _ => {
                // Move any preceding comments into the current program unit
                prog.append(&mut comments);
                prog.push((loc, line));
            }
        }
    }

    // In case a buggy file didn't end with END, or was exclusively comments,
    // make sure we don't lose its data
    if progs.is_empty() || !prog.is_empty() {
        progs.push(prog);
    }

    // Deal with any trailing comments after the final program unit
    progs.last_mut().unwrap().append(&mut comments);

    progs
}
