#![allow(clippy::collapsible_else_if)]

#[derive(thiserror::Error, Debug)]
pub enum FormatError {
    #[error("format descriptor did not start with '('")]
    DidNotStartWithLeftParen,
    #[error("unexpected end of format descriptor")]
    UnexpectedEOF,
    #[error("unexpected character")]
    UnexpectedChar,
    #[error("expected width")]
    ExpectedWidth,
    #[error("expected number after '.'")]
    ExpectedNumAfterDot,
    #[error("expected number after 'E'")]
    ExpectedNumAfterE,
    #[error("expected number after T/TL/TR")]
    ExpectedNumAfterT,
    #[error("expected '.' after width")]
    ExpectedDot,
    #[error("expected edit descriptor letter")]
    ExpectedLetter,
    #[error("expected repeatable edit descriptor")]
    ExpectedRep,
    #[error("expected ')' before end")]
    ExpectedRightParen,
}

type Result<T> = std::result::Result<T, FormatError>;

#[derive(Debug, Clone, PartialEq)]
pub enum Repeatable {
    I { w: u32, m: Option<u32> },
    F { w: u32, d: u32 },
    E { w: u32, d: u32, e: Option<u32> },
    D { w: u32, d: u32 },
    G { w: u32, d: u32, e: Option<u32> },
    L { w: u32 },
    A { w: Option<u32> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Nonrepeatable {
    Char { s: Vec<u8> },
    // H: not supported, deleted in later Fortran standards
    T { c: u32 },
    TL { c: u32 },
    TR { c: u32 },
    X { n: u32 },
    Slash,
    Colon,
    S,
    SP,
    SS,
    P { k: i32 },
    BN,
    BZ,
    Dollar, // non-standard? see e.g. https://gcc.gnu.org/onlinedocs/gcc-3.4.3/g77/I_002fO.html
}

#[derive(Debug, PartialEq)]
enum Descriptor {
    Repeatable(i32, Repeatable),
    Nonrepeatable(Nonrepeatable),
    ParenLeft(i32),
    ParenRight,
}

pub struct FormatParser<'a> {
    fmt: &'a [u8],

    pos: usize,
    sym: Option<u8>,
}

#[derive(Debug, PartialEq)]
pub struct ParsedFormatSpec {
    descriptors: Vec<Descriptor>,
    revert_pos: Option<usize>,
}

pub struct ParsedFormatSpecIter<'a> {
    spec: &'a ParsedFormatSpec,
    pos: i32,
    reps: u32,
    groups: Vec<(i32, u32)>, // (pos, reps)
}

impl ParsedFormatSpec {
    fn new(descriptors: Vec<Descriptor>, revert_pos: Option<usize>) -> Self {
        Self {
            descriptors,
            revert_pos,
        }
    }

    pub fn iter(&self) -> ParsedFormatSpecIter {
        ParsedFormatSpecIter {
            spec: self,
            pos: -1,
            reps: 0,
            groups: Vec::new(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum EditDescriptor {
    Repeatable(Repeatable),
    Nonrepeatable(Nonrepeatable),
    EndOfRecord,
}

impl Iterator for ParsedFormatSpecIter<'_> {
    type Item = EditDescriptor;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // If we're currently repeating an item, keep returning it
            if self.reps > 0 {
                let item = match &self.spec.descriptors[self.pos as usize] {
                    Descriptor::Repeatable(_, d) => EditDescriptor::Repeatable(d.clone()),
                    Descriptor::Nonrepeatable(d) => EditDescriptor::Nonrepeatable(d.clone()),
                    Descriptor::ParenLeft(..) => panic!("invalid descriptor"),
                    Descriptor::ParenRight => panic!("invalid descriptor"),
                };
                self.reps -= 1;
                return Some(item);
            }

            // Move past the completed repeated item
            self.pos += 1;

            // If we reach the end of the format specification, revert to an earlier position
            if self.pos >= self.spec.descriptors.len() as i32 {
                match self.spec.revert_pos {
                    Some(n) => self.pos = n as i32 - 1,
                    None => self.pos = -1,
                }
                return Some(EditDescriptor::EndOfRecord);
            }

            // Handle the current descriptor, then loop around
            match &self.spec.descriptors[self.pos as usize] {
                Descriptor::Repeatable(r, _) => {
                    self.reps = *r as u32;
                }
                Descriptor::Nonrepeatable(_) => {
                    self.reps = 1;
                }
                Descriptor::ParenLeft(r) => {
                    // Remember the start of this group
                    self.groups.push((self.pos, *r as u32 - 1));
                }
                Descriptor::ParenRight => {
                    // Revert to the start of the current group, if we're repeating it
                    let (pos, reps) = self.groups.pop().unwrap();
                    if reps > 0 {
                        self.pos = pos;
                        self.groups.push((pos, reps - 1));
                    }
                }
            }
        }
    }
}

impl<'a> FormatParser<'a> {
    pub fn new(fmt: &'a [u8]) -> Self {
        Self {
            fmt,
            pos: 0,
            sym: None,
        }
    }

    fn next(&mut self) {
        loop {
            self.sym = self.fmt.get(self.pos).copied();
            self.pos += 1;

            // Skip blanks
            if !matches!(self.sym, Some(b' ')) {
                break;
            }
        }
    }

    fn signed(&mut self) -> Result<Option<i32>> {
        if let Some(s) = self.sym {
            match s as char {
                '+' | '-' | '0'..='9' => {
                    let mut num = String::new();
                    num.push(s as char);

                    loop {
                        self.next();
                        if let Some(s) = self.sym {
                            if !s.is_ascii_digit() {
                                return Ok(Some(num.parse().expect("parsing format i32")));
                            }
                            num.push(s as char);
                        } else {
                            return Err(FormatError::UnexpectedEOF);
                        }
                    }
                }
                _ => Ok(None),
            }
        } else {
            Err(FormatError::UnexpectedEOF)
        }
    }

    fn unsigned(&mut self) -> Result<Option<u32>> {
        if let Some(s) = self.sym {
            match s as char {
                '0'..='9' => {
                    let mut num = String::new();
                    num.push(s as char);

                    loop {
                        self.next();
                        if let Some(s) = self.sym {
                            if !s.is_ascii_digit() {
                                return Ok(Some(num.parse().expect("parsing format u32")));
                            }
                            num.push(s as char);
                        } else {
                            return Err(FormatError::UnexpectedEOF);
                        }
                    }
                }
                _ => Ok(None),
            }
        } else {
            Err(FormatError::UnexpectedEOF)
        }
    }

    fn letter(&mut self) -> Result<Option<char>> {
        if let Some(s) = self.sym {
            match s as char {
                'a'..='z' | 'A'..='Z' => {
                    self.next();
                    Ok(Some(s as char))
                }
                _ => Ok(None),
            }
        } else {
            Err(FormatError::UnexpectedEOF)
        }
    }

    fn i_vals(&mut self) -> Result<(u32, Option<u32>)> {
        if let Some(w) = self.unsigned()? {
            if self.sym == Some(b'.') {
                self.next();
                if let Some(m) = self.unsigned()? {
                    Ok((w, Some(m)))
                } else {
                    Err(FormatError::ExpectedNumAfterDot)
                }
            } else {
                Ok((w, None))
            }
        } else {
            Err(FormatError::ExpectedWidth)
        }
    }

    fn fd_vals(&mut self) -> Result<(u32, u32)> {
        if let Some(w) = self.unsigned()? {
            if self.sym == Some(b'.') {
                self.next();
                if let Some(m) = self.unsigned()? {
                    Ok((w, m))
                } else {
                    Err(FormatError::ExpectedNumAfterDot)
                }
            } else {
                Err(FormatError::ExpectedDot)
            }
        } else {
            Err(FormatError::ExpectedWidth)
        }
    }

    fn eg_vals(&mut self) -> Result<(u32, u32, Option<u32>)> {
        let (w, d) = self.fd_vals()?;
        if matches!(self.sym, Some(b'E') | Some(b'e')) {
            self.next();
            if let Some(m) = self.unsigned()? {
                Ok((w, d, Some(m)))
            } else {
                Err(FormatError::ExpectedNumAfterE)
            }
        } else {
            Ok((w, d, None))
        }
    }

    fn descriptor_rep(&mut self, r: i32, letter: char) -> Result<Option<Descriptor>> {
        Ok(Some(match letter.to_ascii_uppercase() {
            'I' => {
                let (w, m) = self.i_vals()?;
                Descriptor::Repeatable(r, Repeatable::I { w, m })
            }
            'F' => {
                let (w, d) = self.fd_vals()?;
                Descriptor::Repeatable(r, Repeatable::F { w, d })
            }
            'E' => {
                let (w, d, e) = self.eg_vals()?;
                Descriptor::Repeatable(r, Repeatable::E { w, d, e })
            }
            'D' => {
                let (w, d) = self.fd_vals()?;
                Descriptor::Repeatable(r, Repeatable::D { w, d })
            }
            'G' => {
                let (w, d, e) = self.eg_vals()?;
                Descriptor::Repeatable(r, Repeatable::G { w, d, e })
            }
            'L' => {
                if let Some(w) = self.unsigned()? {
                    Descriptor::Repeatable(r, Repeatable::L { w })
                } else {
                    return Err(FormatError::ExpectedWidth);
                }
            }
            'A' => {
                let w = self.unsigned()?;
                Descriptor::Repeatable(r, Repeatable::A { w })
            }
            'X' => {
                // Reinterpret 'r'
                // TODO: check >0 before cast
                Descriptor::Nonrepeatable(Nonrepeatable::X { n: r as u32 })
            }
            'P' => {
                // Reinterpret 'r'
                Descriptor::Nonrepeatable(Nonrepeatable::P { k: r })
            }
            _ => {
                return Ok(None);
            }
        }))
    }

    fn descriptor_nonrep(&mut self, letter: char) -> Result<Option<Descriptor>> {
        Ok(Some(match letter.to_ascii_uppercase() {
            'T' => {
                let lr = self.letter()?;
                if let Some(c) = self.unsigned()? {
                    match lr {
                        Some('L') => Descriptor::Nonrepeatable(Nonrepeatable::TL { c }),
                        Some('R') => Descriptor::Nonrepeatable(Nonrepeatable::TR { c }),
                        None => Descriptor::Nonrepeatable(Nonrepeatable::T { c }),
                        _ => return Err(FormatError::UnexpectedChar),
                    }
                } else {
                    return Err(FormatError::ExpectedNumAfterT);
                }
            }
            'S' => {
                let ps = self.letter()?;
                match ps {
                    Some('P') => Descriptor::Nonrepeatable(Nonrepeatable::SP),
                    Some('S') => Descriptor::Nonrepeatable(Nonrepeatable::SS),
                    None => Descriptor::Nonrepeatable(Nonrepeatable::S),
                    _ => return Err(FormatError::UnexpectedChar),
                }
            }
            'B' => {
                let ps = self.letter()?;
                match ps {
                    Some('N') => Descriptor::Nonrepeatable(Nonrepeatable::BN),
                    Some('Z') => Descriptor::Nonrepeatable(Nonrepeatable::BZ),
                    _ => return Err(FormatError::UnexpectedChar),
                }
            }
            _ => {
                return Ok(None);
            }
        }))
    }

    pub fn parse(mut self) -> Result<ParsedFormatSpec> {
        self.next();
        if self.sym != Some(b'(') {
            return Err(FormatError::DidNotStartWithLeftParen);
        }
        self.next();

        let mut ds = Vec::new();
        let mut depth = 0;

        let mut revert_pos = None;

        loop {
            match self.sym {
                None => {
                    return Err(FormatError::ExpectedRightParen);
                }
                Some(b')') => {
                    if depth == 0 {
                        return Ok(ParsedFormatSpec::new(ds, revert_pos));
                    } else {
                        self.next();
                        ds.push(Descriptor::ParenRight);
                        depth -= 1;
                    }
                }
                Some(b'/') => {
                    self.next();
                    ds.push(Descriptor::Nonrepeatable(Nonrepeatable::Slash));
                }
                Some(b':') => {
                    self.next();
                    ds.push(Descriptor::Nonrepeatable(Nonrepeatable::Colon));
                }
                Some(b'$') => {
                    self.next();
                    ds.push(Descriptor::Nonrepeatable(Nonrepeatable::Dollar));
                }
                Some(b',') => {
                    self.next();
                }
                Some(b'\'') => {
                    self.next();
                    let mut s = Vec::new();
                    let mut quoted = true;
                    loop {
                        if let Some(c) = self.sym {
                            if c == b'\'' {
                                if quoted {
                                    quoted = false;
                                } else {
                                    s.push(b'\'');
                                    quoted = true;
                                }
                                self.next();
                            } else if quoted {
                                s.push(c);
                                self.next();
                            } else {
                                break;
                            }
                        } else {
                            return Err(FormatError::UnexpectedEOF);
                        }
                    }
                    ds.push(Descriptor::Nonrepeatable(Nonrepeatable::Char { s }));
                }
                _ => {
                    let r = self.signed()?;

                    if self.sym == Some(b'(') {
                        // At the end of the format spec, the standard says we should loop back
                        // to the left paren corresponding to the last right paren. It's easiest
                        // to track that as the last left paren at the root depth
                        if depth == 0 {
                            revert_pos = Some(ds.len());
                        }

                        self.next();
                        ds.push(Descriptor::ParenLeft(r.unwrap_or(1)));
                        depth += 1;
                    } else if let Some(r) = r {
                        if let Some(letter) = self.letter()? {
                            if let Some(d) = self.descriptor_rep(r, letter)? {
                                ds.push(d)
                            } else {
                                return Err(FormatError::ExpectedRep);
                            }
                        } else {
                            return Err(FormatError::ExpectedLetter);
                        }
                    } else {
                        if let Some(letter) = self.letter()? {
                            if let Some(d) = self.descriptor_rep(1, letter)? {
                                ds.push(d)
                            } else if let Some(d) = self.descriptor_nonrep(letter)? {
                                ds.push(d)
                            } else {
                                return Err(FormatError::ExpectedLetter);
                            }
                        } else {
                            return Err(FormatError::ExpectedLetter);
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_basic() -> Result<()> {
        let fmt = b"('a', I2, 2('bcd''efg', 3I2, SP), 'c' I3)";

        let parser = FormatParser::new(fmt);
        let parsed = parser.parse()?;
        assert_eq!(
            parsed.descriptors,
            vec![
                Descriptor::Nonrepeatable(Nonrepeatable::Char { s: b"a".to_vec() }),
                Descriptor::Repeatable(1, Repeatable::I { w: 2, m: None }),
                Descriptor::ParenLeft(2),
                Descriptor::Nonrepeatable(Nonrepeatable::Char {
                    s: b"bcd'efg".to_vec()
                }),
                Descriptor::Repeatable(3, Repeatable::I { w: 2, m: None }),
                Descriptor::Nonrepeatable(Nonrepeatable::SP),
                Descriptor::ParenRight,
                Descriptor::Nonrepeatable(Nonrepeatable::Char { s: b"c".to_vec() }),
                Descriptor::Repeatable(1, Repeatable::I { w: 3, m: None })
            ]
        );

        assert_eq!(
            parsed.iter().take(28).collect::<Vec<_>>(),
            vec![
                EditDescriptor::Nonrepeatable(Nonrepeatable::Char { s: b"a".to_vec() }),
                EditDescriptor::Repeatable(Repeatable::I { w: 2, m: None }),
                EditDescriptor::Nonrepeatable(Nonrepeatable::Char {
                    s: b"bcd'efg".to_vec()
                }),
                EditDescriptor::Repeatable(Repeatable::I { w: 2, m: None }),
                EditDescriptor::Repeatable(Repeatable::I { w: 2, m: None }),
                EditDescriptor::Repeatable(Repeatable::I { w: 2, m: None }),
                EditDescriptor::Nonrepeatable(Nonrepeatable::SP),
                EditDescriptor::Nonrepeatable(Nonrepeatable::Char {
                    s: b"bcd'efg".to_vec()
                }),
                EditDescriptor::Repeatable(Repeatable::I { w: 2, m: None }),
                EditDescriptor::Repeatable(Repeatable::I { w: 2, m: None }),
                EditDescriptor::Repeatable(Repeatable::I { w: 2, m: None }),
                EditDescriptor::Nonrepeatable(Nonrepeatable::SP),
                EditDescriptor::Nonrepeatable(Nonrepeatable::Char { s: b"c".to_vec() }),
                EditDescriptor::Repeatable(Repeatable::I { w: 3, m: None }),
                EditDescriptor::EndOfRecord,
                EditDescriptor::Nonrepeatable(Nonrepeatable::Char {
                    s: b"bcd'efg".to_vec()
                }),
                EditDescriptor::Repeatable(Repeatable::I { w: 2, m: None }),
                EditDescriptor::Repeatable(Repeatable::I { w: 2, m: None }),
                EditDescriptor::Repeatable(Repeatable::I { w: 2, m: None }),
                EditDescriptor::Nonrepeatable(Nonrepeatable::SP),
                EditDescriptor::Nonrepeatable(Nonrepeatable::Char {
                    s: b"bcd'efg".to_vec()
                }),
                EditDescriptor::Repeatable(Repeatable::I { w: 2, m: None }),
                EditDescriptor::Repeatable(Repeatable::I { w: 2, m: None }),
                EditDescriptor::Repeatable(Repeatable::I { w: 2, m: None }),
                EditDescriptor::Nonrepeatable(Nonrepeatable::SP),
                EditDescriptor::Nonrepeatable(Nonrepeatable::Char { s: b"c".to_vec() }),
                EditDescriptor::Repeatable(Repeatable::I { w: 3, m: None }),
                EditDescriptor::EndOfRecord,
            ]
        );

        Ok(())
    }

    #[test]
    fn parse() -> Result<()> {
        // let fmt = b"(12I45, I2, (I5.99), 5(E1.2, E1.2E3), A, A5, T2, TL4, TR6, 5X)";
        // let fmt = b"(I1, 2(I2, 3(I3, I4), I5), I6)";
        let fmt = b"(I1, 2(I2, 3(I3, I4), I5), 2(I8.1), I6)";
        // let fmt = b"(I1, I6)";

        let parser = FormatParser::new(fmt);
        let parsed = parser.parse()?;
        for d in &parsed.descriptors {
            println!("{:?}", d);
        }
        for x in parsed.iter().take(50) {
            println!("- {x:?}");
        }

        Ok(())
    }
}
