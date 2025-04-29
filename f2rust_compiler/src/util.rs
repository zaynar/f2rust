use indexmap::IndexMap;
use log::error;

// From https://doc.rust-lang.org/reference/keywords.html, 2024 edition
const KEYWORDS: &[&str] = &[
    "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn", "for",
    "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref", "return",
    "self", "Self", "static", "struct", "super", "trait", "true", "type", "unsafe", "use", "where",
    "while", "async", "await", "dyn", "abstract", "become", "box", "do", "final", "macro",
    "override", "priv", "typeof", "unsized", "virtual", "yield", "try", "gen",
];

pub fn safe_identifier(s: &str) -> String {
    if KEYWORDS.contains(&s) {
        format!("{s}_")
    } else {
        s.to_owned()
    }
}

pub fn parse_header_comments(lines: &[String]) -> anyhow::Result<IndexMap<String, Vec<String>>> {
    let mut sections = IndexMap::new();
    let mut section = None;
    for c in lines {
        if let Some(name) = c.strip_prefix("$ ") {
            if sections.contains_key(name) {
                error!("duplicate doc section {name}");
            }
            section = Some(name.to_owned());
        } else if *c == "-&" {
            section = None;
        } else if let Some(section) = &section {
            sections
                .entry(section.clone())
                .or_insert_with(Vec::new)
                .push(c.to_string());
        }
    }
    Ok(sections)
}
