use crate::ast::{DataType, ProgramUnitType, Statement};
use crate::codegen::{
    CodeGen, RustType, SymbolTable, emit_datatype, eval_array_size, eval_character_len,
    format_comment_block,
};
use crate::file::SourceLoc;
use crate::globan::GlobalAnalysis;
use crate::{ast, globan};
use anyhow::{Result, bail};
use indexmap::IndexMap;
use log::error;

fn safe_identifier(s: &str) -> String {
    // From https://doc.rust-lang.org/reference/keywords.html, 2024 edition
    const KEYWORDS: &[&str] = &[
        "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn",
        "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref",
        "return", "self", "Self", "static", "struct", "super", "trait", "true", "type", "unsafe",
        "use", "where", "while", "async", "await", "dyn", "abstract", "become", "box", "do",
        "final", "macro", "override", "priv", "typeof", "unsized", "virtual", "yield", "try",
        "gen",
    ];
    if KEYWORDS.contains(&s) {
        format!("r#{s}")
    } else {
        s.to_owned()
    }
}

struct Arg {
    param: String, // in the Rust API
    arg: String,   // in the call
}

fn emit_api_symbol(loc: &SourceLoc, name: &str, syms: &SymbolTable) -> Result<Arg> {
    let sym = syms.get(name)?;
    let ty = emit_datatype(&sym.ast.base_type);

    let name_lc = safe_identifier(&name.to_ascii_lowercase());

    Ok(match sym.rs_ty {
        RustType::Primitive => Arg {
            param: format!("{name_lc}: {ty}"),
            arg: name_lc.clone(),
        },
        RustType::PrimitiveRefMut => Arg {
            param: format!("{name_lc}: &mut {ty}"),
            arg: name_lc.clone(),
        },

        RustType::DummyArray => {
            let param = if let Some(size) = eval_array_size(&sym.ast.dims, syms)? {
                format!("{name_lc}: &[{ty}; {size}]")
            } else {
                format!("{name_lc}: &[{ty}]")
            };
            Arg {
                param,
                arg: name_lc.clone(),
            }
        }
        RustType::DummyArrayMut => {
            let param = if let Some(size) = eval_array_size(&sym.ast.dims, syms)? {
                format!("{name_lc}: &mut [{ty}; {size}]")
            } else {
                format!("{name_lc}: &mut [{ty}]")
            };
            Arg {
                param,
                arg: name_lc.clone(),
            }
        }
        RustType::DummyCharArray => Arg {
            param: format!("{name_lc}: CharArray"),
            arg: name_lc.clone(),
        },
        RustType::DummyCharArrayMut => Arg {
            param: format!("{name_lc}: CharArrayMut"),
            arg: name_lc.clone(),
        },
        RustType::CharSliceRef => {
            if let Some(size) = eval_character_len(&sym.ast.character_len, syms)? {
                if size == 1 {
                    return Ok(Arg {
                        param: format!("{name_lc}: char"),
                        arg: format!("&[u8::try_from({name_lc}).unwrap()]"),
                    });
                } else {
                    // This only happens in LTIME, which wants a 2-char string
                }
            }
            Arg {
                param: format!("{name_lc}: &str"),
                arg: format!("{name_lc}.as_bytes()"),
            }
            // TODO: if the string is empty, we should pass " " instead,
            // or else prove the FORTRAN code doesn't mind zero-length strings
        }
        RustType::CharSliceMut => {
            if let Some(size) = eval_character_len(&sym.ast.character_len, syms)? {
                Arg {
                    param: format!("{name_lc}: &mut [u8; {size}]"),
                    arg: name_lc.clone(),
                }
            } else {
                Arg {
                    param: format!("{name_lc}: &mut str"),
                    arg: format!("f2rust_std::fstr::StrBytes::new({name_lc}).as_mut()"),
                }
            }
        }
        RustType::Procedure => Arg {
            param: format!("{name_lc}: {ty}"),
            arg: name_lc.clone(),
        },

        RustType::PrimitiveMut
        | RustType::ActualArray
        | RustType::ActualCharArray
        | RustType::CharVec
        | RustType::SavePrimitive
        | RustType::SaveChar
        | RustType::SaveActualArray
        | RustType::SaveActualCharArray
        | RustType::LocalDoVar
        | RustType::EquivArray
        | RustType::EquivArrayMut => {
            bail!("{loc} invalid API symbol {name}: {sym:?}")
        }
    })
}

pub fn parse_header_comments(lines: &[String]) -> Result<IndexMap<String, Vec<String>>> {
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

impl CodeGen<'_> {
    pub fn emit_api(&mut self) -> Result<String> {
        let mut code = String::new();

        for entry in &self.entries {
            let entry_name = &entry.ast.name;

            // Exclude private APIs from pool.f
            if entry_name.starts_with("ZZ") {
                continue;
            }

            let comment_sections = parse_header_comments(
                &entry
                    .ast
                    .body
                    .iter()
                    .filter_map(|(_loc, stmt)| {
                        if let Statement::Comment(c) = stmt {
                            Some(c)
                        } else {
                            None
                        }
                    })
                    .flatten()
                    .cloned()
                    .collect::<Vec<_>>(),
            )?;

            // Sections present in all files:
            //   Abstract
            //   Author_and_Institution
            //   Brief_I/O
            //   Detailed_Input
            //   Detailed_Output
            //   Disclaimer
            //   Examples
            //   Exceptions
            //   Files
            //   Index_Entries
            //   Keywords
            //   Literature_References
            //   Parameters
            //   Particulars
            //   Required_Reading
            //   Restrictions
            //   Version
            // Present in some files:
            //   Declarations
            //   Revisions

            // Exclude functions that don't want to be called
            if comment_sections
                .get("Abstract")
                .is_some_and(|c| c.join("").contains("DO NOT CALL THIS ROUTINE"))
                || comment_sections
                    .get("Particulars")
                    .is_some_and(|c| c.join("").contains("DO NOT CALL THIS ROUTINE"))
            {
                continue;
            }

            // Extract the short description from the "$Procedure" comment

            let proc_comment: Vec<_> = entry
                .ast
                .pre_comments
                .iter()
                .flatten()
                .filter(|c| c.starts_with("$Procedure"))
                .collect();

            let mut docs = String::new();

            if proc_comment.len() > 1 {
                bail!("{entry_name}: too many $Procedure lines");
            } else if let Some(proc_comment) = proc_comment.first() {
                let (_proc, rest) = proc_comment.split_once(" ").unwrap();
                let (name, desc) = rest.split_once(" ").unwrap();
                if name != entry_name {
                    bail!("{entry_name}: incorrect $Procedure name '{name}'");
                }
                if let Some(desc) = desc
                    .trim_ascii()
                    .strip_prefix("(")
                    .and_then(|desc| desc.strip_suffix(")"))
                {
                    // Escape Markdown reference/HTML characters
                    let desc = desc.replace("<", "\\<").replace("[", "\\[");

                    docs += &format!("/// {}\n///\n", desc.trim_ascii());
                } else {
                    bail!("{entry_name} incorrect $Procedure syntax: '{desc}'")
                }
            } else {
                bail!("{entry_name}: no $Procedure line");
            }

            // Copy the Abstract
            if let Some(abstr) = comment_sections.get("Abstract") {
                // Escape Markdown reference/HTML characters
                let abstr: Vec<_> = abstr
                    .iter()
                    .map(|s| s.replace("<", "\\<").replace("[", "\\["))
                    .collect();
                docs += &format_comment_block(&abstr, "///", true).unwrap();
            } else {
                error!("missing Abstract docs");
            }

            // Copy all the other interesting sections
            for (name, content) in &comment_sections {
                if matches!(
                    name.as_str(),
                    "Abstract" | "Declarations" | "Disclaimer" | "Index_Entries" | "Keywords"
                ) {
                    // Abstract was handled separately, since it must be the top of the file.
                    // Declarations is just FORTRAN code.
                    // Disclaimer doesn't need to be repeated on every docs page.
                    // TODO: maybe include keywords, make them into links?
                    continue;
                }

                // Skip sections with no meaningful content
                if matches!(content.join(" ").trim_ascii(), "None.") {
                    continue;
                }

                docs += &format!("///\n/// # {}\n///\n", name.replace("_", " "));

                if name == "Required_Reading" {
                    for req in content.join(" ").split_ascii_whitespace() {
                        if req == "TEXT" {
                            // Referenced by TXTOPR, but doesn't exist
                            docs += "/// * TEXT\n";
                        } else {
                            let page = req.to_ascii_lowercase();
                            docs += &format!("/// * [{req}](crate::required_reading::{page})\n");
                        }
                    }
                } else {
                    docs += "/// ```text\n";
                    docs += &format_comment_block(content, "/// ", true).unwrap();
                    docs += "/// ```\n";
                }
            }

            let mut params = vec![];
            let mut args = vec![];

            for darg in &entry.ast.dargs {
                let arg = emit_api_symbol(&entry.ast.loc, darg, &entry.codegen.syms)?;
                params.push(arg.param);
                args.push(arg.arg);
            }

            let requires_ctx = entry
                .codegen
                .globan
                .requires_ctx(&entry.codegen.program.namespace, entry_name)?;

            if requires_ctx {
                params.insert(0, "ctx: &mut SpiceContext<'a>".to_owned());
                args.push("ctx.raw_context()".to_owned());
            }

            let fn_lifetime = if requires_ctx { "<'a>" } else { "" };

            let is_function = matches!(self.shared.program.ast.ty, ast::ProgramUnitType::Function);
            let ret_type = if is_function {
                &entry.codegen.syms.get(entry_name)?.ast.base_type
            } else {
                &DataType::Void
            };

            let returns_result = entry
                .codegen
                .globan
                .returns_result(&entry.codegen.program.namespace, entry_name)?;

            let ret = if !matches!(ret_type, DataType::Void | DataType::Character) {
                if returns_result {
                    format!("-> crate::Result<{}>", emit_datatype(ret_type))
                } else {
                    format!("-> {}", emit_datatype(ret_type))
                }
            } else {
                if returns_result {
                    "-> crate::Result<()>".to_owned()
                } else {
                    "".to_owned()
                }
            };

            let fn_name = safe_identifier(&entry_name.to_ascii_lowercase());

            code += &docs;
            code += &format!(
                "pub fn {fn_name}{fn_lifetime}({params}) {ret} {{\n",
                params = params.join(", ")
            );
            let call = &format!(
                "rsspice_spicelib::spicelib::{entry_name}({args})",
                args = args.join(", ")
            );
            if matches!(ret_type, DataType::Void | DataType::Character) {
                if returns_result {
                    code += &format!("  {call}?;\n");
                    code += "  Ok(())\n";
                } else {
                    code += &format!("  {call};\n");
                }
            } else {
                if returns_result {
                    code += &format!("  let ret = {call}?;\n");
                    code += "  Ok(ret)\n";
                } else {
                    code += &format!("  let ret = {call};\n");
                    code += "  ret\n";
                }
            }
            code += "}\n";
            code += "\n";
        }

        Ok(code)
    }
}

pub fn emit_constants(syms: ast::SymbolTable) -> Result<String> {
    let globan = GlobalAnalysis::new(&[], vec![]);
    let program = globan::ProgramUnit::new(
        "constants",
        "constants",
        ast::ProgramUnit {
            ty: ProgramUnitType::Program,
            symbols: syms,
            entries: vec![],
            statement_functions: vec![],
            datas: vec![],
        },
    );
    let codegen = CodeGen::new(&globan, &program);

    codegen.shared.emit_constants(true)
}
