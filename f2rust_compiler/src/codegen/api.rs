use crate::ast::{DataType, ProgramUnitType, Statement};
use crate::codegen::{
    CodeGen, Entry, RustType, SymbolTable, emit_datatype, eval_character_len, eval_dims,
    format_comment_block,
};
use crate::file::SourceLoc;
use crate::globan::GlobalAnalysis;
use crate::grammar::Constant;
use crate::util::safe_identifier;
use crate::{ast, globan};
use anyhow::{Result, bail};
use indexmap::IndexMap;
use log::error;
use std::fmt::Write;

struct RawArg {
    name: String,
    param: String,
    arg: String,
}

fn emit_sized_array(ty: &str, dims: &[Option<i32>]) -> (String, bool) {
    let mut r = ty.to_owned();
    let mut depth = 0;
    for dim in dims {
        depth += 1;
        if let Some(n) = dim {
            r = format!("[{r}; {n}]");
        } else {
            return (format!("[{r}]"), depth > 1);
        }
    }
    (r, depth > 1)
}

fn emit_api_symbol(loc: &SourceLoc, name: &str, syms: &SymbolTable) -> Result<RawArg> {
    let sym = syms.get(name)?;
    let ty = emit_datatype(&sym.ast.base_type);

    let name_lc = safe_identifier(&name.to_ascii_lowercase());

    Ok(match sym.rs_ty {
        RustType::Primitive => RawArg {
            name: name_lc.clone(),
            param: format!("{name_lc}: {ty}"),
            arg: name_lc.clone(),
        },
        RustType::PrimitiveRefMut => RawArg {
            name: name_lc.clone(),
            param: format!("{name_lc}: &mut {ty}"),
            arg: name_lc.clone(),
        },

        RustType::DummyArray => {
            let (array, flatten) = emit_sized_array(&ty, &eval_dims(&sym.ast.dims, syms)?);
            RawArg {
                name: name_lc.clone(),
                param: format!("{name_lc}: &{array}"),
                arg: if flatten {
                    format!("{name_lc}.as_flattened()")
                } else {
                    name_lc.clone()
                },
            }
        }
        RustType::DummyArrayMut => {
            let (array, flatten) = emit_sized_array(&ty, &eval_dims(&sym.ast.dims, syms)?);
            RawArg {
                name: name_lc.clone(),
                param: format!("{name_lc}: &mut {array}"),
                arg: if flatten {
                    format!("{name_lc}.as_flattened_mut()")
                } else {
                    name_lc.clone()
                },
            }
        }

        RustType::DummyCharArray => RawArg {
            name: name_lc.clone(),
            param: format!("{name_lc}: CharArray"),
            arg: name_lc.clone(),
        },
        RustType::DummyCharArrayMut => RawArg {
            name: name_lc.clone(),
            param: format!("{name_lc}: CharArrayMut"),
            arg: name_lc.clone(),
        },
        RustType::CharSliceRef => {
            if let Some(size) = eval_character_len(&sym.ast.character_len, syms)? {
                if size == 1 {
                    return Ok(RawArg {
                        name: name_lc.clone(),
                        param: format!("{name_lc}: char"),
                        arg: format!("&[u8::try_from({name_lc}).unwrap()]"),
                    });
                } else {
                    // This only happens in LTIME, which wants a 2-char string
                }
            }
            RawArg {
                name: name_lc.clone(),
                param: format!("{name_lc}: &str"),
                arg: format!("{name_lc}.as_bytes()"),
            }
            // TODO: if the string is empty, we should pass " " instead,
            // or else prove the FORTRAN code doesn't mind zero-length strings
        }
        RustType::CharSliceMut => {
            if let Some(size) = eval_character_len(&sym.ast.character_len, syms)? {
                RawArg {
                    name: name_lc.clone(),
                    param: format!("{name_lc}: &mut [u8; {size}]"),
                    arg: name_lc.clone(),
                }
            } else {
                RawArg {
                    name: name_lc.clone(),
                    param: format!("{name_lc}: &mut str"),
                    arg: format!("fstr::StrBytes::new({name_lc}).as_mut()"),
                }
            }
        }
        RustType::Procedure => RawArg {
            name: name_lc.clone(),
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

fn is_bogus_entry(st: &Statement) -> bool {
    match st {
        Statement::Call(func, args) if func == "SIGERR" => {
            matches!(args.first().unwrap(),
            ast::Expression::Constant(Constant::Character(s)) if
                s.starts_with("SPICE(") && s.ends_with("BOGUSENTRY)")
            )
        }
        Statement::If { bodies, .. } => bodies
            .iter()
            .any(|b| b.iter().any(|(_loc, st)| is_bogus_entry(st))),
        _ => false,
    }
}

impl CodeGen<'_> {
    // This generates two types of API:
    //
    // "raw" is a mostly 1:1 mapping of the FORTRAN API, whose main purpose
    // is to hold the FORTRAN documentation. (rustdoc doesn't work well with
    // enormous pages, so the raw API is implemented as loose functions, meaning
    // each one gets a separate page). We translate arguments into more idiomatic
    // Rust types, and translate errors, but maintain the general API structure.
    //
    // Non-raw is a more idiomatic mapping to `impl SpiceContext` methods.
    // Only brief documentation is included (because we've got a thousand of
    // them on one rustdoc page). Output arguments are turned into return values,
    // etc, with a bunch of heuristics.
    pub(super) fn emit_api(&self, entry: &Entry, raw: bool) -> Result<String> {
        let mut code = String::new();

        let entry_name = &entry.ast.name;
        let fn_name = entry.ast.api_name.as_ref().unwrap();

        // Don't expose functions that return BOGUSENTRY/CKBOGUSENTRY/etc
        // since they're not meant to be called directly.
        // (But they may have useful docs, so still emit the raw function)
        if !raw && entry.ast.body.iter().any(|(_loc, st)| is_bogus_entry(st)) {
            return Ok(code);
        }

        // Comment sections present in all files:
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

        // Extract the short description from the "$Procedure" comment

        let proc_comment: Vec<_> = entry
            .ast
            .pre_comments
            .iter()
            .flatten()
            .filter(|(c, _)| c.starts_with("$Procedure"))
            .collect();

        let mut docs = String::new();

        if proc_comment.len() > 1 {
            bail!("{entry_name}: too many $Procedure lines");
        } else if let Some((proc_comment, _)) = proc_comment.first() {
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

                writeln!(docs, "/// {}\n///", desc.trim_ascii())?;
            } else {
                bail!("{entry_name} incorrect $Procedure syntax: '{desc}'")
            }
        } else {
            bail!("{entry_name}: no $Procedure line");
        }

        // Copy the Abstract
        if let Some(abstr) = entry.ast.comment_sections.get("Abstract") {
            // Escape Markdown reference/HTML characters
            let abstr: Vec<_> = abstr
                .iter()
                .map(|s| s.replace("<", "\\<").replace("[", "\\["))
                .collect();
            docs += &format_comment_block(&abstr, "///", true).unwrap();
        } else {
            error!("missing Abstract docs");
        }

        let mut arg_dirs = IndexMap::new();
        if let Some(io) = entry.ast.comment_sections.get("Brief_I/O") {
            // Parse tables like:
            //
            //     VARIABLE  I/O  DESCRIPTION
            //     --------  ---  --------------------------------------------------
            //     CENTER,
            //     VEC1,
            //     VEC2       I   ...
            //     ELLIPS     O   ...

            let mut in_table = false;
            let mut vars = vec![];
            for line in io {
                if line.trim_ascii_start().starts_with("--------") {
                    in_table = true;
                // } else if line.trim_ascii().is_empty() {
                //     in_table = false;
                } else if in_table {
                    let split: Vec<_> = line.trim_ascii().split_ascii_whitespace().collect();
                    if split.len() == 1 {
                        if let Some(var) = split[0].strip_suffix(",") {
                            vars.push(var);
                        }
                    } else if split.len() >= 2 && matches!(split[1], "I" | "O" | "I-O") {
                        vars.push(split[0]);
                        for v in &vars {
                            arg_dirs.insert(*v, split[1]);
                        }
                        vars.clear();
                    }
                }
            }
        }

        if raw {
            // Copy all the other interesting sections
            for (name, content) in &entry.ast.comment_sections {
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
                            writeln!(docs, "/// * TEXT")?;
                        } else {
                            let page = req.to_ascii_lowercase();
                            writeln!(docs, "/// * [{req}](crate::required_reading::{page})")?;
                        }
                    }
                } else {
                    writeln!(docs, "/// ```text")?;
                    docs += &format_comment_block(content, "/// ", true).unwrap();
                    writeln!(docs, "/// ```")?;
                }
            }
        } else {
            writeln!(docs, "///")?;
            writeln!(
                docs,
                "/// See [`{fn_name}`](raw::{fn_name}) for full documentation."
            )?;
        }

        let mut params = vec![];
        let mut args = vec![];

        for darg in &entry.ast.dargs {
            let arg = emit_api_symbol(&entry.ast.loc, darg, &entry.codegen.syms)?;
            params.push(arg.param);
            if raw {
                args.push(arg.arg);
            } else {
                args.push(arg.name.clone());
            }

            // writeln!(
            //     docs,
            //     "/// * {}: {}",
            //     arg.name,
            //     arg_dirs.get(darg.as_str()).unwrap_or(&"?")
            // )?;
        }

        let requires_ctx = entry
            .codegen
            .globan
            .requires_ctx(&entry.codegen.program.namespace, entry_name)?;

        if raw {
            if requires_ctx {
                params.insert(0, "ctx: &mut SpiceContext<'a>".to_owned());
                args.push("ctx.raw_context()".to_owned());
            }
        } else {
            params.insert(0, "&mut self".to_owned());
            if requires_ctx {
                args.insert(0, "self".to_owned());
            }
        }

        let fn_lifetime = if raw && requires_ctx { "<'a>" } else { "" };

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

        let result = if raw { "crate::Result" } else { "Result" };

        let ret = if !matches!(ret_type, DataType::Void | DataType::Character) {
            if returns_result {
                format!("-> {result}<{}>", emit_datatype(ret_type))
            } else {
                format!("-> {}", emit_datatype(ret_type))
            }
        } else {
            if returns_result {
                format!("-> {result}<()>")
            } else {
                "".to_owned()
            }
        };

        code += &docs;
        writeln!(
            code,
            "pub fn {fn_name}{fn_lifetime}({params}) {ret} {{",
            params = params.join(", ")
        )?;
        if raw {
            let call = &format!("{entry_name}({args})", args = args.join(", "));
            if matches!(ret_type, DataType::Void | DataType::Character) {
                if returns_result {
                    writeln!(code, "  {call}?;")?;
                    writeln!(code, "  ctx.handle_errors()?;")?;
                    writeln!(code, "  Ok(())")?;
                } else {
                    writeln!(code, "  {call};")?;
                }
            } else {
                if returns_result {
                    writeln!(code, "  let ret = {call}?;")?;
                    writeln!(code, "  ctx.handle_errors()?;")?;
                    writeln!(code, "  Ok(ret)")?;
                } else {
                    writeln!(code, "  let ret = {call};")?;
                    writeln!(code, "  ret")?;
                }
            }
        } else {
            let call = &format!("raw::{fn_name}({args})", args = args.join(", "));
            writeln!(code, "  {call}")?;
        }
        writeln!(code, "}}")?;
        writeln!(code)?;

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
