//! Global analysis. Used after parsing each source file individually into an AST.
//! This does all the processing that needs awareness of the whole program:
//! procedure linking, determining argument mutability, function pointer types, etc,
//! to get extra type information needed for the Rust codegen.
//!
//! In particular, constructing function pointer types requires that we know the
//! callers of an API. In SPICE, there are a few shared libraries (spicelib, support)
//! and multiple executables. We need to analyse all the executables at once, to
//! figure out all the function pointers passed into the shared libraries.
//! But some executables define symbols with the same name, so we can't put them all
//! in the global scope together.
//!
//! So, we add the concept of namespaces: every procedure name only needs to be unique
//! within its namespace (corresponding to an executable or library name). Calls will be
//! linked to procedures in either the current namespace or one of the shared library
//! namespaces.

use std::collections::{HashMap, HashSet};

use anyhow::{Result, bail};
use indexmap::IndexMap;
use log::error;

use crate::{
    ast,
    codegen::{self, ProcedureArgs},
    intrinsics,
};

/// PROGRAM/SUBROUTINE/FUNCTION, or ENTRY
#[derive(Debug, Clone)]
pub struct Procedure {
    pub name: Name,
    pub return_type: ast::DataType,
    pub dargs: Vec<DummyArg>,

    /// Whether we need a Context object, for SAVE or IO, or for calling another
    /// procedure that needs it
    pub requires_ctx: bool,

    /// Index within GlobalAnalysis::programs
    pub program_idx: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DummyArg {
    pub name: String,
    pub base_type: ast::DataType,
    pub is_array: bool,
    pub mutated: bool,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Name(pub String, pub String);

impl Name {
    fn new(namespace: &str, local_name: &str) -> Self {
        Self(namespace.to_owned(), local_name.to_owned())
    }
}

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.0, self.1)
    }
}

#[derive(Debug)]
pub struct SymbolTable(IndexMap<String, Symbol>);

impl SymbolTable {
    pub fn get(&self, name: &str) -> Result<&Symbol> {
        if let Some(sym) = self.0.get(name) {
            Ok(sym)
        } else {
            bail!("unrecognized symbol {name}")
        }
    }

    pub fn get_mut(&mut self, name: &str) -> Result<&mut Symbol> {
        if let Some(sym) = self.0.get_mut(name) {
            Ok(sym)
        } else {
            bail!("unrecognized symbol {name}")
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&String, &Symbol)> {
        self.0.iter()
    }
}

/// Extra data added to each ast::Symbol
#[derive(Debug)]
pub struct Symbol {
    /// All global procedures that can be represented by this symbol
    pub actual_procs: Vec<Name>,

    /// List of the current program's procedures, where the symbol is mutated by that procedure
    /// or by any other procedure that it's passed to
    pub mutated: HashSet<String>,

    pub ast: ast::Symbol,
}

pub struct ProgramUnit {
    pub namespace: String,
    pub filename: String,
    pub symbols: SymbolTable,
    pub ast: ast::ProgramUnit,
    // TODO: maybe symbols need to be per-Procedure, in case they have differently-typed
    // callbacks with the same name?
}

impl ProgramUnit {
    pub fn new(namespace: &str, filename: &str, ast: ast::ProgramUnit) -> Self {
        let symbols = IndexMap::from_iter(ast.symbols.iter().map(|(name, sym)| {
            (
                name.clone(),
                Symbol {
                    actual_procs: Vec::new(),
                    mutated: sym.assigned.clone(),
                    ast: sym.clone(),
                },
            )
        }));

        Self {
            namespace: namespace.to_owned(),
            filename: filename.to_owned(),
            symbols: SymbolTable(symbols),
            ast,
        }
    }
}

/// Finds all symbols passed into subroutine/function calls
#[derive(Default)]
struct CallVisitor {
    called: Vec<String>,
    passed: Vec<CallArg>,
}

struct CallArg {
    name: String,
    proc: String,
    idx: usize,
}

impl ast::Visitor for CallVisitor {
    fn call(&mut self, name: &String, args: &[ast::Expression], _is_function: bool) {
        self.called.push(name.clone());
        for (i, arg) in args.iter().enumerate() {
            match arg {
                ast::Expression::Unary(..)
                | ast::Expression::Binary(..)
                | ast::Expression::Constant(..)
                | ast::Expression::Function(..)
                | ast::Expression::ImpliedDo { .. }
                | ast::Expression::ImpliedDoVar(..) => (),

                ast::Expression::Symbol(s)
                | ast::Expression::ArrayElement(s, _)
                | ast::Expression::Substring(s, _, _)
                | ast::Expression::SubstringArrayElement(s, _, _, _) => {
                    self.passed.push(CallArg {
                        name: s.clone(),
                        proc: name.clone(),
                        idx: i,
                    });
                }
            }
        }
    }
}

pub struct GlobalAnalysis {
    /// Namespaces to search when linking symbols
    shared_libs: Vec<String>,

    programs: Vec<ProgramUnit>,

    /// External procedures (subroutines, functions), which may be imported by other procedures
    procedures: HashMap<Name, Procedure>,
}

impl GlobalAnalysis {
    pub fn new(shared_libs: &[&str], program_units: Vec<ProgramUnit>) -> Self {
        Self {
            shared_libs: shared_libs.iter().map(|s| s.to_string()).collect(),
            programs: program_units,
            procedures: HashMap::new(),
        }
    }

    /// Get the argument types for calling a procedure. If the symbol is a dummy argument,
    /// it may correspond to several different actual arguments, in which case we try to
    /// unify them into a single function signature.
    pub fn procedure_args(&self, name: &str, actual_procs: &[Name]) -> Result<ProcedureArgs> {
        // TODO: this is mixing codegen stuff into globan. Should organise it better

        if actual_procs.is_empty() {
            bail!("no actual procedures");
        }

        // TODO: probably not the right condition - what if it's a function pointer with only one value?
        if actual_procs.len() == 1 {
            let proc = self.procedures.get(&actual_procs[0]).unwrap();

            Ok(ProcedureArgs {
                return_type: proc.return_type.clone(),
                dargs: proc.dargs.clone(),
                codegen: codegen::CallSyntax::External(proc.name.clone()),
                requires_ctx: proc.requires_ctx,
            })
        } else {
            let unified = self.unify_actuals(name, actual_procs)?;

            let requires_ctx = actual_procs.iter().any(|actual| {
                let proc = self.procedures.get(actual).unwrap();
                proc.requires_ctx
            });

            Ok(ProcedureArgs {
                return_type: unified[0].base_type.clone(),
                dargs: unified[1..]
                    .iter()
                    .map(|arg| DummyArg {
                        name: "unified".to_owned(),
                        base_type: arg.base_type.clone(),
                        is_array: arg.is_array,
                        mutated: arg.mutated,
                    })
                    .collect(),
                codegen: codegen::CallSyntax::Unified,
                requires_ctx,
            })
        }
    }

    pub fn requires_ctx(&self, namespace: &str, entry_name: &str) -> Result<bool> {
        if let Some(proc) = self.procedures.get(&Name::new(namespace, entry_name)) {
            Ok(proc.requires_ctx)
        } else {
            Ok(false)
        }
    }

    /// Export all defined procedures into self.procedures
    fn export_procs(&mut self) -> Result<()> {
        for (program_idx, program) in self.programs.iter().enumerate() {
            for entry in &program.ast.entries {
                let name = Name::new(&program.namespace, &entry.name);

                let return_type = match program.ast.ty {
                    ast::ProgramUnitType::Function => {
                        program.symbols.get(&entry.name)?.ast.base_type.clone()
                    }
                    _ => ast::DataType::Void,
                };

                // Whether this entry uses any SAVE variables, which require Context
                let uses_save = program
                    .symbols
                    .iter()
                    .any(|(_name, sym)| sym.ast.save && sym.ast.used.contains(&entry.name));

                // Whether this entry contains any IO statements (or STOP), which require Context
                let uses_io = entry
                    .body
                    .iter()
                    .any(|stmt| matches!(stmt, ast::Statement::Stop | ast::Statement::Print(..)));

                let proc = Procedure {
                    name: name.clone(),
                    return_type,
                    dargs: Vec::new(),
                    requires_ctx: uses_save || uses_io,
                    program_idx,
                };

                if self.procedures.insert(name.clone(), proc).is_some() {
                    bail!("duplicate procedure definition {name}");
                }
            }
        }

        Ok(())
    }

    /// For every procedure symbol, link it to the appropriate exported procedure
    /// and store in actual_procs
    fn import_procs(&mut self) -> Result<()> {
        // Collect errors so we can report them all at once
        let mut unresolved = vec![];

        for program in &mut self.programs {
            for (name, sym) in &mut program.symbols.0 {
                if sym.ast.statement_function {
                    // TODO: implement statement functions
                    sym.actual_procs.push(Name::new("statement_function", name));
                } else if (sym.ast.external || sym.ast.called) && !sym.ast.darg {
                    // Search in the current namespace first, then in shared_libs
                    let ns_name = [program.namespace.clone()]
                        .iter()
                        .chain(self.shared_libs.iter())
                        .find_map(|ns| {
                            let ns_name = Name::new(ns, name);
                            if self.procedures.contains_key(&ns_name) {
                                Some(ns_name)
                            } else {
                                None
                            }
                        });

                    if let Some(ns_name) = ns_name {
                        sym.actual_procs.push(ns_name);
                    } else if intrinsics::exists(name) {
                        sym.actual_procs.push(Name::new("intrinsics", name));
                    } else {
                        unresolved.push((
                            name,
                            program.namespace.clone(),
                            program.filename.clone(),
                        ));
                    }
                }

                // Remove duplicates
                sym.actual_procs.sort();
                sym.actual_procs.dedup();
            }
        }

        if !unresolved.is_empty() {
            bail!("cannot resolve symbols: {unresolved:?}");
        }

        Ok(())
    }

    /// Update all self.procedures[k].dargs based on the symbol tables.
    /// This is a bit hacky, because of how we separate out the information needed for
    /// inter-procedural analysis; other steps modify the symbol table and we have to call
    /// this repeatedly to propagate the information between procedures.
    fn compute_dargs(&mut self) -> Result<()> {
        for program in &self.programs {
            for entry in &program.ast.entries {
                let dargs = entry
                    .dargs
                    .iter()
                    .map(|darg| {
                        let sym = program.symbols.get(&darg.name)?;
                        Ok(DummyArg {
                            name: darg.name.clone(),
                            base_type: sym.ast.base_type.clone(),
                            is_array: !sym.ast.dims.is_empty(),
                            mutated: sym.mutated.contains(&entry.name),
                        })
                    })
                    .collect::<Result<_>>()?;

                let name = Name::new(&program.namespace, &entry.name);
                self.procedures.get_mut(&name).unwrap().dargs = dargs;
            }
        }

        Ok(())
    }

    /// Whenever a procedure symbol is passed as an argument, we propagate the actual procedures
    /// into the corresponding dummy argument. Since this only propagates through one call at a
    /// time, we need to call it repeatedly until there are no further changes.
    fn update_actual(&mut self) -> Result<bool> {
        let mut changes = Vec::new();

        for program in &self.programs {
            // Find all the call arguments
            let mut visitor = CallVisitor::default();
            for entry in &program.ast.entries {
                for statement in &entry.body {
                    statement.walk(&mut visitor);
                }
            }

            for p in visitor.passed {
                // The actual procs being called here
                let actual_procs = program.symbols.get(&p.proc)?.actual_procs.clone();

                // The value being passed as the argument
                let sym = program.symbols.get(&p.name)?;

                if sym.actual_procs.is_empty() {
                    // No information to propagate
                    continue;
                }

                for actual in actual_procs {
                    if actual.0 == "intrinsics" {
                        // No intrinsics take procedure arguments
                        continue;
                    }

                    if actual.0 == "statement_function" {
                        // TODO
                        continue;
                    }

                    // The actual proc being called
                    let proc = self.procedures.get(&actual).unwrap();

                    if let Some(darg) = proc.dargs.get(p.idx) {
                        // We need to add sym.actual_procs into darg_sym.actual_procs,
                        // where darg_sym is callee_program.symbols[darg.name],
                        // where callee_program is the ProgramUnit containing proc.
                        //
                        // But we're currently borrowing self.programs so we can't mutate it.
                        // Instead, record the changes to be applied later.
                        changes.push((proc.program_idx, &darg.name, sym.actual_procs.clone()));
                    } else {
                        bail!("passed too many arguments to {actual}");
                    }
                }
            }
        }

        let mut dirty = false;

        // Apply the changes
        for (program_idx, darg_name, actual_procs) in changes {
            let program = &mut self.programs[program_idx];
            let darg_sym = program.symbols.get_mut(darg_name)?;
            for p in actual_procs {
                if !darg_sym.actual_procs.contains(&p) {
                    darg_sym.actual_procs.push(p);
                    dirty = true;
                }
            }
        }

        Ok(dirty)
    }

    /// Set requires_ctx on procedures that call other procedures with requires_ctx.
    fn update_ctx(&mut self) -> Result<bool> {
        let mut dirty = false;

        for program in &mut self.programs {
            for entry in &program.ast.entries {
                let name = Name::new(&program.namespace, &entry.name);
                if self.procedures.get(&name).unwrap().requires_ctx {
                    continue;
                }

                let mut visitor = CallVisitor::default();
                for statement in &entry.body {
                    statement.walk(&mut visitor);
                }

                for called in visitor.called {
                    let actual_procs = program.symbols.get(&called)?.actual_procs.clone();

                    if actual_procs.iter().any(|actual| {
                        if actual.0 == "intrinsics" {
                            intrinsics::requires_ctx(&actual.1)
                        } else if actual.0 == "statement_function" {
                            false
                        } else {
                            let proc = self.procedures.get(actual).unwrap();
                            proc.requires_ctx
                        }
                    }) {
                        self.procedures.get_mut(&name).unwrap().requires_ctx = true;
                        dirty = true;
                    }
                }
            }
        }

        Ok(dirty)
    }

    /// Set symbols to mutable, if they're passed to a procedure that requires a mutable argument.
    /// Needs to be used in combination with compute_dargs to propagate mutability from symbols
    /// back to the DummyArgs.
    fn update_mut(&mut self) -> Result<bool> {
        let mut dirty = false;

        for program in &mut self.programs {
            for entry in &program.ast.entries {
                // Find all the call arguments
                let mut visitor = CallVisitor::default();
                for statement in &entry.body {
                    statement.walk(&mut visitor);
                }

                for p in visitor.passed {
                    // The actual procs being called
                    let actual_procs = program.symbols.get(&p.proc)?.actual_procs.clone();

                    // The value being passed as the argument
                    let sym = program.symbols.get_mut(&p.name)?;

                    // If it's already mutable, we don't need to do anything
                    if sym.mutated.contains(&entry.name) {
                        continue;
                    }

                    // Check every procedure this might be passed to
                    for actual in actual_procs {
                        if actual.0 == "intrinsics" {
                            if intrinsics::arg_is_mutated(&actual.1, p.idx) {
                                sym.mutated.insert(entry.name.clone());
                                dirty = true;
                            }
                            continue;
                        }

                        if actual.0 == "statement_function" {
                            // TODO
                            continue;
                        }

                        // The actual proc being called
                        let proc = self.procedures.get(&actual).unwrap();

                        if let Some(darg) = proc.dargs.get(p.idx) {
                            if darg.mutated {
                                sym.mutated.insert(entry.name.clone());
                                dirty = true;
                            }
                        } else {
                            bail!("passed too many arguments to {actual}");
                        }
                    }
                }
            }
        }

        Ok(dirty)
    }

    /// Given a set of actual procedures, determine a function signature that is acceptable
    /// for all of them (if possible)
    fn unify_actuals(
        &self,
        name: &str,
        actual_procs: &[Name],
    ) -> Result<Vec<ast::ProcedureArgType>> {
        // Get [return_type, dargs...] for each proc
        let mut actual_args = actual_procs.iter().map(|actual| {
            let proc = self.procedures.get(actual).unwrap();
            let mut args = vec![ast::ProcedureArgType {
                base_type: proc.return_type.clone(),
                is_array: false,
                mutated: false,
            }];
            args.extend(proc.dargs.iter().map(|darg| ast::ProcedureArgType {
                base_type: darg.base_type.clone(),
                is_array: darg.is_array,
                mutated: darg.mutated,
            }));
            args
        });

        // Start with the first actual_proc, and try to fold in the rest
        let mut unified = actual_args.next().unwrap();
        for args in actual_args {
            if args.len() != unified.len() {
                // TODO: turn all these error!() into bail!(), when we fix the code that triggers it
                error!(
                    "symbol {name} actual procs {:?} have inconsistent arg counts:\n{args:?}\n{unified:?}",
                    actual_procs
                );
                return Ok(vec![]);
            }

            for (u, arg) in unified.iter_mut().zip(args.into_iter()) {
                if u.base_type != arg.base_type {
                    error!(
                        "symbol {name} actual procs {:?} have inconsistent arg type ({:?} vs {:?})",
                        actual_procs, u.base_type, arg.base_type
                    );
                }
                if u.is_array != arg.is_array {
                    error!(
                        "symbol {name} actual procs {:?} have inconsistent arg arrayness",
                        actual_procs
                    );
                }

                // If any proc is mut, all the others will be turned into mut
                if arg.mutated {
                    u.mutated = true;
                }
            }
        }

        Ok(unified)
    }

    /// When a darg has some actual_procs, update its DataType to the correct function pointer type.
    /// Since this might depend on another function pointer, iterate until stable.
    fn update_fn_types(&mut self) -> Result<bool> {
        let mut dirty = false;

        let mut changes_type = Vec::new();
        let mut changes_mut = Vec::new();

        for (program_idx, program) in self.programs.iter().enumerate() {
            for (name, sym) in &program.symbols.0 {
                if sym.ast.darg && !sym.actual_procs.is_empty() {
                    let unified = self.unify_actuals(name, &sym.actual_procs)?;

                    // Some extra unifying:
                    // If unified arg N is mutated, then mark all the actual proc dummy arg N as mutated,
                    // so we can use the same `fn` type for them all
                    for (i, unified_arg) in unified.iter().skip(1).enumerate() {
                        if unified_arg.mutated {
                            for actual in &sym.actual_procs {
                                let proc = self.procedures.get(actual).unwrap();
                                if !proc.dargs[i].mutated {
                                    changes_mut.push((
                                        proc.program_idx,
                                        proc.name.1.clone(),
                                        proc.dargs[i].name.clone(),
                                    ));
                                    dirty = true;
                                }
                            }
                        }
                    }

                    let ty = ast::DataType::Procedure(unified);
                    if sym.ast.base_type != ty {
                        changes_type.push((program_idx, name.clone(), ty));
                        dirty = true;
                    }
                }
            }
        }

        for (program_idx, name, ty) in changes_type {
            let sym = self.programs[program_idx].symbols.get_mut(&name).unwrap();
            sym.ast.base_type = ty;
        }

        for (program_idx, entry_name, name) in changes_mut {
            let sym = self.programs[program_idx].symbols.get_mut(&name).unwrap();
            sym.mutated.insert(entry_name);
        }

        Ok(dirty)
    }

    pub fn analyse(&mut self) -> Result<()> {
        self.export_procs()?;
        self.import_procs()?;

        self.compute_dargs()?;

        while self.update_actual()? {
            // Loop until there are no further changes
        }

        // Propagate requires_ctx through procedure calls
        while self.update_ctx()? {}

        // Propagate mutability through procedure calls. Iterate until it stops changing
        loop {
            let dirty = self.update_mut()?;
            // This might have changed the mutability of our dargs.
            // Maybe we could update it more efficiently, but don't bother, this is fast enough
            self.compute_dargs()?;
            if !dirty {
                break;
            }
        }

        loop {
            let dirty = self.update_fn_types()?;
            self.compute_dargs()?;
            if !dirty {
                break;
            }
        }

        // TODO: find calls the symbols with no actual_procs,
        // and determine the type from the context of how it's called.
        // (This is for when only users provide the callback)

        Ok(())
    }

    pub fn codegen(&self, target: &str, filename: &str, pretty: bool) -> Result<String> {
        if let Some(pu) = self
            .programs
            .iter()
            .find(|pu| pu.namespace == target && pu.filename == filename)
        {
            let code = codegen::CodeGen::new(self, pu).emit()?;
            if pretty {
                codegen::pretty_print(code)
            } else {
                Ok(code)
            }
        } else {
            bail!("cannot find {target}/{filename}");
        }
    }
}
