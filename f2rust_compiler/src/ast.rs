//! Converts the `grammar` output into an abstract syntax tree, for a single program unit
//! (i.e. one PROGRAM/FUNCTION/SUBROUTINE with potentially multiple ENTRY points).
//!
//! We also set up the symbol table with various information on how each symbol is declared
//! and used.

use std::collections::{HashMap, HashSet};

use anyhow::{Context, Result, bail};
use indexmap::IndexMap;
use log::{error, warn};

use crate::{file::SourceLoc, grammar, intrinsics};

#[derive(Debug, Clone)]
pub enum Expression {
    Unary(grammar::UnaryOp, Box<Expression>),
    Binary(grammar::BinaryOp, Box<Expression>, Box<Expression>),

    Symbol(String),
    ArrayElement(String, Vec<Expression>),
    Function(String, Vec<Expression>),
    Substring(String, Option<Box<Expression>>, Option<Box<Expression>>),
    SubstringArrayElement(
        String,
        Vec<Expression>,
        Option<Box<Expression>>,
        Option<Box<Expression>>,
    ),

    Constant(grammar::Constant),

    ImpliedDo {
        data: Vec<Expression>,
        do_var: String,
        e1: Box<Expression>,
        e2: Box<Expression>,
        e3: Option<Box<Expression>>,
    },
    ImpliedDoVar(String),
}

// UNIT/FMT specifiers in READ/WRITE/PRINT
#[derive(Debug, Clone)]
pub enum Specifier {
    Asterisk,
    Expression(Expression),
}

type Specifiers = HashMap<String, Expression>;

/// Executable statements
#[derive(Debug, Clone)]
pub enum Statement {
    Assignment(Expression, Expression),

    // Contains all the IF / ELSE IF / ELSE cases.
    // ELSE has es[n]==None, all others are Some.
    // es.len() == bodies.len(), except during construction.
    If {
        es: Vec<Option<Expression>>,
        bodies: Vec<Vec<Statement>>,
    },

    Do {
        var: String,
        e1: Expression,
        e2: Expression,
        e3: Option<Expression>,
        body: Vec<Statement>,
    },

    DoWhile {
        e: Expression,
        body: Vec<Statement>,
    },

    Stop,

    Read {
        unit: Specifier,
        fmt: Option<Specifier>,
        other: Specifiers,
        iolist: Vec<Expression>,
    },
    Write {
        unit: Specifier,
        fmt: Option<Specifier>,
        other: Specifiers,
        iolist: Vec<Expression>,
    },
    Print {
        fmt: Specifier,
        iolist: Vec<Expression>,
    },
    Open(Specifiers),
    Close(Specifiers),
    Inquire(Specifiers),
    Backspace(Specifiers),
    Endfile(Specifiers),
    Rewind(Specifiers),
    Call(String, Vec<Expression>),
    Return,
}

#[derive(Debug)]
pub struct ProgramUnit {
    pub ty: ProgramUnitType,

    pub symbols: SymbolTable,

    /// Every program unit has one main entry (PROGRAM/FUNCTION/SUBROUTINE),
    /// and 0+ additional ENTRY
    pub entries: Vec<Entry>,

    /// DATA declarations
    pub datas: Vec<DataStatement>,
}

#[derive(Debug, Clone)]
pub enum ProgramUnitType {
    Program,
    Function,
    Subroutine,
    // BlockData: not supported
}

/// Represents an ENTRY, or the virtual one at the start of a procedure
#[derive(Debug)]
pub struct Entry {
    pub name: String,
    pub dargs: Vec<DummyArg>,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct DataStatement {
    /// List of names to assign to
    pub nlist: Vec<Expression>,

    /// List of reps*constval
    pub clist: Vec<(Option<Expression>, Expression)>,
}

/// Dummy argument (i.e. function parameter) for an Entry
#[derive(Debug, Clone)]
pub struct DummyArg {
    pub name: String,
}

impl DummyArg {
    fn new(name: &String) -> Result<Self> {
        if name == "*" {
            bail!("alternate returns are not supported");
        }
        Ok(DummyArg {
            name: name.to_owned(),
        })
    }
}

// Construct Expression from grammar
impl Expression {
    fn from(syms: &SymbolTable, e: &grammar::Expression) -> Result<Self> {
        Ok(match e {
            grammar::Expression::Unary(op, e2) => {
                Expression::Unary(op.clone(), Box::new(Self::from(syms, e2)?))
            }
            grammar::Expression::Binary(op, e1, e2) => Expression::Binary(
                op.clone(),
                Box::new(Self::from(syms, e1)?),
                Box::new(Self::from(syms, e2)?),
            ),
            grammar::Expression::Symbol(s) => {
                if syms.implied_do_vars.contains(s) {
                    Expression::ImpliedDoVar(s.clone())
                } else {
                    Expression::Symbol(s.clone())
                }
            }
            grammar::Expression::ArrayElementOrFunction(s, es) => {
                let es = es
                    .iter()
                    .map(|e| Expression::from(syms, e))
                    .collect::<Result<_>>()?;

                if syms.is_array(s) {
                    Expression::ArrayElement(s.clone(), es)
                } else {
                    Expression::Function(s.clone(), es)
                }
            }
            grammar::Expression::Substring(s, e1, e2) => {
                let e1 = match e1 {
                    None => None,
                    Some(e) => Some(Box::new(Expression::from(syms, e)?)),
                };
                let e2 = match e2 {
                    None => None,
                    Some(e) => Some(Box::new(Expression::from(syms, e)?)),
                };
                Expression::Substring(s.clone(), e1, e2)
            }
            grammar::Expression::SubstringArrayElement(s, es, e1, e2) => {
                let es = es
                    .iter()
                    .map(|e| Expression::from(syms, e))
                    .collect::<Result<_>>()?;
                let e1 = match e1 {
                    None => None,
                    Some(e) => Some(Box::new(Expression::from(syms, e)?)),
                };
                let e2 = match e2 {
                    None => None,
                    Some(e) => Some(Box::new(Expression::from(syms, e)?)),
                };
                Expression::SubstringArrayElement(s.clone(), es, e1, e2)
            }
            grammar::Expression::Constant(c) => Expression::Constant(c.clone()),
        })
    }

    fn from_dataname(syms: &mut SymbolTable, n: &grammar::DataName) -> Result<Self> {
        Ok(match n {
            grammar::DataName::Variable(s) => Expression::Symbol(s.clone()),
            grammar::DataName::ArrayElement(s, es) => Expression::ArrayElement(
                s.clone(),
                es.iter()
                    .map(|e| Expression::from(syms, e))
                    .collect::<Result<_>>()?,
            ),
            grammar::DataName::Substring(s, e1, e2) => {
                let e1 = match e1 {
                    None => None,
                    Some(e) => Some(Box::new(Expression::from(syms, e)?)),
                };
                let e2 = match e2 {
                    None => None,
                    Some(e) => Some(Box::new(Expression::from(syms, e)?)),
                };
                Expression::Substring(s.clone(), e1, e2)
            }
            grammar::DataName::SubstringArrayElement(s, es, e1, e2) => {
                let es = es
                    .iter()
                    .map(|e| Expression::from(syms, e))
                    .collect::<Result<_>>()?;
                let e1 = match e1 {
                    None => None,
                    Some(e) => Some(Box::new(Expression::from(syms, e)?)),
                };
                let e2 = match e2 {
                    None => None,
                    Some(e) => Some(Box::new(Expression::from(syms, e)?)),
                };
                Expression::SubstringArrayElement(s.clone(), es, e1, e2)
            }
            grammar::DataName::ImpliedDo(data, do_var, e1, e2, e3) => {
                syms.implied_do_vars.push(do_var.clone());

                let data = data
                    .iter()
                    .map(|n| Expression::from_dataname(syms, n))
                    .collect::<Result<_>>()?;

                let e1 = Box::new(Expression::from(syms, e1)?);
                let e2 = Box::new(Expression::from(syms, e2)?);
                let e3 = match e3 {
                    None => None,
                    Some(e) => Some(Box::new(Expression::from(syms, e)?)),
                };

                syms.implied_do_vars.pop();

                Expression::ImpliedDo {
                    data,
                    do_var: do_var.clone(),
                    e1,
                    e2,
                    e3,
                }
            }
            grammar::DataName::Expression(e) => Expression::from(syms, e)?,
        })
    }
}

/// Used for walking the Statement/Expression tree
pub trait Visitor {
    fn statement(&mut self, statement: &Statement) {
        let _ = statement;
    }
    fn symbol(&mut self, name: &String) {
        let _ = name;
    }
    fn call(&mut self, name: &String, args: &[Expression], is_function: bool) {
        let _ = (name, args, is_function);
    }
}

impl Expression {
    pub fn walk<V: Visitor>(&self, v: &mut V) {
        match self {
            Expression::Unary(_op, e2) => e2.walk(v),
            Expression::Binary(_op, e1, e2) => {
                e1.walk(v);
                e2.walk(v);
            }
            Expression::Symbol(s) => {
                v.symbol(s);
            }
            Expression::ArrayElement(s, es) => {
                v.symbol(s);
                for e in es {
                    e.walk(v);
                }
            }
            Expression::Function(s, es) => {
                v.symbol(s);
                for e in es {
                    e.walk(v);
                }

                v.call(s, es, true);
            }
            Expression::Substring(s, e1, e2) => {
                v.symbol(s);
                if let Some(e) = e1 {
                    e.walk(v);
                }
                if let Some(e) = e2 {
                    e.walk(v);
                }
            }
            Expression::SubstringArrayElement(s, es, e1, e2) => {
                v.symbol(s);
                for e in es {
                    e.walk(v);
                }

                if let Some(e) = e1 {
                    e.walk(v);
                }
                if let Some(e) = e2 {
                    e.walk(v);
                }
            }
            Expression::Constant(_c) => (),
            Expression::ImpliedDo {
                data,
                do_var: _,
                e1,
                e2,
                e3,
            } => {
                for e in data {
                    e.walk(v);
                }
                // Don't visit do_var, it's not a real symbol
                e1.walk(v);
                e2.walk(v);
                if let Some(e) = e3 {
                    e.walk(v);
                }
            }
            Expression::ImpliedDoVar(_do_var) => {
                // Don't visit, it's not a real symbol
            }
        }
    }
}

impl Statement {
    pub fn walk<V: Visitor>(&self, v: &mut V) {
        v.statement(self);
        match self {
            Statement::Assignment(e1, e2) => {
                e1.walk(v);
                e2.walk(v);
            }
            Statement::If { es, bodies } => {
                for (e, body) in es.iter().zip(bodies.iter()) {
                    if let Some(e) = e {
                        e.walk(v);
                    }
                    for s in body {
                        s.walk(v);
                    }
                }
            }
            Statement::Do {
                var,
                e1,
                e2,
                e3,
                body,
            } => {
                v.symbol(var);
                e1.walk(v);
                e2.walk(v);
                if let Some(e3) = e3 {
                    e3.walk(v);
                }
                for s in body {
                    s.walk(v);
                }
            }
            Statement::DoWhile { e, body } => {
                e.walk(v);
                for s in body {
                    s.walk(v);
                }
            }
            Statement::Call(name, args) => {
                v.symbol(name);
                for arg in args {
                    arg.walk(v);
                }

                v.call(name, args, false);
            }
            Statement::Read {
                unit,
                fmt,
                other,
                iolist,
            }
            | Statement::Write {
                unit,
                fmt,
                other,
                iolist,
            } => {
                if let Specifier::Expression(e) = unit {
                    e.walk(v);
                }
                if let Some(Specifier::Expression(e)) = fmt {
                    e.walk(v);
                }
                other.values().for_each(|e| e.walk(v));
                iolist.iter().for_each(|e| e.walk(v));
            }
            Statement::Print { fmt, iolist } => {
                if let Specifier::Expression(e) = fmt {
                    e.walk(v);
                }
                for e in iolist {
                    e.walk(v);
                }
            }
            Statement::Open(specs)
            | Statement::Close(specs)
            | Statement::Inquire(specs)
            | Statement::Backspace(specs)
            | Statement::Endfile(specs)
            | Statement::Rewind(specs) => {
                specs.values().for_each(|e| e.walk(v));
            }
            Statement::Return | Statement::Stop => {}
        }
    }
}

/// Visits every non-constant expression while parsing.
///
/// This is used to record whether a DO-variable is used outside of its DO loops (in which case
/// codegen can't use a loop-local variable for it).
struct DoVarVisitor<'a> {
    do_vars: &'a Vec<String>,
    symbols: &'a mut SymbolTable,
}

impl<'a> DoVarVisitor<'a> {
    fn new(do_vars: &'a Vec<String>, symbols: &'a mut SymbolTable) -> Self {
        Self { do_vars, symbols }
    }
}

impl Visitor for DoVarVisitor<'_> {
    fn symbol(&mut self, s: &String) {
        if !self.do_vars.contains(s) {
            self.symbols.set_used_outside_do(s);
        }
    }
}

#[derive(Debug, Clone)]
pub enum LenSpecification {
    // Length may be unspecified for external function, dummy argument of external procedure,
    // constant with symbolic name
    Unspecified,
    // For char constant: length is derived from PARAMETER that defines the value
    Asterisk,
    Integer(i32),
    IntConstantExpr(Expression),
}

impl LenSpecification {
    fn from(syms: &SymbolTable, value: &Option<grammar::LenSpecification>) -> Self {
        match value {
            None => LenSpecification::Unspecified,
            Some(grammar::LenSpecification::Asterisk) => LenSpecification::Asterisk,
            Some(grammar::LenSpecification::Integer(n)) => LenSpecification::Integer(*n),
            Some(grammar::LenSpecification::IntConstantExpr(e)) => {
                LenSpecification::IntConstantExpr(Expression::from(syms, e).unwrap())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProcedureArgType {
    pub base_type: DataType,
    pub is_array: bool,
    pub mutated: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DataType {
    Integer,
    Real,
    Double,
    // Complex: not supported
    Logical,
    Character,

    /// EXTERNAL with no return type, or dummy argument with no type yet, or generic intrinsic
    Unknown,

    /// SUBROUTINE return type
    Void,

    /// This is used by globan once it knows the full type of a procedure argument.
    Procedure {
        requires_ctx: bool,
        returns_result: bool,
        ret_args: Vec<ProcedureArgType>,
    },
}

impl DataType {
    fn from(_syms: &SymbolTable, value: &grammar::Type) -> Self {
        match value {
            grammar::Type::Integer => DataType::Integer,
            grammar::Type::Real => DataType::Real,
            grammar::Type::Double => DataType::Double,
            grammar::Type::Complex => panic!("not supported"),
            grammar::Type::Logical => DataType::Logical,
            grammar::Type::Character(_) => DataType::Character,
        }
    }
}

/// Dimension declarator `[d1:] d2`, where bounds are integer constant expressions.
#[derive(Debug, Clone)]
pub struct Dimension {
    pub lower: Option<Expression>, // None = default (1)
    pub upper: Option<Expression>, // None = asterisk (unbounded, only valid in last dimension)
}

impl Dimension {
    fn from(syms: &SymbolTable, value: &grammar::Dimension) -> Self {
        Self {
            lower: value.0.clone().map(|e| Expression::from(syms, &e).unwrap()),
            upper: value.1.clone().map(|e| Expression::from(syms, &e).unwrap()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Symbol {
    /// Type of value, or return type of procedure
    pub base_type: DataType,

    /// If base_type is CHARACTER, the length of a string
    pub character_len: Option<LenSpecification>,

    /// Array dimensions (or empty if non-array)
    pub dims: Vec<Dimension>,

    /// Listed in an EXTERNAL statement, meaning it's an external procedure (~function pointer)
    pub external: bool,

    /// Listed as a dummy argument for any entry in this program
    pub darg: bool,

    /// Called as a subroutine or function
    pub called: bool,

    /// Target of an assignment statement, in the given ENTRY names.
    /// (This is hacky, but we need per-ENTRY mutability state, while most symbol state
    /// can be (and some must be) shared between entries)
    pub assigned: HashSet<String>,

    /// Read or written, in the given ENTRY names.
    /// (This is used for optimising out unused variables, and in particular detecting
    /// whether an entry uses any SAVE variables.)
    pub used: HashSet<String>,

    /// Used as a DO-variable
    pub do_var: bool,

    /// Accessed outside of a DO where it's a DO-variable
    pub outside_do: bool,

    /// Declared as a statement function
    pub statement_function: bool,

    /// PARAMETER constant value
    pub parameter: Option<Expression>,

    /// Listed in a SAVE statement
    pub save: bool,
}

impl Symbol {
    /// Some basic error detection, to reduce confusion later in the process
    fn validate(&self) -> Result<()> {
        if !self.assigned.is_empty() {
            if self.external {
                bail!("cannot assign to EXTERNAL");
            }

            if self.statement_function {
                bail!("cannot assign to statement function");
            }

            if self.called {
                bail!("cannot both call and assign to one symbol");
            }

            if self.parameter.is_some() {
                bail!("cannot assign to PARAMETER");
            }
        }

        if self.called && !self.dims.is_empty() {
            bail!("cannot call functions that return arrays");
        }

        if self.save {
            // Can't save dargs, procedures, constants
            if self.darg {
                bail!("cannot SAVE dummy arguments");
            }
            if self.called {
                bail!("cannot SAVE procedures");
            }
            if self.parameter.is_some() {
                bail!("cannot SAVE constants");
            }
            if self.do_var && !self.outside_do {
                // Technically allowed, but don't bother
                bail!("SAVE of DO-vars not supported");
            }
        }

        Ok(())
    }
}

impl Default for Symbol {
    fn default() -> Self {
        Self {
            base_type: DataType::Unknown,
            character_len: None,
            dims: Vec::new(),
            external: false,
            darg: false,
            called: false,
            assigned: HashSet::new(),
            used: HashSet::new(),
            do_var: false,
            outside_do: false,
            statement_function: false,
            parameter: None,
            save: false,
        }
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    // Uses IndexMap to preserve the declaration order of symbols
    symbols: IndexMap<String, Symbol>,

    /// Stack of implied-DO-variables currently in scope.
    /// These take precedence over regular symbols.
    implied_do_vars: Vec<String>,
}

// Convenience methods for manipulating symbols. Generally, symbols are added to the table
// wherever they are first referenced, and we accumulate more information about the symbol
// as parsing continues.
impl SymbolTable {
    fn new() -> Self {
        Self {
            symbols: IndexMap::new(),
            implied_do_vars: vec![],
        }
    }

    /// Get symbol, or insert default
    fn entry(&mut self, name: &str) -> &mut Symbol {
        self.symbols.entry(name.to_owned()).or_default()
    }

    fn set_function(&mut self, name: &str, ty: DataType) {
        if intrinsics::exists(name) {
            // This probably won't work correctly
            error!("intrinsic name as FUNCTION is not supported")
        }

        let sym = self.entry(name);
        sym.base_type = ty;
    }

    fn set_darg(&mut self, name: &str) {
        self.entry(name).darg = true;
    }

    fn set_parameter(&mut self, name: &str, exp: Expression) {
        self.entry(name).parameter = Some(exp);
    }

    fn set_external(&mut self, name: &str) {
        if intrinsics::exists(name) {
            // This should override the intrinsic, but we don't support that
            error!("intrinsic name as EXTERNAL is not supported");
        }

        let sym = self.entry(name);
        if sym.external {
            error!("must only specify EXTERNAL once: {name}");
        }
        sym.external = true;
    }

    fn set_type(&mut self, name: &str, ty: DataType, dims: &[Dimension]) {
        // This is permitted to confirm the type of a specific intrinsic. But the required
        // behaviour for different types, or generic intrinsics, is confusing.
        // We probably need to detect how it's used, to decide what it really is.

        let sym = self.entry(name);
        if !matches!(sym.base_type, DataType::Unknown) {
            error!("must only specify symbol's type once: {name}");
        }
        sym.base_type = ty.clone();
        sym.dims = dims.to_vec();
    }

    fn set_character_len(&mut self, name: &str, len: LenSpecification) {
        self.entry(name).character_len = Some(len);
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        if let Some(sym) = self.symbols.get(name) {
            Some(sym)
        } else {
            None
        }
    }

    fn set_statement_function(&mut self, name: &str) {
        let sym = self.entry(name);
        if sym.statement_function {
            error!("must only declare statement function once: {name}");
        }
        sym.statement_function = true;
    }

    fn set_assigned(&mut self, name: &str, entry: &str) {
        self.entry(name).assigned.insert(entry.to_owned());
    }

    fn set_used(&mut self, name: &str, entry: &str) {
        self.entry(name).used.insert(entry.to_owned());
    }

    fn set_called(&mut self, name: &str) {
        self.entry(name).called = true;
    }

    fn set_do_var(&mut self, name: &str) {
        self.entry(name).do_var = true;
    }

    fn set_used_outside_do(&mut self, name: &str) {
        self.entry(name).outside_do = true;
    }

    fn set_save(&mut self, name: &str) {
        self.entry(name).save = true;
    }

    pub fn contains_key(&self, name: &str) -> bool {
        self.symbols.contains_key(name)
    }

    /// Is symbol defined and an array type
    pub fn is_array(&self, name: &String) -> bool {
        if let Some(sym) = self.symbols.get(name) {
            !sym.dims.is_empty()
        } else {
            false
        }
    }

    /// Is symbol defined and a character type
    pub fn is_character(&self, name: &String) -> bool {
        if let Some(sym) = self.symbols.get(name) {
            matches!(sym.base_type, DataType::Character)
        } else {
            false
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&String, &Symbol)> {
        self.symbols.iter()
    }
}

// Implement the statement order from F77 section 3.5 figure 1
enum ParseState {
    Implicit,
    OtherSpec,
    StatementFunction,
    Executable,
    End,
}

pub struct Parser {
    /// Latest entry, currently being parsed
    entry: Option<Entry>,

    /// Completed entries
    entries: Vec<Entry>,

    state: ParseState,

    symbols: SymbolTable,

    /// Top of stack is the currently-active scope. Statements are added there.
    /// IF/DO pushes a new scope onto the stack after the IF/DO statement,
    /// so END IF/DO etc can pop the stack then merge it into the last IF/DO statement
    statements: Vec<Vec<Statement>>,

    /// Used for detecting RETURN outside of IF blocks
    depth: i32,

    /// DO-variables currently in scope
    do_vars: Vec<String>,

    /// DATA statements
    datas: Vec<DataStatement>,

    /// Whether there was a SAVE with no symbol list, so we should save everything possible
    save_all: bool,
}

impl Default for Parser {
    fn default() -> Self {
        Self::new()
    }
}

impl Parser {
    pub fn new() -> Self {
        Self {
            entry: None,
            state: ParseState::Implicit,
            symbols: SymbolTable::new(),
            statements: vec![vec![]],
            depth: 0,
            entries: vec![],
            do_vars: vec![],
            datas: vec![],
            save_all: false,
        }
    }

    pub fn parse(mut self, source: Vec<(SourceLoc, grammar::Statement)>) -> Result<ProgramUnit> {
        let mut pu_ty = None;

        // Merge the INCLUDEs into a single list of statements
        let mut source_complete = Vec::new();
        for (loc, s) in source {
            match s {
                grammar::Statement::Include(_, lines) => {
                    for line in lines {
                        source_complete.push((line.0, line.1));
                    }
                }
                _ => source_complete.push((loc, s)),
            }
        }

        let mut source_iter = source_complete.into_iter();

        'LINES: for (_loc, line) in &mut source_iter {
            match line {
                grammar::Statement::Comment(_) | grammar::Statement::Blank => (),
                grammar::Statement::Program(name) => {
                    pu_ty = Some(ProgramUnitType::Program);
                    self.entry = Some(Entry {
                        name,
                        dargs: Vec::new(),
                        body: Vec::new(),
                    });
                    break 'LINES;
                }
                grammar::Statement::Function(func_type, name, mut dargs) => {
                    pu_ty = Some(ProgramUnitType::Function);
                    dargs.iter().for_each(|d| self.symbols.set_darg(d));

                    let func_type = func_type
                        .context("FUNCTION must have explicit type; implicit not supported")?;

                    let ret_type = DataType::from(&self.symbols, &func_type);

                    // If a function returns "CHARACTER*(*)", we have to depend on the caller
                    // providing the size of the array to write into. To simplify things, create
                    // an extra darg for the caller to pass that array, and we'll handle it
                    // specially in calls. (Though currently we only handle the special case
                    // of "OUT = CHAR_FUNCTION(...)".)
                    if let grammar::Type::Character(len) = func_type {
                        let len = &len.clone().or(Some(grammar::LenSpecification::Integer(1)));

                        dargs.push(name.clone());
                        self.symbols.set_darg(&name);
                        self.symbols.set_assigned(&name, &name);
                        self.symbols.set_type(&name, ret_type, &[]);
                        self.symbols
                            .set_character_len(&name, LenSpecification::from(&self.symbols, len));
                    } else {
                        self.symbols.set_function(&name, ret_type);
                    }

                    self.entry = Some(Entry {
                        name,
                        dargs: dargs.iter().map(DummyArg::new).collect::<Result<_>>()?,
                        body: Vec::new(),
                    });

                    break 'LINES;
                }
                grammar::Statement::Subroutine(name, dargs) => {
                    pu_ty = Some(ProgramUnitType::Subroutine);
                    dargs.iter().for_each(|d| self.symbols.set_darg(d));
                    self.entry = Some(Entry {
                        name,
                        dargs: dargs.iter().map(DummyArg::new).collect::<Result<_>>()?,
                        body: Vec::new(),
                    });
                    break 'LINES;
                }

                _ => bail!("expected PROGRAM/FUNCTION/SUBROUTINE at start of file"),
            }
        }

        for (loc, line) in &mut source_iter {
            self.parse_statement(loc, &line)?;
        }

        if !matches!(self.state, ParseState::End) {
            bail!("missing END");
        }

        assert!(self.entry.is_none(), "must have closed entry before EOF");

        for (loc, line) in &mut source_iter {
            match line {
                // Comments are not technically allowed after END, but some files do it anyway
                grammar::Statement::Blank | grammar::Statement::Comment(..) => (),
                _ => bail!("non-blank line after END: {}: {:?}", loc, line),
            }
        }

        // Detect all used/called symbols
        struct UsedVisitor<'a> {
            entry_name: &'a str,
            symbols: &'a mut SymbolTable,
        }

        impl Visitor for UsedVisitor<'_> {
            fn symbol(&mut self, name: &String) {
                self.symbols.set_used(name, self.entry_name);
            }

            fn call(&mut self, name: &String, _args: &[Expression], _is_function: bool) {
                self.symbols.set_called(name);
            }
        }

        for entry in &self.entries {
            let mut v = UsedVisitor {
                entry_name: &entry.name,
                symbols: &mut self.symbols,
            };
            entry.body.iter().for_each(|s| s.walk(&mut v));
        }

        // Apply "SAVE" to relevant symbols
        if self.save_all {
            for (_, sym) in &mut self.symbols.symbols {
                // Can't save dargs, procedures, constants
                if sym.darg || sym.called || sym.parameter.is_some() {
                    continue;
                }

                // Don't want to save local DO-vars
                if sym.do_var && !sym.outside_do {
                    continue;
                }

                sym.save = true;
            }
        }

        // Ensure any name used in DATA is also SAVE
        for data in &self.datas {
            struct NlistVisitor<'a> {
                symbols: &'a mut SymbolTable,
            }

            impl Visitor for NlistVisitor<'_> {
                fn symbol(&mut self, name: &String) {
                    if let Some(sym) = self.symbols.get(name) {
                        if !sym.save && sym.parameter.is_none() {
                            // FORTRAN 77 says DATA will only initialise at program start,
                            // and if not SAVE it will become undefined on return, which
                            // is a bug if the procedure is called more than once.
                            warn!(
                                "symbol {name} used in DATA and not SAVE; undefined behaviour if procedure called twice"
                            );

                            // Fortran 90 says any DATA variables are implicitly SAVE, so let's do that
                            self.symbols.set_save(name);
                        }
                    } else {
                        error!("symbol {name} used in DATA and not defined");
                    }
                }
            }

            let mut v = NlistVisitor {
                symbols: &mut self.symbols,
            };
            data.nlist.iter().for_each(|s| s.walk(&mut v));
        }

        let pu = ProgramUnit {
            ty: pu_ty.unwrap(),
            symbols: self.symbols,
            entries: self.entries,
            datas: self.datas,
        };

        for e in &pu.entries {
            for d in &e.dargs {
                if !pu.symbols.contains_key(&d.name) {
                    bail!("dummy argument {} must have explicit type", d.name)
                }
            }
        }

        for (name, sym) in &pu.symbols.symbols {
            sym.validate()
                .with_context(|| format!("invalid use of symbol {name}: {sym:?}"))?;
        }

        Ok(pu)
    }

    /// Parse any statement after the opening PROGRAM/FUNCTION/SUBROUTINE
    fn parse_statement(&mut self, loc: SourceLoc, line: &grammar::Statement) -> Result<()> {
        match &line {
            grammar::Statement::Comment(_) | grammar::Statement::Blank => (),

            grammar::Statement::Program(..)
            | grammar::Statement::Function(..)
            | grammar::Statement::Subroutine(..) => {
                bail!(
                    "PROGRAM/FUNCTION/SUBROUTINE only valid at start: {}: {:?}",
                    loc,
                    line
                );
            }

            // FORMAT/ENTRY statements (but we don't support FORMAT)
            grammar::Statement::Entry(name, dargs) => {
                // We don't support entry into the middle of a function, because that's hard
                // to emulate in sensible language
                if self.entry.is_some() {
                    bail!("ENTRY must be after an unconditional RETURN");
                }

                dargs.iter().for_each(|d| self.symbols.set_darg(d));
                self.entry = Some(Entry {
                    name: name.clone(),
                    dargs: dargs.iter().map(DummyArg::new).collect::<Result<_>>()?,
                    body: Vec::new(),
                });

                self.statements.push(Vec::new());
            }

            // PARAMETER statements
            grammar::Statement::Parameter(params) => {
                if !matches!(self.state, ParseState::Implicit | ParseState::OtherSpec) {
                    bail!("PARAMETER invalid here: {}: {:?}", loc, line);
                }

                for (name, expr) in params {
                    self.symbols
                        .set_parameter(name, Expression::from(&self.symbols, expr)?);
                }
            }

            // DATA statements
            grammar::Statement::Data(datas) => {
                if matches!(self.state, ParseState::Implicit | ParseState::OtherSpec) {
                    self.state = ParseState::StatementFunction;
                }

                for grammar::DataLists { nlist, clist } in datas {
                    self.datas.push(DataStatement {
                        nlist: nlist
                            .iter()
                            .map(|n| Expression::from_dataname(&mut self.symbols, n))
                            .collect::<Result<_>>()?,
                        clist: clist
                            .iter()
                            .map(|(r, c)| {
                                let r = match r {
                                    Some(r) => Some(Expression::from(&self.symbols, r)?),
                                    None => None,
                                };
                                let c = Expression::from(&self.symbols, c)?;
                                Ok((r, c))
                            })
                            .collect::<Result<_>>()?,
                    });
                }
            }

            // IMPLICIT statements
            grammar::Statement::ImplicitNone => {
                if !matches!(self.state, ParseState::Implicit) {
                    bail!("IMPLICIT invalid here: {}, {:?}", loc, line);
                }
                self.state = ParseState::OtherSpec;

                // Don't record this, we just assume no code depends on implicit types
            }

            // Specification statements (other than IMPLICIT)
            // Unsupported:
            //   grammar::Statement::Dimension(..)
            //   grammar::Statement::Common(..)
            //   grammar::Statement::Intrinsic(..)
            grammar::Statement::Save(names) => {
                if !matches!(self.state, ParseState::Implicit | ParseState::OtherSpec) {
                    // spicelib/gfposc.f has SAVE after DATA, which violates the standard,
                    // so we handle that specially
                    if matches!(self.state, ParseState::StatementFunction) {
                        // allow this, don't change state
                    } else {
                        bail!("specification statements invalid here: {}: {:?}", loc, line);
                    }
                } else {
                    self.state = ParseState::OtherSpec;
                }

                if names.is_empty() {
                    self.save_all = true;
                } else {
                    for name in names {
                        self.symbols.set_save(name);
                    }
                }
            }

            grammar::Statement::Equivalence(..) => {
                if !matches!(self.state, ParseState::Implicit | ParseState::OtherSpec) {
                    bail!("specification statements invalid here: {}: {:?}", loc, line);
                } else {
                    self.state = ParseState::OtherSpec;
                }

                warn!("EQUIVALENCE not supported");
            }

            grammar::Statement::External(vs) => {
                if !matches!(self.state, ParseState::Implicit | ParseState::OtherSpec) {
                    bail!("specification statements invalid here: {}: {:?}", loc, line);
                } else {
                    self.state = ParseState::OtherSpec;
                }

                for v in vs {
                    self.symbols.set_external(v);
                }
            }

            grammar::Statement::Type(ty, vs) => {
                if !matches!(self.state, ParseState::Implicit | ParseState::OtherSpec) {
                    bail!("specification statements invalid here: {}: {:?}", loc, line);
                } else {
                    self.state = ParseState::OtherSpec;
                }

                for v in vs {
                    let dims: Vec<_> = v
                        .dims
                        .iter()
                        .map(|d| Dimension::from(&self.symbols, d))
                        .collect();

                    let mut visitor = DoVarVisitor::new(&self.do_vars, &mut self.symbols);
                    dims.iter().for_each(|dim| {
                        if let Some(e) = &dim.lower {
                            e.walk(&mut visitor);
                        }
                        if let Some(e) = &dim.upper {
                            e.walk(&mut visitor);
                        }
                    });

                    self.symbols
                        .set_type(&v.name, DataType::from(&self.symbols, ty), &dims);
                }
            }

            grammar::Statement::TypeCharacter(len, vs) => {
                if !matches!(self.state, ParseState::Implicit | ParseState::OtherSpec) {
                    bail!("specification statements invalid here: {}: {:?}", loc, line);
                } else {
                    self.state = ParseState::OtherSpec;
                }

                for (v, vlen) in vs {
                    let dims: Vec<_> = v
                        .dims
                        .iter()
                        .map(|d| Dimension::from(&self.symbols, d))
                        .collect();

                    let mut visitor = DoVarVisitor::new(&self.do_vars, &mut self.symbols);
                    dims.iter().for_each(|dim| {
                        if let Some(e) = &dim.lower {
                            e.walk(&mut visitor);
                        }
                        if let Some(e) = &dim.upper {
                            e.walk(&mut visitor);
                        }
                    });

                    // In `CHARACTER [*len ] v[*len], v[*len]...`, the first `len` is the default
                    // when a specific `v` doesn't give a length. If no length is specified, default is 1
                    let real_len = &vlen
                        .clone()
                        .or(len.clone())
                        .or(Some(grammar::LenSpecification::Integer(1)));

                    self.symbols.set_type(&v.name, DataType::Character, &dims);

                    self.symbols.set_character_len(
                        &v.name,
                        LenSpecification::from(&self.symbols, real_len),
                    );
                }
            }

            // Statement function statements. Ambiguously parsed as Assignment; check if we're
            // in the appropriate state and it wasn't declared as an array
            grammar::Statement::Assignment(grammar::DataName::ArrayElement(name, _), e)
                if matches!(
                    self.state,
                    ParseState::Implicit | ParseState::OtherSpec | ParseState::StatementFunction
                ) && !self.symbols.is_array(name) =>
            {
                // We should switch to StatementFunction here, but e.g. tspice/f_polyds.f uses
                // DOUBLE PRECISION after a statement function, so allow that
                self.state = ParseState::OtherSpec;

                // warn!("Unsupported statement function {name}: {e:?}");

                // TODO: implement this properly
                // (Maybe just expand it out, so codegen doesn't have to care? Otherwise
                // turn into a function/macro)

                self.symbols.set_statement_function(name);

                let e = Expression::from(&self.symbols, e)?;

                let mut visitor = DoVarVisitor::new(&self.do_vars, &mut self.symbols);
                e.walk(&mut visitor);
            }

            // Executable statements
            grammar::Statement::Assignment(..)
            | grammar::Statement::Continue
            | grammar::Statement::Stop
            | grammar::Statement::Read(..)
            | grammar::Statement::Write(..)
            | grammar::Statement::Print(..)
            | grammar::Statement::Open(..)
            | grammar::Statement::Close(..)
            | grammar::Statement::Inquire(..)
            | grammar::Statement::Backspace(..)
            | grammar::Statement::Endfile(..)
            | grammar::Statement::Rewind(..)
            | grammar::Statement::Call(..)
            | grammar::Statement::Return(..) => {
                self.state = ParseState::Executable;

                if let Some(st) = self.parse_basic_statement(loc, line)? {
                    self.statements.last_mut().unwrap().push(st);
                }
            }

            grammar::Statement::Do(label, var, e1, e2, e3) => {
                self.state = ParseState::Executable;

                if label.is_some() {
                    bail!("DO label not supported");
                }

                let e1 = Expression::from(&self.symbols, e1)?;
                let e2 = Expression::from(&self.symbols, e2)?;
                let e3 = match e3 {
                    None => None,
                    Some(e) => Some(Expression::from(&self.symbols, e)?),
                };

                let mut visitor = DoVarVisitor::new(&self.do_vars, &mut self.symbols);
                e1.walk(&mut visitor);
                e2.walk(&mut visitor);
                e3.iter().for_each(|e| e.walk(&mut visitor));

                self.symbols.set_do_var(var);
                if self.do_vars.contains(var) {
                    bail!("can't use same DO-variable in nested loops");
                }
                self.do_vars.push(var.clone());

                self.statements.last_mut().unwrap().push(Statement::Do {
                    var: var.clone(),
                    e1,
                    e2,
                    e3,
                    body: Vec::new(),
                });

                self.statements.push(Vec::new());

                self.depth += 1;
            }

            grammar::Statement::DoWhile(label, e) => {
                self.state = ParseState::Executable;

                if label.is_some() {
                    bail!("DO WHILE label not supported");
                }

                let e = Expression::from(&self.symbols, e)?;

                let mut visitor = DoVarVisitor::new(&self.do_vars, &mut self.symbols);
                e.walk(&mut visitor);

                self.statements
                    .last_mut()
                    .unwrap()
                    .push(Statement::DoWhile {
                        e,
                        body: Vec::new(),
                    });

                self.statements.push(Vec::new());

                self.depth += 1;
            }

            grammar::Statement::EndDo => {
                self.state = ParseState::Executable;

                let body = self.statements.pop().unwrap();
                match self.statements.last_mut().unwrap().last_mut().unwrap() {
                    Statement::Do { body: s_body, .. } => {
                        *s_body = body;
                        self.do_vars.pop();
                    }
                    Statement::DoWhile { body: s_body, .. } => {
                        *s_body = body;
                    }
                    _ => bail!("END DO without matching DO"),
                }

                self.depth -= 1;
                assert!(self.depth >= 0);
            }

            grammar::Statement::LogicalIf(e, body) => {
                self.state = ParseState::Executable;

                let e = Expression::from(&self.symbols, e)?;

                let mut visitor = DoVarVisitor::new(&self.do_vars, &mut self.symbols);
                e.walk(&mut visitor);

                // Temporarily increment depth in case there's a RETURN in the body
                self.depth += 1;

                if let Some(st) = self.parse_basic_statement(loc, body)? {
                    self.statements.last_mut().unwrap().push(Statement::If {
                        es: vec![Some(e)],
                        bodies: vec![vec![st]],
                    });
                } else {
                    bail!("failed to parse logical IF body");
                }

                self.depth -= 1;
            }

            grammar::Statement::BlockIf(e) => {
                self.state = ParseState::Executable;

                let e = Expression::from(&self.symbols, e)?;

                let mut visitor = DoVarVisitor::new(&self.do_vars, &mut self.symbols);
                e.walk(&mut visitor);

                self.statements.last_mut().unwrap().push(Statement::If {
                    es: vec![Some(e)],
                    bodies: vec![],
                });

                self.statements.push(Vec::new());

                self.depth += 1;
            }
            grammar::Statement::ElseIf(e) => {
                self.state = ParseState::Executable;

                let e = Expression::from(&self.symbols, e)?;

                let mut visitor = DoVarVisitor::new(&self.do_vars, &mut self.symbols);
                e.walk(&mut visitor);

                let body = self.statements.pop().unwrap();
                match self.statements.last_mut().unwrap().last_mut().unwrap() {
                    Statement::If { es, bodies } => {
                        es.push(Some(e));
                        bodies.push(body);
                    }
                    _ => bail!("ELSE IF without matching IF"),
                }

                self.statements.push(Vec::new());
            }
            grammar::Statement::Else => {
                self.state = ParseState::Executable;

                let body = self.statements.pop().unwrap();
                match self.statements.last_mut().unwrap().last_mut().unwrap() {
                    Statement::If { es, bodies } => {
                        es.push(None);
                        bodies.push(body);
                    }
                    _ => bail!("ELSE without matching IF"),
                }

                self.statements.push(Vec::new());
            }
            grammar::Statement::EndIf => {
                self.state = ParseState::Executable;

                let body = self.statements.pop().unwrap();
                match self.statements.last_mut().unwrap().last_mut().unwrap() {
                    Statement::If { bodies, .. } => {
                        bodies.push(body);
                    }
                    _ => bail!("END IF without matching IF"),
                }

                self.depth -= 1;
                assert!(self.depth >= 0);
            }

            // INCLUDE should have been expanded already
            grammar::Statement::Include(..) => panic!("unexpected INCLUDE"),

            grammar::Statement::End => {
                self.state = ParseState::End;

                if self.depth != 0 {
                    bail!("END while inside IF/DO");
                }

                if self.entry.is_some() {
                    // implicit RETURN

                    let mut e = self.entry.take().unwrap();
                    assert_eq!(self.statements.len(), 1);
                    e.body = self.statements.pop().unwrap();
                    self.entries.push(e);
                }

                return Ok(());
            }
        }

        Ok(())
    }

    // Any statement that could appear in logical "IF (e) st", i.e. executable statements minus DO/IF/etc
    fn parse_basic_statement(
        &mut self,
        loc: SourceLoc,
        line: &grammar::Statement,
    ) -> Result<Option<Statement>> {
        Ok(match line {
            grammar::Statement::Assignment(name, e) => {
                let name_sym = match name {
                    grammar::DataName::Variable(s) => s,
                    grammar::DataName::ArrayElement(s, _) => s,
                    grammar::DataName::Substring(s, _, _) => s,
                    grammar::DataName::SubstringArrayElement(s, _, _, _) => s,
                    grammar::DataName::ImpliedDo(..) => bail!("implied-DO invalid in assignment"),
                    grammar::DataName::Expression(..) => bail!("expression invalid in assignment"),
                };

                self.symbols
                    .set_assigned(name_sym, &self.entry.as_ref().unwrap().name);

                if self.do_vars.contains(name_sym) {
                    bail!("assigning to active DO-variable");
                }

                // Special case for functions that return CHARACTER: they output to an
                // extra final darg, so call them like a subroutine with the assignment target
                // as that final argument
                if let grammar::Expression::ArrayElementOrFunction(func_name, es) = e {
                    if !self.symbols.is_array(func_name) && self.symbols.is_character(func_name) {
                        self.symbols.set_called(func_name);

                        let target = Expression::from_dataname(&mut self.symbols, name)?;

                        let mut args: Vec<_> = es
                            .iter()
                            .map(|e| Expression::from(&self.symbols, e))
                            .collect::<Result<_>>()?;
                        args.push(target);

                        let mut visitor = DoVarVisitor::new(&self.do_vars, &mut self.symbols);
                        args.iter().for_each(|e| e.walk(&mut visitor));

                        return Ok(Some(Statement::Call(func_name.clone(), args)));
                    }
                }

                let name = Expression::from_dataname(&mut self.symbols, name)?;
                let exp = Expression::from(&self.symbols, e)?;

                let mut visitor = DoVarVisitor::new(&self.do_vars, &mut self.symbols);
                name.walk(&mut visitor);
                exp.walk(&mut visitor);

                Some(Statement::Assignment(name, exp))
            }

            // CONTINUE is a noop
            grammar::Statement::Continue => None,

            grammar::Statement::Stop => Some(Statement::Stop),

            grammar::Statement::Read(specs, iolist)
            | grammar::Statement::Write(specs, iolist)
            | grammar::Statement::Print(specs, iolist) => {
                let mut other = HashMap::new();

                let unit = match specs.0.get("UNIT") {
                    None => {
                        if !matches!(line, grammar::Statement::Print(..)) {
                            bail!("READ/WRITE statement must specify UNIT");
                        }
                        None
                    }
                    Some(grammar::SpecifierValue::Asterisk) => Some(Specifier::Asterisk),
                    Some(grammar::SpecifierValue::Expression(e)) => {
                        Some(Specifier::Expression(Expression::from(&self.symbols, e)?))
                    }
                };

                let fmt = match specs.0.get("FMT") {
                    None => None,
                    Some(grammar::SpecifierValue::Asterisk) => Some(Specifier::Asterisk),
                    Some(grammar::SpecifierValue::Expression(e)) => {
                        Some(Specifier::Expression(Expression::from(&self.symbols, e)?))
                    }
                };

                for (k, v) in &specs.0 {
                    if k == "UNIT" || k == "FMT" {
                        // Already handled
                        continue;
                    }
                    match v {
                        grammar::SpecifierValue::Asterisk => {
                            bail!("asterisk specifier only allowed in UNIT=*")
                        }
                        grammar::SpecifierValue::Expression(e) => {
                            let e = Expression::from(&self.symbols, e)?;

                            if k == "IOSTAT" {
                                match &e {
                                    Expression::Symbol(name)
                                    | Expression::ArrayElement(name, _) => {
                                        self.symbols
                                            .set_assigned(name, &self.entry.as_ref().unwrap().name);
                                    }
                                    _ => {
                                        bail!("IOSTAT must be an integer variable or array element")
                                    }
                                }
                            }

                            other.insert(k.to_owned(), e);
                        }
                    }
                }

                let iolist: Vec<_> = iolist
                    .iter()
                    .map(|io| Expression::from_dataname(&mut self.symbols, io))
                    .collect::<Result<_>>()?;

                let mut visitor = DoVarVisitor::new(&self.do_vars, &mut self.symbols);
                other.values().for_each(|e| e.walk(&mut visitor));
                iolist.iter().for_each(|e| e.walk(&mut visitor));

                Some(match line {
                    grammar::Statement::Read(..) => Statement::Read {
                        unit: unit.unwrap(),
                        fmt,
                        other,
                        iolist,
                    },
                    grammar::Statement::Write(..) => Statement::Write {
                        unit: unit.unwrap(),
                        fmt,
                        other,
                        iolist,
                    },
                    grammar::Statement::Print(..) => Statement::Print {
                        fmt: fmt.unwrap(),
                        iolist,
                    },
                    _ => panic!(),
                })
            }

            grammar::Statement::Open(specs)
            | grammar::Statement::Close(specs)
            | grammar::Statement::Inquire(specs)
            | grammar::Statement::Backspace(specs)
            | grammar::Statement::Endfile(specs)
            | grammar::Statement::Rewind(specs) => {
                let mut ast_specs = HashMap::new();

                let unit = specs.0.get("UNIT");
                match unit {
                    None => {
                        if !matches!(line, grammar::Statement::Inquire(..)) {
                            bail!("IO statement must specify UNIT");
                        }
                    }
                    Some(grammar::SpecifierValue::Asterisk) => {
                        bail!("UNIT=* only allowed in READ, WRITE");
                    }
                    Some(grammar::SpecifierValue::Expression(e)) => {
                        ast_specs.insert("UNIT".to_owned(), Expression::from(&self.symbols, e)?);
                    }
                }

                for (k, v) in &specs.0 {
                    if k == "UNIT" {
                        // Already handled
                        continue;
                    }
                    match v {
                        grammar::SpecifierValue::Asterisk => {
                            bail!("asterisk specifier only allowed in UNIT=*")
                        }
                        grammar::SpecifierValue::Expression(e) => {
                            let e = Expression::from(&self.symbols, e)?;

                            if k == "IOSTAT" {
                                match &e {
                                    Expression::Symbol(name)
                                    | Expression::ArrayElement(name, _) => {
                                        self.symbols
                                            .set_assigned(name, &self.entry.as_ref().unwrap().name);
                                    }
                                    _ => {
                                        bail!("IOSTAT must be an integer variable or array element")
                                    }
                                }
                            }

                            ast_specs.insert(k.to_owned(), e);
                        }
                    }
                }

                let mut visitor = DoVarVisitor::new(&self.do_vars, &mut self.symbols);
                ast_specs.values().for_each(|e| e.walk(&mut visitor));

                Some(match line {
                    grammar::Statement::Open(..) => Statement::Open(ast_specs),
                    grammar::Statement::Close(..) => Statement::Close(ast_specs),
                    grammar::Statement::Inquire(..) => Statement::Inquire(ast_specs),
                    grammar::Statement::Backspace(..) => Statement::Backspace(ast_specs),
                    grammar::Statement::Endfile(..) => Statement::Endfile(ast_specs),
                    grammar::Statement::Rewind(..) => Statement::Rewind(ast_specs),
                    _ => panic!(),
                })
            }

            grammar::Statement::Call(name, args) => {
                self.symbols.set_called(name);

                let args: Vec<_> = args
                    .iter()
                    .map(|e| Expression::from(&self.symbols, e))
                    .collect::<Result<_>>()?;

                let mut visitor = DoVarVisitor::new(&self.do_vars, &mut self.symbols);
                args.iter().for_each(|e| e.walk(&mut visitor));

                Some(Statement::Call(name.clone(), args))
            }

            grammar::Statement::Return(label) => {
                if label.is_some() {
                    bail!("alternate returns are not supported");
                }

                if self.depth == 0 {
                    // If we're returning from outside any DO/IF block, then prepare for a new ENTRY
                    let mut e = self.entry.take().unwrap();
                    assert_eq!(self.statements.len(), 1);
                    e.body = self.statements.pop().unwrap();
                    self.entries.push(e);
                    None
                } else {
                    // Otherwise it's an early return
                    Some(Statement::Return)
                }
            }

            _ => bail!("unrecognised basic statement: {}: {:?}", loc, line),
        })
    }
}
