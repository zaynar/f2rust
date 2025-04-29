//! Converts the `grammar` output into an abstract syntax tree, for a single program unit
//! (i.e. one PROGRAM/FUNCTION/SUBROUTINE with potentially multiple ENTRY points).
//!
//! We also set up the symbol table with various information on how each symbol is declared
//! and used.

use std::collections::HashSet;

use anyhow::{Context, Result, bail};
use indexmap::IndexMap;
use log::{error, warn};

use crate::grammar::DataName;
use crate::util::{parse_header_comments, safe_identifier};
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

pub type Specifiers = IndexMap<String, Expression>;

/// Executable statements
#[derive(Debug, Clone)]
pub enum Statement {
    Comment(Vec<(String, bool)>),
    Blank,

    Assignment(Expression, Expression),

    // Contains all the IF / ELSE IF / ELSE cases.
    // ELSE has es[n]==None, all others are Some.
    // es.len() == bodies.len(), except during construction.
    If {
        es: Vec<Option<Expression>>,
        bodies: Vec<Vec<(SourceLoc, Statement)>>,
    },

    Do {
        var: String,
        e1: Expression,
        e2: Expression,
        e3: Option<Expression>,
        body: Vec<(SourceLoc, Statement)>,
    },

    DoWhile {
        e: Expression,
        body: Vec<(SourceLoc, Statement)>,
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

    pub statement_functions: Vec<StatementFunction>,

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
    pub loc: SourceLoc,
    pub name: String,
    pub pre_comments: Vec<Vec<(String, bool)>>,
    pub post_comments: Vec<Vec<(String, bool)>>,
    pub dargs: Vec<String>,
    pub body: Vec<(SourceLoc, Statement)>,

    pub api_name: Option<String>,
    pub comment_sections: IndexMap<String, Vec<String>>,
}

#[derive(Debug)]
pub struct StatementFunction {
    pub loc: SourceLoc,
    pub name: String,
    pub dargs: Vec<String>,
    pub captured: Vec<String>,
    pub body: Expression,
}

#[derive(Debug)]
pub struct DataStatement {
    pub loc: SourceLoc,

    /// List of names to assign to
    pub nlist: Vec<Expression>,

    /// List of reps*constval
    pub clist: Vec<(Option<Expression>, Expression)>,
}

// Construct Expression from grammar
impl Expression {
    fn from(syms: &SymbolTable, e: &grammar::Expression) -> Result<Self> {
        Ok(match e {
            grammar::Expression::Unary(op, e2) => {
                Expression::Unary(op.clone(), Box::new(Self::from(syms, e2)?))
            }
            grammar::Expression::Binary(op, e1, e2) => {
                let e1 = Self::from(syms, e1)?;
                let e2 = Self::from(syms, e2)?;
                if let (
                    grammar::BinaryOp::Concat,
                    Expression::Constant(grammar::Constant::Character(s1)),
                    Expression::Constant(grammar::Constant::Character(s2)),
                ) = (op, &e1, &e2)
                {
                    // Simplify string literals that were split into multiple concatenated lines
                    Expression::Constant(grammar::Constant::Character(s1.clone() + s2))
                } else {
                    Expression::Binary(op.clone(), Box::new(e1), Box::new(e2))
                }
            }
            grammar::Expression::Symbol(s) => {
                if syms.implied_do_vars.contains(s) {
                    Expression::ImpliedDoVar(s.clone())
                } else if let Some(alias) = syms.get(s).and_then(|s| s.alias.clone()) {
                    alias
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
                    let mut dargs = es.clone();

                    // If this is a statement function call, we need to pass the captured variables too
                    if let Some(captures) = syms.get(s).and_then(|s| s.statement_function.as_ref())
                    {
                        dargs.extend(captures.iter().map(|c| Expression::Symbol(c.clone())));
                    }

                    Expression::Function(s.clone(), dargs)
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
            grammar::DataName::Variable(s) => {
                if let Some(alias) = syms.get(s).and_then(|s| s.alias.clone()) {
                    alias
                } else {
                    Expression::Symbol(s.clone())
                }
            }
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
            Statement::Comment(..) | Statement::Blank => (),
            Statement::Assignment(e1, e2) => {
                e1.walk(v);
                e2.walk(v);
            }
            Statement::If { es, bodies } => {
                for (e, body) in es.iter().zip(bodies.iter()) {
                    if let Some(e) = e {
                        e.walk(v);
                    }
                    for (_loc, s) in body {
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
                for (_loc, s) in body {
                    s.walk(v);
                }
            }
            Statement::DoWhile { e, body } => {
                e.walk(v);
                for (_loc, s) in body {
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
    /// Source location where this was first declared
    pub loc: SourceLoc,

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

    /// Used as an actual argument in any call
    pub used_as_arg: bool,

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

    /// Declared as a statement function, with captured variables
    pub statement_function: Option<Vec<String>>,

    /// PARAMETER constant value
    pub parameter: Option<Expression>,

    /// Whether a PARAMETER is a SPICE public API
    pub parameter_public: bool,

    /// Listed in a SAVE statement
    pub save: bool,

    /// EQUIVALENCE effectively defines this as an alias for an array element
    pub alias: Option<Expression>,

    /// EQUIVALENCE defines this as sharing storage with another equal-sized object
    pub equivalence: Option<String>,
}

impl Symbol {
    /// Some basic error detection, to reduce confusion later in the process
    fn validate(&self) -> Result<()> {
        if !self.assigned.is_empty() {
            if self.external {
                bail!("cannot assign to EXTERNAL");
            }

            if self.statement_function.is_some() {
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
            loc: SourceLoc {
                file: "(unknown)".to_owned(),
                line: 0,
                included: false,
            },
            base_type: DataType::Unknown,
            character_len: None,
            dims: Vec::new(),
            external: false,
            darg: false,
            called: false,
            used_as_arg: false,
            assigned: HashSet::new(),
            used: HashSet::new(),
            do_var: false,
            outside_do: false,
            statement_function: None,
            parameter: None,
            parameter_public: false,
            save: false,
            alias: None,
            equivalence: None,
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

    fn set_loc(&mut self, name: &str, loc: &SourceLoc) {
        self.entry(name).loc = loc.clone();
    }

    fn set_darg(&mut self, name: &str) {
        self.entry(name).darg = true;
    }

    fn set_parameter(&mut self, name: &str, exp: Expression, public: bool) {
        self.entry(name).parameter = Some(exp);
        self.entry(name).parameter_public = public;
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

    fn set_statement_function(&mut self, name: &str, captured: &[String]) {
        let sym = self.entry(name);
        if sym.statement_function.is_some() {
            error!("must only declare statement function once: {name}");
        }
        sym.statement_function = Some(captured.to_vec());
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

    fn set_used_as_arg(&mut self, name: &str) {
        self.entry(name).used_as_arg = true;
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

    fn set_alias(&mut self, name: &str, e: Expression) {
        self.entry(name).alias = Some(e);
    }

    fn set_equivalence(&mut self, name: &str, equiv: &str) {
        self.entry(name).equivalence = Some(equiv.to_owned());
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

    statement_functions: Vec<StatementFunction>,

    state: ParseState,

    symbols: SymbolTable,

    /// Top of stack is the currently-active scope. Statements are added there.
    /// IF/DO pushes a new scope onto the stack after the IF/DO statement,
    /// so END IF/DO etc can pop the stack then merge it into the last IF/DO statement
    statements: Vec<Vec<(SourceLoc, Statement)>>,

    /// Used for detecting RETURN outside of IF blocks
    depth: i32,

    /// DO-variables currently in scope
    do_vars: Vec<String>,

    /// DATA statements
    datas: Vec<DataStatement>,

    /// Whether there was a SAVE with no symbol list, so we should save everything possible
    save_all: bool,

    /// Current comment block; flag indicates SPICE comment sections
    comment: Vec<(String, bool)>,

    /// SPICE comment section heading
    comment_section: Option<String>,
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
            statement_functions: vec![],
            do_vars: vec![],
            datas: vec![],
            save_all: false,
            comment: vec![],
            comment_section: None,
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

        // Sequence of comment blocks (separated by blank lines)
        let mut pre_comments = vec![vec![]];

        let mut source_iter = source_complete.into_iter();

        // Parse up to the PROGRAM/FUNCTION/SUBROUTINE line
        'LINES: for (loc, line) in &mut source_iter {
            match line {
                grammar::Statement::Comment(c) => {
                    pre_comments.last_mut().unwrap().push((c, false));
                }
                grammar::Statement::Blank => {
                    // Use blank lines to separate comment blocks
                    if !pre_comments.last().unwrap().is_empty() {
                        pre_comments.push(vec![]);
                    }
                }
                grammar::Statement::Program(name) => {
                    pu_ty = Some(ProgramUnitType::Program);
                    self.entry = Some(Entry {
                        loc,
                        name,
                        pre_comments,
                        post_comments: Vec::new(),
                        dargs: Vec::new(),
                        body: Vec::new(),
                        api_name: None,
                        comment_sections: Default::default(),
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

                    self.symbols.set_loc(&name, &loc);

                    self.entry = Some(Entry {
                        loc,
                        name,
                        pre_comments,
                        post_comments: Vec::new(),
                        dargs,
                        body: Vec::new(),
                        api_name: None,
                        comment_sections: Default::default(),
                    });

                    break 'LINES;
                }
                grammar::Statement::Subroutine(name, dargs) => {
                    pu_ty = Some(ProgramUnitType::Subroutine);

                    if dargs.iter().any(|d| d == "*") {
                        bail!("alternate returns are not supported");
                    }

                    dargs.iter().for_each(|d| self.symbols.set_darg(d));
                    self.entry = Some(Entry {
                        loc,
                        name,
                        pre_comments,
                        post_comments: Vec::new(),
                        dargs,
                        body: Vec::new(),
                        api_name: None,
                        comment_sections: Default::default(),
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

            fn call(&mut self, name: &String, args: &[Expression], _is_function: bool) {
                self.symbols.set_called(name);

                for arg in args {
                    if let Expression::Symbol(s) = arg {
                        self.symbols.set_used_as_arg(s);
                    }
                }
            }
        }

        for entry in &self.entries {
            let mut v = UsedVisitor {
                entry_name: &entry.name,
                symbols: &mut self.symbols,
            };
            entry.body.iter().for_each(|(_loc, s)| s.walk(&mut v));
        }

        // Apply "SAVE" to relevant symbols
        if self.save_all {
            for (_, sym) in &mut self.symbols.symbols {
                // Can't save dargs, procedures, constants
                if sym.darg || sym.called || sym.external || sym.parameter.is_some() {
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

        // Extract SPICE comment sections
        for entry in &mut self.entries {
            entry.comment_sections = parse_header_comments(
                &entry
                    .body
                    .iter()
                    .filter_map(|(_loc, stmt)| {
                        if let Statement::Comment(c) = stmt {
                            Some(c.iter().map(|(s, _)| s))
                        } else {
                            None
                        }
                    })
                    .flatten()
                    .cloned()
                    .collect::<Vec<_>>(),
            )?;

            entry.api_name = Some(safe_identifier(&entry.name.to_ascii_lowercase()));
        }

        let pu = ProgramUnit {
            ty: pu_ty.unwrap(),
            symbols: self.symbols,
            entries: self.entries,
            statement_functions: self.statement_functions,
            datas: self.datas,
        };

        for e in &pu.entries {
            for d in &e.dargs {
                if !pu.symbols.contains_key(d) {
                    bail!("dummy argument {d} must have explicit type")
                }
            }
        }

        for (name, sym) in &pu.symbols.symbols {
            sym.validate()
                .with_context(|| format!("invalid use of symbol {name}: {sym:?}"))?;
        }

        Ok(pu)
    }

    pub fn parse_constants(
        mut self,
        source: Vec<(SourceLoc, grammar::Statement)>,
    ) -> Result<SymbolTable> {
        let mut source_iter = source.into_iter();

        for (loc, line) in &mut source_iter {
            self.parse_statement(loc, &line)?;
        }

        for (name, sym) in &self.symbols.symbols {
            sym.validate()
                .with_context(|| format!("invalid use of symbol {name}: {sym:?}"))?;
        }

        Ok(self.symbols)
    }

    /// Parse any statement after the opening PROGRAM/FUNCTION/SUBROUTINE
    fn parse_statement(&mut self, loc: SourceLoc, line: &grammar::Statement) -> Result<()> {
        match &line {
            // Collect consecutive comment lines
            grammar::Statement::Comment(c) => {
                let in_section = if let Some(section) = c.strip_prefix("$ ") {
                    self.comment_section = Some(section.to_owned());
                    true
                } else if c == "-&" {
                    self.comment_section = None;
                    true
                } else {
                    self.comment_section.is_some()
                };

                // Skip comments from INCLUDE files
                if !loc.included {
                    self.comment.push((c.clone(), in_section));
                }
            }
            _ => {
                if !self.comment.is_empty() {
                    if let Some(stmts) = self.statements.last_mut() {
                        // If we're inside an entry, push the preceding comments as a statement
                        stmts.push((loc.clone(), Statement::Comment(self.comment.clone())));
                    } else {
                        // Not inside an entry, so tentatively store inside the latest entry
                        assert!(self.entry.is_none());
                        self.entries
                            .last_mut()
                            .unwrap()
                            .post_comments
                            .push(self.comment.clone());
                    }
                    self.comment.clear();
                }
            }
        }

        match &line {
            grammar::Statement::Comment(..) => (),
            grammar::Statement::Blank => {
                if let Some(stmts) = self.statements.last_mut() {
                    stmts.push((loc.clone(), Statement::Blank));
                }
            }

            grammar::Statement::Program(..)
            | grammar::Statement::Function(..)
            | grammar::Statement::Subroutine(..) => {
                bail!("{loc} PROGRAM/FUNCTION/SUBROUTINE only valid at start");
            }

            // FORMAT/ENTRY statements (but we don't support FORMAT)
            grammar::Statement::Entry(name, dargs) => {
                // We don't support entry into the middle of a function, because that's hard
                // to emulate in a sensible language
                if self.entry.is_some() {
                    bail!("{loc} ENTRY must be after an unconditional RETURN");
                }

                // If the previous entry was followed by some comments, they should
                // actually be interpreted as belonging to this entry
                let mut pre_comments = Vec::new();
                std::mem::swap(
                    &mut pre_comments,
                    &mut self.entries.last_mut().unwrap().post_comments,
                );

                dargs.iter().for_each(|d| self.symbols.set_darg(d));
                self.entry = Some(Entry {
                    loc,
                    name: name.clone(),
                    pre_comments,
                    post_comments: Vec::new(),
                    dargs: dargs.clone(),
                    body: Vec::new(),
                    api_name: None,
                    comment_sections: Default::default(),
                });

                self.statements.push(Vec::new());
            }

            // PARAMETER statements
            grammar::Statement::Parameter(params) => {
                if !matches!(self.state, ParseState::Implicit | ParseState::OtherSpec) {
                    bail!("{loc} PARAMETER invalid here");
                }

                let public = self
                    .comment_section
                    .as_ref()
                    .is_some_and(|s| s == "Declarations")
                    && !loc.included;

                for (name, expr) in params {
                    self.symbols.set_parameter(
                        name,
                        Expression::from(&self.symbols, expr)?,
                        public,
                    );
                }
            }

            // DATA statements
            grammar::Statement::Data(datas) => {
                if matches!(self.state, ParseState::Implicit | ParseState::OtherSpec) {
                    self.state = ParseState::StatementFunction;
                }

                for grammar::DataLists { nlist, clist } in datas {
                    self.datas.push(DataStatement {
                        loc: loc.clone(),
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
                    bail!("{loc} IMPLICIT invalid here");
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
                        bail!("{loc} specification statements invalid here");
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

            grammar::Statement::Equivalence(nlist) => {
                if !matches!(self.state, ParseState::Implicit | ParseState::OtherSpec) {
                    bail!("{loc} specification statements invalid here");
                } else {
                    self.state = ParseState::OtherSpec;
                }

                if nlist.len() != 1 || nlist[0].len() != 2 {
                    bail!("{loc} EQUIVALENCE only supported between 2 variables");
                }

                let ns = &nlist[0];

                match (&ns[0], &ns[1]) {
                    (DataName::Variable(s1), DataName::ArrayElement(s2, ..)) => {
                        // Some code says e.g.:
                        //    EQUIVALENCE (BEGIN, PTR(1))
                        //    EQUIVALENCE (END,   PTR(2))
                        // which is basically just an alias for the elements, so handle that case;
                        // all references to the symbol will be replaced with an element access
                        let sym1 = self.symbols.get(s1).unwrap();
                        let sym2 = self.symbols.get(s2).unwrap();
                        if !sym1.dims.is_empty() || sym1.base_type != sym2.base_type {
                            bail!(
                                "{loc} EQUIVALENCE with array element only supported with scalar of same type"
                            );
                        }
                        let e2 = Expression::from_dataname(&mut self.symbols, &ns[1])?;
                        self.symbols.set_alias(s1, e2);
                    }
                    (DataName::Variable(s1), DataName::Variable(s2)) => {
                        // When two scalars/arrays of equal size (in bytes) are equivalenced,
                        // we pick the one with the strongest alignment requirement as the primary
                        // representation. The other symbol is defined as an alias, which
                        // casts to the required type on every access. (That results in some
                        // ugly generated code, but casting on every access keeps the borrow
                        // checker happy.)

                        // Swap them so the DOUBLE comes first, for alignment
                        let (s1, s2) =
                            if self.symbols.get(s2).unwrap().base_type == DataType::Double {
                                (s2, s1)
                            } else {
                                (s1, s2)
                            };

                        let _sym1 = self.symbols.get(s1).unwrap();
                        let sym2 = self.symbols.get(s2).unwrap();

                        // We've only bothered implementing this in DummyArray for now
                        if sym2.dims.len() != 1 {
                            bail!("{loc} EQUIVALENCE only supported with a 1-dimensional array");
                        }

                        // TODO: verify the sizes are the same, to avoid runtime errors

                        self.symbols.set_equivalence(s2, s1);
                    }
                    _ => {
                        bail!(
                            "{loc} EQUIVALENCE only supported between variables and array elements"
                        );
                    }
                }
            }

            grammar::Statement::External(vs) => {
                if !matches!(self.state, ParseState::Implicit | ParseState::OtherSpec) {
                    bail!("{loc} specification statements invalid here");
                } else {
                    self.state = ParseState::OtherSpec;
                }

                for v in vs {
                    self.symbols.set_external(v);
                }
            }

            grammar::Statement::Type(ty, vs) => {
                if !matches!(self.state, ParseState::Implicit | ParseState::OtherSpec) {
                    bail!("{loc} specification statements invalid here");
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
                    self.symbols.set_loc(&v.name, &loc);
                }
            }

            grammar::Statement::TypeCharacter(len, vs) => {
                if !matches!(self.state, ParseState::Implicit | ParseState::OtherSpec) {
                    bail!("{loc} specification statements invalid here");
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
                    self.symbols.set_loc(&v.name, &loc);

                    self.symbols.set_character_len(
                        &v.name,
                        LenSpecification::from(&self.symbols, real_len),
                    );
                }
            }

            // Statement function statements. Ambiguously parsed as Assignment; check if we're
            // in the appropriate state and it wasn't declared as an array
            grammar::Statement::Assignment(grammar::DataName::ArrayElement(name, dargs), body)
                if matches!(
                    self.state,
                    ParseState::Implicit | ParseState::OtherSpec | ParseState::StatementFunction
                ) && !self.symbols.is_array(name) =>
            {
                // We should switch to StatementFunction here, but e.g. tspice/f_polyds.f uses
                // DOUBLE PRECISION after a statement function, so allow that
                self.state = ParseState::OtherSpec;

                let dargs: Vec<_> = dargs
                    .iter()
                    .map(|d| match d {
                        grammar::Expression::Symbol(s) => s.clone(),
                        _ => panic!("{loc} non-symbol in statement function dargs"),
                    })
                    .collect();

                let body = Expression::from(&self.symbols, body)?;

                let mut visitor = DoVarVisitor::new(&self.do_vars, &mut self.symbols);
                body.walk(&mut visitor);

                // Find all the variables used in the statement function, that are captured from
                // the environment and need to be explicitly passed in

                struct SymVisitor<'a> {
                    symbols: &'a mut SymbolTable,
                    found: Vec<String>,
                }

                impl Visitor for SymVisitor<'_> {
                    fn symbol(&mut self, name: &String) {
                        if !self.found.contains(name) {
                            self.found.push(name.clone());
                        }
                    }

                    // We also need to mark them called here, because UsedVisitor won't mark them
                    // until after we needed to determine if they were captured
                    fn call(&mut self, name: &String, _args: &[Expression], _is_function: bool) {
                        self.symbols.set_called(name);
                    }
                }

                let mut visitor = SymVisitor {
                    found: Vec::new(),
                    symbols: &mut self.symbols,
                };
                body.walk(&mut visitor);

                // Capture any necessary local variables (i.e. not external functions or constants)
                let captured: Vec<_> = visitor
                    .found
                    .iter()
                    .filter(|s| {
                        let sym = self.symbols.get(s).unwrap();
                        !(dargs.contains(s) || (sym.called && !sym.darg) || sym.parameter.is_some())
                    })
                    .cloned()
                    .collect();

                self.symbols.set_statement_function(name, &captured);

                self.statement_functions.push(StatementFunction {
                    loc,
                    name: name.clone(),
                    dargs,
                    captured,
                    body,
                });
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

                if let Some(st) = self.parse_basic_statement(loc.clone(), line)? {
                    self.statements.last_mut().unwrap().push((loc, st));
                }
            }

            grammar::Statement::Do(label, var, e1, e2, e3) => {
                self.state = ParseState::Executable;

                if label.is_some() {
                    bail!("{loc} DO label not supported");
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
                    bail!("{loc} can't use same DO-variable in nested loops");
                }
                self.do_vars.push(var.clone());

                self.statements.last_mut().unwrap().push((
                    loc,
                    Statement::Do {
                        var: var.clone(),
                        e1,
                        e2,
                        e3,
                        body: Vec::new(),
                    },
                ));

                self.statements.push(Vec::new());

                self.depth += 1;
            }

            grammar::Statement::DoWhile(label, e) => {
                self.state = ParseState::Executable;

                if label.is_some() {
                    bail!("{loc} DO WHILE label not supported");
                }

                let e = Expression::from(&self.symbols, e)?;

                let mut visitor = DoVarVisitor::new(&self.do_vars, &mut self.symbols);
                e.walk(&mut visitor);

                self.statements.last_mut().unwrap().push((
                    loc,
                    Statement::DoWhile {
                        e,
                        body: Vec::new(),
                    },
                ));

                self.statements.push(Vec::new());

                self.depth += 1;
            }

            grammar::Statement::EndDo => {
                self.state = ParseState::Executable;

                let body = self.statements.pop().unwrap();
                match self.statements.last_mut().unwrap().last_mut().unwrap() {
                    (_loc, Statement::Do { body: s_body, .. }) => {
                        *s_body = body;
                        self.do_vars.pop();
                    }
                    (_loc, Statement::DoWhile { body: s_body, .. }) => {
                        *s_body = body;
                    }
                    _ => bail!("{loc} END DO without matching DO"),
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

                if let Some(st) = self.parse_basic_statement(loc.clone(), body)? {
                    self.statements.last_mut().unwrap().push((
                        loc.clone(),
                        Statement::If {
                            es: vec![Some(e)],
                            bodies: vec![vec![(loc, st)]],
                        },
                    ));
                } else {
                    bail!("{loc} failed to parse logical IF body");
                }

                self.depth -= 1;
            }

            grammar::Statement::BlockIf(e) => {
                self.state = ParseState::Executable;

                let e = Expression::from(&self.symbols, e)?;

                let mut visitor = DoVarVisitor::new(&self.do_vars, &mut self.symbols);
                e.walk(&mut visitor);

                self.statements.last_mut().unwrap().push((
                    loc,
                    Statement::If {
                        es: vec![Some(e)],
                        bodies: vec![],
                    },
                ));

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
                    (_loc, Statement::If { es, bodies }) => {
                        es.push(Some(e));
                        bodies.push(body);
                    }
                    _ => bail!("{loc} ELSE IF without matching IF"),
                }

                self.statements.push(Vec::new());
            }
            grammar::Statement::Else => {
                self.state = ParseState::Executable;

                let body = self.statements.pop().unwrap();
                match self.statements.last_mut().unwrap().last_mut().unwrap() {
                    (_loc, Statement::If { es, bodies }) => {
                        es.push(None);
                        bodies.push(body);
                    }
                    _ => bail!("{loc} ELSE without matching IF"),
                }

                self.statements.push(Vec::new());
            }
            grammar::Statement::EndIf => {
                self.state = ParseState::Executable;

                let body = self.statements.pop().unwrap();
                match self.statements.last_mut().unwrap().last_mut().unwrap() {
                    (_loc, Statement::If { bodies, .. }) => {
                        bodies.push(body);
                    }
                    _ => bail!("{loc} END IF without matching IF"),
                }

                self.depth -= 1;
                assert!(self.depth >= 0);
            }

            // INCLUDE should have been expanded already
            grammar::Statement::Include(..) => panic!("{loc} unexpected INCLUDE"),

            grammar::Statement::End => {
                self.state = ParseState::End;

                if self.depth != 0 {
                    bail!("{loc} END while inside IF/DO");
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
                    grammar::DataName::ImpliedDo(..) => {
                        bail!("{loc} implied-DO invalid in assignment")
                    }
                    grammar::DataName::Expression(..) => {
                        bail!("{loc} expression invalid in assignment")
                    }
                };

                self.symbols
                    .set_assigned(name_sym, &self.entry.as_ref().unwrap().name);

                if self.do_vars.contains(name_sym) {
                    bail!("{loc} assigning to active DO-variable");
                }

                // Special case for functions that return CHARACTER: they output to an
                // extra final darg, so call them like a subroutine with the assignment target
                // as that final argument
                if let grammar::Expression::ArrayElementOrFunction(func_name, es) = e {
                    if !self.symbols.is_array(func_name)
                        && self.symbols.is_character(func_name)
                        && !intrinsics::exists(func_name)
                    {
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
                let mut other = IndexMap::new();

                let unit = match specs.0.get("UNIT") {
                    None => {
                        if !matches!(line, grammar::Statement::Print(..)) {
                            bail!("{loc} READ/WRITE statement must specify UNIT");
                        }
                        None
                    }
                    Some(grammar::SpecifierValue::Asterisk) => Some(Specifier::Asterisk),
                    Some(grammar::SpecifierValue::Expression(e)) => {
                        let e = Expression::from(&self.symbols, e)?;

                        // When writing to internal files, set the variable as mutable
                        if matches!(line, grammar::Statement::Write(..)) {
                            match &e {
                                Expression::Symbol(name) | Expression::ArrayElement(name, _) => {
                                    if self.symbols.is_character(name) {
                                        self.symbols
                                            .set_assigned(name, &self.entry.as_ref().unwrap().name);
                                    }
                                }
                                _ => (),
                            }
                        }

                        Some(Specifier::Expression(e))
                    }
                };

                let fmt = match specs.0.get("FMT") {
                    None => None,
                    Some(grammar::SpecifierValue::Asterisk) => Some(Specifier::Asterisk),
                    Some(grammar::SpecifierValue::Expression(e)) => {
                        let e = Expression::from(&self.symbols, e)?;
                        Some(Specifier::Expression(e))
                    }
                };

                for (k, v) in &specs.0 {
                    if k == "UNIT" || k == "FMT" {
                        // Already handled
                        continue;
                    }
                    match v {
                        grammar::SpecifierValue::Asterisk => {
                            bail!("{loc} asterisk specifier only allowed in UNIT=*")
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
                                        bail!(
                                            "{loc} IOSTAT must be an integer variable or array element"
                                        )
                                    }
                                }
                            }

                            other.insert(k.to_owned(), e);
                        }
                    }
                }

                if matches!(line, grammar::Statement::Read(..)) {
                    for io in iolist.iter() {
                        self.set_dataname_assigned(io);
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
                let mut ast_specs = IndexMap::new();

                let unit = specs.0.get("UNIT");
                match unit {
                    None => {
                        if !matches!(line, grammar::Statement::Inquire(..)) {
                            bail!("{loc} IO statement must specify UNIT");
                        }
                    }
                    Some(grammar::SpecifierValue::Asterisk) => {
                        bail!("{loc} UNIT=* only allowed in READ, WRITE");
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
                            bail!("{loc} asterisk specifier only allowed in UNIT=*")
                        }
                        grammar::SpecifierValue::Expression(e) => {
                            let e = Expression::from(&self.symbols, e)?;

                            // IOSTAT is an output for every statement.
                            // FILE/UNIT are the only inputs for INQUIRE.
                            if k == "IOSTAT"
                                || (matches!(line, grammar::Statement::Inquire(..)) && k != "FILE")
                            {
                                match &e {
                                    Expression::Symbol(name)
                                    | Expression::ArrayElement(name, _) => {
                                        self.symbols
                                            .set_assigned(name, &self.entry.as_ref().unwrap().name);
                                    }
                                    _ => {
                                        bail!("{loc} {k} must be a variable or array element")
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
                    bail!("{loc} alternate returns are not supported");
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

            _ => bail!("{loc} unrecognised basic statement"),
        })
    }

    fn set_dataname_assigned(&mut self, name: &DataName) {
        match name {
            DataName::Variable(s)
            | DataName::ArrayElement(s, ..)
            | DataName::Substring(s, ..)
            | DataName::SubstringArrayElement(s, ..) => {
                self.symbols
                    .set_assigned(s, &self.entry.as_ref().unwrap().name);
            }
            DataName::ImpliedDo(names, ..) => {
                names.iter().for_each(|n| self.set_dataname_assigned(n));
            }
            DataName::Expression(..) => {}
        }
    }
}
