//! Code generation. Emits the Rust code for expressions, statements and program units.

use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

use crate::ast::{DataType, Expression, LenSpecification, Specifier, Statement};
use crate::file::SourceLoc;
use crate::grammar::{BinaryOp, Constant, UnaryOp};
use crate::{ast, globan, intrinsics};
use anyhow::{Context, Result, bail};
use indexmap::IndexMap;
use log::{error, warn};

pub mod api;

/// The Rust representation of a symbol name or an expression, based on how it's
/// used in the main body of the function. (It may have different representations
/// in dargs etc, which get shadowed.)
#[derive(Debug, Clone)]
enum RustType {
    Primitive,           // `let X: i32` / `(1 + 2)`, for non-array non-character types
    PrimitiveMut,        // `let mut X: i32`
    PrimitiveRefMut,     // `fn F(X: &mut i32)`
    ActualArray,         // `let mut X: ActualArray<i32>`
    ActualCharArray,     // `let mut X: ActualCharArray`
    DummyArray,          // `let X: DummyArray<i32>`
    DummyArrayMut,       // `let mut X: DummyArrayMut<i32>`
    DummyCharArray,      // `let X: DummyCharArray`
    DummyCharArrayMut,   // `let X: DummyCharArrayMut`
    CharVec,             // `let X: Vec<u8>`
    CharSliceRef,        // `fn F(X: &[u8])` / `b"1"`
    CharSliceMut,        // `fn F(X: &mut [u8])`
    SavePrimitive,       // `struct SaveVars { X: i32 }`
    SaveChar,            // `struct SaveVars { X: Vec<u8> }`
    SaveActualArray,     // `struct SaveVars { X: ActualArray<i32> }`
    SaveActualCharArray, // `struct SaveVars { X: ActualCharArray }`
    Procedure,           // `fn X(...)` / `fn F(X: fn() -> i32)`
    LocalDoVar,          // `for X in 0..1`
    EquivArray,          // Like DummyArray but in EQUIVALENCE with a Primitive/ActualArray
    EquivArrayMut,       // Like DummyArrayMut but etc
}

/// The syntactic contexts in which a symbol name or expression can be used
#[derive(Debug, Clone, Copy)]
enum Ctx {
    Value,            // `X + 1`
    ValueMut,         // `X.subarray_mut()`
    ArgScalar,        // `F(X)` where F expects a scalar (primitive or string)
    ArgScalarMut,     // `F(&mut X)`
    ArgScalarAliased, // `F(&X.clone())`
    ArgArray,         // `F(X)` where F expects an array
    ArgArrayMut,      // `F(&mut X)`
    ArgArrayAliased,  // `F(&X.to_vec())`
    DummyArg,         // `fn F(X: T)`
    Assignment,       // `X = 1`
    SaveStruct,       // `struct SaveVars { X: T, }`
    SaveInit,         // `X = 1` inside `SaveInit::new()`
}

/// A symbol in a specific Entry
#[derive(Debug, Clone)]
struct Symbol {
    /// All namespaced procedures that can be represented by this symbol
    actual_procs: Vec<globan::Name>,

    /// Whether the symbol is mutated by this Entry, or by any that it's passed to
    mutated: bool,

    ast: ast::Symbol,

    rs_ty: RustType,
}

impl Symbol {
    fn new(sym: &globan::Symbol, mutated: bool) -> Self {
        let chars = matches!(sym.ast.base_type, DataType::Character);

        // Determine the appropriate Rust representation, depending on how the symbol is
        // declared and used
        let rs_ty = if sym.ast.do_var && !sym.ast.outside_do {
            RustType::LocalDoVar
        } else if let Some(_equiv) = &sym.ast.equivalence {
            if mutated {
                RustType::EquivArrayMut
            } else {
                RustType::EquivArray
            }
        } else if sym.ast.called {
            RustType::Procedure
        } else if !sym.ast.dims.is_empty() {
            // Arrays
            if chars {
                // Char arrays
                if sym.ast.darg && mutated {
                    RustType::DummyCharArrayMut
                } else if sym.ast.darg {
                    RustType::DummyCharArray
                } else if sym.ast.save {
                    RustType::SaveActualCharArray
                } else {
                    RustType::ActualCharArray
                }
            } else {
                // Non-char arrays
                if sym.ast.darg && mutated {
                    RustType::DummyArrayMut
                } else if sym.ast.darg {
                    RustType::DummyArray
                } else if sym.ast.save {
                    RustType::SaveActualArray
                } else {
                    RustType::ActualArray
                }
            }
        } else if sym.ast.save {
            // Saved scalars
            if chars {
                RustType::SaveChar
            } else {
                RustType::SavePrimitive
            }
        } else {
            // Scalars
            if chars {
                // Chars
                if sym.ast.darg && mutated {
                    RustType::CharSliceMut
                } else if sym.ast.darg || sym.ast.parameter.is_some() {
                    RustType::CharSliceRef
                } else {
                    RustType::CharVec
                }
            } else {
                // Non-chars
                if sym.ast.darg && mutated {
                    RustType::PrimitiveRefMut
                } else if mutated {
                    RustType::PrimitiveMut
                } else {
                    RustType::Primitive
                }
            }
        };

        Self {
            actual_procs: sym.actual_procs.clone(),
            mutated,
            ast: sym.ast.clone(),
            rs_ty,
        }
    }
}

#[derive(Debug)]
struct SymbolTable {
    syms: IndexMap<String, Symbol>,

    // An ugly hack to support temporarily defining symbols for use in arguments etc.
    // We use RefCell so the rest of the code can use a non-mut &SymbolTable and keep
    // &Symbols around, avoiding the performance cost of cloning them, without the
    // borrow checker worrying that we might invalidate those symbols.
    temp: RefCell<Vec<(String, Symbol)>>,
}

impl SymbolTable {
    fn new(syms: IndexMap<String, Symbol>) -> Self {
        Self {
            syms,
            temp: RefCell::new(Vec::new()),
        }
    }

    fn get(&self, name: &str) -> Result<Cow<Symbol>> {
        if let Some(sym) = self
            .temp
            .borrow()
            .iter()
            .rev()
            .find_map(|(n, s)| if n == name { Some(s) } else { None })
        {
            Ok(Cow::Owned(sym.clone()))
        } else if let Some(sym) = self.syms.get(name) {
            Ok(Cow::Borrowed(sym))
        } else {
            bail!("unrecognized symbol {name}")
        }
    }

    fn iter(&self) -> impl Iterator<Item = (&String, &Symbol)> {
        self.syms.iter()
    }

    /// Call func with an extra symbol added to the symbol table
    fn with_temp<R, F: FnOnce() -> R>(&self, name: &str, sym: Symbol, func: F) -> R {
        self.temp.borrow_mut().push((name.to_owned(), sym));
        let ret = func();
        self.temp.borrow_mut().pop();
        ret
    }
}

impl Expression {
    /// Recursively determine the type of an expression
    fn resolve_type(&self, loc: &SourceLoc, syms: &SymbolTable) -> Result<DataType> {
        Ok(match self {
            Expression::Unary(_op, e2) => e2.resolve_type(loc, syms)?,
            Expression::Binary(op, e1, e2) => {
                let t1 = e1.resolve_type(loc, syms)?;
                let t2 = e2.resolve_type(loc, syms)?;
                match op {
                    BinaryOp::Add
                    | BinaryOp::Sub
                    | BinaryOp::Div
                    | BinaryOp::Mul
                    | BinaryOp::Pow => {
                        if matches!(t1, DataType::Double | DataType::Real | DataType::Integer)
                            && matches!(t2, DataType::Double | DataType::Real | DataType::Integer)
                        {
                            // Lower-precision type gets upgraded automatically
                            if t1 == DataType::Double || t2 == DataType::Double {
                                DataType::Double
                            } else if t1 == DataType::Real || t2 == DataType::Real {
                                DataType::Real
                            } else {
                                DataType::Integer
                            }
                        } else {
                            bail!(
                                "{loc} invalid types in arithmetic operator: {t1:?} {op:?} {t2:?}"
                            );
                        }
                    }
                    BinaryOp::Concat => {
                        if t1 == DataType::Character && t2 == DataType::Character {
                            DataType::Character
                        } else {
                            bail!("{loc} invalid types in CHARACTER concatenation: {t1:?} {t2:?}");
                        }
                    }
                    BinaryOp::Lt
                    | BinaryOp::Le
                    | BinaryOp::Eq
                    | BinaryOp::Ne
                    | BinaryOp::Gt
                    | BinaryOp::Ge => {
                        if (matches!(t1, DataType::Double | DataType::Real | DataType::Integer)
                            && matches!(t2, DataType::Double | DataType::Real | DataType::Integer))
                            || (t1 == DataType::Character && t2 == DataType::Character)
                        {
                            DataType::Logical
                        } else {
                            bail!(
                                "{loc} invalid types in relational operator: {t1:?} {op:?} {t2:?}"
                            );
                        }
                    }

                    BinaryOp::And | BinaryOp::Or | BinaryOp::Eqv | BinaryOp::Neqv => {
                        if t1 == DataType::Logical && t2 == DataType::Logical {
                            DataType::Logical
                        } else {
                            bail!("{loc} invalid types in logical operator: {t1:?} {op:?} {t2:?}");
                        }
                    }
                }
            }

            Expression::Function(name, args) => {
                let ty = &syms.get(name)?.ast.base_type;

                // If no type was declared, and this is an intrinsic function, then use the intrinsic's type
                if matches!(ty, DataType::Unknown) {
                    let first_arg = args
                        .first()
                        .map(|a| a.resolve_type(loc, syms))
                        .transpose()?;
                    if let Some((ret_ty, _)) = intrinsics::call_info(name, first_arg) {
                        ret_ty
                    } else if intrinsics::exists(name) {
                        bail!("{loc} calling intrinsic {name} with incorrect argument type");
                    } else {
                        bail!("{loc} calling non-intrinsic function {name} without declared type");
                    }
                } else if let DataType::Procedure { ret_args, .. } = ty {
                    ret_args.first().unwrap().base_type.clone()
                } else {
                    ty.clone()
                }
            }

            Expression::Symbol(s)
            | Expression::ArrayElement(s, ..)
            | Expression::Substring(s, ..)
            | Expression::SubstringArrayElement(s, ..) => syms.get(s)?.ast.base_type.clone(),

            Expression::Constant(c) => match c {
                Constant::Integer(_) => DataType::Integer,
                Constant::Real(_) => DataType::Real,
                Constant::Double(_) => DataType::Double,
                Constant::Bool(_) => DataType::Logical,
                Constant::Character(_) => DataType::Character,
            },
            Expression::ImpliedDo { .. } => bail!("{loc} cannot resolve type of implied-DO"),
            Expression::ImpliedDoVar(_) => DataType::Integer,
            // (Don't support REAL implied-DO-vars, that'd just be silly)
        })
    }

    fn eval_constant(&self, syms: &SymbolTable) -> Result<Option<i32>> {
        Ok(match self {
            Expression::Unary(op, e2) => {
                if let Some(e2) = e2.eval_constant(syms)? {
                    match op {
                        UnaryOp::Negate => Some(-e2),
                        UnaryOp::Not => None,
                        UnaryOp::Paren => Some(e2),
                    }
                } else {
                    None
                }
            }
            Expression::Binary(op, e1, e2) => {
                if let (Some(e1), Some(e2)) = (e1.eval_constant(syms)?, e2.eval_constant(syms)?) {
                    match op {
                        BinaryOp::Add => Some(e1 + e2),
                        BinaryOp::Sub => Some(e1 - e2),
                        BinaryOp::Div => Some(e1 / e2),
                        BinaryOp::Mul => Some(e1 * e2),
                        BinaryOp::Pow => {
                            if e2 < 0 {
                                Some(1 / e1.pow(e2.unsigned_abs()))
                            } else {
                                Some(e1.pow(e2 as u32))
                            }
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }
            Expression::Symbol(s) => {
                if let Some(p) = &syms.get(s)?.ast.parameter {
                    p.eval_constant(syms)?
                } else {
                    None
                }
            }
            Expression::Constant(Constant::Integer(n)) => Some(*n),
            Expression::Constant(..) => None,

            Expression::ArrayElement(..)
            | Expression::Function(..)
            | Expression::Substring(..)
            | Expression::SubstringArrayElement(..)
            | Expression::ImpliedDo { .. }
            | Expression::ImpliedDoVar(..) => None,
        })
    }

    fn uses_symbol(&self, s: &str) -> bool {
        struct SymVisitor<'a> {
            s: &'a str,
            found: bool,
        }

        impl ast::Visitor for SymVisitor<'_> {
            fn symbol(&mut self, name: &String) {
                if name == self.s {
                    self.found = true;
                }
            }
        }

        let mut visitor = SymVisitor { s, found: false };
        self.walk(&mut visitor);
        visitor.found
    }
}

fn eval_dims(
    dims: &[ast::Dimension],
    syms: &SymbolTable,
) -> Result<Vec<(Option<i32>, Option<i32>)>> {
    let dims: Vec<_> = dims
        .iter()
        .map(|d| -> Result<_> {
            let lower = match &d.lower {
                Some(e) => e.eval_constant(syms)?,
                None => Some(1),
            };
            let upper = match &d.upper {
                Some(e) => e.eval_constant(syms)?,
                None => None,
            };
            Ok((lower, upper))
        })
        .collect::<Result<_>>()?;

    Ok(dims)
}

// Determine whether we should allocate it on the stack,
// and return the number of elements
fn use_stack_array(sym: &Symbol, syms: &SymbolTable) -> Result<Option<i32>> {
    const MAX_SIZE: i32 = 256;

    let is_char = matches!(sym.ast.base_type, DataType::Character);

    let dims = eval_dims(&sym.ast.dims, syms)?;
    if let Some(size) = dims
        .into_iter()
        .map(|(l, u)| Some(u? + 1 - l?))
        .reduce(|a, b| Some(a? * b?))
        .unwrap()
    {
        if size <= MAX_SIZE && !is_char {
            return Ok(Some(size));
        }
    }
    Ok(None)
}

fn eval_character_len(len: &Option<LenSpecification>, syms: &SymbolTable) -> Result<Option<i32>> {
    Ok(match len {
        Some(LenSpecification::Integer(c)) => Some(*c),
        Some(LenSpecification::IntConstantExpr(e)) => e.eval_constant(syms)?,
        _ => None,
    })
}

/// Convert to Rust type, for use in various contexts
fn emit_datatype(ty: &DataType) -> String {
    match ty {
        DataType::Integer => "i32".to_owned(),
        DataType::Real => "f32".to_owned(),
        DataType::Double => "f64".to_owned(),
        DataType::Logical => "bool".to_owned(),
        DataType::Void => "()".to_owned(),

        // This isn't the actual Rust type, but we use it for naming methods like write_str()
        DataType::Character => "str".to_owned(),

        // There shouldn't be any UNKNOWN at this stage
        DataType::Unknown => "INVALID_TYPE_UNKNOWN".to_owned(),

        // Procedures used as variables are Rust function pointers
        DataType::Procedure {
            requires_ctx,
            returns_result,
            ret_args,
        } => {
            if ret_args.is_empty() {
                // TODO: why is this triggered sometimes?
                return "ERROR cannot find procedure's return type".to_owned();
            }

            let mut ret_args = ret_args.iter().map(|arg| {
                // TODO: this is duplicating emit_symbol DummyArg, maybe refactor it
                if arg.base_type == DataType::Character {
                    if arg.is_array {
                        if arg.mutated {
                            "CharArrayMut".to_owned()
                        } else {
                            "CharArray".to_owned()
                        }
                    } else {
                        if arg.mutated {
                            "&mut [u8]".to_owned()
                        } else {
                            "&[u8]".to_owned()
                        }
                    }
                } else {
                    let ty = emit_datatype(&arg.base_type);
                    if arg.is_array {
                        if arg.mutated {
                            format!("&mut [{ty}]")
                        } else {
                            format!("&[{ty}]")
                        }
                    } else {
                        if arg.mutated {
                            format!("&mut {ty}")
                        } else {
                            ty
                        }
                    }
                }
            });

            let ret = ret_args.next().unwrap();
            let mut args = ret_args.collect::<Vec<_>>();
            if *requires_ctx {
                args.push("&mut Context".to_owned());
            }
            let args = args.join(", ");
            if *returns_result {
                format!("fn({args}) -> f2rust_std::Result<{ret}>")
            } else {
                format!("fn({args}) -> {ret}")
            }
        }
    }
}

/// Used to prevent Rust complaining about uninitialised values
fn emit_zero(ty: &DataType) -> Result<&'static str> {
    Ok(match ty {
        DataType::Integer => "0",
        DataType::Real => "0.0",
        DataType::Double => "0.0",
        DataType::Logical => "false",
        _ => bail!("cannot zero-initialize local of type {ty:?}"),
    })
}

/// Performs the code generation for a single program unit
pub struct CodeGen<'a> {
    /// SAVE and PARAMETER symbols, which are shared by all Entry in this program unit
    shared: CodeGenUnit<'a>,

    entries: Vec<Entry<'a>>,
    statement_functions: Vec<StatementFunction<'a>>,
}

/// Represents a procedure/ENTRY, or the global SAVE/PARAMETER state
struct CodeGenUnit<'a> {
    globan: &'a globan::GlobalAnalysis,
    program: &'a globan::ProgramUnit,
    syms: SymbolTable,
}

struct Entry<'a> {
    codegen: CodeGenUnit<'a>,
    ast: &'a ast::Entry,
}

struct StatementFunction<'a> {
    codegen: CodeGenUnit<'a>,
    ast: &'a ast::StatementFunction,
}

/// The various ways of calling a procedure or intrinsic
#[derive(Debug, Clone)]
pub enum CallSyntax {
    Unified,                // cannot emit directly
    External(globan::Name), // a::b(x)
    Cast(&'static str),     // x as a
    Func(&'static str),     // a(x)
    VarFunc(&'static str),  // a(&[x])  (used for variadic intrinsics)
    ArraySubscriptValue,    // special intrinsic
    ArrayClone,             // special intrinsic
}

#[derive(Debug, Clone)]
pub struct ProcedureArgs {
    pub return_type: DataType,
    pub dargs: Vec<globan::DummyArg>,
    pub codegen: CallSyntax,
    pub requires_ctx: bool,
    pub returns_result: bool,
}

impl CodeGenUnit<'_> {
    /// Emit a symbol in the appropriate form for its context.
    /// In FORTRAN everything is a pointer, but in Rust we have to explicitly
    /// convert between values, references, arrays, scalars, etc.
    fn emit_symbol(&self, loc: &SourceLoc, name: &str, ctx: Ctx) -> Result<String> {
        let sym = self.syms.get(name)?;
        let ty = emit_datatype(&sym.ast.base_type);

        let s = match ctx {
            Ctx::Value => match sym.rs_ty {
                // A scalar value used with binary operators, indexes, method calls, etc
                RustType::Primitive
                | RustType::PrimitiveMut
                | RustType::ActualArray
                | RustType::ActualCharArray
                | RustType::DummyArray
                | RustType::DummyArrayMut
                | RustType::DummyCharArray
                | RustType::DummyCharArrayMut
                | RustType::CharSliceRef
                | RustType::CharSliceMut
                | RustType::Procedure
                | RustType::LocalDoVar => name.to_owned(),

                RustType::CharVec => format!("&{name}"),
                RustType::PrimitiveRefMut => format!("*{name}"),
                RustType::SavePrimitive
                | RustType::SaveActualArray
                | RustType::SaveActualCharArray => format!("save.{name}"),
                RustType::SaveChar => format!("&save.{name}"),

                RustType::EquivArray | RustType::EquivArrayMut => format!(
                    "DummyArray::<{ty}>::from_equiv({}, {})",
                    self.emit_symbol(loc, sym.ast.equivalence.as_ref().unwrap(), Ctx::ArgArray)?,
                    self.emit_dims(loc, &sym)?
                ),
            },
            Ctx::ValueMut => match sym.rs_ty {
                RustType::EquivArrayMut => format!(
                    "DummyArrayMut::<{ty}>::from_equiv({}, {})",
                    self.emit_symbol(loc, sym.ast.equivalence.as_ref().unwrap(), Ctx::ArgArrayMut)?,
                    self.emit_dims(loc, &sym)?
                ),
                _ => self.emit_symbol(loc, name, Ctx::Value)?,
            },
            Ctx::ArgScalar => match sym.rs_ty {
                // When passing an array as a scalar, we take the first element
                RustType::ActualArray | RustType::DummyArray | RustType::DummyArrayMut => {
                    format!("*{name}.first()")
                }
                RustType::ActualCharArray
                | RustType::DummyCharArray
                | RustType::DummyCharArrayMut => {
                    format!("{name}.first()")
                }
                RustType::SaveActualArray => format!("*save.{name}.first()"),
                RustType::SaveActualCharArray => format!("save.{name}.first()"),

                _ => self.emit_symbol(loc, name, Ctx::Value)?,
            },
            Ctx::ArgScalarMut => match sym.rs_ty {
                RustType::PrimitiveMut => format!("&mut {name}"),
                RustType::PrimitiveRefMut => name.to_owned(),
                RustType::ActualArray
                | RustType::ActualCharArray
                | RustType::DummyArrayMut
                | RustType::DummyCharArrayMut => format!("{name}.first_mut()"),
                RustType::CharVec => format!("&mut {name}"),
                RustType::CharSliceMut => name.to_owned(),
                RustType::SavePrimitive => format!("&mut save.{name}"),
                RustType::SaveChar => format!("&mut save.{name}"),
                RustType::SaveActualArray | RustType::SaveActualCharArray => {
                    format!("save.{name}.first_mut()")
                }
                RustType::Primitive
                | RustType::DummyArray
                | RustType::DummyCharArray
                | RustType::CharSliceRef
                | RustType::Procedure
                | RustType::LocalDoVar
                | RustType::EquivArray
                | RustType::EquivArrayMut => {
                    bail!("{loc} invalid context {ctx:?} for symbol {name}: {sym:?}")
                }
            },
            Ctx::ArgScalarAliased => match sym.rs_ty {
                // This is the non-mutable copy of a variable that's also being passed as
                // a mutable argument. If not pass-by-value, clone it
                RustType::ActualCharArray
                | RustType::DummyCharArray
                | RustType::DummyCharArrayMut => {
                    format!("&{name}.first().to_vec()")
                }
                RustType::SaveActualCharArray => format!("&save.{name}.first().to_vec()"),
                RustType::CharVec => format!("&{name}.clone()"),
                RustType::CharSliceRef => format!("&{name}.to_vec()"),
                RustType::CharSliceMut => format!("&{name}.to_vec()"),
                RustType::SaveChar => format!("&save.{name}.to_vec()"),
                _ => self.emit_symbol(loc, name, Ctx::ArgScalar)?,
            },
            Ctx::ArgArray => match sym.rs_ty {
                // When passing a scalar as an array, need to convert T to &[T]
                RustType::Primitive | RustType::PrimitiveMut => format!("&[{name}]"),
                RustType::PrimitiveRefMut => format!("&[*{name}]"),
                RustType::ActualArray => format!("{name}.as_slice()"),
                RustType::ActualCharArray => format!("{name}.as_arg()"),
                RustType::DummyArray | RustType::DummyArrayMut => format!("{name}.as_slice()"),
                RustType::DummyCharArray => format!("{name}.as_arg()"),
                RustType::DummyCharArrayMut => format!("{name}.as_arg()"),
                RustType::CharVec => format!("CharArray::from_ref(&{name})"),
                RustType::CharSliceRef | RustType::CharSliceMut => {
                    format!("CharArray::from_ref({name})")
                }
                RustType::SavePrimitive => format!("&[save.{name}]"),
                RustType::SaveChar => format!("CharArray::from_ref(&save.{name})"),
                RustType::SaveActualArray => format!("save.{name}.as_slice()"),
                RustType::SaveActualCharArray => format!("save.{name}.as_arg()"),
                RustType::Procedure => format!("&[{name}]"),
                RustType::LocalDoVar => format!("&[{name}]"),
                RustType::EquivArray | RustType::EquivArrayMut => format!(
                    "DummyArray::<{ty}>::from_equiv({}, {}).as_slice()",
                    self.emit_symbol(loc, sym.ast.equivalence.as_ref().unwrap(), Ctx::ArgArray)?,
                    self.emit_dims(loc, &sym)?
                ),
            },
            Ctx::ArgArrayMut => match sym.rs_ty {
                // slice::from_mut converts &mut T to &mut [T]
                RustType::PrimitiveMut => format!("std::slice::from_mut(&mut {name})"),
                RustType::PrimitiveRefMut => format!("std::slice::from_mut({name})"),
                RustType::ActualArray => format!("{name}.as_slice_mut()"),
                RustType::ActualCharArray => format!("{name}.as_arg_mut()"),
                RustType::DummyArrayMut => format!("{name}.as_slice_mut()"),
                RustType::DummyCharArrayMut => format!("{name}.as_arg_mut()"),
                RustType::CharVec => format!("CharArrayMut::from_mut(&mut {name})"),
                RustType::CharSliceMut => format!("CharArrayMut::from_mut({name})"),
                RustType::SavePrimitive => format!("std::slice::from_mut(&mut save.{name})"),
                RustType::SaveChar => format!("CharArrayMut::from_mut(&mut save.{name})"),
                RustType::SaveActualArray => format!("save.{name}.as_slice_mut()"),
                RustType::SaveActualCharArray => format!("save.{name}.as_arg_mut()"),
                RustType::EquivArrayMut => format!(
                    "DummyArrayMut::<{ty}>::from_equiv({}, {}).as_slice_mut()",
                    self.emit_symbol(loc, sym.ast.equivalence.as_ref().unwrap(), Ctx::ArgArrayMut)?,
                    self.emit_dims(loc, &sym)?
                ),
                RustType::Primitive
                | RustType::DummyArray
                | RustType::DummyCharArray
                | RustType::CharSliceRef
                | RustType::Procedure
                | RustType::LocalDoVar
                | RustType::EquivArray => {
                    bail!("{loc} invalid context {ctx:?} for symbol {name}: {sym:?}")
                }
            },
            Ctx::ArgArrayAliased => match sym.rs_ty {
                RustType::ActualArray | RustType::DummyArray | RustType::DummyArrayMut => {
                    format!("&{name}.as_slice().to_vec()")
                }
                RustType::CharVec => format!("&{name}.clone()"),
                RustType::CharSliceRef => format!("&{name}.to_vec()"),
                RustType::CharSliceMut => format!("&{name}.to_vec()"),
                RustType::SaveChar => format!("&save.{name}.to_vec()"),
                RustType::SaveActualArray => {
                    format!("&save.{name}.as_slice().to_vec()")
                }
                RustType::SaveActualCharArray => {
                    format!("&save.{name}.to_owned().to_arg()")
                }
                _ => self.emit_symbol(loc, name, Ctx::ArgArray)?,
            },
            Ctx::DummyArg => match sym.rs_ty {
                // Used as Rust function parameters, i.e. the public API of our code,
                // so these try to use basic Rust types that the function body will convert to
                // FORTRAN-specific types when necessary
                RustType::Primitive => format!("{name}: {ty}"),
                RustType::PrimitiveRefMut => format!("{name}: &mut {ty}"),
                RustType::DummyArray => format!("{name}: &[{ty}]"),
                RustType::DummyArrayMut => format!("{name}: &mut [{ty}]"),
                RustType::DummyCharArray => format!("{name}: CharArray"),
                RustType::DummyCharArrayMut => format!("{name}: CharArrayMut"),
                RustType::CharSliceRef => format!("{name}: &[u8]"),
                RustType::CharSliceMut => format!("{name}: &mut [u8]"),
                RustType::Procedure => format!("{name}: {ty}"),

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
                    bail!("{loc} invalid context {ctx:?} for symbol {name}: {sym:?}")
                }
            },
            Ctx::Assignment => match sym.rs_ty {
                // Used like "s = e" or "fstr::assign(s, e)"
                RustType::PrimitiveMut => name.to_owned(),
                RustType::PrimitiveRefMut => format!("*{name}"),
                RustType::CharVec => format!("&mut {name}"),
                RustType::CharSliceMut => name.to_owned(),
                RustType::SavePrimitive => format!("save.{name}"),
                RustType::SaveChar => format!("&mut save.{name}"),
                RustType::ActualArray
                | RustType::ActualCharArray
                | RustType::DummyArrayMut
                | RustType::DummyCharArrayMut => name.to_owned(),
                RustType::SaveActualArray | RustType::SaveActualCharArray => format!("save.{name}"),
                RustType::EquivArrayMut => format!(
                    "DummyArrayMut::<{ty}>::from_equiv({}, {})",
                    self.emit_symbol(loc, sym.ast.equivalence.as_ref().unwrap(), Ctx::ArgArrayMut)?,
                    self.emit_dims(loc, &sym)?
                ),
                RustType::Primitive
                | RustType::DummyArray
                | RustType::DummyCharArray
                | RustType::CharSliceRef
                | RustType::Procedure
                | RustType::LocalDoVar
                | RustType::EquivArray => {
                    bail!("{loc} invalid context {ctx:?} for symbol {name}: {sym:?}")
                }
            },
            Ctx::SaveStruct => match sym.rs_ty {
                RustType::SavePrimitive => format!("{name}: {ty}"),
                RustType::SaveChar => format!("{name}: Vec<u8>"),
                RustType::SaveActualArray => match use_stack_array(&sym, &self.syms)? {
                    Some(size) => match sym.ast.dims.len() {
                        1 => format!("{name}: StackArray<{ty}, {size}>"),
                        n => format!("{name}: StackArray{n}D<{ty}, {size}>"),
                    },
                    None => match sym.ast.dims.len() {
                        1 => format!("{name}: ActualArray<{ty}>"),
                        n => format!("{name}: ActualArray{n}D<{ty}>"),
                    },
                },
                RustType::SaveActualCharArray => match sym.ast.dims.len() {
                    1 => format!("{name}: ActualCharArray"),
                    n => format!("{name}: ActualCharArray{n}D"),
                },
                _ => bail!("{loc} invalid context {ctx:?} for symbol {name}: {sym:?}"),
            },
            Ctx::SaveInit => match sym.rs_ty {
                RustType::SaveChar => format!("&mut {name}"),
                RustType::SavePrimitive
                | RustType::SaveActualArray
                | RustType::SaveActualCharArray => name.to_owned(),
                _ => bail!("{loc} invalid context {ctx:?} for symbol {name}: {sym:?}"),
            },
        };

        // Ok(format!("{s} /* {ctx:?} */"))
        Ok(s)
    }

    fn emit_expressions(&self, loc: &SourceLoc, es: &[Expression], ctx: Ctx) -> Result<String> {
        Ok(es
            .iter()
            .map(|e| self.emit_expression_ctx(loc, e, ctx))
            .collect::<Result<Vec<_>>>()?
            .join(", "))
    }

    fn emit_binop_arith(
        &self,
        loc: &SourceLoc,
        e1: &Expression,
        e2: &Expression,
        op: &str,
    ) -> Result<String> {
        self.emit_binop_arith_rel(loc, e1, e2, op, "", false)
    }

    fn emit_binop_rel(
        &self,
        loc: &SourceLoc,
        e1: &Expression,
        e2: &Expression,
        op: &str,
        op_str: &str,
    ) -> Result<String> {
        self.emit_binop_arith_rel(loc, e1, e2, op, op_str, true)
    }

    /// Common behaviour for arithmetic and relation operators
    fn emit_binop_arith_rel(
        &self,
        loc: &SourceLoc,
        e1: &Expression,
        e2: &Expression,
        op: &str,
        op_str: &str,
        allow_char: bool,
    ) -> Result<String> {
        let t1 = e1.resolve_type(loc, &self.syms)?;
        let t2 = e2.resolve_type(loc, &self.syms)?;
        let a = self.emit_expression(loc, e1)?;
        let b = self.emit_expression(loc, e2)?;

        Ok(match (&t1, &t2) {
            (DataType::Integer, DataType::Integer)
            | (DataType::Real, DataType::Real)
            | (DataType::Double, DataType::Double) => format!("({a} {op} {b})"),

            (DataType::Character, DataType::Character) if allow_char => {
                format!("fstr::{op_str}({a}, {b})")
            }

            (DataType::Real, DataType::Integer) => format!("({a} {op} {b} as f32)"),

            (DataType::Double, DataType::Integer) | (DataType::Double, DataType::Real) => {
                format!("({a} {op} {b} as f64)")
            }

            // We need the parens around the cast, because "... as f32 < ..." gets interpreted
            // as a generic type
            (DataType::Integer, DataType::Real) => format!("(({a} as f32) {op} {b})"),

            (DataType::Integer, DataType::Double) | (DataType::Real, DataType::Double) => {
                format!("(({a} as f64) {op} {b})")
            }

            _ => bail!("{loc} invalid types in binary op '{op}': {t1:?}, {t2:?}"),
        })
    }

    fn emit_binop_pow(&self, loc: &SourceLoc, e1: &Expression, e2: &Expression) -> Result<String> {
        let t1 = e1.resolve_type(loc, &self.syms)?;
        let t2 = e2.resolve_type(loc, &self.syms)?;
        let a = self.emit_expression(loc, e1)?;
        let b = self.emit_expression(loc, e2)?;

        Ok(match (&t1, &t2) {
            // Can't use i32::pow, because we need to support negative exponents
            (DataType::Integer, DataType::Integer) => {
                format!("intrinsics::pow({a}, {b})")
            }
            (DataType::Real, DataType::Integer) => format!("f32::powi({a}, {b})"),
            (DataType::Double, DataType::Integer) => format!("f64::powi({a}, {b})"),
            (DataType::Integer, DataType::Real) => format!("f32::powf({a} as f32, {b})"),
            (DataType::Integer, DataType::Double) => format!("f64::powf({a} as f64, {b})"),
            (DataType::Real, DataType::Real) => format!("f32::powf({a}, {b})"),
            (DataType::Double, DataType::Double) => format!("f64::powf({a}, {b})"),
            (DataType::Real, DataType::Double) => format!("f64::powf({a} as f64, {b})"),
            (DataType::Double, DataType::Real) => format!("f64::powf({a}, {b} as f64)"),
            _ => bail!("{loc} invalid types in **: {t1:?}, {t2:?}"),
        })
    }

    fn emit_binop_logic(
        &self,
        loc: &SourceLoc,
        e1: &Expression,
        e2: &Expression,
        op: &str,
    ) -> Result<String> {
        let t1 = e1.resolve_type(loc, &self.syms)?;
        let t2 = e2.resolve_type(loc, &self.syms)?;
        let a = self.emit_expression(loc, e1)?;
        let b = self.emit_expression(loc, e2)?;

        Ok(match (&t1, &t2) {
            (DataType::Logical, DataType::Logical) => format!("({a} {op} {b})"),
            _ => bail!("{loc} invalid types in logical op '{op}': {t1:?}, {t2:?}"),
        })
    }

    fn emit_binop_concat(
        &self,
        loc: &SourceLoc,
        e1: &Expression,
        e2: &Expression,
    ) -> Result<String> {
        let t1 = e1.resolve_type(loc, &self.syms)?;
        let t2 = e2.resolve_type(loc, &self.syms)?;
        let a = self.emit_expression_ctx(loc, e1, Ctx::ArgScalar)?;
        let b = self.emit_expression_ctx(loc, e2, Ctx::ArgScalar)?;

        Ok(match (&t1, &t2) {
            (DataType::Character, DataType::Character) => format!("&fstr::concat({a}, {b})"),
            _ => bail!("{loc} invalid types in character op '//': {t1:?}, {t2:?}"),
        })
    }

    /// Emit array index value. Since Rust requires this to be a single value, for N-dimensional
    /// arrays we use the syntax `a[[x, y]]` (similar to ndarray).
    fn emit_index(&self, loc: &SourceLoc, idx: &[Expression]) -> Result<String> {
        let idx_ex = self.emit_expressions(loc, idx, Ctx::Value)?;
        if idx.len() == 1 {
            Ok(idx_ex)
        } else {
            Ok(format!("[{idx_ex}]"))
        }
    }

    /// Emit substring range: `A..=B` or `A..`
    fn emit_range(
        &self,
        loc: &SourceLoc,
        e1: &Option<Box<Expression>>,
        e2: &Option<Box<Expression>>,
    ) -> Result<String> {
        Ok(match (e1, e2) {
            (None, None) => "1 ..".to_owned(),
            (Some(e1), None) => {
                let e1 = self.emit_expression(loc, e1)?;
                format!("{e1} ..")
            }
            (None, Some(e2)) => {
                let e2 = self.emit_expression(loc, e2)?;
                format!("1 ..= {e2}")
            }
            (Some(e1), Some(e2)) => {
                let e1 = self.emit_expression(loc, e1)?;
                let e2 = self.emit_expression(loc, e2)?;
                format!("{e1} ..= {e2}")
            }
        })
    }

    /// Emit expression in default (Value) context
    fn emit_expression(&self, loc: &SourceLoc, e: &Expression) -> Result<String> {
        self.emit_expression_ctx(loc, e, Ctx::Value)
    }

    /// Emit expression in given context. (Mostly the context only applies to symbols;
    /// expressions involving operators will output like Value regardless.)
    fn emit_expression_ctx(&self, loc: &SourceLoc, e: &Expression, ctx: Ctx) -> Result<String> {
        Ok(match e {
            Expression::Unary(op, e2) => {
                let e2 = self.emit_expression(loc, e2)?;
                match op {
                    UnaryOp::Negate => format!("-{e2}"),
                    UnaryOp::Not => format!("!{e2}"),
                    UnaryOp::Paren => e2,
                }
            }
            Expression::Binary(op, e1, e2) => {
                match op {
                    BinaryOp::Add => self.emit_binop_arith(loc, e1, e2, "+")?,
                    BinaryOp::Sub => self.emit_binop_arith(loc, e1, e2, "-")?,
                    BinaryOp::Div => self.emit_binop_arith(loc, e1, e2, "/")?,
                    BinaryOp::Mul => self.emit_binop_arith(loc, e1, e2, "*")?,
                    BinaryOp::Pow => self.emit_binop_pow(loc, e1, e2)?,
                    BinaryOp::Concat => self.emit_binop_concat(loc, e1, e2)?,
                    BinaryOp::Lt => self.emit_binop_rel(loc, e1, e2, "<", "lt")?,
                    BinaryOp::Le => self.emit_binop_rel(loc, e1, e2, "<=", "le")?,
                    BinaryOp::Eq => self.emit_binop_rel(loc, e1, e2, "==", "eq")?,
                    BinaryOp::Ne => self.emit_binop_rel(loc, e1, e2, "!=", "ne")?,
                    BinaryOp::Gt => self.emit_binop_rel(loc, e1, e2, ">", "gt")?,
                    BinaryOp::Ge => self.emit_binop_rel(loc, e1, e2, ">=", "ge")?,
                    BinaryOp::And => self.emit_binop_logic(loc, e1, e2, "&&")?,
                    BinaryOp::Or => self.emit_binop_logic(loc, e1, e2, "||")?,
                    BinaryOp::Eqv => self.emit_binop_logic(loc, e1, e2, "==")?,
                    BinaryOp::Neqv => self.emit_binop_logic(loc, e1, e2, "!=")?,
                }
                // TODO: would be nice to omit brackets when Rust precedence doesn't require it,
                // and only keep the source's original ones from UnaryOp::Paren
            }

            Expression::Symbol(name) => self.emit_symbol(loc, name, ctx)?,

            Expression::ArrayElement(name, idx) => {
                let s = self.emit_symbol(loc, name, Ctx::Value)?;
                let sym = self.syms.get(name)?;
                let idx_ex = self.emit_index(loc, idx)?;
                if matches!(sym.ast.base_type, DataType::Character) {
                    if matches!(ctx, Ctx::ArgScalarMut) {
                        format!("{s}.get_mut({idx_ex})")
                    } else {
                        format!("{s}.get({idx_ex})")
                    }
                } else {
                    format!("{s}[{idx_ex}]")
                }
            }

            Expression::Function(name, args) => self.emit_call(loc, name, args, true)?,

            Expression::Substring(name, e1, e2) => {
                let s = self.emit_symbol(loc, name, Ctx::Value)?;
                let range = self.emit_range(loc, e1, e2)?;
                format!("fstr::substr({s}, {range})")
            }
            Expression::SubstringArrayElement(name, idx, e1, e2) => {
                let s = self.emit_symbol(loc, name, Ctx::Value)?;
                let idx_ex = self.emit_expressions(loc, idx, Ctx::Value)?;
                let range = self.emit_range(loc, e1, e2)?;

                if matches!(ctx, Ctx::ArgScalarMut) {
                    format!("fstr::substr_mut({s}.get_mut({idx_ex}), {range})")
                } else {
                    format!("fstr::substr({s}.get({idx_ex}), {range})")
                }
            }
            Expression::Constant(c) => match c {
                // The i32 suffix is helpful for type-checking but makes the code ugly
                // Constant::Integer(n) => format!("{n}i32"),
                Constant::Integer(n) => format!("{n}"),

                // TODO: maybe we should try to preserve the original string representation?
                Constant::Real(n) => format!("{n}f32"),

                // Constant::Double(n) => format!("{n}f64"),
                Constant::Double(n) => {
                    let s = n.to_string();
                    if s.contains(".") { s } else { s + ".0" }
                }

                Constant::Bool(n) => if *n { "true" } else { "false" }.to_owned(),

                Constant::Character(s) => {
                    let escaped = s.escape_default();
                    format!("b\"{escaped}\"")
                }
            },
            Expression::ImpliedDo { .. } => bail!("{loc} cannot emit implied-DO"),
            Expression::ImpliedDoVar(name) => name.clone(),
        })
    }

    /// Emit comma-separate arguments for a function call,
    /// plus some optional code to prepare those arguments before the call
    fn emit_args(
        &self,
        loc: &SourceLoc,
        dargs: &[globan::DummyArg],
        args: &[Expression],
        requires_ctx: bool,
    ) -> Result<(String, String)> {
        assert_eq!(dargs.len(), args.len());

        // Detect aliasing violations:
        // If a symbol is used in 2+ arguments, and at least 1 is mutable, that's a problem
        // (Rust won't accept it, and it's probably illegal in FORTRAN unless the mutable
        // argument doesn't actually get mutated.)
        //
        // But we need to support code like `CALL REPLCH (CHOSE2(3:7), ' ', '0', CHOSE2(3:7))`
        // which I think really is illegal, as it's reading and writing the same bytes through
        // different variables.
        //
        // So, warn about the aliasing, and then replace the non-mut arguments with clone()
        // which will probably do the right thing.
        //
        // We also need to support cases like `CALL FOO (X(1), X(2))` which FORTRAN does allow
        // (since the aliasing restriction applies to the elements accessed, not the whole array),
        // but Rust doesn't (because it borrows the whole array). So we detect the specific
        // case of mutable elements of the same array, and use get_disjoint_mut().

        // List of argument indexes where a particular symbol is used
        #[derive(Default)]
        struct ArgUses {
            used: Vec<usize>,
            mutated: Vec<usize>,
            mutated_element: Vec<usize>,
            mutated_slice: Vec<usize>,
        }

        // Value is (total occurrences, mutable occurrences, mutable array elt occurrences)
        let mut sym_counts: HashMap<&str, ArgUses> = HashMap::new();
        for (i, (darg, arg)) in dargs.iter().zip(args.iter()).enumerate() {
            match arg {
                Expression::Symbol(s)
                | Expression::ArrayElement(s, ..)
                | Expression::Substring(s, ..)
                | Expression::SubstringArrayElement(s, ..) => {
                    let c = sym_counts.entry(s).or_default();
                    c.used.push(i);
                    if darg.mutated {
                        c.mutated.push(i);
                    }
                    if matches!(arg, Expression::ArrayElement(..)) {
                        if darg.is_array {
                            c.mutated_slice.push(i);
                        } else {
                            c.mutated_element.push(i);
                        }
                    }
                }
                _ => (),
            }
        }

        let mut aliased = HashSet::new();
        let mut aliased_arrays: IndexMap<&str, Vec<(usize, String)>> = IndexMap::new();
        let mut aliased_slices: IndexMap<&str, Vec<(usize, String)>> = IndexMap::new();
        for (sym, count) in &sym_counts {
            if count.used.len() >= 2 && !count.mutated.is_empty() {
                // warn!(
                //     "{loc} Possible aliasing violation: symbol {sym} used twice in procedure call"
                // );
                aliased.insert(*sym);
            }
            if count.mutated.len() >= 2 {
                if count.mutated == count.mutated_element {
                    warn!(
                        "{loc} Possible aliasing violating: assuming array elements are disjoint"
                    );
                    aliased_arrays.insert(sym, Vec::new());
                } else if count.mutated.len()
                    == count.mutated_element.len() + count.mutated_slice.len()
                {
                    warn!("{loc} Possible aliasing violating: assuming array slice are disjoint");
                    aliased_slices.insert(sym, Vec::new());
                } else {
                    error!(
                        "{loc} Aliasing violation: symbol {sym} used mutably twice in procedure call"
                    );
                }
            }
        }

        let mut prep_code = String::new();

        let mut exprs = dargs.iter().zip(args.iter()).enumerate().map(|(i, (darg, arg))| {
            let conversion = if darg.is_array {
                // Function is expecting an array
                match arg {
                    Expression::Function(name, ..) if intrinsics::returns_array(name) => {
                        // Hack for F2RUST_ARRAY_CLONE
                        self.emit_expression(loc, arg)?
                    }
                    Expression::Unary(..) |
                    Expression::Binary(..) |
                    Expression::Function(..) |
                    Expression::Constant(..) => {
                        // Some code expects this to behave like an array of size 1
                        warn!("{loc} passing expression to dummy argument expecting an array");
                        let e = self.emit_expression(loc, arg)?;
                        if darg.base_type == DataType::Character {
                            format!("CharArray::from_ref({e})")
                        } else {
                            format!("&[{e}]")
                        }
                    }
                    Expression::Symbol(name) => {
                        let sym = self.syms.get(name)?;
                        if sym.ast.base_type != darg.base_type {
                            bail!("{loc} cannot convert array types: actual argument {name}={:?}, dummy argument {}={:?}", sym.ast.base_type, darg.name, darg.base_type);
                        }

                        if darg.mutated {
                            self.emit_symbol(loc, name, Ctx::ArgArrayMut)?
                        } else if aliased.contains(name.as_str()) {
                            self.emit_symbol(loc, name, Ctx::ArgArrayAliased)?
                        } else {
                            self.emit_symbol(loc, name, Ctx::ArgArray)?
                        }
                    }
                    Expression::ArrayElement(name, idx) => {
                        let s = if darg.mutated {
                            self.emit_symbol(loc, name, Ctx::ValueMut)?
                        } else {
                            self.emit_symbol(loc, name, Ctx::Value)?
                        };
                        let sym = self.syms.get(name)?;
                        if sym.ast.dims.is_empty() {
                            bail!("{loc} cannot access element of non-array");
                        }
                        if sym.ast.base_type != darg.base_type {
                            bail!("{loc} cannot convert array types: actual argument {name}={:?}, dummy argument {}={:?}", sym.ast.base_type, darg.name, darg.base_type);
                        }
                        let idx_ex = self.emit_index(loc, idx)?;
                        if darg.mutated {
                            if let Some(entry) = aliased_slices.get_mut(name.as_str()) {
                                entry.push((i, idx_ex));
                                format!("arg{i}")
                            } else {
                                format!("{s}.subarray_mut({idx_ex})")
                            }
                        } else if aliased.contains(name.as_str()) {
                            if darg.base_type == DataType::Character {
                                format!("{s}.subarray({idx_ex}).to_owned().as_arg()")
                            } else {
                                format!("&{s}.subarray({idx_ex}).to_vec()")
                            }
                        } else {
                            format!("{s}.subarray({idx_ex})")
                        }
                    }
                    Expression::Substring(name, e1, e2) => {
                        let range = self.emit_range(loc, e1, e2)?;

                        if darg.mutated {
                            let s = self.emit_symbol(loc, name, Ctx::ArgScalarMut)?;
                            format!("CharArrayMut::from_mut(fstr::substr_mut({s}, {range}))")
                        } else if aliased.contains(name.as_str()) {
                            let s = self.emit_symbol(loc, name, Ctx::ArgScalar)?;
                            format!("CharArray::from_ref(&fstr::substr({s}, {range}).to_vec())")
                        } else {
                            let s = self.emit_symbol(loc, name, Ctx::ArgScalar)?;
                            format!("CharArray::from_ref(fstr::substr({s}, {range}))")
                        }
                    }
                    Expression::SubstringArrayElement(..) => todo!(),
                    Expression::ImpliedDo { .. } => bail!("{loc} cannot use implied-DO as array argument"),
                    Expression::ImpliedDoVar(..) => bail!("{loc} cannot use implied-DO-variable as array argument"),
                }
            } else {
                // Function is expecting a scalar
                match arg {
                    Expression::Unary(..) |
                    Expression::Binary(..) |
                    Expression::Constant(..) => {
                        if darg.mutated {
                            warn!("{loc} Passing expression or constant to mutable dummy argument - will be cloned");
                            let e = self.emit_expression(loc, arg)?;
                            format!("&mut {e}.clone()")
                        } else {
                            self.emit_expression(loc, arg)?
                        }
                    }
                    Expression::Symbol(name) => {
                        let sym = self.syms.get(name)?;
                        if sym.ast.base_type != darg.base_type {
                            bail!("{loc} cannot convert types: actual argument {name}={:?}, dummy argument {}={:?}", sym.ast.base_type, darg.name, darg.base_type);
                        }

                        if !sym.ast.darg && matches!(sym.ast.base_type, DataType::Procedure {..}) {
                            assert_eq!(sym.actual_procs.len(), 1);
                            let proc = sym.actual_procs.first().unwrap();
                            if proc.module == self.program.namespace {
                                proc.local.clone()
                            } else {
                                format!("{}::{}", proc.module, proc.local)
                            }
                        } else if darg.mutated {
                            self.emit_symbol(loc, name, Ctx::ArgScalarMut)?
                        } else if aliased.contains(name.as_str()) {
                            self.emit_symbol(loc, name, Ctx::ArgScalarAliased)?
                        } else {
                            self.emit_symbol(loc, name, Ctx::ArgScalar)?
                        }
                    }
                    Expression::ArrayElement(name, idx) => {
                        let s = self.emit_symbol(loc, name, Ctx::Value)?;
                        let sym = self.syms.get(name)?;
                        if sym.ast.dims.is_empty() {
                            bail!("{loc} cannot access element of non-array");
                        }
                        if sym.ast.base_type != darg.base_type {
                            bail!("{loc} cannot convert types: actual argument {name}={:?}, dummy argument {}={:?}", sym.ast.base_type, darg.name, darg.base_type);
                        }
                        let idx_ex = self.emit_index(loc, idx)?;

                        let get = format!("{s}[{idx_ex}]");

                        if darg.mutated {
                            if let Some(entry) = aliased_slices.get_mut(name.as_str()) {
                                entry.push((i, idx_ex));
                                format!("arg{i}.first_mut().unwrap()")
                            } else if let Some(entry) = aliased_arrays.get_mut(name.as_str()) {
                                entry.push((i, idx_ex));
                                format!("arg{i}")
                            } else {
                                // Pass by reference
                                format!("&mut {get}")
                            }
                        } else if matches!(sym.ast.base_type, DataType::Character) {
                            if aliased.contains(name.as_str()) {
                                // Try to resolve aliasing violation
                                format!("&{get}.to_vec()")
                            } else {
                                // Pass by reference
                                format!("&{get}")
                            }
                        } else {
                            // Pass by value
                            get.clone()
                        }
                    }
                    Expression::Function(name, args) => {
                        if darg.mutated {
                            bail!("{loc} cannot pass function return value to mutable dummy argument");
                        } else if darg.base_type == DataType::Character && !intrinsics::exists(name) {
                            // Non-intrinsic CHARACTER functions write their output into an extra final darg.
                            // To use the function in an argument or expression, we need to
                            // allocate some storage and pass it to the function.
                            let sym = self.syms.get(name)?;
                            let len = match &sym.ast.character_len {
                                Some(LenSpecification::Integer(c)) => c.to_string(),
                                Some(LenSpecification::IntConstantExpr(e)) => self.emit_expression(loc, e)?,
                                _ => bail!("{loc} cannot use CHARACTER-returning function {name} in argument, unless it has an explicit size"),
                            };

                            // Create a temporary symbol to store the character data,
                            // with just enough details to keep emit_args happy
                            let arg_name = format!("arg{i}");
                            prep_code += &format!("let mut {arg_name} = vec![b' '; {len}];\n");
                            let arg_sym = Symbol {
                                actual_procs: Vec::new(),
                                mutated: true,
                                ast: ast::Symbol {
                                    base_type: DataType::Character,
                                    ..Default::default()
                                },
                                rs_ty: RustType::CharVec,
                            };

                            let mut args = args.clone();
                            args.push(Expression::Symbol(arg_name.clone()));

                            // With the temporary symbol in place, construct the
                            // `func(..., &mut argN);` call.
                            let (call, result) = self.syms.with_temp(&arg_name, arg_sym, || -> Result<_> {
                                let call = self.emit_call(loc, name, &args, false)?;
                                let result = self.emit_symbol(loc, &arg_name, Ctx::ArgScalar)?;
                                Ok((call, result))
                            })?;
                            prep_code += &format!("{};\n", call);

                            result
                        } else {
                            self.emit_call(loc, name, args, true)?
                        }
                    }
                    Expression::Substring(name, e1, e2) => {
                        let range = self.emit_range(loc, e1, e2)?;

                        if darg.mutated {
                            let s = self.emit_symbol(loc, name, Ctx::ArgScalarMut)?;
                            format!("fstr::substr_mut({s}, {range})")
                        } else if aliased.contains(name.as_str()) {
                            let s = self.emit_symbol(loc, name, Ctx::ArgScalar)?;
                            format!("&fstr::substr({s}, {range}).to_vec()")
                        } else {
                            let s = self.emit_symbol(loc, name, Ctx::ArgScalar)?;
                            format!("fstr::substr({s}, {range})")
                        }
                    }
                    Expression::SubstringArrayElement(name, idx, e1, e2) => {
                        let s = self.emit_symbol(loc, name, Ctx::Value)?;
                        let sym = self.syms.get(name)?;
                        if sym.ast.dims.is_empty() {
                            bail!("{loc} cannot access element of non-array");
                        }
                        if sym.ast.base_type != darg.base_type {
                            bail!("{loc} cannot convert types: actual argument {name}={:?}, dummy argument {}={:?}", sym.ast.base_type, darg.name, darg.base_type);
                        }
                        let idx_ex = self.emit_index(loc, idx)?;

                        let get = format!("{s}[{idx_ex}]");

                        let range = self.emit_range(loc, e1, e2)?;

                        if darg.mutated {
                            format!("fstr::substr_mut(&mut {get}, {range})")
                        } else if aliased.contains(name.as_str()) {
                            format!("&fstr::substr(&{get}, {range}).to_vec()")
                        } else {
                            format!("fstr::substr(&{get}, {range})")
                        }
                    }
                    Expression::ImpliedDo { .. } => bail!("{loc} cannot use implied-DO as argument"),
                    Expression::ImpliedDoVar(..) => bail!("{loc} cannot use implied-DO-variable as argument"),
                }
            };

            Ok(conversion)
        }).collect::<Result<Vec<_>>>()?;

        // Context gets passed as the final argument (which means it has the shortest
        // lifetime, reducing borrowing errors when other arguments use ctx too)
        if requires_ctx {
            exprs.push("ctx".to_owned());
        }

        // If a symbol has 1 mut and 1+ non-mut uses, the non-mut ones have been replaced with
        // clones above. But Rust evaluates arguments left to right, so the non-mut ones must
        // be cloned before we take the mut reference. Fix that.
        for sym in &aliased {
            let counts = &sym_counts[sym];
            if let Some(first_mut) = counts.mutated.first() {
                for i in &counts.used {
                    if i > first_mut && !counts.mutated.contains(i) {
                        let arg_name = format!("arg{i}");
                        prep_code += &format!("let {arg_name} = {};\n", &exprs[*i]);
                        exprs[*i] = arg_name;
                    }
                }
            }
        }

        for (name, args) in &aliased_arrays {
            let obj = self.emit_symbol(loc, name, Ctx::ValueMut)?;
            let names: Vec<_> = args.iter().map(|(i, _e)| format!("arg{i}")).collect();
            let idxs: Vec<_> = args.iter().map(|(_i, e)| e.clone()).collect();
            prep_code += &format!(
                "let [{}] = {obj}.get_disjoint_mut([{}]).expect(\"mutable array elements passed to function must have disjoint indexes\");\n",
                names.join(", "),
                idxs.join(", ")
            );
        }

        for (name, args) in &aliased_slices {
            let obj = self.emit_symbol(loc, name, Ctx::ValueMut)?;
            let names: Vec<_> = args.iter().map(|(i, _e)| format!("arg{i}")).collect();
            let idxs: Vec<_> = args.iter().map(|(_i, e)| e.clone()).collect();
            prep_code += &format!(
                "let [{}] = {obj}.get_disjoint_slices_mut([{}]).unwrap();\n",
                names.join(", "),
                idxs.join(", ")
            );
        }

        Ok((exprs.join(", "), prep_code))
    }

    /// Get the argument types for calling a procedure or intrinsic
    fn procedure_args(
        &self,
        loc: &SourceLoc,
        name: &str,
        actual_procs: &[globan::Name],
        args: &[Expression],
    ) -> Result<ProcedureArgs> {
        if intrinsics::exists(name) {
            // There is an intrinsic, but check that it has the correct types too
            let first_arg = args
                .first()
                .map(|e| e.resolve_type(loc, &self.syms))
                .transpose()?;
            if let Some((return_type, codegen)) = intrinsics::call_info(name, first_arg) {
                return Ok(ProcedureArgs {
                    return_type,
                    dargs: intrinsics::dummy_args(name).map_or_else(
                        || {
                            // TODO: handle everything in intrinsics::dummy_args, instead
                            // of guessing based on the actual args
                            args.iter()
                                .map(|arg| {
                                    Ok(globan::DummyArg {
                                        name: "_".to_owned(),
                                        base_type: arg.resolve_type(loc, &self.syms)?,
                                        is_array: false,
                                        mutated: false,
                                    })
                                })
                                .collect::<Result<_>>()
                        },
                        Ok,
                    )?,
                    codegen,
                    requires_ctx: false,
                    returns_result: intrinsics::returns_result(name),
                });
            } else {
                warn!(
                    "{loc} Call to {name} not interpreted as an intrinsic, because the types don't match"
                );
            }
        }

        self.globan.procedure_args(name, actual_procs)
    }

    // Emit call to SUBROUTINE or FUNCTION or intrinsic
    fn emit_call(
        &self,
        loc: &SourceLoc,
        name: &str,
        args: &[Expression],
        is_function: bool,
    ) -> Result<String> {
        let sym = self.syms.get(name)?;
        let actual_procs = &sym.actual_procs;
        if actual_procs.is_empty() {
            // We need at least one, so we can figure out the argument conversions
            bail!("{loc} call to symbol {name} with no known actual procedures");
        }
        let actual = self.procedure_args(loc, name, actual_procs, args)?;

        if matches!(actual.return_type, DataType::Unknown | DataType::Void) {
            if is_function {
                bail!("{loc} called subroutine as function: {}", name);
            }
        } else if !is_function {
            // Allow CALL if it returns Character, because `ast` has translated the return
            // value into an extra darg. Otherwise complain
            if !matches!(actual.return_type, DataType::Character) {
                warn!("{loc} CALL to function, not subroutine: {}", name);
            }
        }

        if args.len() != actual.dargs.len() {
            bail!(
                "{loc} call to {name} with incorrect number of arguments (got {}, expected {})",
                args.len(),
                actual.dargs.len()
            );
        }

        let q = if actual.returns_result { "?" } else { "" };

        let (args_ex, args_prep) = self
            .emit_args(loc, &actual.dargs, args, actual.requires_ctx)
            .with_context(|| format!("failed args in call to {name}"))?;

        if !args_prep.is_empty() && is_function {
            bail!("TODO: args_pre for function");
        }

        if sym.ast.darg {
            // Ok(format!(
            // "{name}({args_ex}) /* possible actual procedures: {actual_procs:?} */\n"
            // ))
            Ok(format!("{args_prep}{name}({args_ex}){q}"))
        } else {
            let call = match actual.codegen {
                CallSyntax::Unified => bail!("{loc} unexpected unified proc"),
                CallSyntax::External(name) => {
                    let ns_name = if name.module == self.program.namespace {
                        name.local
                    } else {
                        format!("{}::{}", name.module, name.local)
                    };
                    // format!("{ns_name}({args_ex}) /* possible actual procedures: {actual_procs:?} */\n")
                    format!("{ns_name}({args_ex}){q}")
                }
                CallSyntax::Cast(ty) => format!("({args_ex} as {ty}){q}"),
                CallSyntax::Func(name) => format!("{name}({args_ex}){q}"),
                CallSyntax::VarFunc(name) => format!("{name}(&[{args_ex}]){q}"),
                CallSyntax::ArraySubscriptValue => match args.first() {
                    Some(Expression::ArrayElement(name, idx)) => {
                        let s = self.emit_symbol(loc, name, Ctx::Value)?;
                        let idx_ex = self.emit_index(loc, idx)?;
                        format!("{s}.subscript({idx_ex})")
                    }
                    _ => bail!("{loc} ArraySubscriptValue invalid args {args:?}"),
                },
                CallSyntax::ArrayClone => match args.first() {
                    Some(Expression::Symbol(name)) => {
                        let s = self.emit_symbol(loc, name, Ctx::Value)?;
                        format!("&mut {s}.data().to_vec()")
                    }
                    _ => bail!("{loc} ArrayClone invalid args {args:?}"),
                },
            };
            Ok(format!("{args_prep}{call}"))
        }
    }

    fn emit_save_borrow(&self, entry_name: &str) -> Result<String> {
        let mut code = String::new();

        // Check if we actually need to save anything in this entry
        let saved = self
            .syms
            .iter()
            .filter(|(_, sym)| sym.ast.save && sym.ast.used.contains(entry_name))
            .collect::<Vec<_>>();

        if saved.is_empty() {
            return Ok(code);
        }

        // Have to do this in two steps, to keep the Rc<RefCell> alive throughout the function
        code += "let save = ctx.get_vars::<SaveVars>();\n";
        code += "let save = &mut *save.borrow_mut();\n\n";

        Ok(code)
    }

    /// Emit local variable definitions, at the top of a function
    fn emit_locals(&self, entry_name: &str) -> Result<String> {
        let mut code = String::new();

        for (name, sym) in self.syms.iter() {
            if !sym.ast.used.contains(entry_name) {
                // Skip unused symbols (just for tidiness)
            } else if sym.ast.save {
                // Skip SAVE variables (they're handled specially elsewhere)
            } else if sym.ast.parameter.is_some() {
                // Skip PARAMETER constants (defined at module scope)
            } else if sym.ast.do_var && !sym.ast.outside_do {
                // Skip DO-vars that are local to their loop
            } else if sym.ast.called {
                // Skip procedures
            } else {
                code += &self.emit_initialiser(&sym.ast.loc, name, sym, &self.syms, sym.mutated)?;
            }
        }

        code += "\n";

        Ok(code)
    }

    /// Dimension declarator for an array
    fn emit_dims(&self, loc: &SourceLoc, sym: &Symbol) -> Result<String> {
        let dims = sym.ast.dims.iter().map(|dim| {
            let lower = match &dim.lower {
                None => "1".to_owned(),
                Some(e) => self.emit_expression(loc, e)?,
            };
            match &dim.upper {
                None => Ok(format!("{lower} .. ")),
                Some(e) => {
                    let e = self.emit_expression(loc, e)?;
                    Ok(format!("{lower} ..= {e}"))
                }
            }
        });
        Ok(dims.collect::<Result<Vec<_>>>()?.join(", "))
    }

    /// Initialise local variables (including DummyArray accessors etc)
    fn emit_initialiser(
        &self,
        loc: &SourceLoc,
        name: &String,
        sym: &Symbol,
        syms: &SymbolTable,
        is_mut: bool,
    ) -> Result<String> {
        let mut code = String::new();

        if !sym.ast.dims.is_empty() {
            let dims = self.emit_dims(loc, sym)?;

            // Find whether this is a Char array (with optional string length) or normal array
            let (char_label, char_len) = match sym.ast.base_type {
                DataType::Character => {
                    let len = match &sym.ast.character_len {
                        Some(LenSpecification::Asterisk) => None,
                        Some(LenSpecification::Integer(c)) => Some(c.to_string()),
                        Some(LenSpecification::IntConstantExpr(e)) => {
                            Some(self.emit_expression(loc, e)?)
                        }
                        _ => bail!("character array with undefined length"),
                    };
                    ("Char", len)
                }
                _ => ("", None),
            };

            let mut_label = if is_mut { "mut " } else { "" };
            if sym.ast.darg {
                // Wrap the darg array with our new dimensions

                let array = if is_mut {
                    match sym.ast.dims.len() {
                        1 => format!("Dummy{char_label}ArrayMut"),
                        n => format!("Dummy{char_label}ArrayMut{n}D"),
                    }
                } else {
                    match sym.ast.dims.len() {
                        1 => format!("Dummy{char_label}Array"),
                        n => format!("Dummy{char_label}Array{n}D"),
                    }
                };

                let len_label = match sym.ast.base_type {
                    DataType::Character => match char_len {
                        Some(e) => format!("Some({e}), "),
                        None => "None, ".to_owned(),
                    },
                    _ => "".to_owned(),
                };

                code +=
                    &format!("let {mut_label}{name} = {array}::new({name}, {len_label}{dims});\n");
            } else {
                // Allocate the array

                let is_char = matches!(sym.ast.base_type, DataType::Character);

                let (alloc, size_label) = match use_stack_array(sym, &self.syms)? {
                    Some(size) => ("Stack", format!(", {size}")),
                    None => ("Actual", "".to_owned()),
                };

                let array = match sym.ast.dims.len() {
                    1 => format!("{alloc}{char_label}Array"),
                    n => format!("{alloc}{char_label}Array{n}D"),
                };

                let len_label = if is_char {
                    match char_len {
                        Some(e) => format!("{e}, "),
                        None => bail!("actual character array with undefined length"),
                    }
                } else {
                    "".to_owned()
                };

                let ty = if is_char {
                    "".to_owned()
                } else {
                    format!("::<{}{size_label}>", emit_datatype(&sym.ast.base_type))
                };
                code += &format!("let {mut_label}{name} = {array}{ty}::new({len_label}{dims});\n");
            }
        } else if sym.ast.base_type == DataType::Character {
            let len = sym
                .ast
                .character_len
                .as_ref()
                .expect("character sym must have length");

            if sym.ast.darg {
                // Reduce the length of the string, if requested
                match len {
                    LenSpecification::Unspecified => bail!("unspecified character length"),
                    LenSpecification::Asterisk => {
                        // nothing to do, we'll keep the length of the actual argument
                    }

                    LenSpecification::Integer(n) => {
                        let mut_label = if is_mut { "mut " } else { "" };
                        code += &format!("let {name} = &{mut_label}{name}[..{n}];\n");
                    }
                    LenSpecification::IntConstantExpr(e) => {
                        let e = self.emit_expression(loc, e)?;
                        let mut_label = if is_mut { "mut " } else { "" };
                        code += &format!("let {name} = &{mut_label}{name}[..{e} as usize];\n");
                    }
                }
            } else {
                let stack_alloc_threshold = 2048;

                match len {
                    LenSpecification::Unspecified => bail!("unspecified character length"),
                    LenSpecification::Asterisk => {
                        // nothing to do, we'll keep the length of the actual argument
                    }

                    LenSpecification::Integer(n) => {
                        let mut_label = if is_mut { "mut " } else { "" };
                        let vec_label = if *n <= stack_alloc_threshold && !sym.ast.save {
                            ""
                        } else {
                            "vec!"
                        };

                        code += &format!("let {mut_label}{name} = {vec_label}[b' '; {n}];\n");
                    }
                    LenSpecification::IntConstantExpr(e) => {
                        if let Some(n) = e.eval_constant(syms)? {
                            let expr = self.emit_expression(loc, e)?;
                            let mut_label = if is_mut { "mut " } else { "" };
                            let vec_label = if n <= stack_alloc_threshold && !sym.ast.save {
                                ""
                            } else {
                                "vec!"
                            };

                            code += &format!(
                                "let {mut_label}{name} = {vec_label}[b' '; {expr} as usize];\n"
                            );
                        } else {
                            bail!(
                                "{loc} cannot evaluate CHARACTER length as integer constant expression"
                            );
                        }
                    }
                }
            }
        } else if !sym.ast.darg && !sym.ast.external {
            // Non-array, non-character, local variable

            let ty = emit_datatype(&sym.ast.base_type);

            let zero =
                emit_zero(&sym.ast.base_type).with_context(|| format!("initialising {name}"))?;
            if is_mut {
                // Zero-initialise because rustc can't always tell it's initialised on every code path
                // (TODO: if we can detect it's safe, we could remove this. If it's assigned exactly
                // once, get rid of the 'mut' too.)
                code += &format!("let mut {name}: {ty} = {zero};\n");
            } else {
                // Zero-initialise because e.g. ZZPLTCHK doesn't write to its 'output' variable,
                // so ZZDDHOPN passes an uninitialised value
                code += &format!("let {name}: {ty} = {zero};\n");
            }
        }

        Ok(code)
    }

    /// Emit code for DATA, assigning a sequence of constant expressions to a sequence of names
    fn emit_data_init(&self, data: &ast::DataStatement) -> Result<String> {
        let loc = &data.loc;
        let mut code = String::new();

        // Handle the simple case, assigning to a single scalar
        if data.nlist.len() == 1 {
            if let Some(Expression::Symbol(name)) = data.nlist.first() {
                let sym = self.syms.get(name)?;
                if matches!(sym.rs_ty, RustType::SavePrimitive | RustType::SaveChar) {
                    if data.clist.len() != 1 {
                        bail!("{loc} DATA {name} trying to assign wrong number of items to scalar");
                    }
                    let (reps, value) = data.clist.first().unwrap();

                    // reps must be 1, but it could be a constant expression that gives 1,
                    // and we can't evaluate constants, so skip any expression
                    if reps.is_none() {
                        let target = Expression::Symbol(name.clone());
                        code += &self.emit_assignment(loc, &target, value, Ctx::SaveInit)?;

                        return Ok(code);
                    }
                }
            }
        }

        // For the non-simple cases:
        //
        // Our aim is to minimise the number of lines of Rust code generated, not necessarily
        // to optimise the resulting machine code, because we don't want FORTRAN code initialising
        // large arrays with loops/reps/etc to explode into a huge amount of Rust code.
        //
        // So we construct an iterator over the whole clist, by chaining slices and repeat_n.
        // `Val` is an enum that lets us store any primitive, and cast back to the required type
        // (it will panic on type mismatch).
        //
        // Then we walk through nlist, translating implied-DO into `for` loops, consuming from
        // clist as we go. This results in some runtime overhead, but the Rust code is a fairly
        // direct match to the FORTRAN.
        //
        // We could optimise some other special cases, but this generic approach will do for now.

        code += "{\n";
        code += "  use f2rust_std::data::Val;\n\n";
        code += "  let mut clist = [\n";
        let mut needs_into_iter = true;

        for (reps, value) in &data.clist {
            let e = self.emit_expression(loc, value)?;

            let val = match value.resolve_type(loc, &self.syms)? {
                DataType::Integer => format!("Val::I({e})"),
                DataType::Real => format!("Val::R({e})"),
                DataType::Double => format!("Val::D({e})"),
                DataType::Logical => format!("Val::L({e})"),
                DataType::Character => format!("Val::C({e})"),
                _ => bail!("invalid type in DATA clist"),
            };

            // TODO: could generate more efficient code here
            if let Some(reps) = reps {
                let reps_ex = self.emit_expression(loc, reps)?;
                if needs_into_iter {
                    code += "    ].into_iter()";
                    needs_into_iter = false;
                } else {
                    code += "    ])";
                }
                code +=
                    &format!(".chain(std::iter::repeat_n({val}, {reps_ex} as usize)).chain([\n");
            } else {
                code += &format!("    {val},\n");
            }
        }

        if needs_into_iter {
            code += "  ].into_iter();\n";
        } else {
            code += "  ]);\n\n";
        }
        code += &self.emit_data_nlist::<NlistCallbackData>(loc, &data.nlist, Ctx::SaveInit)?;
        code += "\n";
        code += "  debug_assert!(clist.next().is_none(), \"DATA not fully initialised\");\n";
        code += "}\n";

        Ok(code)
    }

    // Used for DATA, READ, WRITE. Recursively handles implied-DO loops
    fn emit_data_nlist<C: NlistCallback>(
        &self,
        loc: &SourceLoc,
        nlist: &[Expression],
        ctx: Ctx,
    ) -> Result<String> {
        let mut code = String::new();

        for v in nlist {
            match v {
                Expression::Symbol(name) => {
                    let sym = self.syms.get(name)?;
                    let vt = v.resolve_type(loc, &self.syms)?;
                    let s = self.emit_symbol(loc, name, ctx)?;

                    if !sym.ast.dims.is_empty() {
                        code += &C::array(&s, &vt)?;
                    } else {
                        code += &C::scalar(&s, &vt)?;
                    }
                }
                Expression::ArrayElement(name, idx) => {
                    let vt = v.resolve_type(loc, &self.syms)?;
                    let s = self.emit_symbol(loc, name, ctx)?;
                    let idx_ex = self.emit_index(loc, idx)?;
                    code += &C::element(&s, &idx_ex, &vt)?;
                }
                Expression::Substring(name, e1, e2) => {
                    let s = self.emit_symbol(loc, name, ctx)?;
                    let range = self.emit_range(loc, e1, e2)?;
                    code += &C::substring(&s, &range)?;
                }
                Expression::SubstringArrayElement(name, idx, e1, e2) => {
                    let s = self.emit_symbol(loc, name, ctx)?;
                    let idx_ex = self.emit_expressions(loc, idx, Ctx::Value)?;
                    let range = self.emit_range(loc, e1, e2)?;
                    code += &C::substring_element(&s, &idx_ex, &range)?;
                }
                Expression::ImpliedDo {
                    data,
                    do_var,
                    e1,
                    e2,
                    e3,
                } => {
                    let m1 = self.emit_expression(loc, e1)?;
                    let m2 = self.emit_expression(loc, e2)?;
                    let m3 = e3
                        .clone()
                        .map_or_else(|| Ok("1".to_owned()), |e| self.emit_expression(loc, &e))?;

                    code += &format!("for {do_var} in intrinsics::range({m1}, {m2}, {m3}) {{\n");
                    code += &self.emit_data_nlist::<C>(loc, data, ctx)?;
                    code += "}\n";
                }

                Expression::Unary(..)
                | Expression::Binary(..)
                | Expression::Function(..)
                | Expression::Constant(..) => {
                    let vt = v.resolve_type(loc, &self.syms)?;
                    let e = self.emit_expression(loc, v)?;
                    code += &C::value(&e, &vt)?;
                }
                Expression::ImpliedDoVar(_) => bail!("{loc} invalid expression in nlist"),
            }
        }

        Ok(code)
    }

    fn emit_statements(
        &self,
        entry: &ast::Entry,
        statements: &Vec<(SourceLoc, Statement)>,
    ) -> Result<String> {
        let mut code = String::new();
        for (loc, statement) in statements {
            code += &self.emit_statement(loc, entry, statement)?;
        }
        Ok(code)
    }

    fn emit_arith_conversion(
        &self,
        loc: &SourceLoc,
        target: DataType,
        value: DataType,
        e: String,
    ) -> Result<String> {
        Ok(match (&target, &value) {
            (DataType::Integer, DataType::Integer)
            | (DataType::Real, DataType::Real)
            | (DataType::Double, DataType::Double)
            | (DataType::Logical, DataType::Logical) => e,
            (DataType::Integer, DataType::Real) => format!("{e} as i32"),
            (DataType::Integer, DataType::Double) => format!("{e} as i32"),
            (DataType::Real, DataType::Integer) => format!("{e} as f32"),
            (DataType::Real, DataType::Double) => format!("{e} as f32"),
            (DataType::Double, DataType::Integer) => format!("{e} as f64"),
            (DataType::Double, DataType::Real) => format!("{e} as f64"),
            _ => bail!("{loc} invalid arithmetic conversion from {value:?} to {target:?}"),
        })
    }

    fn emit_assignment(
        &self,
        loc: &SourceLoc,
        target: &Expression,
        value: &Expression,
        ctx: Ctx,
    ) -> Result<String> {
        let mut code = String::new();

        let tt = target.resolve_type(loc, &self.syms)?;
        let e = self.emit_expression(loc, value)?;
        match target {
            Expression::Symbol(name) => {
                let s = self.emit_symbol(loc, name, ctx)?;
                if matches!(tt, DataType::Character) {
                    let aliased = value.uses_symbol(name);
                    if aliased {
                        code += &format!("let val = {e}.to_vec();\n");
                        code += &format!("fstr::assign({s}, &val);\n");
                    } else {
                        code += &format!("fstr::assign({s}, {e});\n");
                    }
                } else {
                    let e = self.emit_arith_conversion(
                        loc,
                        tt,
                        value.resolve_type(loc, &self.syms)?,
                        e,
                    )?;
                    code += &format!("{s} = {e};\n");
                }
            }
            Expression::ArrayElement(name, idx) => {
                let s = self.emit_symbol(loc, name, ctx)?;
                let idx_ex = self.emit_index(loc, idx)?;
                if matches!(tt, DataType::Character) {
                    let aliased = value.uses_symbol(name);
                    if aliased {
                        code += &format!("let val = {e}.to_vec();\n");
                        code += &format!("fstr::assign({s}.get_mut({idx_ex}), &val);\n");
                    } else {
                        code += &format!("fstr::assign({s}.get_mut({idx_ex}), {e});\n");
                    }
                } else {
                    let e = self.emit_arith_conversion(
                        loc,
                        tt,
                        value.resolve_type(loc, &self.syms)?,
                        e,
                    )?;
                    code += &format!("{s}[{idx_ex}] = {e};\n");
                }
            }
            Expression::Substring(name, e1, e2) => {
                let s = self.emit_symbol(loc, name, ctx)?;
                let range = self.emit_range(loc, e1, e2)?;

                let aliased = value.uses_symbol(name);
                if aliased {
                    code += &format!("let val = {e}.to_vec();\n");
                    code += &format!("fstr::assign(fstr::substr_mut({s}, {range}), &val);\n");
                } else {
                    code += &format!("fstr::assign(fstr::substr_mut({s}, {range}), {e});\n");
                }
            }
            Expression::SubstringArrayElement(name, idx, e1, e2) => {
                let s = self.emit_symbol(loc, name, ctx)?;
                let idx_ex = self.emit_expressions(loc, idx, Ctx::Value)?;
                let range = self.emit_range(loc, e1, e2)?;

                let aliased = value.uses_symbol(name);

                if aliased {
                    code += &format!("let val = {e}.to_vec();\n");
                    code += &format!(
                        "fstr::assign(fstr::substr_mut({s}.get_mut({idx_ex}), {range}), &val);\n"
                    );
                } else {
                    code += &format!(
                        "fstr::assign(fstr::substr_mut({s}.get_mut({idx_ex}), {range}), {e});\n"
                    );
                }
            }
            _ => bail!("{loc} invalid assignment LHS"),
        }

        Ok(code)
    }

    fn emit_statement(
        &self,
        loc: &SourceLoc,
        entry: &ast::Entry,
        statement: &Statement,
    ) -> Result<String> {
        let mut code = String::new();
        match statement {
            Statement::Comment(c) => {
                if let Some(c) = format_comment_block(&strip_comment_sections(&c), "//", false) {
                    code += &c;
                }
            }
            Statement::Blank => {
                code += "\n";
            }

            Statement::Assignment(v, e) => {
                code += &self.emit_assignment(loc, v, e, Ctx::Assignment)?;
            }
            Statement::If { es, bodies } => {
                for (i, (e, body)) in es.iter().zip(bodies.iter()).enumerate() {
                    if i > 0 {
                        code += " else ";
                    }

                    let body = self.emit_statements(entry, body)?;

                    if let Some(e) = e {
                        let e = self.emit_expression(loc, e)?;
                        code += &format!("if {e} ");
                    }

                    code += " {\n";
                    code += &body;
                    code += "}";
                }
                code += "\n";
            }
            Statement::Do {
                var,
                e1,
                e2,
                e3,
                body,
            } => {
                let var_sym = self.syms.get(var)?;

                assert!(var_sym.ast.do_var);
                if var_sym.ast.base_type != DataType::Integer {
                    // TODO: maybe support reals/doubles (though it's obsolescent in Fortran 90)
                    bail!("{loc} DO is currently only supported over INTEGER");
                }

                let m1 = self.emit_expression(loc, e1)?;
                let m2 = self.emit_expression(loc, e2)?;
                let body = self.emit_statements(entry, body)?;

                // Step defaults to 1
                let m3 = e3
                    .clone()
                    .map_or_else(|| Ok("1".to_owned()), |e| self.emit_expression(loc, &e))?;

                if var_sym.ast.outside_do {
                    // If the DO-var is accessed outside, we'll fall back to implementing the
                    // loop as the standard describes it. (TODO: could make this much less ugly)
                    code += "{\n";
                    code += &format!("  let m1__: i32 = {m1};\n");
                    code += &format!("  let m2__: i32 = {m2};\n");
                    code += &format!("  let m3__: i32 = {m3};\n");
                    code += &(self.emit_symbol(loc, var, Ctx::Assignment)? + " = m1__;\n");
                    code += "  for _ in 0..((m2__ - m1__ + m3__) / m3__) as i32 {\n";
                    code += &body;
                    code += &(self.emit_symbol(loc, var, Ctx::Assignment)? + " += m3__;\n");
                    code += "  }\n";
                    code += "}\n";
                } else if e3.is_some() {
                    code += &format!("for {var} in intrinsics::range({m1}, {m2}, {m3}) {{\n");
                    code += &body;
                    code += "}\n";
                } else {
                    code += &format!("for {var} in {m1} ..= {m2} {{\n");
                    code += &body;
                    code += "}\n";
                }
            }
            Statement::DoWhile { e, body } => {
                let e = self.emit_expression(loc, e)?;
                let body = self.emit_statements(entry, body)?;
                code += &format!("while {e} {{\n{body}}}\n");
            }
            Statement::Stop => {
                code += "ctx.stop()?;\n";
            }
            Statement::Read {
                unit,
                fmt,
                other,
                iolist,
            } => {
                // TODO: refactor the shared code with ::Write

                code += "{\n";
                code += "  use f2rust_std::{data::Val, io::{self, Reader}};\n\n";

                let unit = match unit {
                    Specifier::Asterisk => "ctx.default_read_unit()?".to_owned(),
                    Specifier::Expression(e) => {
                        if e.resolve_type(loc, &self.syms)? != DataType::Integer {
                            bail!("{loc} TODO: internal files not supported in READ");
                        }
                        format!(
                            "ctx.io_unit({})?",
                            self.emit_expression_ctx(loc, e, Ctx::ArgScalar)?
                        )
                    }
                };
                let rec = match other.get("REC") {
                    Some(e) => format!(
                        "Some({})",
                        self.emit_expression_ctx(loc, e, Ctx::ArgScalar)?
                    ),
                    None => "None".to_owned(),
                };

                code += "  let mut reader = ";
                match fmt {
                    Some(ast::Specifier::Expression(e)) => {
                        let fmt = self.emit_expression_ctx(loc, e, Ctx::ArgScalar)?;
                        code += &format!("io::FormattedReader::new({unit}, {rec}, {fmt})?;\n");
                    }
                    Some(ast::Specifier::Asterisk) => {
                        code += &format!("io::ListDirectedReader::new({unit}, {rec})?;\n");
                    }
                    None => {
                        code += &format!("io::UnformattedReader::new({unit}, {rec})?;\n");
                    }
                }

                for name in other.keys() {
                    if !matches!(name.as_str(), "IOSTAT" | "REC") {
                        bail!("{loc} unrecognised specifier {name} in READ");
                    }
                }

                if let Some(iostat) = other.get("IOSTAT") {
                    let e = self.emit_expression_ctx(loc, iostat, Ctx::Assignment)?;
                    code += &format!("  {e} = io::capture_iostat(|| {{\n");
                }
                code += "    reader.start()?;\n";
                code += &self.emit_data_nlist::<NlistCallbackRead>(loc, iolist, Ctx::Assignment)?;
                code += "    reader.finish()?;\n";
                if other.contains_key("IOSTAT") {
                    code += "    Ok(())\n";
                    code += "  })?;\n";
                }
                code += "}\n";
            }
            Statement::Write {
                unit,
                fmt,
                other,
                iolist,
            } => {
                code += "{\n";
                code += "  use f2rust_std::{data::Val, io::{self, Writer}};\n\n";

                let unit = match unit {
                    Specifier::Asterisk => "ctx.default_write_unit()?".to_owned(),
                    Specifier::Expression(e) => {
                        if e.resolve_type(loc, &self.syms)? == DataType::Integer {
                            format!(
                                "ctx.io_unit({})?",
                                self.emit_expression_ctx(loc, e, Ctx::ArgScalar)?
                            )
                        } else {
                            code += &format!(
                                "  let internal_file = io::InternalFile::open({});\n",
                                self.emit_expression_ctx(loc, e, Ctx::ArgScalarMut)?
                            );
                            "internal_file".to_owned()
                        }
                    }
                };
                let rec = match other.get("REC") {
                    Some(e) => format!(
                        "Some({})",
                        self.emit_expression_ctx(loc, e, Ctx::ArgScalar)?
                    ),
                    None => "None".to_owned(),
                };

                code += "  let mut writer = ";
                match fmt {
                    Some(ast::Specifier::Expression(e)) => {
                        let fmt = self.emit_expression_ctx(loc, e, Ctx::ArgScalar)?;
                        code += &format!("io::FormattedWriter::new({unit}, {rec}, {fmt})?;\n");
                    }
                    Some(ast::Specifier::Asterisk) => {
                        code += &format!("io::ListDirectedWriter::new({unit}, {rec})?;\n");
                    }
                    None => {
                        code += &format!("io::UnformattedWriter::new({unit}, {rec})?;\n");
                    }
                }

                for name in other.keys() {
                    if !matches!(name.as_str(), "IOSTAT" | "REC") {
                        bail!("{loc} unrecognised specifier {name} in WRITE");
                    }
                }

                if let Some(iostat) = other.get("IOSTAT") {
                    let e = self.emit_expression_ctx(loc, iostat, Ctx::Assignment)?;
                    code += &format!("  {e} = io::capture_iostat(|| {{\n");
                }
                code += "    writer.start()?;\n";
                code += &self.emit_data_nlist::<NlistCallbackWrite>(loc, iolist, Ctx::Value)?;
                code += "    writer.finish()?;\n";
                if other.contains_key("IOSTAT") {
                    code += "    Ok(())\n";
                    code += "  })?;\n";
                }
                code += "}\n";
            }
            Statement::Print { fmt, iolist } => {
                code += "{\n";
                code += "  use f2rust_std::{data::Val, io::{self, Writer}};\n\n";

                code += "  let mut writer = ";
                match fmt {
                    ast::Specifier::Expression(e) => {
                        let fmt = self.emit_expression_ctx(loc, e, Ctx::ArgScalar)?;
                        code += &format!(
                            "io::FormattedWriter::new(ctx.default_write_unit()?, None, {fmt})?;\n"
                        );
                    }
                    ast::Specifier::Asterisk => {
                        code += "io::ListDirectedWriter::new(ctx.default_write_unit()?, None)?;\n";
                    }
                }
                code += "  writer.start()?;\n";
                code += &self.emit_data_nlist::<NlistCallbackWrite>(loc, iolist, Ctx::Value)?;
                code += "  writer.finish()?;\n";
                code += "}\n";
            }
            Statement::Open(specs) => {
                code += "{\n";
                code += "  use f2rust_std::io;\n\n";
                code += "  let specs = io::OpenSpecs {\n";

                match specs.get("UNIT") {
                    None => bail!("{loc} OPEN must have UNIT"),
                    Some(e) => {
                        if e.resolve_type(loc, &self.syms)? != DataType::Integer {
                            bail!("{loc} OPEN must not use internal files");
                        }
                    }
                }

                for (name, e) in specs {
                    match name.as_str() {
                        "IOSTAT" => (),
                        "UNIT" | "FILE" | "STATUS" | "ACCESS" | "FORM" | "RECL" => {
                            let e = self.emit_expression_ctx(loc, e, Ctx::ArgScalar)?;
                            code += &format!("    {}: Some({e}),\n", name.to_ascii_lowercase());
                        }
                        "BLANK" => {
                            bail!("{loc} TODO: OPEN has unsupported specifier {name}")
                        }
                        _ => bail!("{loc} OPEN has invalid specifier {name}"),
                    }
                }

                code += "    ..Default::default()\n";
                code += "  };\n";

                if let Some(iostat) = specs.get("IOSTAT") {
                    let e = self.emit_expression_ctx(loc, iostat, Ctx::Assignment)?;
                    code += &format!("  {e} = io::capture_iostat(|| ctx.open(specs))?;\n");
                } else {
                    code += "  ctx.open(specs)?;\n";
                }
                code += "}\n";
            }
            Statement::Close(specs) => {
                code += "{\n";
                code += "  use f2rust_std::io;\n\n";
                code += "  let specs = io::CloseSpecs {\n";

                match specs.get("UNIT") {
                    None => bail!("{loc} CLOSE must have UNIT"),
                    Some(e) => {
                        if e.resolve_type(loc, &self.syms)? != DataType::Integer {
                            bail!("{loc} CLOSE must not use internal files");
                        }
                    }
                }

                for (name, e) in specs {
                    match name.as_str() {
                        "IOSTAT" => (),
                        "UNIT" | "STATUS" => {
                            let e = self.emit_expression_ctx(loc, e, Ctx::ArgScalar)?;
                            code += &format!("    {}: Some({e}),\n", name.to_ascii_lowercase());
                        }
                        _ => bail!("{loc} CLOSE has invalid specifier {name}"),
                    }
                }

                code += "    ..Default::default()\n";
                code += "  };\n";

                if let Some(iostat) = specs.get("IOSTAT") {
                    let e = self.emit_expression_ctx(loc, iostat, Ctx::Assignment)?;
                    code += &format!("  {e} = io::capture_iostat(|| ctx.close(specs))?;\n");
                } else {
                    code += "  ctx.close(specs)?;\n";
                }
                code += "}\n";
            }
            Statement::Inquire(specs) => {
                code += "{\n";
                code += "  use f2rust_std::io;\n\n";
                code += "  let specs = io::InquireSpecs {\n";

                match specs.get("UNIT") {
                    None => {
                        if !specs.contains_key("FILE") {
                            bail!("{loc} INQUIRE must have either FILE or UNIT");
                        }
                    }
                    Some(e) => {
                        if e.resolve_type(loc, &self.syms)? != DataType::Integer {
                            bail!("{loc} INQUIRE must not use internal files");
                        }
                        if specs.contains_key("FILE") {
                            bail!("{loc} INQUIRE must not have both FILE and UNIT");
                        }
                    }
                }

                for (name, e) in specs {
                    match name.as_str() {
                        "IOSTAT" => (),
                        "UNIT" | "FILE" => {
                            let e = self.emit_expression_ctx(loc, e, Ctx::ArgScalar)?;
                            code += &format!("    {}: Some({e}),\n", name.to_ascii_lowercase());
                        }
                        "EXIST" | "OPENED" | "NUMBER" | "NAMED" | "NAME" => {
                            let e = self.emit_expression_ctx(loc, e, Ctx::ArgScalarMut)?;
                            code += &format!("    {}: Some({e}),\n", name.to_ascii_lowercase());
                        }
                        "ACCESS" | "SEQUENTIAL" | "DIRECT" | "FORM" | "FORMATTED"
                        | "UNFORMATTED" | "RECL" | "NEXTREC" | "BLANK" => {
                            bail!("{loc} TODO: INQUIRE has unsupported specifier {name}")
                        }
                        _ => bail!("{loc} INQUIRE has invalid specifier {name}"),
                    }
                }

                code += "    ..Default::default()\n";
                code += "  };\n";

                if let Some(iostat) = specs.get("IOSTAT") {
                    let e = self.emit_expression_ctx(loc, iostat, Ctx::Assignment)?;
                    code += &format!("  {e} = io::capture_iostat(|| ctx.inquire(specs))?;\n");
                } else {
                    code += "  ctx.inquire(specs)?;\n";
                }
                code += "}\n";
            }
            Statement::Backspace(specs) => {
                code +=
                    &self.emit_io_statement(loc, specs, "BACKSPACE", "PosSpecs", "backspace")?;
            }
            Statement::Endfile(specs) => {
                code += &self.emit_io_statement(loc, specs, "ENDFILE", "PosSpecs", "endfile")?;
            }
            Statement::Rewind(specs) => {
                code += &self.emit_io_statement(loc, specs, "REWIND", "PosSpecs", "rewind")?;
            }
            Statement::Call(name, args) => {
                code += &self
                    .emit_call(loc, name, args, false)
                    .with_context(|| format!("failed statement {:?}", statement))?;
                code += ";\n";
            }
            Statement::Return => {
                let returns_result = self
                    .globan
                    .returns_result(&self.program.namespace, &entry.name)?;

                if matches!(self.program.ast.ty, ast::ProgramUnitType::Function)
                    && self.syms.get(&entry.name)?.ast.base_type != DataType::Character
                {
                    let name = self.emit_symbol(loc, &entry.name, Ctx::Value)?;
                    if returns_result {
                        code += &format!("return Ok({name});\n");
                    } else {
                        code += &format!("return {name};\n");
                    }
                } else {
                    if returns_result {
                        code += "return Ok(());\n";
                    } else {
                        code += "return;\n";
                    }
                };
            }
        }

        Ok(code)
    }

    // TODO: refactor this to support the more complex IO commands
    fn emit_io_statement(
        &self,
        loc: &SourceLoc,
        specs: &ast::Specifiers,
        statement: &str,
        specs_name: &str,
        method: &str,
    ) -> Result<String> {
        let mut code = String::new();

        code += "{\n";
        code += "  use f2rust_std::io;\n\n";
        code += &format!("  let specs = io::{specs_name} {{\n");

        match specs.get("UNIT") {
            None => bail!("{loc} {statement} must have UNIT"),
            Some(e) => {
                if e.resolve_type(loc, &self.syms)? != DataType::Integer {
                    bail!("{loc} {statement} must not use internal files");
                }
            }
        }

        for (name, e) in specs {
            match name.as_str() {
                "IOSTAT" => (),
                "UNIT" => {
                    let e = self.emit_expression_ctx(loc, e, Ctx::ArgScalar)?;
                    code += &format!("    {}: Some({e}),\n", name.to_ascii_lowercase());
                }
                _ => bail!("{loc} {statement} has invalid specifier {name}"),
            }
        }

        code += "    ..Default::default()\n";
        code += "  };\n";

        if let Some(iostat) = specs.get("IOSTAT") {
            let e = self.emit_expression_ctx(loc, iostat, Ctx::Assignment)?;
            code += &format!("  {e} = io::capture_iostat(|| ctx.{method}(specs))?;\n");
        } else {
            code += &format!("  ctx.{method}(specs)?;\n");
        }
        code += "}\n";

        Ok(code)
    }

    fn emit_constants(&self, api: bool) -> Result<String> {
        let mut code = String::new();

        for (name, sym) in self.syms.iter() {
            let loc = &sym.ast.loc;

            let vis = if api || sym.ast.parameter_public {
                "pub "
            } else {
                ""
            };

            if let Some(param) = &sym.ast.parameter {
                if let DataType::Character = &sym.ast.base_type {
                    let len = sym
                        .ast
                        .character_len
                        .as_ref()
                        .expect("{loc} character sym must have len");

                    let param_exp = self.emit_expression(loc, param)?;

                    match len {
                        LenSpecification::Unspecified => {
                            bail!("{loc} unspecified character length")
                        }
                        LenSpecification::Asterisk => {
                            if api {
                                // Hack: Convert b"" to ""
                                let s = param_exp.strip_prefix("b").unwrap().to_owned();
                                code += &format!("{vis}const {name}: &str = {s};\n")
                            } else {
                                code += &format!("{vis}const {name}: &[u8] = {param_exp};\n")
                            }
                        }
                        LenSpecification::Integer(n) => {
                            code += &format!(
                                "{vis}const {name}: &[u8; {n}] = &fstr::extend_const::<{n}>({param_exp});\n"
                            );
                        }
                        LenSpecification::IntConstantExpr(e) => match param {
                            Expression::Constant(Constant::Character(s))
                                if api && e.eval_constant(&self.syms)? == Some(s.len() as i32) =>
                            {
                                // Simplify the code for API constants, when the declared size
                                // matches the actual string
                                code += &format!(
                                    "{vis}const {name}: &str = \"{}\";\n",
                                    s.escape_default()
                                );
                            }
                            _ => {
                                let e = self.emit_expression(loc, e)?;
                                code += &format!(
                                    "{vis}const {name}: &[u8; {e} as usize] = &fstr::extend_const::<{{{e} as usize}}>({param_exp});\n"
                                );
                            }
                        },
                    }
                } else {
                    let ty = emit_datatype(&sym.ast.base_type);

                    let param_ex = self.emit_expression(loc, param)?;

                    let param_ex = self.emit_arith_conversion(
                        loc,
                        sym.ast.base_type.clone(),
                        param.resolve_type(loc, &self.syms)?,
                        param_ex,
                    )?;

                    code += &format!("{vis}const {name}: {ty} = {param_ex};\n");
                }
            }
        }

        code += "\n";

        Ok(code)
    }

    fn emit_save_struct(&self) -> Result<String> {
        let mut code = String::new();

        // Check if we actually need to save anything in this entry
        let saved = self
            .syms
            .iter()
            .filter(|(_, sym)| sym.ast.save)
            .collect::<Vec<_>>();
        if saved.is_empty() {
            return Ok(code);
        }

        // Declare struct's members
        code += "struct SaveVars {\n";
        for (name, sym) in &saved {
            let decl = self.emit_symbol(&sym.ast.loc, name, Ctx::SaveStruct)?;
            code += &format!("  {decl},\n");
        }
        code += "}\n\n";

        // Default-initialise members
        code += "impl SaveInit for SaveVars {\n";
        code += "  fn new() -> Self {\n";
        for (name, sym) in &saved {
            code += &self.emit_initialiser(&sym.ast.loc, name, sym, &self.syms, true)?;
        }
        code += "\n";

        // DATA initialisation
        for data in &self.program.ast.datas {
            code += &self.emit_data_init(data)?;
        }
        code += "\n";

        // Return the object
        code += "    Self {\n";
        for (name, _sym) in &saved {
            code += &format!("  {name},\n");
        }
        code += "    }\n";
        code += "  }\n";
        code += "}\n\n";

        Ok(code)
    }
}

impl<'a> CodeGen<'a> {
    pub fn new(globan: &'a globan::GlobalAnalysis, program: &'a globan::ProgramUnit) -> Self {
        let entries = program
            .ast
            .entries
            .iter()
            .map(|entry| {
                let syms = IndexMap::from_iter(program.symbols.iter().map(|(name, sym)| {
                    let mutated =
                        sym.mutated.contains(&entry.name) || (sym.ast.do_var && sym.ast.outside_do);
                    (name.clone(), Symbol::new(sym, mutated))
                }));

                Entry {
                    codegen: CodeGenUnit {
                        globan,
                        program,
                        syms: SymbolTable::new(syms),
                    },
                    ast: entry,
                }
            })
            .collect();

        let statement_functions = program
            .ast
            .statement_functions
            .iter()
            .map(|statement_function| {
                let syms = IndexMap::from_iter(program.symbols.iter().map(|(name, sym)| {
                    if statement_function.dargs.contains(name)
                        || statement_function.captured.contains(name)
                    {
                        // The original symbol might be SAVE etc, and we need to treat it
                        // as a darg when compiling this function
                        let darg_ast = ast::Symbol {
                            darg: true,
                            do_var: false,
                            save: false,
                            ..sym.ast.clone()
                        };
                        let darg_sym = globan::Symbol {
                            ast: darg_ast,
                            actual_procs: sym.actual_procs.clone(),
                            mutated: sym.mutated.clone(),
                        };
                        (name.clone(), Symbol::new(&darg_sym, false))
                    } else {
                        (name.clone(), Symbol::new(sym, false))
                    }
                }));

                StatementFunction {
                    codegen: CodeGenUnit {
                        globan,
                        program,
                        syms: SymbolTable::new(syms),
                    },
                    ast: statement_function,
                }
            })
            .collect();

        // SAVE and PARAMETER constants are shared by all entries, and will need to be emitted
        // before any entries, so gather them now
        let shared_syms = IndexMap::from_iter(program.symbols.iter().filter_map(|(name, sym)| {
            if sym.ast.save || sym.ast.parameter.is_some() {
                Some((name.clone(), Symbol::new(sym, false)))
            } else {
                None
            }
        }));

        Self {
            shared: CodeGenUnit {
                globan,
                program,
                syms: SymbolTable::new(shared_syms),
            },
            entries,
            statement_functions,
        }
    }

    pub fn emit(&mut self, api: bool) -> Result<(String, String)> {
        let mut code = String::new();
        let mut code_api = String::new();

        code += &self.shared.emit_constants(false)?;
        code += &self.shared.emit_save_struct()?;

        for statement_function in &self.statement_functions {
            let codegen = &statement_function.codegen;
            let loc = &statement_function.ast.loc;

            let darg_names: Vec<_> = statement_function
                .ast
                .dargs
                .iter()
                .chain(&statement_function.ast.captured)
                .collect();
            let dargs = darg_names
                .iter()
                .map(|darg| codegen.emit_symbol(loc, darg, Ctx::DummyArg));

            let func_name = &statement_function.ast.name;

            let ret_type = &statement_function
                .codegen
                .syms
                .get(func_name)?
                .ast
                .base_type;

            let ret = format!("-> {}", emit_datatype(ret_type));

            let dargs = dargs.collect::<Result<Vec<_>>>()?;
            let dargs = dargs.join(", ");
            code += &format!("fn {func_name}({dargs}) {ret} {{\n");

            for name in &darg_names {
                let sym = codegen.syms.get(name)?;
                code += &codegen.emit_initialiser(
                    loc,
                    name,
                    &sym,
                    &statement_function.codegen.syms,
                    false,
                )?;
            }

            code += &statement_function
                .codegen
                .emit_expression(loc, &statement_function.ast.body)?;
            code += "\n}\n\n";
        }

        for entry in &self.entries {
            if api && entry.ast.api_name.is_some() {
                code += &self.emit_api(entry, true)?;
                code_api += &self.emit_api(entry, false)?;
            }

            let pre_comments = entry
                .ast
                .pre_comments
                .iter()
                .filter_map(|c| format_comment_block(&strip_comment_sections(c), "//", false))
                .collect::<Vec<_>>()
                .join("\n");

            let post_comments = entry
                .ast
                .post_comments
                .iter()
                .filter_map(|c| format_comment_block(&strip_comment_sections(c), "//", false))
                .collect::<Vec<_>>()
                .join("\n");

            let dargs = entry.ast.dargs.iter().map(|darg| {
                entry
                    .codegen
                    .emit_symbol(&entry.ast.loc, darg, Ctx::DummyArg)
            });

            let entry_name = &entry.ast.name;

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
                    format!("-> f2rust_std::Result<{}>", emit_datatype(ret_type))
                } else {
                    format!("-> {}", emit_datatype(ret_type))
                }
            } else {
                if returns_result {
                    "-> f2rust_std::Result<()>".to_owned()
                } else {
                    "".to_owned()
                }
            };

            let mut dargs = dargs.collect::<Result<Vec<_>>>()?;
            if entry
                .codegen
                .globan
                .requires_ctx(&entry.codegen.program.namespace, entry_name)?
            {
                dargs.push("ctx: &mut Context".to_owned());
            }
            let dargs = dargs.join(", ");
            code += &pre_comments;
            code += &format!("pub fn {entry_name}({dargs}) {ret} {{\n");
            code += &entry.codegen.emit_save_borrow(entry_name)?;
            code += &entry.codegen.emit_locals(entry_name)?;
            code += &entry.codegen.emit_statements(entry.ast, &entry.ast.body)?;
            if matches!(ret_type, DataType::Void | DataType::Character) {
                if returns_result {
                    code += "Ok(())\n";
                }
            } else {
                let name = entry
                    .codegen
                    .emit_symbol(&entry.ast.loc, entry_name, Ctx::Value)?;
                if returns_result {
                    code += &format!("Ok({name})\n");
                } else {
                    code += &format!("{name}\n");
                }
            };
            code += "}\n";
            code += &post_comments;
            code += "\n";
        }

        Ok((code, code_api))
    }
}

pub fn strip_comment_sections(block: &[(String, bool)]) -> Vec<String> {
    block
        .iter()
        .filter_map(|(c, in_section)| if *in_section { None } else { Some(c.clone()) })
        .collect()
}

// Convert SPICE's comment blocks into more conventional Rust style.
// (Specifically we change the indentation, because FORTRAN indents after the "C"
// while Rust indents before the "//")
pub fn format_comment_block(
    block: &[String],
    prefix: &str,
    trim_empty_lines: bool,
) -> Option<String> {
    if block.is_empty() {
        return None;
    }

    // Find the amount of leading whitespace in each line
    let indents = block.iter().filter_map(|line| {
        let trimmed = line.trim_ascii_start();
        if trimmed.is_empty() {
            None
        } else {
            Some(line.len() - trimmed.len())
        }
    });

    // If it's >0, we want to trim each line so there's only 1 space after the "//"
    let trim = if let Some(indent) = indents.min() {
        if indent == 0 { 0 } else { indent - 1 }
    } else {
        0
    };

    let mut lines: Vec<_> = if trim_empty_lines {
        block.iter().skip_while(|line| line.is_empty()).collect()
    } else {
        block.iter().collect()
    };

    if trim_empty_lines {
        while lines.pop_if(|line| line.is_empty()).is_some() {}
    }

    Some(
        lines
            .iter()
            .map(|line| {
                if line.len() < trim {
                    format!("{prefix}\n")
                } else {
                    format!("{prefix}{}\n", &line[trim..])
                }
            })
            .collect::<String>(),
    )
}

trait NlistCallback {
    fn array(s: &str, vt: &DataType) -> Result<String>;
    fn scalar(s: &str, vt: &DataType) -> Result<String>;
    fn value(e: &str, vt: &DataType) -> Result<String>;
    fn element(s: &str, idx: &str, vt: &DataType) -> Result<String>;
    fn substring(s: &str, range: &str) -> Result<String>;
    fn substring_element(s: &str, idx: &str, range: &str) -> Result<String>;
}

struct NlistCallbackData {}
impl NlistCallback for NlistCallbackData {
    fn array(s: &str, vt: &DataType) -> Result<String> {
        if matches!(vt, DataType::Character) {
            Ok(format!(
                "{s}.iter_mut().for_each(|n| fstr::assign(n, clist.next().unwrap().into_str()));\n"
            ))
        } else {
            let ty = emit_datatype(vt);
            Ok(format!(
                "{s}.iter_mut().for_each(|n| *n = clist.next().unwrap().into_{ty}());\n"
            ))
        }
    }

    fn scalar(s: &str, vt: &DataType) -> Result<String> {
        if matches!(vt, DataType::Character) {
            Ok(format!(
                "fstr::assign({s}, clist.next().unwrap().into_str());\n"
            ))
        } else {
            let ty = emit_datatype(vt);
            Ok(format!("{s} = clist.next().unwrap().into_{ty}();\n"))
        }
    }

    fn value(_e: &str, _vt: &DataType) -> Result<String> {
        bail!("invalid expression in DATA nlist");
    }

    fn element(s: &str, idx: &str, vt: &DataType) -> Result<String> {
        if matches!(vt, DataType::Character) {
            Ok(format!(
                "fstr::assign({s}.get_mut({idx}), clist.next().unwrap().into_str());\n"
            ))
        } else {
            let ty = emit_datatype(vt);
            Ok(format!("{s}[{idx}] = clist.next().unwrap().into_{ty}();\n"))
        }
    }
    fn substring(s: &str, range: &str) -> Result<String> {
        Ok(format!(
            "fstr::assign(fstr::substr_mut({s}, {range}), clist.next().unwrap().into_str());\n"
        ))
    }
    fn substring_element(s: &str, idx: &str, range: &str) -> Result<String> {
        Ok(format!(
            "fstr::assign(fstr::substr_mut({s}.get_mut({idx}), {range}), clist.next().unwrap().into_str());\n"
        ))
    }
}

struct NlistCallbackWrite {}
impl NlistCallback for NlistCallbackWrite {
    fn array(s: &str, vt: &DataType) -> Result<String> {
        if matches!(vt, DataType::Character) {
            Ok(format!("for n in {s}.iter() {{ writer.write_str(n)?; }}\n"))
        } else {
            let ty = emit_datatype(vt);
            Ok(format!(
                "for n in {s}.iter() {{ writer.write_{ty}(*n)?; }}\n"
            ))
        }
    }

    fn scalar(s: &str, vt: &DataType) -> Result<String> {
        let ty = emit_datatype(vt);
        Ok(format!("writer.write_{ty}({s})?;\n"))
    }

    fn value(e: &str, vt: &DataType) -> Result<String> {
        let ty = emit_datatype(vt);
        Ok(format!("writer.write_{ty}({e})?;\n"))
    }

    fn element(s: &str, idx: &str, vt: &DataType) -> Result<String> {
        if matches!(vt, DataType::Character) {
            Ok(format!("writer.write_str(&{s}[{idx}])?;\n"))
        } else {
            let ty = emit_datatype(vt);
            Ok(format!("writer.write_{ty}({s}[{idx}])?;\n"))
        }
    }

    fn substring(s: &str, range: &str) -> Result<String> {
        Ok(format!("writer.write_str(fstr::substr({s}, {range}))?;\n"))
    }

    fn substring_element(s: &str, idx: &str, range: &str) -> Result<String> {
        Ok(format!(
            "writer.write_str(fstr::substr({s}.get({idx}), {range}))?;\n"
        ))
    }
}

struct NlistCallbackRead {}
impl NlistCallback for NlistCallbackRead {
    fn array(s: &str, vt: &DataType) -> Result<String> {
        if matches!(vt, DataType::Character) {
            Ok(format!(
                "for n in {s}.iter_mut() {{ reader.read_str(n)?; }}\n"
            ))
        } else {
            let ty = emit_datatype(vt);
            Ok(format!(
                "for n in {s}.iter_mut() {{ *n = reader.read_{ty}()?; }}\n"
            ))
        }
    }

    fn scalar(s: &str, vt: &DataType) -> Result<String> {
        if matches!(vt, DataType::Character) {
            Ok(format!("reader.read_str({s})?;\n"))
        } else {
            let ty = emit_datatype(vt);
            Ok(format!("{s} = reader.read_{ty}()?;\n"))
        }
    }

    fn value(_e: &str, _vt: &DataType) -> Result<String> {
        bail!("cannot use expression in READ iolist");
    }

    fn element(s: &str, idx: &str, vt: &DataType) -> Result<String> {
        if matches!(vt, DataType::Character) {
            Ok(format!("reader.read_str(&mut {s}[{idx}])?;\n"))
        } else {
            let ty = emit_datatype(vt);
            Ok(format!("{s}[{idx}] = reader.read_{ty}()?;\n"))
        }
    }

    fn substring(s: &str, range: &str) -> Result<String> {
        Ok(format!(
            "reader.read_str(fstr::substr_mut({s}, {range}))?;\n"
        ))
    }

    fn substring_element(s: &str, idx: &str, range: &str) -> Result<String> {
        Ok(format!(
            "reader.read_str(fstr::substr_mut({s}.get_mut({idx}), {range}))?;\n"
        ))
    }
}
