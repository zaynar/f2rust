//! Code generation. Emits the Rust code for expressions, statements and program units.

use std::collections::{HashMap, HashSet};
use std::io::Write;

use anyhow::{Context, Result, bail};
use indexmap::IndexMap;
use log::warn;

use crate::ast::{DataType, Expression, LenSpecification, Statement};
use crate::grammar::{BinaryOp, Constant, UnaryOp};
use crate::{ast, globan, intrinsics};

/// The Rust representation of a symbol name or an expression, based on how it's
/// used in the main body of the function. (It may have different representations
/// in dargs etc, which get shadowed.)
#[derive(Debug)]
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
}

/// The syntactic contexts in which a symbol name or expression can be used
#[derive(Debug, Clone, Copy)]
enum Ctx {
    Value,            // `X + 1`
    ArgScalar,        // `F(X)` where F expects a scalar (primitive or string)
    ArgScalarMut,     // `F(&mut X)`
    ArgScalarAliased, // `F(&X.clone())`
    ArgArray,         // `F(X)` where F expects an array
    ArgArrayMut,      // `F(&mut X)`
    DummyArg,         // `fn F(X: T)`
    Assignment,       // `X = 1`
    SaveStruct,       // `struct SaveVars { X: T, }`
    SaveInit,         // `X = 1` inside `SaveInit::new()`
}

/// A symbol in a specific Entry
#[derive(Debug)]
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
struct SymbolTable(IndexMap<String, Symbol>);

impl SymbolTable {
    fn get(&self, name: &str) -> Result<&Symbol> {
        if let Some(sym) = self.0.get(name) {
            Ok(sym)
        } else {
            bail!("unrecognized symbol {name}")
        }
    }

    fn iter(&self) -> impl Iterator<Item = (&String, &Symbol)> {
        self.0.iter()
    }
}

impl Expression {
    /// Recursively determine the type of an expression
    fn resolve_type(&self, syms: &SymbolTable) -> Result<DataType> {
        Ok(match self {
            Expression::Unary(_op, e2) => e2.resolve_type(syms)?,
            Expression::Binary(op, e1, e2) => {
                let t1 = e1.resolve_type(syms)?;
                let t2 = e2.resolve_type(syms)?;
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
                            bail!("invalid types in arithmetic operator: {t1:?} {op:?} {t2:?}");
                        }
                    }
                    BinaryOp::Concat => {
                        if t1 == DataType::Character && t2 == DataType::Character {
                            DataType::Character
                        } else {
                            bail!("invalid types in CHARACTER concatenation: {t1:?} {t2:?}");
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
                            bail!("invalid types in relational operator: {t1:?} {op:?} {t2:?}");
                        }
                    }

                    BinaryOp::And | BinaryOp::Or | BinaryOp::Eqv | BinaryOp::Neqv => {
                        if t1 == DataType::Logical && t2 == DataType::Logical {
                            DataType::Logical
                        } else {
                            bail!("invalid types in logical operator: {t1:?} {op:?} {t2:?}");
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
                        .expect("intrinsic call must have some arguments")
                        .resolve_type(syms)?;
                    if let Some((ret_ty, _)) = intrinsics::call_info(name, Some(first_arg)) {
                        ret_ty
                    } else if intrinsics::exists(name) {
                        bail!("calling intrinsic {name} with incorrect argument type");
                    } else {
                        bail!("calling non-intrinsic function {name} without declared type");
                    }
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
            Expression::ImpliedDo { .. } => bail!("cannot resolve type of implied-DO"),
            Expression::ImpliedDoVar(_) => DataType::Integer,
            // (Don't support REAL implied-DO-vars, that'd just be silly)
        })
    }
}

/// Convert to Rust type, for use in various contexts
fn emit_datatype(ty: &DataType) -> String {
    match ty {
        DataType::Integer => "i32".to_owned(),
        DataType::Real => "f32".to_owned(),
        DataType::Double => "f64".to_owned(),
        DataType::Logical => "bool".to_owned(),
        DataType::Void => "()".to_owned(),

        // CHARACTER types are handled specially, so this should never be seen
        DataType::Character => "INVALID_TYPE_CHARACTER".to_owned(),

        // There shouldn't be any UNKNOWN at this stage
        DataType::Unknown => "INVALID_TYPE_UNKNOWN".to_owned(),

        // Procedures used as variables are Rust function pointers
        DataType::Procedure(args) => {
            if args.is_empty() {
                // TODO: why is this triggered sometimes?
                return "ERROR cannot find procedure's return type".to_owned();
            }

            let mut args = args.iter().map(|arg| {
                let ty = emit_datatype(&arg.base_type);
                if arg.is_array {
                    if arg.mutated {
                        format!("&mut [{ty}]")
                    } else {
                        format!("&[{ty}]")
                    }
                } else if arg.mutated {
                    format!("&mut {ty}")
                } else {
                    ty
                }
            });

            let ret = args.next().unwrap();
            let args = args.collect::<Vec<_>>().join(", ");
            format!("fn({args}) -> {ret}")
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

/// The various ways of calling a procedure or intrinsic
#[derive(Debug, Clone)]
pub enum CallSyntax {
    Unified,                // cannot emit directly
    External(globan::Name), // a::b(x)
    Cast(&'static str),     // x as a
    Func(&'static str),     // a(x)
    VarFunc(&'static str),  // a(&[x])  (used for variadic intrinsics)
}

#[derive(Debug, Clone)]
pub struct ProcedureArgs {
    pub return_type: DataType,
    pub dargs: Vec<globan::DummyArg>,
    pub codegen: CallSyntax,
    pub requires_ctx: bool,
}

impl CodeGenUnit<'_> {
    /// Emit a symbol in the appropriate form for its context.
    /// In FORTRAN everything is a pointer, but in Rust we have to explicitly
    /// convert between values, references, arrays, scalars, etc.
    fn emit_symbol(&self, name: &str, ctx: Ctx) -> Result<String> {
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
            },
            Ctx::ArgScalar => match sym.rs_ty {
                // When passing an array as a scalar, we take the first element
                RustType::ActualArray
                | RustType::ActualCharArray
                | RustType::DummyArray
                | RustType::DummyArrayMut
                | RustType::DummyCharArray
                | RustType::DummyCharArrayMut => format!("{name}.first()"),
                RustType::SaveActualArray | RustType::SaveActualCharArray => {
                    format!("save.{name}.first()")
                }
                _ => self.emit_symbol(name, Ctx::Value)?,
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
                | RustType::LocalDoVar => {
                    bail!("invalid context {ctx:?} for symbol {name}: {sym:?}")
                }
            },
            Ctx::ArgScalarAliased => match sym.rs_ty {
                // This is the non-mutable copy of a variable that's also being passed as
                // a mutable argument. If not pass-by-value, clone it
                RustType::CharVec => format!("&{name}.clone()"),
                RustType::CharSliceRef => format!("&{name}.to_vec()"),
                RustType::CharSliceMut => format!("&mut {name}.to_vec()"),
                RustType::SaveChar => format!("&save.{name}.to_vec()"),
                _ => self.emit_symbol(name, Ctx::ArgScalar)?,
            },
            Ctx::ArgArray => match sym.rs_ty {
                // When passing a scalar as an array, need to convert T to &[T]
                RustType::Primitive | RustType::PrimitiveMut => format!("&[{name}]"),
                RustType::PrimitiveRefMut => format!("&[*{name}]"),
                RustType::ActualArray => format!("&{name}"),
                RustType::ActualCharArray => format!("{name}.as_arg()"),
                RustType::DummyArray | RustType::DummyArrayMut => format!("&{name}"),
                RustType::DummyCharArray => format!("{name}.as_arg()"),
                RustType::DummyCharArrayMut => format!("{name}.as_arg()"),
                RustType::CharVec => format!("&[&{name}]"),
                RustType::CharSliceRef | RustType::CharSliceMut => format!("&[{name}]"),
                RustType::SavePrimitive => format!("&[save.{name}]"),
                RustType::SaveChar => format!("&[&save.{name}]"),
                RustType::SaveActualArray => format!("&save.{name}"),
                RustType::SaveActualCharArray => format!("save.{name}.as_arg()"),
                RustType::Procedure => format!("&[{name}]"),
                RustType::LocalDoVar => format!("&[{name}]"),
            },
            Ctx::ArgArrayMut => match sym.rs_ty {
                // slice::from_mut converts &mut T to &mut [T]
                RustType::PrimitiveMut => format!("std::slice::from_mut(&mut {name})"),
                RustType::PrimitiveRefMut => format!("std::slice::from_mut({name})"),
                RustType::ActualArray => format!("&mut {name}"),
                RustType::ActualCharArray => format!("{name}.as_arg_mut()"),
                RustType::DummyArrayMut => format!("&mut {name}"),
                RustType::DummyCharArrayMut => format!("{name}.as_arg_mut()"),
                RustType::CharVec => format!("std::slice::from_mut(&mut {name})"),
                RustType::CharSliceMut => format!("std::slice::from_mut(&mut {name})"),
                RustType::SavePrimitive => format!("std::slice::from_mut(&mut save.{name})"),
                RustType::SaveChar => format!("std::slice::from_mut(&mut save.{name})"),
                RustType::SaveActualArray => format!("std::slice::from_mut(&mut save.{name})"),
                RustType::SaveActualCharArray => {
                    format!("std::slice::from_mut(save.{name}.as_arg_mut())")
                }
                RustType::Primitive
                | RustType::DummyArray
                | RustType::DummyCharArray
                | RustType::CharSliceRef
                | RustType::Procedure
                | RustType::LocalDoVar => {
                    bail!("invalid context {ctx:?} for symbol {name}: {sym:?}")
                }
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
                | RustType::LocalDoVar => {
                    bail!("invalid context {ctx:?} for symbol {name}: {sym:?}")
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
                RustType::Primitive
                | RustType::DummyArray
                | RustType::DummyCharArray
                | RustType::CharSliceRef
                | RustType::Procedure
                | RustType::LocalDoVar => {
                    bail!("invalid context {ctx:?} for symbol {name}: {sym:?}")
                }
            },
            Ctx::SaveStruct => match sym.rs_ty {
                RustType::SavePrimitive => format!("{name}: {ty}"),
                RustType::SaveChar => format!("{name}: Vec<u8>"),
                RustType::SaveActualArray => match sym.ast.dims.len() {
                    1 => format!("{name}: ActualArray<{ty}>"),
                    n => format!("{name}: ActualArray{n}D<{ty}>"),
                },
                RustType::SaveActualCharArray => match sym.ast.dims.len() {
                    1 => format!("{name}: ActualCharArray"),
                    n => format!("{name}: ActualCharArray{n}D"),
                },
                _ => bail!("invalid context {ctx:?} for symbol {name}: {sym:?}"),
            },
            Ctx::SaveInit => match sym.rs_ty {
                RustType::SaveChar => format!("&mut {name}"),
                RustType::SavePrimitive
                | RustType::SaveActualArray
                | RustType::SaveActualCharArray => name.to_owned(),
                _ => bail!("invalid context {ctx:?} for symbol {name}: {sym:?}"),
            },
        };

        // Ok(format!("{s} /* {ctx:?} */"))
        Ok(s)
    }

    fn emit_expressions(&self, es: &[Expression], ctx: Ctx) -> Result<String> {
        Ok(es
            .iter()
            .map(|e| self.emit_expression_ctx(e, ctx))
            .collect::<Result<Vec<_>>>()?
            .join(", "))
    }

    fn emit_binop_arith(&self, e1: &Expression, e2: &Expression, op: &str) -> Result<String> {
        self.emit_binop_arith_rel(e1, e2, op, "", false)
    }

    fn emit_binop_rel(
        &self,
        e1: &Expression,
        e2: &Expression,
        op: &str,
        op_str: &str,
    ) -> Result<String> {
        self.emit_binop_arith_rel(e1, e2, op, op_str, true)
    }

    /// Common behaviour for arithmetic and relation operators
    fn emit_binop_arith_rel(
        &self,
        e1: &Expression,
        e2: &Expression,
        op: &str,
        op_str: &str,
        allow_char: bool,
    ) -> Result<String> {
        let t1 = e1.resolve_type(&self.syms)?;
        let t2 = e2.resolve_type(&self.syms)?;
        let a = self.emit_expression(e1)?;
        let b = self.emit_expression(e2)?;

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

            _ => bail!("invalid types in binary op '{op}': {t1:?}, {t2:?}"),
        })
    }

    fn emit_binop_pow(&self, e1: &Expression, e2: &Expression) -> Result<String> {
        let t1 = e1.resolve_type(&self.syms)?;
        let t2 = e2.resolve_type(&self.syms)?;
        let a = self.emit_expression(e1)?;
        let b = self.emit_expression(e2)?;

        Ok(match (&t1, &t2) {
            // Can't use i32::pow, because we need to support negative exponents
            (DataType::Integer, DataType::Integer) => {
                format!("intrinsics::pow({a}, {b})")
            }

            (DataType::Real, DataType::Integer) | (DataType::Double, DataType::Integer) => {
                format!("{a}.powi({b})")
            }

            (DataType::Integer, DataType::Real) => format!("({a} as f32).powf({b})"),
            (DataType::Integer, DataType::Double) => format!("({a} as f64).powf({b})"),

            (DataType::Real, DataType::Real) | (DataType::Double, DataType::Double) => {
                format!("{a}.powf({b})")
            }

            (DataType::Real, DataType::Double) => format!("({a} as f64).powf({b})"),
            (DataType::Double, DataType::Real) => format!("{a}.powf({b} as f64)"),

            _ => bail!("invalid types in **: {t1:?}, {t2:?}"),
        })
    }

    fn emit_binop_logic(&self, e1: &Expression, e2: &Expression, op: &str) -> Result<String> {
        let t1 = e1.resolve_type(&self.syms)?;
        let t2 = e2.resolve_type(&self.syms)?;
        let a = self.emit_expression(e1)?;
        let b = self.emit_expression(e2)?;

        Ok(match (&t1, &t2) {
            (DataType::Logical, DataType::Logical) => format!("({a} {op} {b})"),
            _ => bail!("invalid types in logical op '{op}': {t1:?}, {t2:?}"),
        })
    }

    fn emit_binop_concat(&self, e1: &Expression, e2: &Expression) -> Result<String> {
        let t1 = e1.resolve_type(&self.syms)?;
        let t2 = e2.resolve_type(&self.syms)?;
        let a = self.emit_expression_ctx(e1, Ctx::ArgScalar)?;
        let b = self.emit_expression_ctx(e2, Ctx::ArgScalar)?;

        Ok(match (&t1, &t2) {
            (DataType::Character, DataType::Character) => format!("&fstr::concat({a}, {b})"),
            _ => bail!("invalid types in character op '//': {t1:?}, {t2:?}"),
        })
    }

    /// Emit array index value. Since Rust requires this to be a single value, for N-dimensional
    /// arrays we use the syntax `a[[x, y]]` (similar to ndarray).
    fn emit_index(&self, idx: &[Expression]) -> Result<String> {
        let idx_ex = self.emit_expressions(idx, Ctx::Value)?;
        if idx.len() == 1 {
            Ok(idx_ex)
        } else {
            Ok(format!("[{idx_ex}]"))
        }
    }

    /// Emit substring range: `A..=B` or `A..`
    fn emit_range(
        &self,
        e1: &Option<Box<Expression>>,
        e2: &Option<Box<Expression>>,
    ) -> Result<String> {
        Ok(match (e1, e2) {
            (None, None) => "1 ..".to_owned(),
            (Some(e1), None) => {
                let e1 = self.emit_expression(e1)?;
                format!("{e1} ..")
            }
            (None, Some(e2)) => {
                let e2 = self.emit_expression(e2)?;
                format!("1 ..= {e2}")
            }
            (Some(e1), Some(e2)) => {
                let e1 = self.emit_expression(e1)?;
                let e2 = self.emit_expression(e2)?;
                format!("{e1} ..= {e2}")
            }
        })
    }

    /// Emit expression in default (Value) context
    fn emit_expression(&self, e: &Expression) -> Result<String> {
        self.emit_expression_ctx(e, Ctx::Value)
    }

    /// Emit expression in given context. (Mostly the context only applies to symbols;
    /// expressions involving operators will output like Value regardless.)
    fn emit_expression_ctx(&self, e: &Expression, ctx: Ctx) -> Result<String> {
        Ok(match e {
            Expression::Unary(op, e2) => {
                let e2 = self.emit_expression(e2)?;
                match op {
                    UnaryOp::Negate => format!("-{e2}"),
                    UnaryOp::Not => format!("!{e2}"),
                    UnaryOp::Paren => e2,
                }
            }
            Expression::Binary(op, e1, e2) => {
                match op {
                    BinaryOp::Add => self.emit_binop_arith(e1, e2, "+")?,
                    BinaryOp::Sub => self.emit_binop_arith(e1, e2, "-")?,
                    BinaryOp::Div => self.emit_binop_arith(e1, e2, "/")?,
                    BinaryOp::Mul => self.emit_binop_arith(e1, e2, "*")?,
                    BinaryOp::Pow => self.emit_binop_pow(e1, e2)?,
                    BinaryOp::Concat => self.emit_binop_concat(e1, e2)?,
                    BinaryOp::Lt => self.emit_binop_rel(e1, e2, "<", "lt")?,
                    BinaryOp::Le => self.emit_binop_rel(e1, e2, "<=", "lte")?,
                    BinaryOp::Eq => self.emit_binop_rel(e1, e2, "==", "eq")?,
                    BinaryOp::Ne => self.emit_binop_rel(e1, e2, "!=", "ne")?,
                    BinaryOp::Gt => self.emit_binop_rel(e1, e2, ">", "gt")?,
                    BinaryOp::Ge => self.emit_binop_rel(e1, e2, ">=", "ge")?,
                    BinaryOp::And => self.emit_binop_logic(e1, e2, "&&")?,
                    BinaryOp::Or => self.emit_binop_logic(e1, e2, "||")?,
                    BinaryOp::Eqv => self.emit_binop_logic(e1, e2, "==")?,
                    BinaryOp::Neqv => self.emit_binop_logic(e1, e2, "!=")?,
                }
                // TODO: would be nice to omit brackets when Rust precedence doesn't require it,
                // and only keep the source's original ones from UnaryOp::Paren
            }

            Expression::Symbol(name) => self.emit_symbol(name, ctx)?,

            Expression::ArrayElement(name, idx) => {
                let s = self.emit_symbol(name, Ctx::Value)?;
                let sym = self.syms.get(name)?;
                let idx_ex = self.emit_index(idx)?;
                if matches!(sym.ast.base_type, DataType::Character) {
                    format!("{s}.get({idx_ex})")
                } else {
                    format!("{s}[{idx_ex}]")
                }
            }

            Expression::Function(name, args) => self.emit_call(name, args, true)?,

            Expression::Substring(name, e1, e2) => {
                let s = self.emit_symbol(name, Ctx::Value)?;
                let range = self.emit_range(e1, e2)?;
                format!("fstr::substr({s}, {range})")
            }
            Expression::SubstringArrayElement(name, idx, e1, e2) => {
                let s = self.emit_symbol(name, Ctx::Value)?;
                let idx_ex = self.emit_expressions(idx, Ctx::Value)?;
                let range = self.emit_range(e1, e2)?;

                // TODO: work out how to implement this properly
                format!("fstr::substr({s}.get({idx_ex}), {range})")
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
            Expression::ImpliedDo { .. } => bail!("cannot emit implied-DO"),
            Expression::ImpliedDoVar(..) => bail!("cannot emit implied-DO-var"),
        })
    }

    /// Emit comma-separate arguments for a function call
    fn emit_args(
        &self,
        dargs: &[globan::DummyArg],
        args: &[Expression],
        requires_context: bool,
    ) -> Result<String> {
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

        // Value is (total occurrences, mutable occurrences)
        let mut sym_counts: HashMap<&str, (usize, usize)> = HashMap::new();
        dargs
            .iter()
            .zip(args.iter())
            .for_each(|(darg, arg)| match arg {
                Expression::Symbol(s)
                | Expression::ArrayElement(s, ..)
                | Expression::Substring(s, ..)
                | Expression::SubstringArrayElement(s, ..) => {
                    let c = sym_counts.entry(s).or_insert((0, 0));
                    c.0 += 1;
                    if darg.mutated {
                        c.1 += 1;
                    }
                }
                _ => (),
            });

        let mut aliased = HashSet::new();
        for (sym, count) in sym_counts {
            if count.0 >= 2 && count.1 >= 1 {
                warn!(
                    "{}: Possible aliasing violation: symbol {sym} used twice in procedure call",
                    self.program.filename
                );
                aliased.insert(sym);
            }
            if count.1 >= 2 {
                bail!("Aliasing violation: symbol {sym} used mutably twice in procedure call");
            }
        }

        let mut exprs = vec![];

        // Context gets passed as the first argument
        if requires_context {
            exprs.push(Ok("ctx".to_owned()));
        }

        exprs.extend(dargs.iter().zip(args.iter()).map(|(darg, arg)| {
            let conversion = if darg.is_array {
                // Function is expecting an array
                match arg {
                    Expression::Unary(..) |
                    Expression::Binary(..) => bail!("cannot pass expression to dummy argument expecting an array"),
                    Expression::Symbol(name) => {
                        let sym = self.syms.get(name)?;
                        if sym.ast.base_type != darg.base_type {
                            bail!("cannot convert array types: actual argument {name}={:?}, dummy argument {}={:?}", sym.ast.base_type, darg.name, darg.base_type);
                        }

                        if darg.mutated {
                            self.emit_symbol(name, Ctx::ArgArrayMut)?
                        } else {
                            self.emit_symbol(name, Ctx::ArgArray)?
                        }
                    }
                    Expression::ArrayElement(name, idx) => {
                        let s = self.emit_symbol(name, Ctx::Value)?;
                        let sym = self.syms.get(name)?;
                        if sym.ast.dims.is_empty() {
                            bail!("cannot access element of non-array");
                        }
                        if sym.ast.base_type != darg.base_type {
                            bail!("cannot convert array types: actual argument {name}={:?}, dummy argument {}={:?}", sym.ast.base_type, darg.name, darg.base_type);
                        }
                        let idx = self.emit_expressions(idx, Ctx::Value)?;
                        if darg.mutated {
                            format!("{s}.slice_mut({idx})")
                        } else {
                            format!("{s}.slice({idx})")
                        }
                    }
                    Expression::Function(..) => bail!("cannot pass function expression to dummy argument expecting an array"),
                    Expression::Substring(..) => {
                        warn!("TODO: emit_args Substring as array");
                        "todo!()".to_owned()
                    }
                    Expression::SubstringArrayElement(..) => todo!(),
                    Expression::Constant(..) => bail!("cannot pass constant to dummy argument expecting an array"),
                    Expression::ImpliedDo { .. } => bail!("cannot use implied-DO as argument"),
                    Expression::ImpliedDoVar(..) => bail!("cannot use implied-DO-variable as argument"),
                }

            } else {
                // Function is expecting a scalar
                match arg {
                    Expression::Unary(..) |
                    Expression::Binary(..) |
                    Expression::Constant(..) => {
                        if darg.mutated {
                            bail!("cannot pass expression or constant to mutable dummy argument");
                        } else {
                            self.emit_expression(arg)?
                        }
                    }
                    Expression::Symbol(name) => {
                        let sym = self.syms.get(name)?;
                        if sym.ast.base_type != darg.base_type {
                            bail!("cannot convert types: actual argument {name}={:?}, dummy argument {}={:?}", sym.ast.base_type, darg.name, darg.base_type);
                        }

                        if darg.mutated {
                            self.emit_symbol(name, Ctx::ArgScalarMut)?
                        } else if aliased.contains(name.as_str()) {
                            self.emit_symbol(name, Ctx::ArgScalarAliased)?
                        } else {
                            self.emit_symbol(name, Ctx::ArgScalar)?
                        }
                    }
                    Expression::ArrayElement(name, idx) => {
                        let s = self.emit_symbol(name, Ctx::Value)?;
                        let sym = self.syms.get(name)?;
                        if sym.ast.dims.is_empty() {
                            bail!("cannot access element of non-array");
                        }
                        if sym.ast.base_type != darg.base_type {
                            bail!("cannot convert types: actual argument {name}={:?}, dummy argument {}={:?}", sym.ast.base_type, darg.name, darg.base_type);
                        }
                        let idx_ex = self.emit_index(idx)?;

                        let get = format!("{s}[{idx_ex}]");

                        if darg.mutated {
                            // Pass by reference
                            format!("&mut {get}")
                        } else if matches!(sym.ast.base_type, DataType::Character) {
                            if aliased.contains(s.as_str()) {
                                // Try to resolve aliasing violation
                                format!("&{get}.clone()")
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
                            bail!("cannot pass function return value to mutable dummy argument");
                        } else {
                            self.emit_call(name, args, true)?
                        }
                    }
                    Expression::Substring(name, e1, e2) => {
                        let s = self.emit_symbol(name, Ctx::Value)?;
                        let range = self.emit_range(e1, e2)?;

                        // TODO: work out how to implement this properly
                        // TODO: handle aliasing
                        format!("fstr::substr({s}, {range})")
                    }
                    Expression::SubstringArrayElement(..) => {
                        warn!("TODO: emit_args SubstringArrayElement");
                        "todo!()".to_owned()
                    }
                    Expression::ImpliedDo { .. } => bail!("cannot use implied-DO as argument"),
                    Expression::ImpliedDoVar(..) => bail!("cannot use implied-DO-variable as argument"),
                }
            };
            Ok(conversion)
        }));

        Ok(exprs.into_iter().collect::<Result<Vec<_>>>()?.join(", "))
    }

    /// Get the argument types for calling a procedure or intrinsic
    fn procedure_args(
        &self,
        name: &str,
        actual_procs: &[globan::Name],
        args: &[Expression],
    ) -> Result<ProcedureArgs> {
        if intrinsics::exists(name) {
            // There is an intrinsic, but check that it has the correct types too
            let first_arg = args
                .first()
                .map(|e| e.resolve_type(&self.syms))
                .transpose()?;
            if let Some((return_type, codegen)) = intrinsics::call_info(name, first_arg) {
                return Ok(ProcedureArgs {
                    return_type,
                    dargs: args
                        .iter()
                        .map(|arg| {
                            Ok(globan::DummyArg {
                                name: "_".to_owned(),
                                base_type: arg.resolve_type(&self.syms)?,
                                is_array: false,
                                mutated: false,
                            })
                        })
                        .collect::<Result<_>>()?,
                    codegen,
                    requires_ctx: false,
                });
            } else {
                warn!(
                    "Call to {name} not interpreted as an intrinsic, because the types don't match"
                );
            }
        }

        self.globan.procedure_args(name, actual_procs)
    }

    // Emit call to SUBROUTINE or FUNCTION or intrinsic
    fn emit_call(&self, name: &str, args: &[Expression], is_function: bool) -> Result<String> {
        let sym = self.syms.get(name)?;
        let actual_procs = &sym.actual_procs;
        if actual_procs.is_empty() {
            // We need at least one, so we can figure out the argument conversions
            bail!("call to symbol {name} with no known actual procedures");
        }
        if sym.ast.statement_function {
            bail!("TODO: statement functions not supported");
        }
        let actual = self.procedure_args(name, actual_procs, args)?;

        if matches!(actual.return_type, DataType::Unknown | DataType::Void) {
            if is_function {
                bail!("called subroutine as function: {}", name);
            }
        } else if !is_function {
            bail!("CALL to function, not subroutine: {}", name);
        }

        if args.len() != actual.dargs.len() {
            bail!(
                "call to {name} with incorrect number of arguments (got {}, expected {})",
                args.len(),
                actual.dargs.len()
            );
        }

        let args_ex = self
            .emit_args(&actual.dargs, args, actual.requires_ctx)
            .with_context(|| format!("failed args in call to {name}"))?;

        if sym.ast.darg {
            // Ok(format!(
            // "{name}({args_ex}) /* possible actual procedures: {actual_procs:?} */\n"
            // ))
            Ok(format!("{name}({args_ex})"))
        } else {
            match actual.codegen {
                CallSyntax::Unified => bail!("unexpected unified proc"),
                CallSyntax::External(name) => {
                    let ns_name = if name.0 == self.program.namespace {
                        name.1
                    } else {
                        format!("{}::{}", name.0, name.1)
                    };
                    // Ok(format!("{ns_name}({args_ex}) /* possible actual procedures: {actual_procs:?} */\n"))
                    Ok(format!("{ns_name}({args_ex})"))
                }
                CallSyntax::Cast(ty) => Ok(format!("({args_ex} as {ty})")),
                CallSyntax::Func(name) => Ok(format!("{name}({args_ex})")),
                CallSyntax::VarFunc(name) => Ok(format!("{name}(&[{args_ex}])")),
            }
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
                code += &self.emit_initialiser(name, sym, sym.mutated)?;
            }
        }

        code += "\n";

        Ok(code)
    }

    /// Initialise local variables (including DummyArray accessors etc)
    fn emit_initialiser(&self, name: &String, sym: &Symbol, is_mut: bool) -> Result<String> {
        let mut code = String::new();

        if !sym.ast.dims.is_empty() {
            // Dimension declarator for an array
            let dims = sym.ast.dims.iter().map(|dim| {
                let lower = match &dim.lower {
                    None => "1".to_owned(),
                    Some(e) => self.emit_expression(e)?,
                };
                match &dim.upper {
                    None => Ok(format!("{lower} .. ")),
                    Some(e) => {
                        let e = self.emit_expression(e)?;
                        Ok(format!("{lower} ..= {e}"))
                    }
                }
            });
            let dims = dims.collect::<Result<Vec<_>>>()?.join(", ");

            // Find whether this is a Char array (with optional string length) or normal array
            let (char_label, char_len) = match sym.ast.base_type {
                DataType::Character => {
                    let len = match &sym.ast.character_len {
                        Some(LenSpecification::Asterisk) => None,
                        Some(LenSpecification::Integer(c)) => Some(c.to_string()),
                        Some(LenSpecification::IntConstantExpr(e)) => {
                            Some(self.emit_expression(e)?)
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

                let array = match sym.ast.dims.len() {
                    1 => format!("Actual{char_label}Array"),
                    n => format!("Actual{char_label}Array{n}D"),
                };

                let len_label = match sym.ast.base_type {
                    DataType::Character => match char_len {
                        Some(e) => format!("{e}, "),
                        None => bail!("actual character array with undefined length"),
                    },
                    _ => "".to_owned(),
                };

                code += &format!("let {mut_label}{name} = {array}::new({len_label}{dims});\n");
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
                        let e = self.emit_expression(e)?;
                        let mut_label = if is_mut { "mut " } else { "" };
                        code += &format!("let {name} = &{mut_label}{name}[..{e}];\n");
                    }
                }
            } else {
                match len {
                    LenSpecification::Unspecified => bail!("unspecified character length"),
                    LenSpecification::Asterisk => {
                        // nothing to do, we'll keep the length of the actual argument
                    }

                    LenSpecification::Integer(n) => {
                        // TODO: maybe we should stack-allocate instead of Vec, for performance
                        let mut_label = if is_mut { "mut " } else { "" };
                        code += &format!("let {mut_label}{name} = vec![b' '; {n} as usize];\n");
                    }
                    LenSpecification::IntConstantExpr(e) => {
                        let e = self.emit_expression(e)?;
                        let mut_label = if is_mut { "mut " } else { "" };
                        code += &format!("let {mut_label}{name} = vec![b' '; {e} as usize];\n");
                    }
                }
            }
        } else if !sym.ast.darg {
            // Non-array, non-character, local variable

            let ty = emit_datatype(&sym.ast.base_type);

            if is_mut {
                // Zero-initialise because rustc can't always tell it's initialised on every code path
                // (TODO: if we can detect it's safe, we could remove this. If it's assigned exactly
                // once, get rid of the 'mut' too.)
                let zero = emit_zero(&sym.ast.base_type)
                    .with_context(|| format!("initialising {name}"))?;
                code += &format!("let mut {name}: {ty} = {zero};\n");
            } else {
                // TODO: why bother emitting this, if we never assign to it?
                let actual = &sym.actual_procs;
                if actual.is_empty() {
                    code += &format!("let {name}: {ty};\n");
                } else {
                    code += &format!(
                        "let {name}: {ty}; /* possible actual procedures: {actual:?} */\n"
                    );
                }
            }
        }

        Ok(code)
    }

    /// Emit code for DATA, assigning a sequence of constant expressions to a sequence of names
    fn emit_data_init(&self, data: &ast::DataStatement) -> Result<String> {
        let mut code = String::new();

        // Handle the simple case, assigning to a single variable
        if data.nlist.len() == 1 {
            if let Some(Expression::Symbol(name)) = data.nlist.first() {
                let sym = self.syms.get(name)?;
                match sym.rs_ty {
                    RustType::SavePrimitive | RustType::SaveChar => {
                        if data.clist.len() != 1 {
                            bail!("DATA {name} trying to assign wrong number of items to scalar");
                        }
                        let (reps, value) = data.clist.first().unwrap();
                        if reps.is_some() {
                            // TODO: should allow an explicit 1
                            bail!("DATA {name} using reps with scalar");
                        }

                        let target = Expression::Symbol(name.clone());
                        code += &self.emit_assignment(&target, value, Ctx::SaveInit)?;
                    }
                    RustType::SaveActualArray | RustType::SaveActualCharArray => {
                        // To assign to arrays, without needing to know indexes and dimensions,
                        // we create an iterator over the array and assign sequentially to that

                        code += "{\n";
                        code += &format!("  let mut nlist = {name}.iter_mut();\n");

                        for (reps, value) in &data.clist {
                            let e = self.emit_expression(value)?;

                            let assign = match sym.rs_ty {
                                RustType::SaveActualArray => {
                                    format!("*nlist.next().unwrap() = {e}")
                                }
                                RustType::SaveActualCharArray => {
                                    format!("fstr::assign(nlist.next().unwrap(), {e})")
                                }
                                _ => panic!(),
                            };

                            if let Some(reps) = reps {
                                let reps_ex = self.emit_expression(reps)?;
                                code += &format!("  for _ in 1..={reps_ex} {{\n");
                                code += &format!("    {assign};\n");
                                code += "  }\n";
                            } else {
                                code += &format!("  {assign};\n");
                            }
                        }
                        code += "}\n";
                    }
                    _ => bail!("non-save type in DATA"),
                }

                return Ok(code);
            }
        }

        warn!("unsupported DATA statement");
        code += &format!("todo!(); /* {:?} */\n", data);
        Ok(code)
    }

    fn emit_statements(&self, entry: &ast::Entry, statements: &Vec<Statement>) -> Result<String> {
        let mut code = String::new();
        for statement in statements {
            code += &self.emit_statement(entry, statement)?;
        }
        Ok(code)
    }

    fn emit_assignment(&self, v: &Expression, e: &Expression, ctx: Ctx) -> Result<String> {
        let mut code = String::new();

        // TODO: we should do arithmetic conversions to the target type
        // (equivalent to calling INT, REAL, DBLE)

        let vt = v.resolve_type(&self.syms)?;
        let e = self.emit_expression(e)?;
        match v {
            Expression::Symbol(name) => {
                let s = self.emit_symbol(name, ctx)?;
                if matches!(vt, DataType::Character) {
                    code += &format!("fstr::assign({s}, {e});\n");
                } else {
                    code += &format!("{s} = {e};\n");
                }
            }
            Expression::ArrayElement(name, idx) => {
                let s = self.emit_symbol(name, ctx)?;
                let idx_ex = self.emit_index(idx)?;
                if matches!(vt, DataType::Character) {
                    code += &format!("fstr::assign({s}.get_mut({idx_ex}), {e});\n");
                } else {
                    code += &format!("{s}[{idx_ex}] = {e};\n");
                }
            }
            Expression::Substring(name, e1, e2) => {
                let s = self.emit_symbol(name, ctx)?;
                let range = self.emit_range(e1, e2)?;

                code += &format!("fstr::assign(fstr::substr_mut({s}, {range}), {e});\n");
            }
            Expression::SubstringArrayElement(name, idx, e1, e2) => {
                let s = self.emit_symbol(name, ctx)?;
                let idx_ex = self.emit_expressions(idx, Ctx::Value)?;
                let range = self.emit_range(e1, e2)?;

                code +=
                    &format!("fstr::assign(fstr::substr_mut({s}.get({idx_ex}), {range}), {e});\n");
            }
            _ => bail!("invalid assignment LHS"),
        }

        Ok(code)
    }

    fn emit_statement(&self, entry: &ast::Entry, statement: &Statement) -> Result<String> {
        let mut code = String::new();
        match statement {
            Statement::Assignment(v, e) => {
                code += &self.emit_assignment(v, e, Ctx::Assignment)?;
            }
            Statement::If { es, bodies } => {
                for (i, (e, body)) in es.iter().zip(bodies.iter()).enumerate() {
                    if i > 0 {
                        code += " else ";
                    }

                    let body = self.emit_statements(entry, body)?;

                    if let Some(e) = e {
                        let e = self.emit_expression(e)?;
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
                let var_sym = self.syms.get(var).unwrap();

                assert!(var_sym.ast.do_var);
                if var_sym.ast.base_type != DataType::Integer {
                    // TODO: maybe support reals/doubles (though it's obsolescent in Fortran 90)
                    bail!("DO is currently only supported over INTEGER");
                }

                let m1 = self.emit_expression(e1)?;
                let m2 = self.emit_expression(e2)?;
                let body = self.emit_statements(entry, body)?;

                // Step defaults to 1
                let m3 = e3
                    .clone()
                    .map_or_else(|| Ok("1".to_owned()), |e| self.emit_expression(&e))?;

                if var_sym.ast.outside_do {
                    // If the DO-var is accessed outside, we'll fall back to implementing the
                    // loop as the standard describes it. (TODO: could make this much less ugly)
                    code += &format!("let m1__: i32 = {m1};\n");
                    code += &format!("let m2__: i32 = {m2};\n");
                    code += &format!("let m3__: i32 = {m3};\n");
                    code += &format!("{var} = m1__;\n");
                    code += "for _ in 0..((m2__ - m1__ + m3__) / m3__) as i32 {\n";
                    code += &body;
                    code += &format!("{var} += m3__;\n");
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
                let e = self.emit_expression(e)?;
                let body = self.emit_statements(entry, body)?;
                code += &format!("while {e} {{\n{body}}}\n");
            }
            Statement::Stop => {
                code += "ctx.stop();\n";
            }
            Statement::Print(fmt, iolist) => {
                assert!(fmt.is_none());

                // TODO: implement this properly. This is just a quick hack for tests

                let mut fmt = vec![];
                let mut args = vec![];
                let mut prev_char = false;
                for expr in iolist {
                    let s = self.emit_expression_ctx(expr, Ctx::ArgScalar)?;
                    match expr.resolve_type(&self.syms)? {
                        DataType::Character => {
                            if prev_char {
                                fmt.push("{}");
                            } else {
                                fmt.push(" {}");
                            }
                            args.push(format!("String::from_utf8_lossy({s})"));
                            prev_char = true;
                        }
                        _ => {
                            fmt.push(" {:>11}");
                            args.push(s);
                            prev_char = false;
                        }
                    }
                }

                let fmt = fmt.join("");
                args.insert(0, format!("ctx.stdout(), \"{fmt}\""));

                code += "writeln!(";
                code += &args.join(", ");
                code += ").expect(\"PRINT\");\n";
            }
            Statement::Call(name, args) => {
                code += &self
                    .emit_call(name, args, false)
                    .with_context(|| format!("failed statement {:?}", statement))?;
                code += ";\n";
            }
            Statement::Return => {
                // Functions return the value assigned to their own name
                // (except CHARACTER functions which return via a hidden parameter)
                if matches!(self.program.ast.ty, ast::ProgramUnitType::Function)
                    && self.syms.get(&entry.name)?.ast.base_type != DataType::Character
                {
                    let name = &entry.name;
                    code += &format!("return {name};\n");
                } else {
                    code += "return;\n";
                };
            }
        }

        Ok(code)
    }

    fn emit_constants(&self) -> Result<String> {
        let mut code = String::new();

        for (name, sym) in self.syms.iter() {
            if let Some(param) = &sym.ast.parameter {
                if let DataType::Character = &sym.ast.base_type {
                    let len = sym
                        .ast
                        .character_len
                        .as_ref()
                        .expect("character sym must have len");

                    let param_exp = self.emit_expression(param)?;

                    match len {
                        LenSpecification::Unspecified => bail!("unspecified character length"),
                        LenSpecification::Asterisk => {
                            code += &format!("const {name}: &[u8] = {param_exp};\n",)
                        }

                        LenSpecification::Integer(n) => {
                            code += &format!(
                                "const {name}: &[u8; {n}] = &fstr::extend_const::<{n}>({param_exp});\n"
                            );
                        }
                        LenSpecification::IntConstantExpr(e) => {
                            let e = self.emit_expression(e)?;
                            code += &format!(
                                "const {name}: &[u8; {e} as usize] = &fstr::extend_const::<{{{e} as usize}}>({param_exp});\n"
                            );
                        }
                    }
                } else {
                    let ty = emit_datatype(&sym.ast.base_type);

                    let param_ex = self.emit_expression(param)?;
                    code += &format!("const {name}: {ty} = {param_ex};\n");
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
        for (name, _sym) in &saved {
            let decl = self.emit_symbol(name, Ctx::SaveStruct)?;
            code += &format!("  {decl},\n");
        }
        code += "}\n\n";

        // Default-initialise members
        code += "impl SaveInit for SaveVars {\n";
        code += "  fn new() -> Self {\n";
        for (name, sym) in &saved {
            code += &self.emit_initialiser(name, sym, true)?;
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
                        syms: SymbolTable(syms),
                    },
                    ast: entry,
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
                syms: SymbolTable(shared_syms),
            },
            entries,
        }
    }

    pub fn emit(self) -> Result<String> {
        let mut code = String::new();

        code += "use f2rust_std::*;\n";
        code += "\n";

        code += &self.shared.emit_constants()?;
        code += &self.shared.emit_save_struct()?;

        for entry in &self.entries {
            let dargs = entry
                .ast
                .dargs
                .iter()
                .map(|darg| entry.codegen.emit_symbol(&darg.name, Ctx::DummyArg));

            let is_function = matches!(self.shared.program.ast.ty, ast::ProgramUnitType::Function);
            let ret_type = if is_function {
                &entry.codegen.syms.get(&entry.ast.name)?.ast.base_type
            } else {
                &DataType::Void
            };

            let ret = if !matches!(ret_type, DataType::Void | DataType::Character) {
                format!("-> {}", emit_datatype(ret_type))
            } else {
                "".to_owned()
            };

            let entry_name = &entry.ast.name;

            let mut dargs = dargs.collect::<Result<Vec<_>>>()?;
            if entry
                .codegen
                .globan
                .requires_ctx(&entry.codegen.program.namespace, entry_name)?
            {
                dargs.insert(0, "ctx: &mut Context".to_owned());
            }
            let dargs = dargs.join(", ");
            code += &format!("pub fn {entry_name}({dargs}) {ret} {{\n");
            code += &entry.codegen.emit_save_borrow(entry_name)?;
            code += &entry.codegen.emit_locals(entry_name)?;
            code += &entry.codegen.emit_statements(entry.ast, &entry.ast.body)?;
            if !matches!(ret_type, DataType::Void | DataType::Character) {
                code += &format!("return {entry_name};\n");
            };
            code += "}\n\n";
        }

        Ok(code)
    }
}

/// Pass through rustfmt
pub fn pretty_print(code: String) -> Result<String> {
    let mut child = std::process::Command::new("rustfmt")
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()?;

    let mut stdin = child.stdin.take().expect("Failed to open stdin");
    std::thread::spawn(move || {
        stdin
            .write_all(code.as_bytes())
            .expect("Failed to write to stdin");
    });

    let output = child.wait_with_output().expect("Failed to read stdout");

    std::io::stderr().write_all(&output.stderr)?;
    Ok(String::from_utf8_lossy(&output.stdout).to_string())
}
