use crate::{
    ast,
    ast::DataType,
    codegen::{self, CallSyntax},
    globan::DummyArg,
};

// Save some typing for intrinsics following the pattern "XYZ" = f32::xyz, "DXYZ" = f64::xyz
macro_rules! common_f32 {
    ($name:literal, $func:literal) => {
        (
            $name,
            $name,
            DataType::Real,
            DataType::Real,
            CallSyntax::Func(concat!("f32::", $func)),
        )
    };
}
macro_rules! common_f64 {
    ($name:literal, $func:literal) => {
        (
            $name,
            concat!("D", $name),
            DataType::Double,
            DataType::Double,
            CallSyntax::Func(concat!("f64::", $func)),
        )
    };
}

// (generic name, specific name, first arg type, return type, syntax)
type IntrinsicDef = (&'static str, &'static str, DataType, DataType, CallSyntax);

#[rustfmt::skip]
const INTRINSICS: &[IntrinsicDef] = &[
    ("INT", "", DataType::Integer, DataType::Integer, CallSyntax::Cast("i32")),
    ("INT", "INT", DataType::Real, DataType::Integer, CallSyntax::Cast("i32")),
    ("INT", "IFIX", DataType::Real, DataType::Integer, CallSyntax::Cast("i32")),
    ("INT", "IDINT", DataType::Double, DataType::Integer, CallSyntax::Cast("i32")),
    // ("INT", "", DataType::Complex, DataType::Integer, ...),

    ("REAL", "REAL", DataType::Integer, DataType::Real, CallSyntax::Cast("f32")),
    ("REAL", "FLOAT", DataType::Integer, DataType::Real, CallSyntax::Cast("f32")),
    ("REAL", "", DataType::Real, DataType::Real, CallSyntax::Cast("f32")),
    ("REAL", "SNGL", DataType::Double, DataType::Real, CallSyntax::Cast("f32")),
    // ("REAL", "", DataType::Complex, DataType::Real, ...),

    ("DBLE", "", DataType::Integer, DataType::Double, CallSyntax::Cast("f64")),
    ("DBLE", "", DataType::Real, DataType::Double, CallSyntax::Cast("f64")),
    ("DBLE", "", DataType::Double, DataType::Double, CallSyntax::Cast("f64")),
    // ("DBLE", "", DataType::Complex, DataType::Double, ...),

    // CMPLX

    ("", "ICHAR", DataType::Character, DataType::Integer, CallSyntax::Func("intrinsics::ICHAR")),
    // CHAR returns [u8] for ownership reasons, so we have to convert to &[u8]
    ("", "CHAR", DataType::Integer, DataType::Character, CallSyntax::Func("&intrinsics::CHAR")),

    ("AINT", "AINT", DataType::Real, DataType::Real, CallSyntax::Func("f32::trunc")),
    ("AINT", "DINT", DataType::Double, DataType::Double, CallSyntax::Func("f64::trunc")),

    ("ANINT", "ANINT", DataType::Real, DataType::Real, CallSyntax::Func("f32::round")),
    ("ANINT", "DNINT", DataType::Double, DataType::Double, CallSyntax::Func("f64::round")),

    ("NINT", "NINT", DataType::Real, DataType::Integer, CallSyntax::Func("intrinsics::NINT")),
    ("NINT", "IDNINT", DataType::Double, DataType::Integer, CallSyntax::Func("intrinsics::IDNINT")),

    ("ABS", "IABS", DataType::Integer, DataType::Integer, CallSyntax::Func("i32::abs")),
    ("ABS", "ABS", DataType::Real, DataType::Real, CallSyntax::Func("f32::abs")),
    ("ABS", "DABS", DataType::Double, DataType::Double, CallSyntax::Func("f64::abs")),
    // ("ABS", "CABS", DataType::Complex, DataType::Complex, ...),

    ("MOD", "MOD", DataType::Integer, DataType::Integer, CallSyntax::Func("intrinsics::MOD")),
    ("MOD", "AMOD", DataType::Real, DataType::Real, CallSyntax::Func("intrinsics::AMOD")),
    ("MOD", "DMOD", DataType::Double, DataType::Double, CallSyntax::Func("intrinsics::DMOD")),

    ("SIGN", "ISIGN", DataType::Integer, DataType::Integer, CallSyntax::Func("i32::copysign")),
    ("SIGN", "SIGN", DataType::Real, DataType::Real, CallSyntax::Func("f32::copysign")),
    ("SIGN", "DSIGN", DataType::Double, DataType::Double, CallSyntax::Func("f64::copysign")),

    ("DIM", "IDIM", DataType::Integer, DataType::Integer, CallSyntax::Func("intrinsics::IDIM")),
    ("DIM", "DIM", DataType::Real, DataType::Real, CallSyntax::Func("intrinsics::DIM")),
    ("DIM", "DDIM", DataType::Double, DataType::Double, CallSyntax::Func("intrinsics::DDIM")),

    ("", "DPROD", DataType::Real, DataType::Double, CallSyntax::Func("intrinsics::DPROD")),

    ("MAX", "MAX0", DataType::Integer, DataType::Integer, CallSyntax::VarFunc("intrinsics::MAX0")),
    ("MAX", "AMAX1", DataType::Real, DataType::Real, CallSyntax::VarFunc("intrinsics::AMAX1")),
    ("MAX", "DMAX1", DataType::Double, DataType::Double, CallSyntax::VarFunc("intrinsics::DMAX1")),
    ("", "AMAX0", DataType::Integer, DataType::Real, CallSyntax::VarFunc("intrinsics::AMAX0")),
    ("", "MAX1", DataType::Real, DataType::Integer, CallSyntax::VarFunc("intrinsics::MAX1")),

    ("MIN", "MIN0", DataType::Integer, DataType::Integer, CallSyntax::VarFunc("intrinsics::MIN0")),
    ("MIN", "AMIN1", DataType::Real, DataType::Real, CallSyntax::VarFunc("intrinsics::AMIN1")),
    ("MIN", "DMIN1", DataType::Double, DataType::Double, CallSyntax::VarFunc("intrinsics::DMIN1")),
    ("", "AMIN0", DataType::Integer, DataType::Real, CallSyntax::VarFunc("intrinsics::AMIN0")),
    ("", "MIN1", DataType::Real, DataType::Integer, CallSyntax::VarFunc("intrinsics::MIN1")),

    ("", "LEN", DataType::Character, DataType::Integer, CallSyntax::Func("intrinsics::LEN")),

    ("", "INDEX", DataType::Character, DataType::Integer, CallSyntax::Func("intrinsics::INDEX")),

    // AIMAG
    // CONJG

    common_f32!("SQRT", "sqrt"),
    common_f64!("SQRT", "sqrt"),
    // CSQRT

    common_f32!("EXP", "exp"),
    common_f64!("EXP", "exp"),
    // CEXP

    ("LOG", "ALOG", DataType::Real, DataType::Real, CallSyntax::Func("f32::ln")),
    ("LOG", "DLOG", DataType::Double, DataType::Double, CallSyntax::Func("f64::ln")),
    // ("LOG", "CLOG", DataType::Complex, DataType::Complex, ...),

    ("LOG10", "ALOG10", DataType::Real, DataType::Real, CallSyntax::Func("f32::log10")),
    ("LOG10", "DLOG10", DataType::Double, DataType::Double, CallSyntax::Func("f64::log10")),

    common_f32!("SIN", "sin"),
    common_f64!("SIN", "sin"),
    // CSIN
    common_f32!("COS", "cos"),
    common_f64!("COS", "cos"),
    // CCOS
    common_f32!("TAN", "tan"),
    common_f64!("TAN", "tan"),

    common_f32!("ASIN", "asin"),
    common_f64!("ASIN", "asin"),
    common_f32!("ACOS", "acos"),
    common_f64!("ACOS", "acos"),
    common_f32!("ATAN", "atan"),
    common_f64!("ATAN", "atan"),
    common_f32!("ATAN2", "atan2"),
    common_f64!("ATAN2", "atan2"),

    common_f32!("SINH", "sinh"),
    common_f64!("SINH", "sinh"),
    common_f32!("COSH", "cosh"),
    common_f64!("COSH", "cosh"),
    common_f32!("TANH", "tanh"),
    common_f64!("TANH", "tanh"),

    ("", "LGE", DataType::Character, DataType::Logical, CallSyntax::Func("fstr::ge")),
    ("", "LGT", DataType::Character, DataType::Logical, CallSyntax::Func("fstr::gt")),
    ("", "LLE", DataType::Character, DataType::Logical, CallSyntax::Func("fstr::le")),
    ("", "LLT", DataType::Character, DataType::Logical, CallSyntax::Func("fstr::lt")),

    // MIL-STD 1753:
    ("", "ISHFT", DataType::Integer, DataType::Integer, CallSyntax::Func("intrinsics::ISHFT")),
    ("", "ISHFTC", DataType::Integer, DataType::Integer, CallSyntax::Func("intrinsics::ISHFTC")),
    ("", "IBITS", DataType::Integer, DataType::Integer, CallSyntax::Func("intrinsics::IBITS")),
    ("", "MVBITS", DataType::Integer, DataType::Integer, CallSyntax::Func("intrinsics::MVBITS")),
    ("", "BTEST", DataType::Integer, DataType::Integer, CallSyntax::Func("intrinsics::BTEST")),
    ("", "IBSET", DataType::Integer, DataType::Integer, CallSyntax::Func("intrinsics::IBSET")),
    ("", "IBCLR", DataType::Integer, DataType::Integer, CallSyntax::Func("intrinsics::IBCLR")),

    // UNIX extensions:
    ("DATE_AND_TIME", "", DataType::Unknown, DataType::Void, CallSyntax::Func("ctx.date_and_time")),
    ("EXIT", "", DataType::Unknown, DataType::Void, CallSyntax::VarFunc("ctx.exit")),
    ("GETARG", "", DataType::Integer, DataType::Void, CallSyntax::Func("ctx.getarg")),
    ("GETENV", "", DataType::Character, DataType::Void, CallSyntax::Func("ctx.getenv")),
    ("IARGC", "", DataType::Unknown, DataType::Integer, CallSyntax::Func("ctx.iargc")),
    ("SYSTEM", "", DataType::Character, DataType::Void, CallSyntax::Func("ctx.system")),

    // Hacks:
    ("", "ARRAY_SUBSCRIPT_VALUE", DataType::Unknown, DataType::Integer, CallSyntax::ArraySubscriptValue),
];

fn find_intrinsic(name: &str, first_arg: Option<DataType>) -> Option<&'static IntrinsicDef> {
    for intrinsic @ (generic, specific, arg, _ret, _call) in INTRINSICS {
        if (*generic == name || *specific == name)
            && (matches!(arg, DataType::Unknown) || Some(arg) == first_arg.as_ref())
        {
            return Some(intrinsic);
        }
    }
    None
}

pub fn exists(name: &str) -> bool {
    for (generic, specific, _arg, _ret, _call) in INTRINSICS {
        if *generic == name || *specific == name {
            return true;
        }
    }
    false
}

/// Get information for calling the given intrinsic. If it's the generic name,
/// this will depend on the type of the first argument.
pub fn call_info(
    name: &str,
    first_arg: Option<DataType>,
) -> Option<(DataType, codegen::CallSyntax)> {
    find_intrinsic(name, first_arg).map(|i| (i.3.clone(), i.4.clone()))
}

/// Whether the named intrinsic mutates the given argument (starting from 0).
/// (None of the standard intrinsics mutate, only the UNIX extensions)
pub fn arg_is_mutated(name: &str, idx: usize) -> bool {
    match name {
        "DATE_AND_TIME" => true,
        "GETARG" | "GETENV" | "SYSTEM" => idx == 1,
        _ => false,
    }
}

pub fn requires_ctx(name: &str) -> bool {
    matches!(
        name,
        "DATE_AND_TIME" | "EXIT" | "GETARG" | "GETENV" | "IARGC" | "SYSTEM"
    )
}

pub fn returns_result(name: &str) -> bool {
    matches!(name, "EXIT" | "SYSTEM")
}

pub fn character_len(name: &str) -> Option<ast::LenSpecification> {
    match name {
        "CHAR" => Some(ast::LenSpecification::Integer(1)),
        _ => None,
    }
}

pub fn dummy_args(name: &str) -> Option<Vec<DummyArg>> {
    // TODO: implement all of these

    Some(match name {
        "DATE_AND_TIME" => vec![
            DummyArg {
                name: "DATE".to_owned(),
                base_type: DataType::Character,
                is_array: false,
                mutated: true,
            },
            DummyArg {
                name: "TIME".to_owned(),
                base_type: DataType::Character,
                is_array: false,
                mutated: true,
            },
            DummyArg {
                name: "ZONE".to_owned(),
                base_type: DataType::Character,
                is_array: false,
                mutated: true,
            },
            DummyArg {
                name: "VALUES".to_owned(),
                base_type: DataType::Integer,
                is_array: true,
                mutated: true,
            },
        ],
        _ => return None,
    })
}
