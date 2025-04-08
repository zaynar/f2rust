//! Parses individual lines of FORTRAN 77(ish) text into a more structured form.
//!
//! Specifically it is based on the FORTRAN 77 standard with the following changes:
//!
//! * Allow symbols >6 characters (as in Fortran 90)
//! * Allow "_" in symbols (as in Fortran 90)
//! * Allow double-quoted strings (as in Fortran 90)
//! * Allow some illegal syntax such as "A ** -B" (because it's easier to implement this way)
//! * Support DO WHILE, END DO, INCLUDE, IMPLICIT NONE (from MIL-STD 1753)
//! * Support `<` as equivalent to `.LT.`, etc (from Fortran 90)
//!
//! * No COMPLEX type
//! * No BLOCK DATA, DIMENSION, COMMON, INTRINSIC
//! * No GO TO, arithmetic IF, ASSIGN
//! * No PAUSE (doesn't make sense on modern computers)
//! * No "STOP n" (only "STOP")
//! * No IMPLICIT except IMPLICIT NONE
//!
//! Some of these are omitted because we don't support line labels and unstructured control flow
//! at all (because that's hard). Others are omitted just because we haven't needed them yet.

use std::collections::HashMap;

use crate::file::SourceLoc;

#[derive(Debug, Clone, PartialEq)]
pub enum LenSpecification {
    Asterisk,
    Integer(i32),
    IntConstantExpr(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Integer,
    Real,
    Double,
    Complex,
    Logical,
    Character(Option<LenSpecification>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Dimension(pub Option<Expression>, pub Option<Expression>);

#[derive(Debug, Clone, PartialEq)]
pub struct TypeElement {
    pub name: String,
    pub dims: Vec<Dimension>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Negate,
    Not,

    // Not semantically meaningful, but we preserve the original parenthesisation
    // so the Rust code can better reflect the original source
    Paren,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Div,
    Mul,
    Pow,
    Concat,

    Lt,
    Le,
    Eq,
    Ne,
    Gt,
    Ge,

    And,
    Or,
    Eqv,
    Neqv,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    Integer(i32),
    Real(f32),
    Double(f64),
    // ComplexReal(f32, f32),
    // ComplexInt(i32, i32),
    Bool(bool),
    Character(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Unary(UnaryOp, Box<Expression>),
    Binary(BinaryOp, Box<Expression>, Box<Expression>),

    Symbol(String),
    ArrayElementOrFunction(String, Vec<Expression>),
    Substring(String, Option<Box<Expression>>, Option<Box<Expression>>),
    SubstringArrayElement(
        String,
        Vec<Expression>,
        Option<Box<Expression>>,
        Option<Box<Expression>>,
    ),

    Constant(Constant),
}

/// Used to identify targets of assignment, DATA, READ and WRITE statements
#[derive(Debug, Clone, PartialEq)]
pub enum DataName {
    Variable(String),
    ArrayElement(String, Vec<Expression>),
    Substring(String, Option<Expression>, Option<Expression>),
    SubstringArrayElement(
        String,
        Vec<Expression>,
        Option<Expression>,
        Option<Expression>,
    ),

    /// Only valid in DATA, READ, WRITE
    ImpliedDo(
        Vec<DataName>,
        String,
        Expression,
        Expression,
        Option<Expression>,
    ),

    /// Only valid in WRITE
    Expression(Expression),
}

type AssignmentName = DataName;

// Only Constant or Symbol
type DataConstant = Expression;

#[derive(Debug, Clone, PartialEq)]
pub enum SpecifierValue {
    Asterisk,
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Specifier(pub Option<String>, pub SpecifierValue);

#[derive(Debug, Clone, PartialEq)]
pub struct Specifiers(pub HashMap<String, SpecifierValue>);

#[derive(Debug, Clone, PartialEq)]
pub struct DataLists {
    pub nlist: Vec<DataName>,
    pub clist: Vec<(Option<DataConstant>, DataConstant)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    // Special syntax things:
    Comment(String),
    Blank,
    // Include's Vec is not filled in by this parser, it must be handled by the caller
    Include(String, Vec<(SourceLoc, Statement)>),

    Program(String),
    Function(Option<Type>, String, Vec<String>),
    Subroutine(String, Vec<String>),
    Entry(String, Vec<String>),
    // BlockData
    // Dimension
    Equivalence(Vec<Vec<AssignmentName>>),
    // Common: not supported
    Type(Type, Vec<TypeElement>),
    TypeCharacter(
        Option<LenSpecification>,
        Vec<(TypeElement, Option<LenSpecification>)>,
    ),
    ImplicitNone, // we don't support other IMPLICITs
    External(Vec<String>),
    // Intrinsic(Vec<String>),
    Parameter(Vec<(String, Expression)>),
    Save(Vec<String>), // don't support /common_block_name/
    Data(Vec<DataLists>),

    // Executable statements:
    Assignment(AssignmentName, Expression),
    // Goto: not supported
    // ArithmeticIf: not supported
    LogicalIf(Expression, Box<Statement>),
    BlockIf(Expression),
    ElseIf(Expression),
    Else,
    EndIf,
    Do(
        Option<u32>,
        String,
        Expression,
        Expression,
        Option<Expression>,
    ),
    DoWhile(Option<u32>, Expression),
    EndDo,
    Continue,
    Stop,
    // Pause: not supported
    Read(Specifiers, Vec<DataName>),
    Write(Specifiers, Vec<DataName>),
    Print(SpecifierValue, Vec<DataName>),
    Open(Specifiers),
    Close(Specifiers),
    Inquire(Specifiers),
    Backspace(Specifiers),
    Endfile(Specifiers),
    Rewind(Specifiers),

    Call(String, Vec<Expression>),
    Return(Option<Expression>),

    End,
}

/// Convert character constant like "'ab''cd'" into "ab'cd"
fn unescape_characters(s: &str, delim: char) -> String {
    let mut r = String::new();

    enum State {
        Start,
        Normal,
        Quote,
    }
    let mut state = State::Start;

    for c in s.chars() {
        match state {
            State::Start => {
                state = State::Normal;
            }
            State::Normal => {
                if c == delim {
                    state = State::Quote;
                } else {
                    r.push(c);
                }
            }
            State::Quote => {
                r.push(delim);
                state = State::Normal;
            }
        }
    }

    r
}

peg::parser! {
    pub grammar fortran_parser() for str {
        rule unsigned_int_constant() -> i32
            = n:$(['0'..='9']+) {? n.parse().or(Err("invalid i32 constant")) }

        rule nonzero_unsigned_int_constant() -> i32
            = n:unsigned_int_constant() {? if n == 0 { Err("constant must be non-zero") } else { Ok(n) } }

        rule sign() = ['-' | '+']

        rule integer_constant()
            = sign()? ['0'..='9']+

        rule integer_constant_i32() -> i32
            = n:$(integer_constant()) {? n.parse().or(Err("invalid i32 constant")) }

        // We have problems with expressions like "1.LT.2", because we greedily parse "1." as a
        // real constant. To avoid that, we need to check it's not followed by a keyword
        rule binop_keyword()
            = "EQV"
            / "NEQV"
            / "OR"
            / "AND"
            / "LT"
            / "LE"
            / "EQ"
            / "NE"
            / "GT"
            / "GE"

        rule basic_real_constant()
            = sign()? ['0'..='9']* "." ['0'..='9']+
            / sign()? ['0'..='9']+ "." ['0'..='9']*

        rule real_constant() -> f32
            = b:$(basic_real_constant() / integer_constant()) "E" e:$integer_constant()
                {? (b.to_owned() + "e" + e).parse().or(Err("invalid real constant")) }
            / b:$basic_real_constant() !binop_keyword()
                {? b.parse().or(Err("invalid real constant")) }

        rule double_constant() -> f64
            = b:$(basic_real_constant() / integer_constant()) "D" e:$integer_constant()
                {? (b.to_owned() + "e" + e).parse().or(Err("invalid double precision constant")) }

        rule character_constant() -> String
            = s:$(("'" [^ '\'']* "'")+) { unescape_characters(s, '\'') }
            / s:$(("\"" [^ '"']* "\"")+) { unescape_characters(s, '"') }

        rule symbol() -> String
            = s:$(['A'..='Z'] ['A'..='Z' | '0'..='9' | '_']*) { s.to_owned() }
            // F77 limits it to 6, but e.g. cronos.f uses longer symbols.
            // F90 says _ is alphanumeric, and e.g. bgroup_1.f uses that.

        rule asterisk() -> String
            = "*" { "*".to_owned() }

        rule label() -> u32
            = n:$(['0'..='9'] * <1,5>) {?
                let n = n.parse::<u32>().map_err(|_| "label must be number")?;
                if n == 0 { Err("label must not be 0") } else { Ok(n) }
            }

        rule program_statement() -> Statement
            = "PROGRAM" pgm:symbol() { Statement::Program(pgm) }

        rule function_statement() -> Statement
            = typ:(
                "INTEGER" { Type::Integer }
                / "REAL" { Type::Real }
                / "DOUBLEPRECISION" { Type::Double }
                / "COMPLEX" { Type::Complex }
                / "LOGICAL" { Type::Logical }
                / "CHARACTER" len:("*" len:len_specification() { len })? { Type::Character(len) }
            )?
            "FUNCTION" fun:symbol() d:(
                "("
                    d:(symbol() / asterisk()) ** ","
                ")" ![_] { d }
            )? { Statement::Function(typ, fun, d.unwrap_or_default()) }

        rule subroutine_statement() -> Statement
            = "SUBROUTINE" sub:symbol() d:(
                "("
                    d:(symbol() / asterisk()) ** ","
                ")" ![_] { d }
            )? { Statement::Subroutine(sub, d.unwrap_or_default()) }

        rule entry_statement() -> Statement
            = "ENTRY" n:symbol() d:(
                "("
                    d:(symbol() / asterisk()) ** ","
                ")" ![_] { d }
            )? { Statement::Entry(n, d.unwrap_or_default()) }

        rule other_specification_statement() -> Statement
            // / dimension_statement() // not supported
            = equivalence_statement()
            // / common_statement() // not supported
            / type_statement()
            / external_statement()
            // / intrinsic_statement() // not supported
            / save_statement()

        rule equivalence_statement() -> Statement
            = "EQUIVALENCE" n:("(" n:assignment_name() ++ "," ")" { n }) ++ "," ![_] { Statement::Equivalence(n) }

        rule len_specification() -> LenSpecification
            = "(*)" { LenSpecification::Asterisk }
            / n:nonzero_unsigned_int_constant() { LenSpecification::Integer(n) }
            / "(" e:expression() ")" { LenSpecification::IntConstantExpr(e) }

        rule array_declarator() -> TypeElement
            = s:symbol() "("
            d:(
                d1:(e:expression() ":" { e })?
                d2:(e:expression() { Some(e) } / "*" { None })
                { Dimension(d1, d2) }
            ) ++ ","
            ")"
            { TypeElement { name: s, dims: d } }

        rule type_statement() -> Statement
            = ty:(
                "INTEGER" { Type::Integer }
                / "REAL" { Type::Real }
                / "DOUBLEPRECISION" { Type::Double }
                / "COMPLEX" { Type::Complex }
                / "LOGICAL" { Type::Logical }
                )
                e:(sd:array_declarator()
                    / s:symbol() { TypeElement { name: s, dims: Vec::new() } }
                ) ++ "," ![_]
            { Statement::Type(ty, e) }
            / "CHARACTER" len:("*" len:len_specification() ","? { len })?
            nam:(
                e:(d:array_declarator()
                    / s:symbol() { TypeElement { name: s, dims: Vec::new() } })
                len:("*" len:len_specification() { len })?
                { (e, len) }
            ) ++ "," ![_]
            { Statement::TypeCharacter(len, nam) }

        rule external_statement() -> Statement
            = "EXTERNAL" proc:symbol() ++ "," ![_] { Statement::External(proc) }

        rule parameter_statement() -> Statement
            = "PARAMETER("
                pe:(p:symbol() "=" e:expression() { (p, e) }) ++ ","
                ")" ![_] { Statement::Parameter(pe) }

        rule save_statement() -> Statement
            = "SAVE" a:symbol() ** "," ![_] { Statement::Save(a) }

        rule data_name() -> DataName
            = "("
                dlist:(d:data_name() "," { d })+
                i:symbol() "="
                m1:expression() "," m2:expression() m3:("," m3:expression() { m3 })?
             ")" { DataName::ImpliedDo(dlist, i, m1, m2, m3) }
            / s:symbol() "(" e1:expression()? ":" e2:expression()? ")"
                { DataName::Substring(s, e1, e2) }
            / s:symbol() "(" e:(expression() ** ",") ")" "(" e1:expression()? ":" e2:expression()? ")"
                { DataName::SubstringArrayElement(s, e, e1, e2) }
            / s:symbol() "(" e:(expression() ** ",") ")"
                { DataName::ArrayElement(s, e) }
            / s:symbol() { DataName::Variable(s) }

        rule data_constant() -> DataConstant
            = c:constant() { DataConstant::Constant(c) }
            / s:symbol() { DataConstant::Symbol(s) }

        rule data_statement() -> Statement
            = "DATA"
            v:(
                nlist:(data_name() ++ ",")
                "/"
                clist:(
                    r:(r:data_constant() "*" { r })? c:data_constant() { (r, c) }
                ) ++ ","
                "/"
                { DataLists { nlist, clist } }
            )+ ![_] { Statement::Data(v) }

        rule include_statement() -> Statement
            = "INCLUDE" filename:character_constant() ![_] { Statement::Include(filename, Vec::new()) }

        rule implicit_statement() -> Statement
            = "IMPLICITNONE" ![_] { Statement::ImplicitNone }

        rule executable_statement() -> Statement
            // / goto_statement() // not supported
            // / arithmetic_if_statement() // not supported
            = block_if_statement()
            / logical_if_statement()
            / else_if_statement()
            / else_statement()
            / end_if_statement()
            / do_statement()
            / do_while_statement()
            / end_do_statement() // non-F77 block DO
            / continue_statement()
            / stop_statement()
            // / pause_statement() // not supported
            / read_statement()
            / write_statement()
            / print_statement()
            / open_statement()
            / close_statement()
            / inquire_statement()
            / backspace_statement()
            / endfile_statement()
            / rewind_statement()

            / call_statement()
            / return_statement()
            / end_statement()
            / assignment_statement()

        rule assignment_name() -> AssignmentName
            = s:symbol() "(" e1:(expression() ++ ",") ")"
                "(" e2:expression()? ":" e3:expression()? ")"
                { AssignmentName::SubstringArrayElement(s, e1, e2, e3) }
            / s:symbol() "(" e2:expression()? ":" e3:expression()? ")"
                { AssignmentName::Substring(s, e2, e3) }
            / s:symbol() "(" e:(expression() ++ ",") ")"
                { AssignmentName::ArrayElement(s, e) }
            / s:symbol()
                { AssignmentName::Variable(s) }

        rule assignment_statement() -> Statement
            = n:assignment_name() "=" e:expression() ![_] { Statement::Assignment(n, e) }
            // not supported: ASSIGN TO

        rule logical_if_statement() -> Statement
            = "IF(" e:expression() ")" st:statement() ![_] { Statement::LogicalIf(e, Box::new(st)) }

        rule block_if_statement() -> Statement
            = "IF(" e:expression() ")THEN" ![_] { Statement::BlockIf(e) }

        rule else_if_statement() -> Statement
            = "ELSEIF(" e:expression() ")THEN" ![_] { Statement::ElseIf(e) }

        rule else_statement() -> Statement
            = "ELSE" ![_] { Statement::Else }

        rule end_if_statement() -> Statement
            = "ENDIF" ![_] { Statement::EndIf }

        rule do_statement() -> Statement
            = "DO"
                l:(l:label() ","? { l })? // support non-F77 block DO, without the label
                s:symbol() "=" e1:expression() "," e2:expression() e3:("," e3:expression() { e3 })?
                ![_]
            {
                Statement::Do(l, s, e1, e2, e3)
            }

        rule do_while_statement() -> Statement // MIL-STD 1753
            = "DO"
                l:(l:label() ","? { l })?
                "WHILE(" e:expression() ")" ![_]
            {
                Statement::DoWhile(l, e)
            }

        rule end_do_statement() -> Statement
            = "ENDDO" ![_] { Statement::EndDo }

        rule end_statement() -> Statement
            = "END" ![_] { Statement::End }

        rule continue_statement() -> Statement
            = "CONTINUE" ![_] { Statement::Continue }

        rule stop_statement() -> Statement
            = "STOP" ![_] { Statement::Stop }
            // not supported: "STOP n"


        rule specifier_value() -> SpecifierValue
            = "*" { SpecifierValue::Asterisk }
            / e:expression() { SpecifierValue::Expression(e) }

        rule specifier() -> Specifier
            = n:(n:$(['A'..='Z']+) "=" { n })? v:specifier_value() { Specifier(n.map(str::to_owned), v) }

        // def is used for the first specifiers without the optional NAME=
        rule specifiers(def: &[&str]) -> Specifiers
            = s:specifier() ++ "," {
                Specifiers(HashMap::from_iter(
                s.into_iter().enumerate().map(|(i, Specifier(k, v))|
                    (k.unwrap_or_else(|| (*def.get(i).expect("missing required specifier name")).to_owned()), v)
                )))
            }

        rule read_statement() -> Statement
            = "READ(" s:specifiers(&["UNIT", "FMT"]) ")"
                d:data_name() ** "," ![_]
                { Statement::Read(s, d) }
            / "READ" "FMT="? fmt:specifier_value()
                d:("," e:expression() { DataName::Expression(e) } / data_name())* ![_]
                { Statement::Read(Specifiers(HashMap::from([("FMT".to_owned(), fmt)])), d) }

        rule write_statement() -> Statement
            = "WRITE(" s:specifiers(&["UNIT", "FMT"]) ")"
                d:(e:expression() { DataName::Expression(e) } / data_name()) ** "," ![_]
                { Statement::Write(s, d) }

        rule print_statement() -> Statement
            = "PRINT" "FMT="? fmt:specifier_value()
                d:("," e:expression() { DataName::Expression(e) } / "," d:data_name() { d })* ![_]
                { Statement::Print(fmt, d) }

        rule open_statement() -> Statement
            = "OPEN(" s:specifiers(&["UNIT"]) ")" ![_] { Statement::Open(s) }

        rule close_statement() -> Statement
            = "CLOSE(" s:specifiers(&["UNIT"]) ")" ![_] { Statement::Close(s) }

        rule inquire_statement() -> Statement
            = "INQUIRE(" s:specifiers(&[]) ")" ![_] { Statement::Inquire(s) }

        rule backspace_statement() -> Statement
            = "BACKSPACE(" s:specifiers(&["UNIT"]) ")" ![_] { Statement::Backspace(s) }

        rule endfile_statement() -> Statement
            = "ENDFILE(" s:specifiers(&["UNIT"]) ")" ![_] { Statement::Endfile(s) }

        rule rewind_statement() -> Statement
            = "REWIND(" s:specifiers(&["UNIT"]) ")" ![_] { Statement::Rewind(s) }



        rule call_statement() -> Statement
            = "CALL" sub:symbol() a:(
                "(" a:(expression() ** ",") ")" { a }
            )? ![_] { Statement::Call(sub, a.unwrap_or_default()) }
            // not supported: alternate return specifier

        rule return_statement() -> Statement
            = "RETURN" e:expression()? ![_] { Statement::Return(e) }

        // NOTE: This accepts some things that F77 doesn't, like A**-B, but that's easier
        // than trying to implement the grammar exactly
        rule expression() -> Expression = precedence!{
            // Logical (lowest to highest)
            x1:(@) ".EQV." x2:@ { Expression::Binary(BinaryOp::Eqv, Box::new(x1), Box::new(x2)) }
            x1:(@) ".NEQV." x2:@ { Expression::Binary(BinaryOp::Neqv, Box::new(x1), Box::new(x2)) }
            --
            x1:(@) ".OR." x2:@ { Expression::Binary(BinaryOp::Or, Box::new(x1), Box::new(x2)) }
            --
            x1:(@) ".AND." x2:@ { Expression::Binary(BinaryOp::And, Box::new(x1), Box::new(x2)) }
            --
            ".NOT." x2:@ { Expression::Unary(UnaryOp::Not, Box::new(x2)) }
            --
            // Relational
            x1:(@) ".LT." x2:@ { Expression::Binary(BinaryOp::Lt, Box::new(x1), Box::new(x2)) }
            x1:(@) ".LE." x2:@ { Expression::Binary(BinaryOp::Le, Box::new(x1), Box::new(x2)) }
            x1:(@) ".EQ." x2:@ { Expression::Binary(BinaryOp::Eq, Box::new(x1), Box::new(x2)) }
            x1:(@) ".NE." x2:@ { Expression::Binary(BinaryOp::Ne, Box::new(x1), Box::new(x2)) }
            x1:(@) ".GT." x2:@ { Expression::Binary(BinaryOp::Gt, Box::new(x1), Box::new(x2)) }
            x1:(@) ".GE." x2:@ { Expression::Binary(BinaryOp::Ge, Box::new(x1), Box::new(x2)) }
            x1:(@) "<"    x2:@ { Expression::Binary(BinaryOp::Lt, Box::new(x1), Box::new(x2)) }
            x1:(@) "<="   x2:@ { Expression::Binary(BinaryOp::Le, Box::new(x1), Box::new(x2)) }
            x1:(@) "=="   x2:@ { Expression::Binary(BinaryOp::Eq, Box::new(x1), Box::new(x2)) }
            x1:(@) "!="   x2:@ { Expression::Binary(BinaryOp::Ne, Box::new(x1), Box::new(x2)) }
            x1:(@) ">"    x2:@ { Expression::Binary(BinaryOp::Gt, Box::new(x1), Box::new(x2)) }
            x1:(@) ">="   x2:@ { Expression::Binary(BinaryOp::Ge, Box::new(x1), Box::new(x2)) }
            --
            // Character
            x1:(@) "//" x2:@ { Expression::Binary(BinaryOp::Concat, Box::new(x1), Box::new(x2)) }
            --
            // Arithmetic (lowest to highest)
            x1:(@) "+" x2:@ { Expression::Binary(BinaryOp::Add, Box::new(x1), Box::new(x2)) }
            x1:(@) "-" x2:@ { Expression::Binary(BinaryOp::Sub, Box::new(x1), Box::new(x2)) }
            "+" x2:@ { x2 }
            "-" x2:@ { Expression::Unary(UnaryOp::Negate, Box::new(x2)) }
            --
            x1:(@) "*" x2:@ { Expression::Binary(BinaryOp::Mul, Box::new(x1), Box::new(x2)) }
            x1:(@) "/" x2:@ { Expression::Binary(BinaryOp::Div, Box::new(x1), Box::new(x2)) }
            --
            x1:@ "**" x2:(@) { Expression::Binary(BinaryOp::Pow, Box::new(x1), Box::new(x2)) }
            --
            // Primaries
            s:symbol() "(" e:(expression() ** ",") ")(" e1:expression()? ":" e2:expression()? ")"
                { Expression::SubstringArrayElement(s, e, e1.map(Box::new), e2.map(Box::new)) }
            s:symbol() "(" e:(expression() ** ",") ")"
                { Expression::ArrayElementOrFunction(s, e) }
            s:symbol() "(" e1:expression()? ":" e2:expression()? ")"
                { Expression::Substring(s, e1.map(Box::new), e2.map(Box::new)) }
            s:symbol() { Expression::Symbol(s) }
            c:constant() { Expression::Constant(c) }
            "(" x:expression() ")" { Expression::Unary(UnaryOp::Paren, Box::new(x)) }
        }

        rule constant() -> Constant
            = ".TRUE." { Constant::Bool(true) }
            / ".FALSE." { Constant::Bool(false) }
            / n:double_constant() { Constant::Double(n) }
            / n:real_constant() { Constant::Real(n) }
            // not supported: complex
            / n:integer_constant_i32() { Constant::Integer(n) }
            / s:character_constant() { Constant::Character(s) }

        pub rule statement() -> Statement
            = program_statement()
            / function_statement()
            / subroutine_statement()
            / entry_statement()
            / executable_statement()
            / implicit_statement()
            / parameter_statement()
            / other_specification_statement()
            / data_statement()
            / include_statement()
    }
}
