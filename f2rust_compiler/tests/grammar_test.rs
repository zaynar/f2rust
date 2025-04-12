use anyhow::Result;
use f2rust_compiler::{
    file,
    grammar::{
        self, BinaryOp, Constant, DataName, Expression, SpecifierValue, Specifiers, Statement,
        UnaryOp,
    },
};
use indexmap::IndexMap;

fn parse(src: &str) -> Statement {
    let input = &file::remove_blanks(src);
    grammar::fortran_parser::statement(input).expect("parse error")
}

fn parse_expr(expr: &str) -> Expression {
    let input = &file::remove_blanks(&format!("X = {expr}"));
    match grammar::fortran_parser::statement(input) {
        Err(err) => panic!("parse failed: {err}\ninput: {input}"),
        Ok(Statement::Assignment(DataName::Variable(_), expr)) => expr,
        Ok(_) => panic!("unexpected parse"),
    }
}

#[test]
fn test_literals() {
    assert_eq!(
        parse_expr("42"),
        Expression::Constant(Constant::Integer(42))
    );

    assert_eq!(parse_expr("+3"), Expression::Constant(Constant::Integer(3)));

    assert_eq!(
        parse_expr("-3"),
        Expression::Unary(
            UnaryOp::Negate,
            Box::new(Expression::Constant(Constant::Integer(3)))
        )
    );

    assert_eq!(
        parse_expr("1.2E+3"),
        Expression::Constant(Constant::Real(1.2e3))
    );

    assert_eq!(
        parse_expr("1.2D+3"),
        Expression::Constant(Constant::Double(1.2e3))
    );

    assert_eq!(parse_expr("0."), Expression::Constant(Constant::Real(0.0)));

    assert_eq!(
        parse_expr("1.E2"),
        Expression::Constant(Constant::Real(1.0e2))
    );

    assert_eq!(
        parse_expr("-.1E-2"),
        Expression::Unary(
            UnaryOp::Negate,
            Box::new(Expression::Constant(Constant::Real(0.1e-2)))
        )
    );

    assert_eq!(
        parse_expr("1.1D2"),
        Expression::Constant(Constant::Double(1.1e2))
    );

    assert_eq!(
        parse_expr("1.1E2"),
        Expression::Constant(Constant::Real(1.1e2))
    );

    assert_eq!(
        parse_expr("'hello world'"),
        Expression::Constant(Constant::Character("hello world".to_owned()))
    );

    assert_eq!(
        parse_expr("'hello''world'"),
        Expression::Constant(Constant::Character("hello'world".to_owned()))
    );

    assert_eq!(
        parse_expr("''''''"),
        Expression::Constant(Constant::Character("''".to_owned()))
    );

    assert_eq!(
        parse_expr(".TRUE."),
        Expression::Constant(Constant::Bool(true))
    );

    assert_eq!(
        parse_expr(".FALSE."),
        Expression::Constant(Constant::Bool(false))
    );

    assert_eq!(
        parse_expr("9876543214321D0"),
        Expression::Constant(Constant::Double(9876543214321.0))
    );
}

#[test]
fn test_if() {
    assert_eq!(
        parse("IF(COMP.LT.1.OR.COMP.GT.OBJSIZ)THEN"),
        Statement::BlockIf(Expression::Binary(
            BinaryOp::Or,
            Box::new(Expression::Binary(
                BinaryOp::Lt,
                Box::new(Expression::Symbol("COMP".to_owned())),
                Box::new(Expression::Constant(Constant::Integer(1)))
            )),
            Box::new(Expression::Binary(
                BinaryOp::Gt,
                Box::new(Expression::Symbol("COMP".to_owned())),
                Box::new(Expression::Symbol("OBJSIZ".to_owned()))
            ))
        ))
    );

    assert_eq!(
        parse("READ (TEXT,*,IOSTAT=IOSTAT) ( IC(I), I = 1, NI - 2 )"),
        Statement::Read(
            Specifiers(IndexMap::from([
                (
                    "UNIT".to_owned(),
                    SpecifierValue::Expression(Expression::Symbol("TEXT".to_owned()))
                ),
                ("FMT".to_owned(), SpecifierValue::Asterisk),
                (
                    "IOSTAT".to_owned(),
                    SpecifierValue::Expression(Expression::Symbol("IOSTAT".to_owned()))
                ),
            ])),
            vec![DataName::ImpliedDo(
                vec![DataName::ArrayElement(
                    "IC".to_owned(),
                    vec![Expression::Symbol("I".to_owned())]
                )],
                "I".to_owned(),
                Expression::Constant(Constant::Integer(1)),
                Expression::Binary(
                    BinaryOp::Sub,
                    Box::new(Expression::Symbol("NI".to_owned())),
                    Box::new(Expression::Constant(Constant::Integer(2)))
                ),
                None
            )]
        )
    );
}

// TODO: clean this up, turn into proper tests
#[test]
fn test_parsing() -> Result<()> {
    for st in [
        "A = 1 + 2 + 3 + 4",
        "A = 1 * 2 + 3 * 4",
        "A = 1 * (2 + 3) * 4",
        "A = 1 ** 2 ** 3 ** 4",
        "A = -1 - 2 + +3",
        "A = -A**2",
        "A = A**-B", // should fail, per the standard
        "A = A**(-B)",
        "A = A+-B", // should fail, per the standard
        "A = A+(-B)",
        "A = 'AB' // 'CD' // 'EF'",
        "A = B + C",
        "A(I) = B + C",
        "A(I) = B(I) + C",
        "A(I) = B(I) + C(2)",
        "X = A .OR. B .AND. C",
        "X = L .OR. A + B .GE. C",
        "X = L .OR. ((A + B) .GE. C)",
        "Y = F(G(X))",
        "DATA (( X(J,I), I=1,J), J=1,5) / 15*0. /",
        "LINE(S:)='Start of Interval ('//TIMLBL(:RTRIM(TIMLBL))//')'",
        "CALLED",
        "CALLED=.TRUE.",
        "CLOSE(SCRLUN)",
        "OPEN(UNIT=SCRLUN,FORM='FORMATTED',ACCESS='SEQUENTIAL',STATUS='SCRATCH',IOSTAT=IOSTAT)",
        "WRITE(SCRLUN,FMT='(A)',IOSTAT=IOSTAT) LINE(1:RTRIM(LINE))",
        "WRITE(6,*)' '",
        "WRITE(*,'(A)')CHAR(27)//'[B'",
        "DATA DEFMSG(4)(41:LL) /'test'/",
        "EQUIVALENCE(END,PTR(2))",
        "HYPOT(X, Y) = X**2 + Y**2",
        "IF((EL>0.D0).AND.(EL<HALFPI()))THEN",
    ] {
        let s = &file::remove_blanks(st);
        println!("{:?}", grammar::fortran_parser::statement(s)?);
    }

    Ok(())
}
