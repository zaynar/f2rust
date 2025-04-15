use f2rust_std::io::*;
use f2rust_std::{Context, Error, Result};
use std::io::{Read, Seek};
use std::rc::Rc;
use tempfile::NamedTempFile;

#[test]
fn sequential_formatted() -> Result<()> {
    let mut ctx = Context::new();
    let mut tmp = NamedTempFile::with_prefix("f2rust_std-")?;

    let unit = 1;
    let file = tmp
        .path()
        .as_os_str()
        .to_str()
        .ok_or(Error::NonUnicodePath)?
        .to_owned();

    ctx.open(OpenSpecs {
        unit: Some(unit),
        file: Some(file.as_bytes()),
        status: Some(b"OLD"),
        access: Some(b"SEQUENTIAL"),
        form: Some(b"FORMATTED"),
        recl: None,
    })?;

    for i in 1..=3 {
        let mut w = FormattedWriter::new(ctx.io_unit(unit)?, None, b"(A,A,I4)")?;
        w.start()?;

        w.write_str(b"Hello ")?;
        w.write_str(b"world")?;
        w.write_i32(i)?;

        w.finish()?;
    }

    ctx.close(CloseSpecs {
        unit: Some(unit),
        status: None,
    })?;

    let mut content = Vec::new();
    tmp.read_to_end(&mut content)?;
    assert_eq!(
        content,
        b"Hello world   1\nHello world   2\nHello world   3\n"
    );
    tmp.rewind()?;

    ctx.open(OpenSpecs {
        unit: Some(unit),
        file: Some(file.as_bytes()),
        status: Some(b"OLD"),
        access: Some(b"SEQUENTIAL"),
        form: Some(b"FORMATTED"),
        recl: None,
    })?;

    {
        let mut r = FormattedReader::new(ctx.io_unit(unit)?, None, b"(A)")?;
        r.start()?;

        let mut buf = vec![b'x'; 100];
        r.read_str(&mut buf)?;
        assert_eq!(buf.trim_ascii_end(), b"Hello world   1");
        r.read_str(&mut buf)?;
        assert_eq!(buf.trim_ascii_end(), b"Hello world   2");

        r.finish()?;
    }

    {
        let mut w = FormattedWriter::new(ctx.io_unit(unit)?, None, b"(A)")?;
        w.start()?;
        w.write_str(b"Bye")?;
        w.finish()?;
    }

    {
        let mut r = FormattedReader::new(ctx.io_unit(unit)?, None, b"(A)")?;
        r.start()?;
        let mut buf = vec![b'x'; 100];
        assert!(matches!(r.read_str(&mut buf), Err(Error::EndOfFile)));
        r.finish()?;
    }

    ctx.close(CloseSpecs {
        unit: Some(unit),
        status: None,
    })?;

    let mut content = Vec::new();
    tmp.read_to_end(&mut content)?;
    assert_eq!(content, b"Hello world   1\nHello world   2\nBye\n");
    tmp.rewind()?;

    Ok(())
}

#[test]
fn list_directed() -> Result<()> {
    let mut ctx = Context::new();
    let mut tmp = NamedTempFile::with_prefix("f2rust_std-")?;

    let unit = 1;
    let file = tmp
        .path()
        .as_os_str()
        .to_str()
        .ok_or(Error::NonUnicodePath)?
        .to_owned();

    ctx.open(OpenSpecs {
        unit: Some(unit),
        file: Some(file.as_bytes()),
        status: Some(b"OLD"),
        access: Some(b"SEQUENTIAL"),
        form: Some(b"FORMATTED"),
        recl: None,
    })?;

    for i in 1..=3 {
        let mut w = ListDirectedWriter::new(ctx.io_unit(unit)?, None)?;
        w.start()?;

        w.write_str(b"Hello ")?;
        w.write_str(b"world")?;
        w.write_i32(i)?;

        w.finish()?;
    }

    ctx.close(CloseSpecs {
        unit: Some(unit),
        status: None,
    })?;

    let mut content = Vec::new();
    tmp.read_to_end(&mut content)?;
    assert_eq!(
        content,
        b" Hello world           1\n Hello world           2\n Hello world           3\n"
    );
    tmp.rewind()?;

    Ok(())
}

#[test]
fn sequential_unformatted() -> Result<()> {
    let mut ctx = Context::new();
    let mut tmp = NamedTempFile::with_prefix("f2rust_std-")?;

    let unit = 1;
    let file = tmp
        .path()
        .as_os_str()
        .to_str()
        .ok_or(Error::NonUnicodePath)?
        .to_owned();

    ctx.open(OpenSpecs {
        unit: Some(unit),
        file: Some(file.as_bytes()),
        status: Some(b"OLD"),
        access: Some(b"SEQUENTIAL"),
        form: Some(b"UNFORMATTED"),
        recl: None,
    })?;

    for i in 1..=2 {
        let mut w = UnformattedWriter::new(ctx.io_unit(unit)?, None)?;
        w.start()?;

        w.write_i32(0x11223344 * i)?;
        w.write_f32(-0.5f32)?;
        w.write_f64(-0.5f64)?;
        w.write_bool(true)?;
        w.write_bool(false)?;
        w.write_str(b"test")?;

        w.finish()?;
    }

    ctx.close(CloseSpecs {
        unit: Some(unit),
        status: None,
    })?;

    let mut content = Vec::new();
    tmp.read_to_end(&mut content)?;
    assert_eq!(
        content,
        [
            0x1c, 0x00, 0x00, 0x00, // header
            0x44, 0x33, 0x22, 0x11, // i32
            0x00, 0x00, 0x00, 0xbf, // f32
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xe0, 0xbf, // f64
            0x01, 0x00, 0x00, 0x00, // true
            0x00, 0x00, 0x00, 0x00, // false
            0x74, 0x65, 0x73, 0x74, // str
            0x1c, 0x00, 0x00, 0x00, // trailer
            0x1c, 0x00, 0x00, 0x00, // header
            0x88, 0x66, 0x44, 0x22, // i32
            0x00, 0x00, 0x00, 0xbf, // f32
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xe0, 0xbf, // f64
            0x01, 0x00, 0x00, 0x00, // true
            0x00, 0x00, 0x00, 0x00, // false
            0x74, 0x65, 0x73, 0x74, // str
            0x1c, 0x00, 0x00, 0x00, // trailer
        ]
    );
    tmp.rewind()?;

    ctx.open(OpenSpecs {
        unit: Some(unit),
        file: Some(file.as_bytes()),
        status: Some(b"OLD"),
        access: Some(b"SEQUENTIAL"),
        form: Some(b"UNFORMATTED"),
        recl: None,
    })?;

    for i in 1..=2 {
        let mut r = UnformattedReader::new(ctx.io_unit(unit)?, None)?;
        r.start()?;

        assert_eq!(r.read_i32()?, 0x11223344 * i);
        assert_eq!(r.read_f32()?, -0.5f32);
        assert_eq!(r.read_f64()?, -0.5f64);
        assert_eq!(r.read_bool()?, true);
        assert_eq!(r.read_bool()?, false);

        let mut buf = vec![b'x'; 4];
        r.read_str(&mut buf)?;
        assert_eq!(buf, b"test");

        assert_eq!(r.read_i32()?, 0);

        r.finish()?;
    }

    {
        let mut r = UnformattedReader::new(ctx.io_unit(unit)?, None)?;
        r.start()?;

        println!("{:?}", r.read_i32());
        assert!(matches!(r.read_i32(), Err(Error::EndOfFile)));
    }

    ctx.close(CloseSpecs {
        unit: Some(unit),
        status: None,
    })?;

    Ok(())
}

#[test]
fn internal_file() -> Result<()> {
    let mut record = vec![b'x'; 30];

    {
        let file = InternalFile::open(&mut record);
        let mut w = ListDirectedWriter::new(Rc::clone(&file), None)?;
        w.start()?;

        w.write_str(b"Hello ")?;
        w.write_str(b"world")?;
        w.write_i32(1)?;

        w.finish()?;
    }

    assert_eq!(record, b" Hello world           1      ");

    {
        let mut buf = vec![b'x'; 20];

        let file = InternalFile::open(&mut record);
        let mut r = FormattedReader::new(Rc::clone(&file), None, b"(A12,I12)")?;
        r.start()?;
        r.read_str(&mut buf)?;
        assert_eq!(buf.trim_ascii_end(), b" Hello world");
        r.finish()?;

        let mut r = FormattedReader::new(Rc::clone(&file), None, b"(A12,I12)")?;
        r.start()?;
        assert!(matches!(r.read_str(&mut buf), Err(Error::EndOfFile)));
    }

    Ok(())
}

#[test]
fn formatted_seek() -> Result<()> {
    let mut record = vec![b'x'; 20];

    {
        let file = InternalFile::open(&mut record);
        let mut w = FormattedWriter::new(Rc::clone(&file), None, b"(4X,A,4X,A)")?;
        w.start()?;

        w.write_str(b"abcd")?;
        w.write_str(b"hijk")?;

        w.finish()?;
    }

    assert_eq!(record, b"    abcd    hijk    ");

    Ok(())
}
