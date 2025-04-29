use crate::ast::{DataType, LenSpecification, ProgramUnitType, Statement};
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
    ret_ty: String,
    param: String,
    arg: String,
}

fn emit_sized_array(ty: &str, dims: &[Option<i32>]) -> (String, String, bool) {
    let mut r = ty.to_owned();
    let mut depth = 0;
    for dim in dims {
        depth += 1;
        if let Some(n) = dim {
            r = format!("[{r}; {n}]");
        } else {
            return (format!("[{r}]"), format!("Vec<{r}>"), depth > 1);
        }
    }
    (r.clone(), r, depth > 1)
}

fn emit_api_symbol(loc: &SourceLoc, name: &str, syms: &SymbolTable) -> Result<RawArg> {
    let sym = syms.get(name)?;
    let ty = emit_datatype(&sym.ast.base_type);

    let ident = safe_identifier(&name.to_ascii_lowercase());

    Ok(match sym.rs_ty {
        RustType::Primitive => RawArg {
            name: ident.clone(),
            ret_ty: ty.clone(),
            param: format!("{ident}: {ty}"),
            arg: ident.clone(),
        },
        RustType::PrimitiveRefMut => RawArg {
            name: ident.clone(),
            ret_ty: ty.clone(),
            param: format!("{ident}: &mut {ty}"),
            arg: ident.clone(),
        },

        RustType::DummyArray => {
            let (array, ret_ty, flatten) = emit_sized_array(&ty, &eval_dims(&sym.ast.dims, syms)?);
            RawArg {
                name: ident.clone(),
                ret_ty,
                param: format!("{ident}: &{array}"),
                arg: if flatten {
                    format!("{ident}.as_flattened()")
                } else {
                    ident.clone()
                },
            }
        }
        RustType::DummyArrayMut => {
            let (array, ret_ty, flatten) = emit_sized_array(&ty, &eval_dims(&sym.ast.dims, syms)?);
            RawArg {
                name: ident.clone(),
                ret_ty,
                param: format!("{ident}: &mut {array}"),
                arg: if flatten {
                    format!("{ident}.as_flattened_mut()")
                } else {
                    ident.clone()
                },
            }
        }

        RustType::DummyCharArray => RawArg {
            name: ident.clone(),
            ret_ty: "String".to_owned(),
            param: format!("{ident}: CharArray"),
            arg: ident.clone(),
        },
        RustType::DummyCharArrayMut => RawArg {
            name: ident.clone(),
            ret_ty: "String".to_owned(),
            param: format!("{ident}: CharArrayMut"),
            arg: ident.clone(),
        },
        RustType::CharSliceRef => {
            if let Some(size) = eval_character_len(&sym.ast.character_len, syms)? {
                if size == 1 {
                    return Ok(RawArg {
                        name: ident.clone(),
                        ret_ty: "String".to_owned(),
                        param: format!("{ident}: char"),
                        arg: format!("&[u8::try_from({ident}).unwrap()]"),
                    });
                } else {
                    // This only happens in LTIME, which wants a 2-char string
                }
            }
            RawArg {
                name: ident.clone(),
                ret_ty: "String".to_owned(),
                param: format!("{ident}: &str"),
                arg: format!("{ident}.as_bytes()"),
            }
            // TODO: if the string is empty, we should pass " " instead,
            // or else prove the FORTRAN code doesn't mind zero-length strings
        }
        RustType::CharSliceMut => {
            if let Some(size) = eval_character_len(&sym.ast.character_len, syms)? {
                RawArg {
                    name: ident.clone(),
                    ret_ty: "String".to_owned(),
                    param: format!("{ident}: &mut [u8; {size}]"),
                    arg: ident.clone(),
                }
            } else {
                RawArg {
                    name: ident.clone(),
                    ret_ty: "String".to_owned(),
                    param: format!("{ident}: &mut str"),
                    arg: format!("fstr::StrBytes::new({ident}).as_mut()"),
                }
            }
        }
        RustType::Procedure => RawArg {
            name: ident.clone(),
            ret_ty: ty.clone(),
            param: format!("{ident}: {ty}"),
            arg: ident.clone(),
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

// Does this statement (recursively) call SIGERR('SPICE(...BOGUSENTRY)')
// or (for TRCPKG) WRLINE(..., 'SPICE(...BOGUSENTRY)')
fn is_bogus_entry(st: &Statement) -> bool {
    match st {
        Statement::Call(func, args) if func == "SIGERR" => {
            matches!(&args[0],
            ast::Expression::Constant(Constant::Character(s)) if
                s.starts_with("SPICE(") && s.ends_with("BOGUSENTRY)")
            )
        }
        Statement::Call(func, args) if func == "WRLINE" => {
            matches!(&args[1],
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

fn should_expose(entry: &Entry) -> bool {
    let name = entry.ast.name.as_str();

    // Don't expose functions that return BOGUSENTRY/CKBOGUSENTRY/etc
    // since they're not meant to be called directly.
    // (But they may have useful docs, so still emit the raw function)
    if entry.ast.body.iter().any(|(_loc, st)| is_bogus_entry(st)) {
        return false;
    }

    // Don't expose deprecated/obsolete functions, especially
    // BODVAR/RTPOOL/etc which have no way to avoid buffer overflows
    if entry
        .ast
        .comment_sections
        .get("Abstract")
        .is_some_and(|c| c.join("").trim_ascii_start().starts_with("Deprecated:"))
        || matches!(name, "RTPOOL" | "DAFRDR" | "DASOPN" | "PCKEUL")
    {
        return false;
    }

    // Don't expose functions that explicitly don't want to be called
    if entry
        .ast
        .comment_sections
        .get("Abstract")
        .is_some_and(|c| c.join("").contains("DO NOT CALL THIS ROUTINE"))
        || entry
            .ast
            .comment_sections
            .get("Particulars")
            .is_some_and(|c| c.join("").contains("DO NOT CALL THIS ROUTINE"))
    {
        return false;
    }

    // Exclude private APIs from pool.f
    if name.starts_with("ZZ") {
        return false;
    }

    // Exclude functions that are redundant with basic Rust functionality,
    // and that require non-trivial interfaces (especially string allocation)
    if matches!(
        name,
        "ASTRIP"
            | "CMPRSS"
            | "LCASE"
            | "UCASE"
            | "LJUST"
            | "RJUST"
            | "PREFIX"
            | "SUFFIX"
            | "QUOTE"
            | "SHIFTL"
            | "SHIFTR"
            | "INSSUB"
            | "LBUILD"
            | "MAXAC"
            | "MINAC"
            | "MEQUG"
            | "MOVEC"
            | "MOVED"
            | "MOVEI"
            | "MOVEL"
            | "NEXTWD"
            | "NTHWD"
            | "ORDERC"
            | "ORDERD"
            | "ORDERI"
            | "CLEARC"
            | "CLEARD"
            | "CLEARI"
            | "FILLC"
            | "FILLD"
            | "FILLI"
            | "SWAPC"
            | "SWAPD"
            | "SWAPI"
    ) {
        return false;
    }

    true
}

fn should_return_arg(
    entry: &Entry,
    arg_dirs: &IndexMap<&str, &str>,
    darg: &String,
) -> Result<bool> {
    if arg_dirs.get(darg.as_str()) != Some(&"O") {
        return Ok(false);
    }

    // Don't return CharArrayMut, since we typically have no idea how much space to allocate
    let sym = entry.codegen.syms.get(darg)?;
    if matches!(sym.ast.base_type, DataType::Character) && !sym.ast.dims.is_empty() {
        return Ok(false);
    }

    // Don't return arrays where we don't know how much to allocate
    if try_output_array_size(&entry.ast.name, darg).is_none() {
        return Ok(false);
    }

    Ok(true)
}

fn try_output_array_size(func: &str, arg: &str) -> Option<&'static str> {
    // Basic approach:
    // * Args documented as sets cannot be pure outputs. The caller needs to initialise it.
    //   Return None for these.
    // * Some functions explicitly state the size of the arguments.
    // * Some state it in Required Reading.
    // * For others, we have to look at examples and actual usage to figure out a reasonable value.
    // * Where possible, get an exact size. Otherwise, use the max size, and let the caller
    //   figure out how much has real data.
    // * Where possible, use a constant from an .inc file.

    // frames.req
    const FRAME_NAME: &str = "26";

    // ck.req / ckbstr.f
    const DSCSIZ: &str = "5"; // 2 doubles + 6 ints
    const SIDLEN: &str = "40";

    // keeper.f
    const FILSIZ: &str = "255";

    match format!("{func}::{arg}").as_str() {
        "BLTFRM::IDSET" => None,
        "BODC2N::NAME" => Some("inc::zzbodtrn::MAXL"),
        "BODC2S::NAME" => Some("inc::zzbodtrn::MAXL"),
        "BODVCD::VALUES" => Some("maxn"),
        "BODVRD::VALUES" => Some("maxn"),
        "CHBDER::DPDXS" => Some("(nderiv + 1)"),
        "IRFNAM::NAME" => Some(FRAME_NAME),
        "CKSNS::DESCR" => Some(DSCSIZ),
        "CKSNS::SEGID" => Some(SIDLEN),
        "CKGR01::RECORD" => Some("8"),
        "CKGR02::RECORD" => Some("10"),
        "CKGR03::RECORD" => Some("8"),
        "CKGR04::RECORD" => Some("inc::ckparam::CK4RSZ"),
        "CKGR05::RECORD" => Some("16"),
        "CKGR06::RECORD" => Some("17"),
        "CKR01::RECORD" => Some("inc::ckparam::CK1RSZ"),
        "CKR02::RECORD" => Some("inc::ckparam::CK2RSZ"),
        "CKR03::RECORD" => Some("inc::ckparam::CK3RSZ"),
        "CKR04::RECORD" => Some("inc::ckparam::CK4RSZ"),
        "CKR05::RECORD" => Some("inc::ckparam::CK5RSZ"),
        "CKR06::RECORD" => Some("inc::ckparam::CK6RSZ"),
        "COPYD::COPY" => None,
        "COPYI::COPY" => None,
        "CYCLAD::OUT" => Some("nelt"),
        "CYCLAI::OUT" => Some("nelt"),
        "CYCLEC::OUTSTR" => Some("(instr.len() as i32)"),
        "DAFHFN::FNAME" => Some("inc::zzddhman::FILEN"),
        "DAFHOF::FHSET" => None,
        "DAFGS::SUM" => Some("128"),   // SUMLEN in DAFRA
        "DAFGN::NAME" => Some("1000"), // NAMLEN in DAFRA
        "DAFGDA::DATA" => Some("(eaddr + 1 - baddr).max(0)"),
        "DAFPS::SUM" => Some("(nd + (ni + 1)/2)"),
        "DAFUS::DC" => Some("nd"),
        "DAFUS::IC" => Some("ni"),
        "DAFRCR::CREC" => Some("1000"),
        "DAFRFR::IFNAME" => Some("60"), // see daf.req
        "DAFGDR::DATA" => Some("(end.min(128) + 1 - begin.max(1))"),
        "DAFGSR::DATA" => Some("(end.min(128) + 1 - begin.max(1))"),
        "DASHFN::FNAME" => Some("inc::zzddhman::FILEN"),
        "DASHOF::FHSET" => None,
        "DASHAM::ACCESS" => Some("5"), // 'READ' or 'WRITE'
        "DASRDD::DATA" => Some("(last + 1 - first).max(0)"),
        "DASRDI::DATA" => Some("(last + 1 - first).max(0)"),
        "DASRFR::IDWORD" => Some("8"),
        "DASRFR::IFNAME" => Some("60"),
        "DASRRD::DATAD" => Some("(last + 1 - first).max(0)"),
        "DASRRI::DATAI" => Some("(last + 1 - first).max(0)"),
        "DASRRC::DATAC" => Some("(last + 1 - first).max(0)"),
        "DIFFD::C" => None,
        "DIFFI::C" => None,
        "DLABBS::DLADSC" => Some("inc::dla::DLADSZ"),
        "DLABFS::DLADSC" => Some("inc::dla::DLADSZ"),
        "DLAFNS::NXTDSC" => Some("inc::dla::DLADSZ"),
        "DLAFPS::PRVDSC" => Some("inc::dla::DLADSZ"),
        "DP2HX::HXSTR" => Some("255"), // STRLEN
        "DPFMT::STR" => Some("(pictur.len() as i32)"),
        "DPSTR::STRING" => Some("(sigdig + 6)"),
        "DPSTRF::STRING" => Some("(sigdig + 6)"),
        "DSKD02::VALUES" => Some("room"),
        "DSKGD::DSKDSC" => Some("inc::dla::DLADSZ"),
        "DSKI02::VALUES" => Some("room"),
        "DSKMI2::SPAIXI" => Some("spxisz"),
        "DSKP02::PLATES" => Some("room"),
        "DSKV02::VRTCES" => Some("room"),
        "DSKXSI::DLADSC" => Some("inc::dla::DLADSZ"),
        "DSKXSI::DSKDSC" => Some("inc::dla::DLADSZ"),
        "DSKXSI::DC" => Some("inc::dsksrc::DCSIZE"),
        "DSKXSI::IC" => Some("inc::dsksrc::ICSIZE"),
        "DSKXV::XPTARR" => Some("nrays"),
        "DSKXV::FNDARR" => Some("nrays"),
        "DXTRCT::VALUES" => Some("maxwds"),
        "EDTERM::TRMPTS" => Some("npts"),
        "EKFIND::ERRMSG" => Some("inc::errhnd::LMSGLN"), // size used by tspice
        "EKIFLD::RCPTRS" => Some("nrows"),
        "EKPSEL::XBEGS" => Some("inc::ekqlimit::MAXSEL"),
        "EKPSEL::XENDS" => Some("inc::ekqlimit::MAXSEL"),
        "EKPSEL::ERRMSG" => Some("inc::errhnd::LMSGLN"),
        "EKTNAM::TABLE" => Some("inc::ektnamsz::TNAMSZ"),
        "EKCII::COLUMN" => Some("inc::ekattdsc::ADSCSZ"),
        "EKSRCH::ERRMSG" => Some("inc::errhnd::LMSGLN"),
        "EKGC::CDATA" => Some("inc::ekqlimit::MAXSTR"),
        "EKRCED::DVALS" => None, // can't find any max for this
        "EKRCEI::IVALS" => None,
        "EKSSUM::TABNAM" => Some("inc::ektnamsz::TNAMSZ"),
        "EKSSUM::SIZES" => Some("inc::ekglimit::MXCLSG"),
        "EKSSUM::STRLNS" => Some("inc::ekglimit::MXCLSG"),
        "EKSSUM::INDEXD" => Some("inc::ekglimit::MXCLSG"),
        "EKSSUM::NULLOK" => Some("inc::ekglimit::MXCLSG"),
        "ENCHAR::STRING" => Some("5"), // MINLEN
        "ET2LST::TIME" => Some("50"),  // from t_et2lst
        "ET2LST::AMPM" => Some("50"),
        "ET2UTC::UTCSTR" => Some("(24 + prec)"),
        "ETCAL::CALSTR" => Some("48"),
        "EXPLN::EXPL" => Some("256"),
        "FRMNAM::FRNAME" => Some(FRAME_NAME),
        "CIDFRM::FRNAME" => Some(FRAME_NAME),
        "CNMFRM::FRNAME" => Some(FRAME_NAME),
        "CCIFRM::FRNAME" => Some(FRAME_NAME),
        "GETFAT::ARCH" => Some("3"),
        "GETFAT::KERTYP" => Some("4"),
        "GETFOV::SHAPE" => Some("9"), // longest value is 'RECTANGLE'
        "GETFOV::FRAME" => Some(FRAME_NAME),
        "GETFOV::BOUNDS" => Some("room"),
        "GETFVN::SHAPE" => Some("9"), // longest value is 'RECTANGLE'
        "GETFVN::FRAME" => Some(FRAME_NAME),
        "GETFVN::BOUNDS" => Some("room"),
        "GETMSG::MSG" => Some("inc::errhnd::LMSGLN"),
        "GFDIST::WORK" => Some("((mw + 1 - LBCELL) * inc::gf::NWDIST)"),
        "GFEVNT::WORK" => Some("((mw + 1 - LBCELL) * nw)"),
        "GFILUM::WORK" => Some("((mw + 1 - LBCELL) * inc::gf::NWILUM)"),
        "GFPA::WORK" => Some("((mw + 1 - LBCELL) * inc::gf::NWPA)"),
        "GFPOSC::WORK" => Some("((mw + 1 - LBCELL) * inc::gf::NWMAX)"),
        "GFRR::WORK" => Some("((mw + 1 - LBCELL) * inc::gf::NWRR)"),
        "GFSEP::WORK" => Some("((mw + 1 - LBCELL) * inc::gf::NWSEP)"),
        "GFSNTC::WORK" => Some("((mw + 1 - LBCELL) * inc::gf::NWMAX)"),
        "GFSUBC::WORK" => Some("((mw + 1 - LBCELL) * inc::gf::NWMAX)"),
        "GFUDS::WORK" => Some("((mw + 1 - LBCELL) * nw)"),
        "HX2DP::ERRMSG" => Some("inc::errhnd::LMSGLN"),
        "HX2INT::ERRMSG" => Some("inc::errhnd::LMSGLN"),
        "IDW2AT::ARCH" => Some("3"),
        "IDW2AT::TYPE" => Some("4"),
        "INT2HX::STRING" => Some("9"), // 32-bit
        "INTERD::C" => None,
        "INTERI::C" => None,
        "INTORD::STRING" => Some("148"), // MAXORD
        "INTSTR::STRING" => Some("80"),  // STRLEN
        "INTTXT::STRING" => Some("146"), // 2 less than INTORD
        "KDATA::FILE" => Some(FILSIZ),
        "KDATA::FILTYP" => Some("8"), // TYPLEN in keeper.f
        "KDATA::SRCFIL" => Some(FILSIZ),
        "KINFO::FILTYP" => Some("8"),
        "KINFO::SRCFIL" => Some(FILSIZ),
        "KPLFRM::IDSET" => None,
        "KXTRCT::SUBSTR" => Some("(terms.element_length() as i32)"),
        "LATSRF::SRFPTS" => Some("npts"),
        "LIMBPT::NPTS" => Some("ncuts"),
        "LIMBPT::POINTS" => Some("maxn"),
        "LIMBPT::EPOCHS" => Some("maxn"),
        "LIMBPT::TANGTS" => Some("maxn"),
        "LJUCRS::OUTPUT" => Some("(input.len() as i32)"),
        "LUN2FN::FILNAM" => Some(FILSIZ),
        "MTXMG::MOUT" => Some("(nc1 * nc2)"),
        "MTXVG::VOUT" => Some("nc1"),
        "MXMG::MOUT" => Some("(nr1 * nc2)"),
        "MXMTG::MOUT" => Some("(nr1 * nr2)"),
        "MXVG::VOUT" => Some("nr1"),
        "NPARSD::ERROR" => Some("inc::errhnd::LMSGLN"),
        "NPARSI::ERROR" => Some("inc::errhnd::LMSGLN"),
        "OSCLTX::ELTS" => Some("inc::oscltx::OSCXSZ"),
        "PACKAD::OUT" => Some("todo!()"),
        "PACKAI::OUT" => Some("todo!()"),
        "PARSQS::VALUE" => Some("todo!()"),
        "PARSQS::ERRMSG" => Some("todo!()"),
        "PCKSFS::DESCR" => Some("todo!()"),
        "PCKSFS::IDENT" => Some("todo!()"),
        "PCKPDS::DESCR" => Some("todo!()"),
        "PCKR02::RECORD" => Some("todo!()"),
        "PCKR03::RECORD" => Some("todo!()"),
        "PCKR20::RECORD" => Some("todo!()"),
        "POLYDS::P" => Some("todo!()"),
        "GDPOOL::VALUES" => Some("todo!()"),
        "GIPOOL::IVALS" => Some("todo!()"),
        "DTPOOL::TYPE" => Some("todo!()"),
        "PROMPT::BUFFER" => Some("todo!()"),
        "PRTENC::STRING" => Some("todo!()"),
        "GETDEV::DEVICE" => Some("todo!()"),
        "GETLMS::MSG" => Some("todo!()"),
        "GETSMS::MSG" => Some("todo!()"),
        "QDERIV::DFDT" => Some("todo!()"),
        "RDENCD::DATA" => Some("todo!()"),
        "RDENCI::DATA" => Some("todo!()"),
        "RDKDAT::LINE" => Some("todo!()"),
        "RDKLIN::KERNEL" => Some("todo!()"),
        "RDKVAR::NAME" => Some("todo!()"),
        "RDNBL::LINE" => Some("todo!()"),
        "RDTEXT::LINE" => Some("todo!()"),
        "READLN::LINE" => Some("todo!()"),
        "REMSUB::OUT" => Some("todo!()"),
        "REPLCH::OUTSTR" => Some("todo!()"),
        "REPLWD::OUTSTR" => Some("todo!()"),
        "REPMC::OUT" => Some("todo!()"),
        "REPMCT::OUT" => Some("todo!()"),
        "REPMD::OUT" => Some("todo!()"),
        "REPMF::OUT" => Some("todo!()"),
        "REPMI::OUT" => Some("todo!()"),
        "REPML::OUT" => Some("todo!()"),
        "REPMOT::OUT" => Some("todo!()"),
        "REPSUB::OUT" => Some("todo!()"),
        "SCFM01::CLKSTR" => Some("todo!()"),
        "SCPR01::PARBEG" => Some("todo!()"),
        "SCPR01::PAREND" => Some("todo!()"),
        "SCANPR::MRKLEN" => Some("todo!()"),
        "SCANPR::PNTERS" => Some("todo!()"),
        "SCAN::IDENT" => Some("todo!()"),
        "SCAN::BEG" => Some("todo!()"),
        "SCAN::END" => Some("todo!()"),
        "SCARDD::CELL" => Some("todo!()"),
        "SCARDI::CELL" => Some("todo!()"),
        "SCDECD::SCLKCH" => Some("todo!()"),
        "SCE2S::SCLKCH" => Some("todo!()"),
        "SCFMT::CLKSTR" => Some("todo!()"),
        "SCLI01::IVAL" => Some("todo!()"),
        "SCLD01::DVAL" => Some("todo!()"),
        "SCPARS::MSG" => Some("todo!()"),
        "SCPART::PSTART" => Some("todo!()"),
        "SCPART::PSTOP" => Some("todo!()"),
        "SCPS01::MSG" => Some("todo!()"),
        "SCID2N::CLKNAM" => Some("todo!()"),
        "SDIFFD::C" => Some("todo!()"),
        "SDIFFI::C" => Some("todo!()"),
        "SEPOOL::STRING" => Some("todo!()"),
        "SGFCON::VALUES" => Some("todo!()"),
        "SGFPKT::VALUES" => Some("todo!()"),
        "SGFPKT::ENDS" => Some("todo!()"),
        "SGFREF::VALUES" => Some("todo!()"),
        "SHIFTC::OUT" => Some("todo!()"),
        "SIGDGT::OUT" => Some("todo!()"),
        "SPCRFL::LINE" => Some("todo!()"),
        "SPCRNL::LINE" => Some("todo!()"),
        "SPKSFS::DESCR" => Some("todo!()"),
        "SPKSFS::IDENT" => Some("todo!()"),
        "SPKPDS::DESCR" => Some("todo!()"),
        "SPKR01::RECORD" => Some("todo!()"),
        "SPKR02::RECORD" => Some("todo!()"),
        "SPKR03::RECORD" => Some("todo!()"),
        "SPKR05::RECORD" => Some("todo!()"),
        "SPKR08::RECORD" => Some("todo!()"),
        "SPKR09::RECORD" => Some("todo!()"),
        "SPKR10::RECORD" => Some("todo!()"),
        "SPKR12::RECORD" => Some("todo!()"),
        "SPKR13::RECORD" => Some("todo!()"),
        "SPKR14::RECORD" => Some("todo!()"),
        "SPKR15::RECORD" => Some("todo!()"),
        "SPKR17::RECORD" => Some("todo!()"),
        "SPKR18::RECORD" => Some("todo!()"),
        "SPKR19::RECORD" => Some("todo!()"),
        "SPKR20::RECORD" => Some("todo!()"),
        "SPKR21::RECORD" => Some("todo!()"),
        "SRFC2S::SRFSTR" => Some("todo!()"),
        "SRFCSS::SRFSTR" => Some("todo!()"),
        "SRFNRM::NORMLS" => Some("todo!()"),
        "SSIZED::CELL" => Some("todo!()"),
        "SSIZEI::CELL" => Some("todo!()"),
        "STCC01::TABNAM" => Some("todo!()"),
        "STCC01::ERRMSG" => Some("todo!()"),
        "STCG01::SPTYPE" => Some("todo!()"),
        "STCL01::TABNAM" => Some("todo!()"),
        "STPOOL::NTHSTR" => Some("todo!()"),
        "SYFETC::NAME" => Some("todo!()"),
        "SYFETD::NAME" => Some("todo!()"),
        "SYFETI::NAME" => Some("todo!()"),
        "SYGETD::VALUES" => Some("todo!()"),
        "SYGETI::VALUES" => Some("todo!()"),
        "SYNTHC::VALUE" => Some("todo!()"),
        "SYPOPC::VALUE" => Some("todo!()"),
        "SYSELD::VALUES" => Some("todo!()"),
        "SYSELI::VALUES" => Some("todo!()"),
        "TCHECK::ERROR" => Some("todo!()"),
        "TCHCKD::TYPE" => Some("todo!()"),
        "TERMPT::NPTS" => Some("todo!()"),
        "TERMPT::POINTS" => Some("todo!()"),
        "TERMPT::EPOCHS" => Some("todo!()"),
        "TERMPT::TRMVCS" => Some("todo!()"),
        "TIMOUT::OUTPUT" => Some("todo!()"),
        "TKVRSN::VERSTR" => Some("todo!()"),
        "TPARSE::ERRMSG" => Some("todo!()"),
        "TPARTV::TVEC" => Some("todo!()"),
        "TPARTV::TYPE" => Some("todo!()"),
        "TPARTV::PICTUR" => Some("todo!()"),
        "TPARTV::ERROR" => Some("todo!()"),
        "TPICTR::PICTUR" => Some("todo!()"),
        "TPICTR::ERRMSG" => Some("todo!()"),
        "TRCNAM::NAME" => Some("todo!()"),
        "QCKTRC::TRACE" => Some("todo!()"),
        "UNIOND::C" => Some("todo!()"),
        "UNIONI::C" => Some("todo!()"),
        "UNORMG::VOUT" => Some("todo!()"),
        "VADDG::VOUT" => Some("todo!()"),
        "VEQUG::VOUT" => Some("todo!()"),
        "VHATG::VOUT" => Some("todo!()"),
        "VLCOMG::SUM" => Some("todo!()"),
        "VMINUG::VOUT" => Some("todo!()"),
        "VPROJG::P" => Some("todo!()"),
        "VSCLG::VOUT" => Some("todo!()"),
        "VSUBG::VOUT" => Some("todo!()"),
        "WNCOMD::RESULT" => Some("todo!()"),
        "WNDIFD::C" => Some("todo!()"),
        "WNINTD::C" => Some("todo!()"),
        "WNUNID::C" => Some("todo!()"),
        "XDDA::VOXLST" => Some("todo!()"),
        "XPOSBL::BTMAT" => Some("todo!()"),
        "XPOSEG::XPOSEM" => Some("todo!()"),
        _ => None,
    }
}

fn output_array_size(func: &str, arg: &str) -> String {
    try_output_array_size(func, arg).unwrap().to_owned()
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

        if !raw && !should_expose(entry) {
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

        let mut params = vec![];
        let mut args = vec![];
        let mut init = String::new();
        let mut ret_ty = vec![];
        let mut ret_vals = vec![];
        let mut ret_names = vec![];
        let mut ret_found = false;

        for darg in &entry.ast.dargs {
            let arg = emit_api_symbol(&entry.ast.loc, darg, &entry.codegen.syms)?;
            let ident = safe_identifier(&arg.name);

            if raw {
                params.push(arg.param);
                args.push(arg.arg);
            } else if should_return_arg(&entry, &arg_dirs, darg)? {
                let sym = entry.codegen.syms.get(darg)?;
                if matches!(sym.ast.base_type, DataType::Character) {
                    let len = match &sym.ast.character_len {
                        Some(LenSpecification::Unspecified) => bail!("{darg} unspecified length"),
                        Some(LenSpecification::Integer(n)) => format!("{n}"),
                        Some(LenSpecification::IntConstantExpr(e)) => {
                            if let Some(n) = e.eval_constant(&entry.codegen.syms)? {
                                format!("{n}")
                            } else {
                                output_array_size(&entry.ast.name, darg)
                            }
                        }
                        _ => output_array_size(&entry.ast.name, darg),
                    };

                    writeln!(init, "  let mut {ident} = blank({len});")?;

                    ret_ty.push("String".to_owned());
                    ret_vals.push(format!("trim({ident})"));
                    ret_names.push(arg.name.clone());
                } else {
                    if arg.ret_ty.starts_with("Vec<") {
                        writeln!(
                            init,
                            "  let mut {ident}: {} = vec![Default::default(); {} as usize];",
                            arg.ret_ty,
                            output_array_size(&entry.ast.name, darg)
                        )?;
                    } else {
                        writeln!(
                            init,
                            "  let mut {ident}: {} = Default::default();",
                            arg.ret_ty
                        )?;
                    }

                    if darg == "FOUND" {
                        ret_found = true;
                    } else {
                        ret_ty.push(arg.ret_ty);
                        ret_vals.push(ident.clone());
                        ret_names.push(arg.name.clone());
                    }
                }
                args.push(format!("&mut {ident}"));
            } else {
                params.push(arg.param);
                args.push(ident.clone());
            }

            // writeln!(
            //     docs,
            //     "/// * {}: {}",
            //     arg.name,
            //     arg_dirs.get(darg.as_str()).unwrap_or(&"?")
            // )?;
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
            if !ret_names.is_empty() {
                let mut r = ret_names.join(", ");

                if ret_names.len() > 1 {
                    r = format!("({r})");
                }

                writeln!(docs, "///")?;
                writeln!(docs, "/// Returns `{r}`.")?;
            }

            writeln!(docs, "///")?;
            writeln!(
                docs,
                "/// See [`{fn_name}`](raw::{fn_name}) for full documentation."
            )?;
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
            if requires_ctx {
                params.insert(0, "&mut self".to_owned());
                args.insert(0, "self".to_owned());
            } else {
                params.insert(0, "&self".to_owned());
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
            let mut ret = ret_ty.join(", ");
            if ret_ty.len() != 1 {
                ret = format!("({ret})");
            }
            if ret_found {
                ret = format!("Option<{ret}>");
            }
            if returns_result {
                ret = format!("{result}<{ret}>");
            }
            format!("-> {ret}")
        };

        code += &docs;
        writeln!(
            code,
            "pub fn {fn_ident}{fn_lifetime}({params}) {ret} {{",
            fn_ident = safe_identifier(fn_name),
            params = params.join(", ")
        )?;
        code += &init;
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
            let call = &format!("raw::{}({})", safe_identifier(fn_name), args.join(", "));
            if matches!(ret_type, DataType::Void | DataType::Character) {
                let mut ret = ret_vals.join(", ");
                if ret_vals.len() != 1 {
                    ret = format!("({ret})");
                }
                if ret_found {
                    ret = format!("if found {{ Some({ret}) }} else {{ None }}");
                }
                if returns_result {
                    writeln!(code, "  {call}?;")?;
                    writeln!(code, "  Ok({ret})")?;
                } else {
                    writeln!(code, "  {call};")?;
                    writeln!(code, "  {ret}")?;
                }
            } else {
                if !ret_vals.is_empty() {
                    bail!("function has output parameters");
                }
                writeln!(code, "  {call}")?;
            }
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
