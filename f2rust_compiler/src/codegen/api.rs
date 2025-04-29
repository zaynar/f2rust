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

    // Private APIs from pool.f
    if name.starts_with("ZZ") {
        return false;
    }

    // Functions documented as "essentially .. private",
    // or "does not need to be called by the user's program"
    if matches!(name, "SCPR01" | "STCC01") {
        return false;
    }

    // Functions that are redundant with basic Rust functionality,
    // and that require non-trivial interfaces (especially string allocation),
    // so it's easier to just get rid of them
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
            | "SHIFTC"
            | "INSSUB"
            | "REMSUB"
            | "REPLCH"
            | "REPMC"
            | "REPMCT"
            | "REPMD"
            | "REPMF"
            | "REPMI"
            | "REPML"
            | "REPMOT"
            | "REPSUB"
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

    // Unsupported functionality
    if matches!(name, "PROMPT") {
        return false;
    }

    true
}

fn should_return_arg(entry: &Entry, arg_dirs: &IndexMap<&str, &str>, darg: &str) -> Result<bool> {
    if arg_dirs.get(darg) != Some(&"O") {
        return Ok(false);
    }

    // Don't return CharArrayMut, since we typically have no idea how much space to allocate
    let sym = entry.codegen.syms.get(darg)?;
    if matches!(sym.ast.base_type, DataType::Character) && !sym.ast.dims.is_empty() {
        return Ok(false);
    }

    Ok(true)
}

fn output_array_size(func: &str, arg: &str) -> Option<&'static str> {
    // To simplify the API, we want to allocate output strings/arrays with appropriate sizes.
    // (We don't care about letting users avoid dynamic memory allocation.)
    // But in many cases it's not trivially obvious how much to allocate, so this
    // table represents the sizes found in documentation and code.
    //
    // There are probably bugs here.
    //
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
    const CK_DSCSIZ: &str = "5"; // 2 doubles + 6 ints
    const CK_SIDLEN: &str = "40";

    // pck.req / pckbsr.f
    const PCK_DESCSZ: &str = "5";
    const PCK_IDSIZE: &str = "40";

    // spk.req / spkbsr.f
    const SPK_DSCSIZ: &str = "5";
    const SPK_SIDLEN: &str = "40";

    // keeper.f
    const FILSIZ: &str = "255";

    // Probably much larger than needed
    const ERRMSG: &str = "inc::errhnd::LMSGLN";

    match format!("{func}::{arg}").as_str() {
        "BLTFRM::IDSET" => None,
        "BODC2N::NAME" => Some("inc::zzbodtrn::MAXL"),
        "BODC2S::NAME" => Some("inc::zzbodtrn::MAXL"),
        "BODVCD::VALUES" => Some("maxn.max(0)"),
        "BODVRD::VALUES" => Some("maxn.max(0)"),
        "CHBDER::DPDXS" => Some("(nderiv + 1).max(0)"),
        "IRFNAM::NAME" => Some(FRAME_NAME),
        "CKSNS::DESCR" => Some(CK_DSCSIZ),
        "CKSNS::SEGID" => Some(CK_SIDLEN),
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
        "CYCLAD::OUT" => Some("nelt.max(0)"),
        "CYCLAI::OUT" => Some("nelt.max(0)"),
        "CYCLEC::OUTSTR" => Some("(instr.len() as i32)"),
        "DAFHFN::FNAME" => Some("inc::zzddhman::FILEN"),
        "DAFHOF::FHSET" => None,
        "DAFGS::SUM" => Some("128"),   // SUMLEN in DAFRA
        "DAFGN::NAME" => Some("1000"), // NAMLEN in DAFRA
        "DAFGDA::DATA" => Some("(eaddr + 1 - baddr).max(0)"),
        "DAFPS::SUM" => Some("(nd + (ni + 1)/2).max(0)"),
        "DAFUS::DC" => Some("nd.max(0)"),
        "DAFUS::IC" => Some("ni.max(0)"),
        "DAFRCR::CREC" => Some("1000"),
        "DAFRFR::IFNAME" => Some("60"), // see daf.req
        "DAFGDR::DATA" => Some("(end.min(128) + 1 - begin.max(1)).max(0)"),
        "DAFGSR::DATA" => Some("(end.min(128) + 1 - begin.max(1)).max(0)"),
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
        "DPSTR::STRING" => Some("(sigdig + 6).max(0)"),
        "DPSTRF::STRING" => Some("(sigdig + 6).max(0)"),
        "DSKD02::VALUES" => Some("room.max(0)"),
        "DSKGD::DSKDSC" => Some("inc::dla::DLADSZ"),
        "DSKI02::VALUES" => Some("room.max(0)"),
        "DSKMI2::SPAIXI" => Some("spxisz.max(0)"),
        "DSKP02::PLATES" => Some("room.max(0)"),
        "DSKV02::VRTCES" => Some("room.max(0)"),
        "DSKXSI::DLADSC" => Some("inc::dla::DLADSZ"),
        "DSKXSI::DSKDSC" => Some("inc::dla::DLADSZ"),
        "DSKXSI::DC" => Some("inc::dsksrc::DCSIZE"),
        "DSKXSI::IC" => Some("inc::dsksrc::ICSIZE"),
        "DSKXV::XPTARR" => Some("nrays.max(0)"),
        "DSKXV::FNDARR" => Some("nrays.max(0)"),
        "DXTRCT::VALUES" => Some("maxwds.max(0)"),
        "EDTERM::TRMPTS" => Some("npts.max(0)"),
        "EKFIND::ERRMSG" => Some(ERRMSG),
        "EKIFLD::RCPTRS" => Some("nrows.max(0)"),
        "EKPSEL::XBEGS" => Some("inc::ekqlimit::MAXSEL"),
        "EKPSEL::XENDS" => Some("inc::ekqlimit::MAXSEL"),
        "EKPSEL::ERRMSG" => Some(ERRMSG),
        "EKTNAM::TABLE" => Some("inc::ektnamsz::TNAMSZ"),
        "EKCII::COLUMN" => Some("inc::ekattdsc::ADSCSZ"),
        "EKSRCH::ERRMSG" => Some(ERRMSG),
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
        "ET2UTC::UTCSTR" => Some("(24 + prec).max(0)"),
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
        "GETFOV::BOUNDS" => Some("room.max(0)"),
        "GETFVN::SHAPE" => Some("9"), // longest value is 'RECTANGLE'
        "GETFVN::FRAME" => Some(FRAME_NAME),
        "GETFVN::BOUNDS" => Some("room.max(0)"),
        "GETMSG::MSG" => Some("inc::errhnd::LMSGLN"),
        "GFDIST::WORK" => Some("((mw + 1 - LBCELL) * inc::gf::NWDIST).max(0)"),
        "GFEVNT::WORK" => Some("((mw + 1 - LBCELL) * nw).max(0)"),
        "GFILUM::WORK" => Some("((mw + 1 - LBCELL) * inc::gf::NWILUM).max(0)"),
        "GFPA::WORK" => Some("((mw + 1 - LBCELL) * inc::gf::NWPA).max(0)"),
        "GFPOSC::WORK" => Some("((mw + 1 - LBCELL) * inc::gf::NWMAX).max(0)"),
        "GFRR::WORK" => Some("((mw + 1 - LBCELL) * inc::gf::NWRR).max(0)"),
        "GFSEP::WORK" => Some("((mw + 1 - LBCELL) * inc::gf::NWSEP).max(0)"),
        "GFSNTC::WORK" => Some("((mw + 1 - LBCELL) * inc::gf::NWMAX).max(0)"),
        "GFSUBC::WORK" => Some("((mw + 1 - LBCELL) * inc::gf::NWMAX).max(0)"),
        "GFUDS::WORK" => Some("((mw + 1 - LBCELL) * nw).max(0)"),
        "HX2DP::ERRMSG" => Some(ERRMSG),
        "HX2INT::ERRMSG" => Some(ERRMSG),
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
        "LATSRF::SRFPTS" => Some("npts.max(0)"),
        "LIMBPT::NPTS" => Some("ncuts.max(0)"),
        "LIMBPT::POINTS" => Some("maxn.max(0)"),
        "LIMBPT::EPOCHS" => Some("maxn.max(0)"),
        "LIMBPT::TANGTS" => Some("maxn.max(0)"),
        "LJUCRS::OUTPUT" => Some("(input.len() as i32)"),
        "LUN2FN::FILNAM" => Some(FILSIZ),
        "MTXMG::MOUT" => Some("(nc1 * nc2).max(0)"),
        "MTXVG::VOUT" => Some("nc1.max(0)"),
        "MXMG::MOUT" => Some("(nr1 * nc2).max(0)"),
        "MXMTG::MOUT" => Some("(nr1 * nr2).max(0)"),
        "MXVG::VOUT" => Some("nr1.max(0)"),
        "NPARSD::ERROR" => Some(ERRMSG),
        "NPARSI::ERROR" => Some(ERRMSG),
        "OSCLTX::ELTS" => Some("inc::oscltx::OSCXSZ"),
        "PACKAD::OUT" => Some("maxout.max(0)"),
        "PACKAI::OUT" => Some("maxout.max(0)"),
        "PARSQS::VALUE" => Some("(string.len() as i32)"),
        "PARSQS::ERRMSG" => Some(ERRMSG),
        "PCKSFS::DESCR" => Some(PCK_DESCSZ),
        "PCKSFS::IDENT" => Some(PCK_IDSIZE),
        "PCKPDS::DESCR" => Some(PCK_DESCSZ),
        "PCKR02::RECORD" => Some("128"), // DAF record limit
        "PCKR03::RECORD" => Some("128"),
        "PCKR20::RECORD" => Some("128"),
        "POLYDS::P" => Some("(nderiv + 1).max(0)"),
        "GDPOOL::VALUES" => Some("room.max(0)"),
        "GIPOOL::IVALS" => Some("room.max(0)"),
        "DTPOOL::TYPE" => Some("1"),
        "PRTENC::STRING" => Some("5"), // MINLEN
        "GETDEV::DEVICE" => Some(FILSIZ),
        "GETLMS::MSG" => Some("inc::errhnd::LMSGLN"),
        "GETSMS::MSG" => Some("inc::errhnd::SMSGLN"),
        "QDERIV::DFDT" => Some("ndim.max(0)"),
        "RDENCD::DATA" => Some("n.max(0)"),
        "RDENCI::DATA" => Some("n.max(0)"),
        "RDKDAT::LINE" => Some("80"), // LINLEN in rdkvar.f
        "RDKLIN::KERNEL" => Some(FILSIZ),
        "RDKVAR::NAME" => Some("80"), // LINLEN in rdkvar.f
        "RDNBL::LINE" => None,        // ckbrief uses 256; no obvious max
        "RDTEXT::LINE" => None,
        "READLN::LINE" => None,
        "SCFM01::CLKSTR" => Some("256"), // spacecraft-dependent; this is arbitrary but should be plenty
        "SCANPR::MRKLEN" => Some("(*nmarks).max(0)"),
        "SCANPR::PNTERS" => Some("132"), // worst-case, "anything might be a mark"
        "SCAN::IDENT" => Some("room.max(0)"),
        "SCAN::BEG" => Some("room.max(0)"),
        "SCAN::END" => Some("room.max(0)"),
        "SCARDD::CELL" => None,
        "SCARDI::CELL" => None,
        "SCDECD::SCLKCH" => Some("256"), // see SCFM01::CLKSTR
        "SCE2S::SCLKCH" => Some("256"),
        "SCFMT::CLKSTR" => Some("256"),
        "SCLI01::IVAL" => Some("maxnv.max(0)"),
        "SCLD01::DVAL" => Some("maxnv.max(0)"),
        "SCPARS::MSG" => Some(ERRMSG),
        "SCPART::PSTART" => Some("inc::sclk::MXPART"), // 9999, but we don't care about RAM
        "SCPART::PSTOP" => Some("inc::sclk::MXPART"),
        "SCPS01::MSG" => Some(ERRMSG),
        "SCID2N::CLKNAM" => Some("256"), // "short, human-readable string"
        "SDIFFD::C" => None,
        "SDIFFI::C" => None,
        "SEPOOL::STRING" => None, // can't tell the max
        "SGFCON::VALUES" => Some("(last + 1 - first).max(0)"),
        "SGFPKT::VALUES" => Some("(last + 1 - first).max(0)"),
        "SGFPKT::ENDS" => Some("(last + 1 - first).max(0)"),
        "SGFREF::VALUES" => Some("(last + 1 - first).max(0)"),
        "SIGDGT::OUT" => Some("(in_.len() as i32)"),
        "SPCRFL::LINE" => Some("80"), // spc.req
        "SPCRNL::LINE" => Some("80"),
        "SPKSFS::DESCR" => Some(SPK_DSCSIZ),
        "SPKSFS::IDENT" => Some(SPK_SIDLEN),
        "SPKPDS::DESCR" => Some(SPK_DSCSIZ),
        "SPKR01::RECORD" => Some("128"), // DAF record limit
        "SPKR02::RECORD" => Some("128"),
        "SPKR03::RECORD" => Some("128"),
        "SPKR05::RECORD" => Some("128"),
        "SPKR08::RECORD" => Some("128"),
        "SPKR09::RECORD" => Some("128"),
        "SPKR10::RECORD" => Some("128"),
        "SPKR12::RECORD" => Some("128"),
        "SPKR13::RECORD" => Some("128"),
        "SPKR14::RECORD" => Some("128"),
        "SPKR15::RECORD" => Some("128"),
        "SPKR17::RECORD" => Some("128"),
        "SPKR18::RECORD" => Some("128"),
        "SPKR19::RECORD" => Some("128"),
        "SPKR20::RECORD" => Some("128"),
        "SPKR21::RECORD" => Some("128"),
        "SRFC2S::SRFSTR" => Some("inc::srftrn::SFNMLN"),
        "SRFCSS::SRFSTR" => Some("inc::srftrn::SFNMLN"),
        "SRFNRM::NORMLS" => Some("npts.max(0)"),
        "SSIZED::CELL" => None,
        "SSIZEI::CELL" => None,
        "STCG01::SPTYPE" => Some("4"),
        "STCL01::TABNAM" => Some("inc::ektnamsz::TNAMSZ"),
        "STPOOL::NTHSTR" => None,
        "SYFETC::NAME" => Some("(tabsym.element_length() as i32)"),
        "SYFETD::NAME" => Some("(tabsym.element_length() as i32)"),
        "SYFETI::NAME" => Some("(tabsym.element_length() as i32)"),
        "SYGETD::VALUES" => Some("(tabval.len() as i32)"), // worst-case, if it matches every value
        "SYGETI::VALUES" => Some("(tabval.len() as i32)"),
        "SYNTHC::VALUE" => Some("(tabval.element_length() as i32)"),
        "SYPOPC::VALUE" => Some("(tabval.element_length() as i32)"),
        "SYSELD::VALUES" => Some("(end + 1 - begin).max(0)"),
        "SYSELI::VALUES" => Some("(end + 1 - begin).max(0)"),
        "TCHECK::ERROR" => Some(ERRMSG),
        "TCHCKD::TYPE" => Some("3"), // 'YES' or 'NO'
        "TERMPT::NPTS" => Some("ncuts.max(0)"),
        "TERMPT::POINTS" => Some("maxn.max(0)"),
        "TERMPT::EPOCHS" => Some("maxn.max(0)"),
        "TERMPT::TRMVCS" => Some("maxn.max(0)"),
        "TIMOUT::OUTPUT" => Some("(pictur.len() as i32 * 2)"), // probably worst expansion is 'Month'=>'September'
        "TKVRSN::VERSTR" => Some("80"),
        "TPARSE::ERRMSG" => Some(ERRMSG),
        "TPARTV::TVEC" => Some("6"),
        "TPARTV::TYPE" => Some("3"),          // 'YMD', 'YD', 'JD'
        "TPARTV::PICTUR" => Some("(64 * 5)"), // PICLEN in zztime.f
        "TPARTV::ERROR" => Some(ERRMSG),
        "TPICTR::PICTUR" => Some("(64 * 5)"),
        "TPICTR::ERRMSG" => Some(ERRMSG),
        "TRCNAM::NAME" => Some(FILSIZ),
        "QCKTRC::TRACE" => Some("1200"), // LNGSTR in tcase.f
        "UNIOND::C" => None,
        "UNIONI::C" => None,
        "UNORMG::VOUT" => Some("ndim.max(0)"),
        "VADDG::VOUT" => Some("ndim.max(0)"),
        "VEQUG::VOUT" => Some("ndim.max(0)"),
        "VHATG::VOUT" => Some("ndim.max(0)"),
        "VLCOMG::SUM" => Some("n.max(0)"),
        "VMINUG::VOUT" => Some("ndim.max(0)"),
        "VPROJG::P" => Some("ndim.max(0)"),
        "VSCLG::VOUT" => Some("ndim.max(0)"),
        "VSUBG::VOUT" => Some("ndim.max(0)"),
        "WNCOMD::RESULT" => None,
        "WNDIFD::C" => None,
        "WNINTD::C" => None,
        "WNUNID::C" => None,
        "XDDA::VOXLST" => Some("maxnvx.max(0)"),
        "XPOSBL::BTMAT" => Some("(nrow * ncol).max(0)"),
        "XPOSEG::XPOSEM" => Some("(nrow * ncol).max(0)"),
        _ => None,
    }
}

struct ArgBuilder {
    params: Vec<String>,
    args: Vec<String>,
    init: String,
    ret_ty: Vec<String>,
    ret_vals: Vec<String>,
    ret_names: Vec<String>,
    ret_found: bool,
}

impl ArgBuilder {
    fn new() -> Self {
        Self {
            params: vec![],
            args: vec![],
            init: String::new(),
            ret_ty: vec![],
            ret_vals: vec![],
            ret_names: vec![],
            ret_found: false,
        }
    }

    fn build(&mut self, entry: &Entry, raw: bool, arg_dirs: &IndexMap<&str, &str>) -> Result<()> {
        for darg in &entry.ast.dargs {
            let arg = emit_api_symbol(&entry.ast.loc, darg, &entry.codegen.syms)?;
            let ident = safe_identifier(&arg.name);

            if raw {
                self.params.push(arg.param);
                self.args.push(arg.arg);
            } else {
                if !self.add_return_arg(entry, arg_dirs, darg, &arg, &ident)? {
                    self.params.push(arg.param);
                    self.args.push(ident.clone());
                }
            }
        }

        Ok(())
    }

    fn add_return_arg(
        &mut self,
        entry: &Entry,
        arg_dirs: &IndexMap<&str, &str>,
        darg: &String,
        arg: &RawArg,
        ident: &String,
    ) -> Result<bool> {
        if !should_return_arg(&entry, &arg_dirs, darg)? {
            return Ok(false);
        }

        let sym = entry.codegen.syms.get(darg)?;
        if matches!(sym.ast.base_type, DataType::Character) {
            let len = match &sym.ast.character_len {
                Some(LenSpecification::Unspecified) => bail!("{darg} unspecified length"),
                Some(LenSpecification::Integer(n)) => format!("{n}"),
                Some(LenSpecification::IntConstantExpr(e)) => {
                    if let Some(n) = e.eval_constant(&entry.codegen.syms)? {
                        format!("{n}")
                    } else {
                        if let Some(n) = output_array_size(&entry.ast.name, darg) {
                            n.to_owned()
                        } else {
                            return Ok(false);
                        }
                    }
                }
                _ => {
                    if let Some(n) = output_array_size(&entry.ast.name, darg) {
                        n.to_owned()
                    } else {
                        return Ok(false);
                    }
                }
            };

            writeln!(self.init, "  let mut {ident} = blank({len});")?;

            self.ret_ty.push("String".to_owned());
            self.ret_vals.push(format!("trim({ident})"));
            self.ret_names.push(arg.name.clone());
        } else {
            if arg.ret_ty.starts_with("Vec<") {
                let n = if let Some(n) = output_array_size(&entry.ast.name, darg) {
                    n.to_owned()
                } else {
                    return Ok(false);
                };

                writeln!(
                    self.init,
                    "  let mut {ident}: {} = vec![Default::default(); {n} as usize];",
                    arg.ret_ty
                )?;
            } else {
                writeln!(
                    self.init,
                    "  let mut {ident}: {} = Default::default();",
                    arg.ret_ty
                )?;
            }

            if darg == "FOUND" {
                self.ret_found = true;
            } else {
                self.ret_ty.push(arg.ret_ty.clone());
                self.ret_vals.push(ident.clone());
                self.ret_names.push(arg.name.clone());
            }
        }
        self.args.push(format!("&mut {ident}"));

        Ok(true)
    }
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

        let mut args = ArgBuilder::new();
        args.build(entry, raw, &arg_dirs)?;

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
            if !args.ret_names.is_empty() {
                let mut r = args.ret_names.join(", ");

                if args.ret_names.len() > 1 {
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
                args.params
                    .insert(0, "ctx: &mut SpiceContext<'a>".to_owned());
                args.args.push("ctx.raw_context()".to_owned());
            }
        } else {
            if requires_ctx {
                args.params.insert(0, "&mut self".to_owned());
                args.args.insert(0, "self".to_owned());
            } else {
                args.params.insert(0, "&self".to_owned());
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
            let mut ret = args.ret_ty.join(", ");
            if args.ret_ty.len() != 1 {
                ret = format!("({ret})");
            }
            if args.ret_found {
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
            params = args.params.join(", ")
        )?;
        code += &args.init;
        if raw {
            let call = &format!("{entry_name}({args})", args = args.args.join(", "));
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
            let call = &format!(
                "raw::{}({})",
                safe_identifier(fn_name),
                args.args.join(", ")
            );
            if matches!(ret_type, DataType::Void | DataType::Character) {
                let mut ret = args.ret_vals.join(", ");
                if args.ret_vals.len() != 1 {
                    ret = format!("({ret})");
                }
                if args.ret_found {
                    ret = format!("if found {{ Some({ret}) }} else {{ None }}");
                }
                if returns_result {
                    writeln!(code, "  {call}?;")?;
                    writeln!(code, "  Ok({ret})")?;
                } else {
                    writeln!(code, "  {call};")?;
                    if ret != "()" {
                        writeln!(code, "  {ret}")?;
                    }
                }
            } else {
                if !args.ret_vals.is_empty() {
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
