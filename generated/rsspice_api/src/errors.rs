#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("internal error {0}")]
    InternalError(#[from] f2rust_std::Error),

    #[error("Unknown SPICELIB error: {short}")]
    UNKNOWN { short: String, long: String },

    // perl -ne'if (/b"SPICE\((.*?)\)/) { $e = $1 } if (/EXPL, b"(.*)"/) { print qq{#[error("$1")]\n$e(String),\n} }' expln.rs
    #[error("Invalid Endpoints--Left Endpoint Exceeds Right Endpoint")]
    BADENDPOINTS(String),
    #[error("Version Identification of GEF File is Invalid")]
    BADGEFVERSION(String),
    #[error("A blank string was used as a module name")]
    BLANKMODULENAME(String),
    #[error("This Entry Point Contains No Executable Code")]
    BOGUSENTRY(String),
    #[error("Cardinality of Output Cell is Too Small")]
    CELLTOOSMALL(String),
    #[error("Error Writing to Ephemeris File")]
    CLUSTERWRITEERROR(String),
    #[error("Unrecognized Data Type Specification was Encountered")]
    DATATYPENOTRECOG(String),
    #[error("The Value in the Kernel File was Expected to be a date.")]
    DATEEXPECTED(String),
    #[error("Name of Device Exceeds 128-Character Limit")]
    DEVICENAMETOOLONG(String),
    #[error("Invalid embedded blank was found in character string")]
    EMBEDDEDBLANK(String),
    #[error("File Open Failed Because the File was Already Open")]
    FILEALREADYOPEN(String),
    #[error("An Attempt to Open a File Failed")]
    FILEOPENFAILED(String),
    #[error("An Attempt to Read a File Failed")]
    FILEREADFAILED(String),
    #[error("An Attempt to Write a File Failed")]
    FILEWRITEFAILED(String),
    #[error("The Input and Output Units are Incompatible")]
    INCOMPATIBLEUNITS(String),
    #[error("An Invalid Action Value Was Supplied")]
    INVALIDACTION(String),
    #[error("An Invalid Function Argument was Supplied")]
    INVALIDARGUMENT(String),
    #[error("Checkout Was Attempted When No Routines Were Checked In")]
    INVALIDCHECKOUT(String),
    #[error("Invalid Cluster Number -- Cluster Numbers Must Exceed 1 ")]
    INVALIDCLUSTERNUM(String),
    #[error("An Invalid Epoch Type Specification Was Supplied")]
    INVALIDEPOCH(String),
    #[error("There Is No Element Corresponding to the Supplied Index")]
    INVALIDINDEX(String),
    #[error("Time String Could Not Be Parsed")]
    INVALIDTIMESTRING(String),
    #[error("An Invalid Item Was Found in a List")]
    INVALIDLISTITEM(String),
    #[error("An Invalid Error Message Type Was Specified")]
    INVALIDMSGTYPE(String),
    #[error("An Invalid Operation Value Was Supplied")]
    INVALIDOPERATION(String),
    #[error("An Invalid Option Value Was Supplied")]
    INVALIDOPTION(String),
    #[error("Specification of Time String Format Was Not Recognized")]
    INVALIDTIMEFORMAT(String),
    #[error("The Variable Was not Found in the Kernel Pool.")]
    KERNELVARNOTFOUND(String),
    #[error("No Further Symbols Can be Inserted; the Name Table is Full")]
    NAMETABLEFULL(String),
    #[error("No More Logical Units are Available for Allocation")]
    NOFREELOGICALUNIT(String),
    #[error("Window Does Not Contain Interval Corresponding to the Supplied Index")]
    NOINTERVAL(String),
    #[error("No Applicable Segment Found in Ephemeris File")]
    NOSEGMENT(String),
    #[error("The Symbol Does Not Exist in the Symbol Table")]
    NOSUCHSYMBOL(String),
    #[error("The Elements Must Be Distinct")]
    NOTDISTINCT(String),
    #[error("The Value in the Kernel File was Expected to be a Number.")]
    NUMBEREXPECTED(String),
    #[error("No Further Symbols Can be Inserted; the Pointer Table is Full")]
    POINTERTABLEFULL(String),
    #[error("A Reference Frame Specification was Not Recognized")]
    REFNOTREC(String),
    #[error("Cardinality of Set Is Too Small to Contain Result of the Requested Operation")]
    SETEXCESS(String),
    #[error("The SPICELIB Limit for Number of Open Files Has Already Been Reached")]
    TOOMANYFILESOPEN(String),
    #[error("No More Entries Can Be Added to the Traceback Representation")]
    TRACEBACKOVERFLOW(String),
    #[error("The Input or Output Units Were Not Recognized")]
    UNITSNOTREC(String),
    #[error("Window Does Not Have an Even Number of Endpoints")]
    UNMATCHENDPTS(String),
    #[error("No Further Symbols Can be Inserted; the Value Table is Full")]
    VALUETABLEFULL(String),
    #[error("Cardinality of Window Is Too Small to Contain Result of the Requested Operation")]
    WINDOWEXCESS(String),
    #[error("Cardinality of Output Window is Too Small")]
    WINDOWTOOSMALL(String),
    #[error("An Attempt to write to a specified unit failed.")]
    WRITEERROR(String),
    #[error("Invalid Radius--Equatorial or Polar Radius is Zero")]
    ZERORADIUS(String),
    #[error("Input Vector is the Zero Vector")]
    ZEROVECTOR(String),
    #[error("Input Axis Length is Zero")]
    ZEROAXISLENGTH(String),
}

impl Error {
    pub(crate) fn from_short(short: &str, long: &str) -> Self {
        let long = long.to_owned();

        // perl -ne'if (/b"SPICE\((.*?)\)/) { print qq{"SPICE($1)" => Error::$1(long),\n} }' expln.rs
        match short {
            "SPICE(BADENDPOINTS)" => Error::BADENDPOINTS(long),
            "SPICE(BADGEFVERSION)" => Error::BADGEFVERSION(long),
            "SPICE(BLANKMODULENAME)" => Error::BLANKMODULENAME(long),
            "SPICE(BOGUSENTRY)" => Error::BOGUSENTRY(long),
            "SPICE(CELLTOOSMALL)" => Error::CELLTOOSMALL(long),
            "SPICE(CLUSTERWRITEERROR)" => Error::CLUSTERWRITEERROR(long),
            "SPICE(DATATYPENOTRECOG)" => Error::DATATYPENOTRECOG(long),
            "SPICE(DATEEXPECTED)" => Error::DATEEXPECTED(long),
            "SPICE(DEVICENAMETOOLONG)" => Error::DEVICENAMETOOLONG(long),
            "SPICE(EMBEDDEDBLANK)" => Error::EMBEDDEDBLANK(long),
            "SPICE(FILEALREADYOPEN)" => Error::FILEALREADYOPEN(long),
            "SPICE(FILEOPENFAILED)" => Error::FILEOPENFAILED(long),
            "SPICE(FILEREADFAILED)" => Error::FILEREADFAILED(long),
            "SPICE(FILEWRITEFAILED)" => Error::FILEWRITEFAILED(long),
            "SPICE(INCOMPATIBLEUNITS)" => Error::INCOMPATIBLEUNITS(long),
            "SPICE(INVALIDACTION)" => Error::INVALIDACTION(long),
            "SPICE(INVALIDARGUMENT)" => Error::INVALIDARGUMENT(long),
            "SPICE(INVALIDCHECKOUT)" => Error::INVALIDCHECKOUT(long),
            "SPICE(INVALIDCLUSTERNUM)" => Error::INVALIDCLUSTERNUM(long),
            "SPICE(INVALIDEPOCH)" => Error::INVALIDEPOCH(long),
            "SPICE(INVALIDINDEX)" => Error::INVALIDINDEX(long),
            "SPICE(INVALIDTIMESTRING)" => Error::INVALIDTIMESTRING(long),
            "SPICE(INVALIDLISTITEM)" => Error::INVALIDLISTITEM(long),
            "SPICE(INVALIDMSGTYPE)" => Error::INVALIDMSGTYPE(long),
            "SPICE(INVALIDOPERATION)" => Error::INVALIDOPERATION(long),
            "SPICE(INVALIDOPTION)" => Error::INVALIDOPTION(long),
            "SPICE(INVALIDTIMEFORMAT)" => Error::INVALIDTIMEFORMAT(long),
            "SPICE(KERNELVARNOTFOUND)" => Error::KERNELVARNOTFOUND(long),
            "SPICE(NAMETABLEFULL)" => Error::NAMETABLEFULL(long),
            "SPICE(NOFREELOGICALUNIT)" => Error::NOFREELOGICALUNIT(long),
            "SPICE(NOINTERVAL)" => Error::NOINTERVAL(long),
            "SPICE(NOSEGMENT)" => Error::NOSEGMENT(long),
            "SPICE(NOSUCHSYMBOL)" => Error::NOSUCHSYMBOL(long),
            "SPICE(NOTDISTINCT)" => Error::NOTDISTINCT(long),
            "SPICE(NUMBEREXPECTED)" => Error::NUMBEREXPECTED(long),
            "SPICE(POINTERTABLEFULL)" => Error::POINTERTABLEFULL(long),
            "SPICE(REFNOTREC)" => Error::REFNOTREC(long),
            "SPICE(SETEXCESS)" => Error::SETEXCESS(long),
            "SPICE(TOOMANYFILESOPEN)" => Error::TOOMANYFILESOPEN(long),
            "SPICE(TRACEBACKOVERFLOW)" => Error::TRACEBACKOVERFLOW(long),
            "SPICE(UNITSNOTREC)" => Error::UNITSNOTREC(long),
            "SPICE(UNMATCHENDPTS)" => Error::UNMATCHENDPTS(long),
            "SPICE(VALUETABLEFULL)" => Error::VALUETABLEFULL(long),
            "SPICE(WINDOWEXCESS)" => Error::WINDOWEXCESS(long),
            "SPICE(WINDOWTOOSMALL)" => Error::WINDOWTOOSMALL(long),
            "SPICE(WRITEERROR)" => Error::WRITEERROR(long),
            "SPICE(ZERORADIUS)" => Error::ZERORADIUS(long),
            "SPICE(ZEROVECTOR)" => Error::ZEROVECTOR(long),
            "SPICE(ZEROAXISLENGTH)" => Error::ZEROAXISLENGTH(long),
            _ => Error::UNKNOWN {
                short: short.to_owned(),
                long,
            },
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;
