{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{        Delphi plain interface to ntwdblib.dll          }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZLibMsSql;

interface

uses Windows, Classes, ZSqlTypes;

{$INCLUDE ..\Zeos.inc}

{***************** Plain API Constants definition ****************}

const
 DEFAULT_DLL_LOCATION = 'ntwdblib.dll';

{ General  #define }
  TIMEOUT_IGNORE        = Cardinal(-1);
  TIMEOUT_INFINITE      = 0;
  TIMEOUT_MAXIMUM       = 1200; { 20 minutes maximum timeout value }

{ Used for ServerType in dbgetprocinfo }
  SERVTYPE_UNKNOWN      = 0;
  SERVTYPE_MICROSOFT    = 1;

{ Used by dbcolinfo }
{enum CI_TYPES }
  CI_REGULAR            = 1;
  CI_ALTERNATE          = 2;
  CI_CURSOR             = 3;

{ Bulk Copy Definitions (bcp) }
  DB_IN	                = 1;  { Transfer from client to server }
  DB_OUT	        = 2;  { Transfer from server to client }

  BCPMAXERRS            = 1;  { bcp_control parameter }
  BCPFIRST              = 2;  { bcp_control parameter }
  BCPLAST               = 3;  { bcp_control parameter }
  BCPBATCH              = 4;  { bcp_control parameter }
  BCPKEEPNULLS          = 5;  { bcp_control parameter }
  BCPABORT              = 6;  { bcp_control parameter }

  TINYBIND              = 1;
  SMALLBIND             = 2;
  INTBIND               = 3;
  CHARBIND              = 4;
  BINARYBIND            = 5;
  BITBIND               = 6;
  DATETIMEBIND          = 7;
  MONEYBIND             = 8;
  FLT8BIND              = 9;
  STRINGBIND            = 10;
  NTBSTRINGBIND         = 11;
  VARYCHARBIND          = 12;
  VARYBINBIND           = 13;
  FLT4BIND              = 14;
  SMALLMONEYBIND        = 15;
  SMALLDATETIBIND       = 16;
  DECIMALBIND           = 17;
  NUMERICBIND           = 18;
  SRCDECIMALBIND        = 19;
  SRCNUMERICBIND        = 20;
  MAXBIND               = SRCNUMERICBIND;

  DBSAVE                = 1;
  DBNOSAVE              = 0;

  DBNOERR               = -1;
  DBFAIL                = 0;
  DBSUCCEED             = 1;
  DBFINDONE             = $04;  { Definately done }
  DBMORE                = $10;  { Maybe more commands waiting }
  DBMORE_ROWS           = $20;  { This command returned rows }

  MAXNAME               = 31;
  DBTXTSLEN             = 8;     { Timestamp length }
  DBTXPLEN              = 16;    { Text pointer length }

{ Error code returns }
  INT_EXIT              = 0;
  INT_CONTINUE          = 1;
  INT_CANCEL            = 2;

{ dboptions }
  DBBUFFER              = 0;
  DBOFFSET              = 1;
  DBROWCOUNT            = 2;
  DBSTAT                = 3;
  DBTEXTLIMIT           = 4;
  DBTEXTSIZE            = 5;
  DBARITHABORT          = 6;
  DBARITHIGNORE         = 7;
  DBNOAUTOFREE          = 8;
  DBNOCOUNT             = 9;
  DBNOEXEC              = 10;
  DBPARSEONLY           = 11;
  DBSHOWPLAN            = 12;
  DBSTORPROCID		= 13;
  DBANSITOOEM		= 14;
  DBOEMTOANSI	        = 15;
  DBCLIENTCURSORS       = 16;
  DBSET_TIME            = 17;
  DBQUOTEDIDENT         = 18;

{ Data Type Tokens }
  SQLVOID               = $1f;
  SQLTEXT               = $23;
  SQLVARBINARY          = $25;
  SQLINTN               = $26;
  SQLVARCHAR            = $27;
  SQLBINARY             = $2d;
  SQLIMAGE              = $22;
  SQLCHAR               = $2f;
  SQLINT1               = $30;
  SQLBIT                = $32;
  SQLINT2               = $34;
  SQLINT4               = $38;
  SQLMONEY              = $3c;
  SQLDATETIME           = $3d;
  SQLFLT8               = $3e;
  SQLFLTN               = $6d;
  SQLMONEYN             = $6e;
  SQLDATETIMN           = $6f;
  SQLFLT4               = $3b;
  SQLMONEY4             = $7a;
  SQLDATETIM4           = $3a;
  SQLDECIMAL            = $6a;
  SQLNUMERIC            = $6c;

{ Data stream tokens }
  SQLCOLFMT             = $a1;
  OLD_SQLCOLFMT         = $2a;
  SQLPROCID             = $7c;
  SQLCOLNAME            = $a0;
  SQLTABNAME            = $a4;
  SQLCOLINFO            = $a5;
  SQLALTNAME            = $a7;
  SQLALTFMT             = $a8;
  SQLERROR              = $aa;
  SQLINFO               = $ab;
  SQLRETURNVALUE        = $ac;
  SQLRETURNSTATUS       = $79;
  SQLRETURN             = $db;
  SQLCONTROL            = $ae;
  SQLALTCONTROL         = $af;
  SQLROW                = $d1;
  SQLALTROW             = $d3;
  SQLDONE               = $fd;
  SQLDONEPROC           = $fe;
  SQLDONEINPROC         = $ff;
  SQLOFFSET             = $78;
  SQLORDER              = $a9;
  SQLLOGINACK           = $ad; { NOTICE: change to real value }

{ Ag op tokens }
  SQLAOPCNT		= $4b;
  SQLAOPSUM             = $4d;
  SQLAOPAVG             = $4f;
  SQLAOPMIN             = $51;
  SQLAOPMAX             = $52;
  SQLAOPANY             = $53;
  SQLAOPNOOP            = $56;

{ Error numbers (dberrs) DB-Library error codes }
  SQLEMEM               = 10000;
  SQLENULL              = 10001;
  SQLENLOG              = 10002;
  SQLEPWD               = 10003;
  SQLECONN              = 10004;
  SQLEDDNE              = 10005;
  SQLENULLO             = 10006;
  SQLESMSG              = 10007;
  SQLEBTOK              = 10008;
  SQLENSPE              = 10009;
  SQLEREAD              = 10010;
  SQLECNOR              = 10011;
  SQLETSIT              = 10012;
  SQLEPARM              = 10013;
  SQLEAUTN              = 10014;
  SQLECOFL              = 10015;
  SQLERDCN              = 10016;
  SQLEICN               = 10017;
  SQLECLOS              = 10018;
  SQLENTXT              = 10019;
  SQLEDNTI              = 10020;
  SQLETMTD              = 10021;
  SQLEASEC              = 10022;
  SQLENTLL              = 10023;
  SQLETIME              = 10024;
  SQLEWRIT              = 10025;
  SQLEMODE              = 10026;
  SQLEOOB               = 10027;
  SQLEITIM              = 10028;
  SQLEDBPS              = 10029;
  SQLEIOPT              = 10030;
  SQLEASNL              = 10031;
  SQLEASUL              = 10032;
  SQLENPRM              = 10033;
  SQLEDBOP              = 10034;
  SQLENSIP              = 10035;
  SQLECNULL             = 10036;
  SQLESEOF              = 10037;
  SQLERPND              = 10038;
  SQLECSYN              = 10039;
  SQLENONET             = 10040;
  SQLEBTYP              = 10041;
  SQLEABNC              = 10042;
  SQLEABMT              = 10043;
  SQLEABNP              = 10044;
  SQLEBNCR              = 10045;
  SQLEAAMT              = 10046;
  SQLENXID              = 10047;
  SQLEIFNB              = 10048;
  SQLEKBCO              = 10049;
  SQLEBBCI              = 10050;
  SQLEKBCI              = 10051;
  SQLEBCWE              = 10052;
  SQLEBCNN              = 10053;
  SQLEBCOR              = 10054;
  SQLEBCPI              = 10055;
  SQLEBCPN              = 10056;
  SQLEBCPB              = 10057;
  SQLEVDPT              = 10058;
  SQLEBIVI              = 10059;
  SQLEBCBC              = 10060;
  SQLEBCFO              = 10061;
  SQLEBCVH              = 10062;
  SQLEBCUO              = 10063;
  SQLEBUOE              = 10064;
  SQLEBWEF              = 10065;
  SQLEBTMT              = 10066;
  SQLEBEOF              = 10067;
  SQLEBCSI              = 10068;
  SQLEPNUL              = 10069;
  SQLEBSKERR            = 10070;
  SQLEBDIO              = 10071;
  SQLEBCNT              = 10072;
  SQLEMDBP              = 10073;
  SQLINIT               = 10074;
  SQLCRSINV             = 10075;
  SQLCRSCMD             = 10076;
  SQLCRSNOIND           = 10077;
  SQLCRSDIS             = 10078;
  SQLCRSAGR             = 10079;
  SQLCRSORD             = 10080;
  SQLCRSMEM             = 10081;
  SQLCRSBSKEY           = 10082;
  SQLCRSNORES           = 10083;
  SQLCRSVIEW            = 10084;
  SQLCRSBUFR            = 10085;
  SQLCRSFROWN           = 10086;
  SQLCRSBROL            = 10087;
  SQLCRSFRAND           = 10088;
  SQLCRSFLAST           = 10089;
  SQLCRSRO              = 10090;
  SQLCRSTAB             = 10091;
  SQLCRSUPDTAB          = 10092;
  SQLCRSUPDNB           = 10093;
  SQLCRSVIIND           = 10094;
  SQLCRSNOUPD           = 10095;
  SQLCRSOS2             = 10096;
  SQLEBCSA              = 10097;
  SQLEBCRO              = 10098;
  SQLEBCNE              = 10099;
  SQLEBCSK              = 10100;
  SQLEUVBF              = 10101;
  SQLEBIHC              = 10102;
  SQLEBWFF              = 10103;
  SQLNUMVAL             = 10104;
  SQLEOLDVR             = 10105;
  SQLEBCPS	        = 10106;
  SQLEDTC 	        = 10107;
  SQLENOTIMPL	        = 10108;
  SQLENONFLOAT	        = 10109;
  SQLECONNFB            = 10110;

{ The severity levels are defined here }
  EXINFO                = 1;  { Informational, non-error }
  EXUSER                = 2;  { User error }
  EXNONFATAL            = 3;  { Non-fatal error }
  EXCONVERSION          = 4;  { Error in DB-LIBRARY data conversion }
  EXSERVER              = 5;  { The Server has returned an error flag }
  EXTIME                = 6;  { We have exceeded our timeout period while }
                           { waiting for a response from the Server - the }
                           { DBPROCESS is still alive }
  EXPROGRAM             = 7;  { Coding error in user program }
  EXRESOURCE            = 8;  { Running out of resources - the DBPROCESS may be dead }
  EXCOMM                = 9;  { Failure in communication with Server - the DBPROCESS is dead }
  EXFATAL               = 10; { Fatal error - the DBPROCESS is dead }
  EXCONSISTENCY         = 11; { Internal software error  - notify MS Technical Supprt }

{ Offset identifiers }
  OFF_SELECT            = $16d;
  OFF_FROM              = $14f;
  OFF_ORDER             = $165;
  OFF_COMPUTE           = $139;
  OFF_TABLE             = $173;
  OFF_PROCEDURE         = $16a;
  OFF_STATEMENT         = $1cb;
  OFF_PARAM             = $1c4;
  OFF_EXEC              = $12c;

{ Decimal constants }
  MAXNUMERICLEN = 16;
  MAXNUMERICDIG = 38;

  DEFAULTPRECISION = 18;
  DEFAULTSCALE     = 0;

{ Print lengths for certain fixed length data types }
  PRINT4                = 11;
  PRINT2                = 6;
  PRINT1                = 3;
  PRFLT8                = 20;
  PRMONEY               = 26;
  PRBIT                 = 3;
  PRDATETIME            = 27;
  PRDECIMAL             = (MAXNUMERICDIG + 2);
  PRNUMERIC             = (MAXNUMERICDIG + 2);

  SUCCEED               = 1;
  FAIL                  = 0;
  SUCCEED_ABORT         = 2;

  DBUNKNOWN             = 2;

  MORE_ROWS             = -1;
  NO_MORE_ROWS          = -2;
  REG_ROW               = MORE_ROWS;
  BUF_FULL              = -3;

{ Status code for dbresults(). Possible return values are }
{ SUCCEED, FAIL, and NO_MORE_RESULTS. }
  NO_MORE_RESULTS       = 2;
  NO_MORE_RPC_RESULTS   = 3;

{ Macros for dbsetlname() }
  DBSETHOST             = 1;
  DBSETUSER             = 2;
  DBSETPWD              = 3;
  DBSETAPP              = 4;
  DBSETID               = 5;
  DBSETLANG             = 6;

  DBSETSECURE           = 7;
  DBVER42               = 8;
  DBVER60               = 9;
  DBSET_LOGIN_TIME      = 10;
  DBSETFALLBACK         = 12;

{ Standard exit and error values }
  STDEXIT               = 0;
  ERREXIT               = -1;

{ dbrpcinit flags }
  DBRPCRECOMPILE        = $0001;
  DBRPCRESET            = $0004;
  DBRPCCURSOR           = $0008;

{ dbrpcparam flags }
  DBRPCRETURN           = $1;
  DBRPCDEFAULT          = $2;

{ Cursor related constants }

{ Following flags are used in the concuropt parameter in the dbcursoropen function }
  CUR_READONLY          = 1; { Read only cursor, no data modifications }
  CUR_LOCKCC            = 2; { Intent to update, all fetched data locked when }
                       { dbcursorfetch is called inside a transaction block }
  CUR_OPTCC             = 3; { Optimistic concurrency control, data modifications }
                       { succeed only if the row hasn't been updated since }
                       { the last fetch. }
  CUR_OPTCCVAL          = 4; { Optimistic concurrency control based on selected column values }

{ Following flags are used in the scrollopt parameter in dbcursoropen }
  CUR_FORWARD           = 0;   { Forward only scrolling }
  CUR_KEYSET            = -1;  { Keyset driven scrolling }
  CUR_DYNAMIC           = 1;   { Fully dynamic }
  CUR_INSENSITIVE       = -2;  { Server-side cursors only }

{ Following flags define the fetchtype in the dbcursorfetch function }
  FETCH_FIRST           = 1;  { Fetch first n rows }
  FETCH_NEXT            = 2;  { Fetch next n rows }
  FETCH_PREV            = 3;  { Fetch previous n rows }
  FETCH_RANDOM          = 4;  { Fetch n rows beginning with given row # }
  FETCH_RELATIVE        = 5;  { Fetch relative to previous fetch row # }
  FETCH_LAST            = 6;  { Fetch the last n rows }

{ Following flags define the per row status as filled by dbcursorfetch and/or dbcursorfetchex }
  FTC_EMPTY             = $00;  { No row available }
  FTC_SUCCEED           = $01;  { Fetch succeeded, (failed if not set) }
  FTC_MISSING           = $02;  { The row is missing }
  FTC_ENDOFKEYSET       = $04;  { End of the keyset reached }
  FTC_ENDOFRESULTS      = $08;  { End of results set reached }

{ Following flags define the operator types for the dbcursor function }
  CRS_UPDATE            = 1;  { Update operation }
  CRS_DELETE            = 2;  { Delete operation }
  CRS_INSERT            = 3;  { Insert operation }
  CRS_REFRESH           = 4;  { Refetch given row }
  CRS_LOCKCC            = 5;  { Lock given row }

{ Following value can be passed to the dbcursorbind function for NOBIND type }
  NOBIND                = -2; { Return length and pointer to data }

{ Following are values used by DBCURSORINFO's Type parameter }
  CU_CLIENT             = $00000001;
  CU_SERVER             = $00000002;
  CU_KEYSET             = $00000004;
  CU_MIXED              = $00000008;
  CU_DYNAMIC            = $00000010;
  CU_FORWARD            = $00000020;
  CU_INSENSITIVE        = $00000040;
  CU_READONLY           = $00000080;
  CU_LOCKCC             = $00000100;
  CU_OPTCC              = $00000200;
  CU_OPTCCVAL           = $00000400;

{ Following are values used by DBCURSORINFO's Status parameter }
  CU_FILLING            = $00000001;
  CU_FILLED             = $00000002;

{ Following are values used by dbupdatetext's type parameter }
  UT_TEXTPTR            = $0001;
  UT_TEXT               = $0002;
  UT_MORETEXT           = $0004;
  UT_DELETEONLY         = $0008;
  UT_LOG                = $0010;

{ The following values are passed to dbserverenum for searching criteria. }
  NET_SEARCH            = $0001;
  LOC_SEARCH            = $0002;

{ These constants are the possible return values from dbserverenum. }
  ENUM_SUCCESS          = $0000;
  MORE_DATA             = $0001;
  NET_NOT_AVAIL         = $0002;
  OUT_OF_MEMORY         = $0004;
  NOT_SUPPORTED         = $0008;
  ENUM_INVALID_PARAM    = $0010;

{ Netlib Error problem codes.  ConnectionError() should return one of }
{ these as the dblib-mapped problem code, so the corresponding string }
{ is sent to the dblib app's error handler as dberrstr.  Return NE_E_NOMAP }
{ for a generic DB-Library error string (as in prior versions of dblib). }

  NE_E_NOMAP            = 0;   { No string; uses dblib default. }
  NE_E_NOMEMORY         = 1;   { Insufficient memory. }
  NE_E_NOACCESS         = 2;   { Access denied. }
  NE_E_CONNBUSY         = 3;   { Connection is busy. }
  NE_E_CONNBROKEN       = 4;   { Connection broken. }
  NE_E_TOOMANYCONN      = 5;   { Connection limit exceeded. }
  NE_E_SERVERNOTFOUND   = 6;   { Specified SQL server not found. }
  NE_E_NETNOTSTARTED    = 7;   { The network has not been started. }
  NE_E_NORESOURCE       = 8;   { Insufficient network resources. }
  NE_E_NETBUSY          = 9;   { Network is busy. }
  NE_E_NONETACCESS      = 10;  { Network access denied. }
  NE_E_GENERAL          = 11;  { General network error.  Check your documentation. }
  NE_E_CONNMODE         = 12;  { Incorrect connection mode. }
  NE_E_NAMENOTFOUND     = 13;  { Name not found in directory service. }
  NE_E_INVALIDCONN      = 14;  { Invalid connection. }
  NE_E_NETDATAERR       = 15;  { Error reading or writing network data. }
  NE_E_TOOMANYFILES     = 16;  { Too many open file handles. }
  NE_E_CANTCONNECT	= 17;  { SQL Server does not exist or access denied. }

  NE_MAX_NETERROR       = 17;

{****************** Plain API Types definition *****************}

type
{ DBPROCESS, LOGINREC and DBCURSOR }
  PDBPROCESS            = Pointer;
  PLOGINREC             = Pointer;
  PDBCURSOR             = Pointer;
  PDBHANDLE             = Pointer;

//typedef int (SQLAPI *SQLFARPROC)();

//typedef       CHAR PTR LPSTR;
//typedef       BYTE PTR LPBYTE;
//typedef       void PTR LPVOID;
//typedef const CHAR PTR LPCSTR;

//typedef int BOOL;

{ DB-Library datatype definitions }
const
  DBMAXCHAR             = 256; { Max length of DBVARBINARY and DBVARCHAR, etc. }

type
  RETCODE               = Integer;
  STATUS                = Integer;

{ DB-Library datatypes }
  DBCHAR                = Char;
  DBBINARY              = Byte;
  DBTINYINT             = Byte;
  DBSMALLINT            = SmallInt;
  DBUSMALLINT           = Word;
  DBINT                 = LongInt;
  DBFLT8                = Double;
  DBBIT                 = Byte;
  DBBOOL                = Byte;
  DBFLT4                = Single;
  DBMONEY4              = LongInt;

  DBREAL                = DBFLT4;
  DBUBOOL               = Cardinal;

  DBDATETIM4 = packed record
    numdays:    Word;        { No of days since Jan-1-1900 }
    nummins:    Word;        { No. of minutes since midnight }
  end;
  PDBDATETIM4 = ^DBDATETIM4;

  DBVARYCHAR = packed record
    Len:        DBSMALLINT;
    Str:        array[0..DBMAXCHAR-1] of DBCHAR;
  end;

  DBVARYBIN = packed record
    Len:        DBSMALLINT;
    Bytes:	array[0..DBMAXCHAR-1] of Byte;
  end;

  DBMONEY = packed record
    mnyhigh:    DBINT;
    mnylow:     Cardinal;
  end;

  DBDATETIME = packed record
    dtdays:	DBINT;
    dttime:	Cardinal;
  end;
  PDBDATETIME = ^DBDATETIME;

{ DBDATEREC structure used by dbdatecrack }
  DBDATEREC = packed record
    year:       Integer;      { 1753 - 9999 }
    quarter:    Integer;      { 1 - 4 }
    month:      Integer;      { 1 - 12 }
    dayofyear:  Integer;      { 1 - 366 }
    day:        Integer;      { 1 - 31 }
    week:       Integer;      { 1 - 54 (for leap years) }
    weekday:    Integer;      { 1 - 7  (Mon - Sun) }
    hour:       Integer;      { 0 - 23 }
    minute:     Integer;      { 0 - 59 }
    second:     Integer;      { 0 - 59 }
    millisecond: Integer;     { 0 - 999 }
  end;
  PDBDATEREC = ^DBDATEREC;

type
  DBNUMERIC = packed record
    Precision:  Byte;
    Scale:      Byte;
    Sign:       Byte; { 1 = Positive, 0 = Negative }
    Val:        array[0..MAXNUMERICLEN-1] of Byte;
  end;

  DBDECIMAL = DBNUMERIC;

const
{ Pack the following structures on a word boundary }
  MAXCOLNAMELEN = 30;
  MAXTABLENAME  = 30;

type
  DBCOL = packed record
    SizeOfStruct: DBINT;
    Name:       array[0..MAXCOLNAMELEN] of Char;
    ActualName: array[0..MAXCOLNAMELEN] of Char;
    TableName:  array[0..MAXTABLENAME] of Char;
    Typ:        SmallInt;
    UserType:   DBINT;
    MaxLength:  DBINT;
    Precision:  Byte;
    Scale:      Byte;
    VarLength:  Bool;    { TRUE, FALSE }
    Null:       Byte;    { TRUE, FALSE or DBUNKNOWN }
    CaseSensitive: Byte; { TRUE, FALSE or DBUNKNOWN }
    Updatable:  Byte;    { TRUE, FALSE or DBUNKNOWN }
    Identity:   BOOL;    { TRUE, FALSE }
  end;
  PDBCOL = ^DBCOL;

const
  MAXSERVERNAME = 30;
  MAXNETLIBNAME = 255;
  MAXNETLIBCONNSTR = 255;

type
  DBPROC_INFO = packed record
    SizeOfStruct:       DBINT;
    ServerType:         Byte;
    ServerMajor:        Word;
    ServerMinor:        Word;
    ServerRevision:     Word;
    ServerName:         array[0..MAXSERVERNAME] of Char;
    NetLibName:         array[0..MAXNETLIBNAME] of Char;
    NetLibConnStr:      array[0..MAXNETLIBCONNSTR] of Char;
  end;
  PDBPROCINFO = ^DBPROC_INFO;

  DBCURSOR_INFO = packed record
    SizeOfStruct:       DBINT;    { Use sizeof(DBCURSORINFO) }
    TotCols:            Cardinal; { Total Columns in cursor }
    TotRows:            Cardinal; { Total Rows in cursor }
    CurRow:             Cardinal; { Current actual row in server }
    TotRowsFetched:     Cardinal; { Total rows actually fetched }
    CurType:            Cardinal; { See CU_... }
    Status:             Cardinal; { See CU_... }
  end;
  PDBCURSORINFO = ^DBCURSOR_INFO;

const
  INVALID_UROWNUM       = Cardinal(-1);

type
{ Pointer Datatypes }

//typedef const LPINT          LPCINT;
//typedef const LPBYTE         LPCBYTE ;
//typedef       USHORT PTR     LPUSHORT;
//typedef const LPUSHORT       LPCUSHORT;
//typedef       DBINT PTR      LPDBINT;
  PDBINT        = ^DBINT;
  PDBBINARY     = ^DBBINARY;
//typedef const LPDBBINARY     LPCDBBINARY;
//typedef       DBDATEREC PTR  LPDBDATEREC;
//typedef const LPDBDATEREC    LPCDBDATEREC;
//typedef       DBDATETIME PTR LPDBDATETIME;
//typedef const LPDBDATETIME   LPCDBDATETIME;

{************** Plain API Function types definition *************}

{ Macros for setting the PLOGINREC }
function DBSETLHOST(Login: PLOGINREC; ClientHost: PChar): RETCODE;
function DBSETLUSER(Login: PLOGINREC; UserName: PChar): RETCODE;
function DBSETLPWD(Login: PLOGINREC; Passwd: PChar): RETCODE;
function DBSETLAPP(Login: PLOGINREC; AppName: PChar): RETCODE;
function DBSETLNATLANG(Login: PLOGINREC; Lang: PChar): RETCODE;
function DBSETLSECURE(Login: PLOGINREC): RETCODE;
function DBSETLVERSION(Login: PLOGINREC; Version: Byte): RETCODE;
function DBSETLTIME(Login: PLOGINREC; Seconds: DWORD): RETCODE;
function DBSETLFALLBACK(Login: PLOGINREC; Fallback: PChar): RETCODE;

{ Function macros }
function dbrbuf(Proc: PDBPROCESS): DBINT;

type
  DBERRHANDLE_PROC = function(Proc: PDBPROCESS; Severity, DbErr, OsErr: Integer;
    DbErrStr, OsErrStr: PChar): Integer; cdecl;
  DBMSGHANDLE_PROC = function(Proc: PDBPROCESS; MsgNo: DBINT; MsgState,
    Severity: Integer; MsgText, SrvName, ProcName: PChar; Line: DBUSMALLINT):
    Integer; cdecl;

  Tdberrhandle = function(Handler: DBERRHANDLE_PROC): DBERRHANDLE_PROC; cdecl;
  Tdbmsghandle = function(Handler: DBMSGHANDLE_PROC): DBMSGHANDLE_PROC; cdecl;

  Tdbprocerrhandle = function(DbHandle: PDBHANDLE; Handler: DBERRHANDLE_PROC):
    DBERRHANDLE_PROC; cdecl;
  Tdbprocmsghandle = function(DbHandle: PDBHANDLE; Handler: DBMSGHANDLE_PROC):
    DBMSGHANDLE_PROC; cdecl;

  { Two-phase commit functions }
  Tabort_xact = function(Proc: PDBPROCESS; CommId: DBINT): RETCODE; cdecl;
  Tbuild_xact_string = procedure(XActName, Service: PChar; CommId: DBINT;
    Result: PChar); cdecl;
  Tclose_commit = procedure(Proc: PDBPROCESS); cdecl;
  Tcommit_xact = function(Proc: PDBPROCESS; CommId: DBINT): RETCODE; cdecl;
  Topen_commit = function(Login: PLOGINREC; ServerName: PChar): PDBPROCESS; cdecl;
  Tremove_xact = function(Proc: PDBPROCESS; CommId: DBINT; SiteCount: Integer):
    RETCODE; cdecl;
  Tscan_xact = function(Proc: PDBPROCESS; CommId: DBINT): RETCODE; cdecl;
  Tstart_xact = function(Proc: PDBPROCESS; AppName, XActName: PChar;
    SiteCount: Integer): DBINT; cdecl;
  Tstat_xact = function(Proc: PDBPROCESS; CommId: DBINT): Integer; cdecl;

{ BCP functions }
  Tbcp_batch = function(Proc: PDBPROCESS): DBINT; cdecl;
  Tbcp_bind = function(Proc: PDBPROCESS; VarAddr: PByte; PrefixLen: Integer;
    VarLen: DBINT; Terminator: PByte; TermLen, Typ, TableColumn: Integer):
    RETCODE; cdecl;
  Tbcp_colfmt = function(Proc: PDBPROCESS; FileColumn: Integer; FileType: Byte;
    FilePrefixLen: Integer; FileColLen: DBINT; FileTerm: PByte; FileTermLen,
    TableColumn: Integer): RETCODE; cdecl;
  Tbcp_collen = function(Proc: PDBPROCESS; VarLen: DBINT; TableColumn: Integer):
    RETCODE; cdecl;
  Tbcp_colptr = function(Proc: PDBPROCESS; ColPtr: PByte; TableColumn: Integer):
    RETCODE; cdecl;
  Tbcp_columns = function(Proc: PDBPROCESS; FileColCount: Integer): RETCODE; cdecl;
  Tbcp_control = function(Proc: PDBPROCESS; Field: Integer; Value: DBINT):
    RETCODE; cdecl;
  Tbcp_done = function(Proc: PDBPROCESS): DBINT; cdecl;
  Tbcp_exec = function(Proc: PDBPROCESS; RowsCopied: PDBINT): RETCODE; cdecl;
  Tbcp_init = function(Proc: PDBPROCESS; TableName, hFile, ErrFile: PChar;
    Direction: Integer): RETCODE; cdecl;
  Tbcp_moretext = function(Proc: PDBPROCESS; Size: DBINT; Text: PByte):
    RETCODE; cdecl;
  Tbcp_readfmt = function(Proc: PDBPROCESS; FileName: PChar): RETCODE; cdecl;
  Tbcp_sendrow = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tbcp_setl = function(Login: PLOGINREC; Enable: BOOL): RETCODE; cdecl;
  Tbcp_writefmt = function(Proc: PDBPROCESS; FileName: PChar): RETCODE; cdecl;

{ Standard DB-Library functions }
  Tdbadata = function(Proc: PDBPROCESS; ComputeId, Column: Integer): PByte; cdecl;
  Tdbadlen = function(Proc: PDBPROCESS; ComputeId, Column: Integer): DBINT; cdecl;
  Tdbaltbind = function(Proc: PDBPROCESS; ComputeId, Column: Integer;
    VarType: Integer; VarLen: DBINT; VarAddr: PByte): RETCODE; cdecl;
  Tdbaltcolid = function(Proc: PDBPROCESS; ComputeId, Column: Integer): Integer; cdecl;
  Tdbaltlen = function(Proc: PDBPROCESS; ComputeId, Column: Integer): DBINT; cdecl;
  Tdbaltop = function(Proc: PDBPROCESS; ComputeId, Column: Integer): Integer; cdecl;
  Tdbalttype = function(Proc: PDBPROCESS; ComputeId, Column: Integer): Integer; cdecl;
  Tdbaltutype = function(Proc: PDBPROCESS; ComputeId, Column: Integer): DBINT; cdecl;
  Tdbanullbind = function(Proc: PDBPROCESS; ComputeId, Column: Integer;
    Indicator: PDBINT): RETCODE; cdecl;
  Tdbbind = function(Proc: PDBPROCESS; Column, VarType, VarLen: Integer;
    VarAddr: PByte): RETCODE; cdecl;
  Tdbbylist = function(Proc: PDBPROCESS; ComputeId: Integer; Size: PInteger):
    PByte; cdecl;
  Tdbcancel = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbcanquery = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbchange = function(Proc: PDBPROCESS): PChar; cdecl;
  Tdbclose = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbclrbuf = procedure(Proc: PDBPROCESS; N: DBINT); cdecl;
  Tdbclropt = function(Proc: PDBPROCESS; Option: Integer; Param: PChar): RETCODE; cdecl;
  Tdbcmd = function(Proc: PDBPROCESS; Cmd: PChar): RETCODE; cdecl;
  Tdbcmdrow = function(Proc: PDBPROCESS): RETCODE; cdecl;  //!!!
  Tdbcolbrowse = function(Proc: PDBPROCESS; Column: Integer): BOOL; cdecl;
  Tdbcolinfo = function(Handle: PDBHANDLE; Typ, Column, ComputeId: Integer;
    DbColumn: PDBCOL): RETCODE; cdecl;
  Tdbcollen = function(Proc: PDBPROCESS; Column: Integer): DBINT; cdecl;
  Tdbcolname = function(Proc: PDBPROCESS; Column: Integer): PChar; cdecl;
  Tdbcolsource = function(Proc: PDBPROCESS; Column: Integer): PChar; cdecl;
  Tdbcoltype = function(Proc: PDBPROCESS; Column: Integer): Integer; cdecl;
  Tdbcolutype = function(Proc: PDBPROCESS; Column: Integer): DBINT; cdecl;
  Tdbconvert = function(Proc: PDBPROCESS; SrcType: Integer; Src: PByte;
    SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT): Integer; cdecl;
  Tdbcount = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbcurcmd = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbcurrow = function(Proc: PDBPROCESS): DBINT; cdecl;

  Tdbcursor = function(hCursor: PDBCURSOR; OpType, Row: Integer; Table,
    Values: PChar): RETCODE; cdecl;
  Tdbcursorbind = function(hCursor: PDBCURSOR; Col, VarType: Integer; VarLen: DBINT;
    POutLen: PDBINT; VarAddr: PByte): RETCODE; cdecl;
  Tdbcursorclose = function(DbHandle: PDBHANDLE): RETCODE; cdecl;
  Tdbcursorcolinfo = function(hCursor: PDBCURSOR; Column: Integer; ColName: PChar;
    ColType: PInteger; ColLen: PDBINT; UserType: PInteger): RETCODE; cdecl;
  Tdbcursorfetch = function(hCursor: PDBCURSOR; FetchType, RowNum: Integer):
    RETCODE; cdecl;
  Tdbcursorfetchex = function(hCursor: PDBCURSOR; FetchType: Integer; RowNum,
    nFetchRows, Reserved: DBINT): RETCODE; cdecl;
  Tdbcursorinfo = function(hCursor: PDBCURSOR; nCols: PInteger; nRows: PDBINT):
    RETCODE; cdecl;
  Tdbcursorinfoex = function(hCursor: PDBCURSOR; DbCursorInfo: PDBCURSORINFO):
    RETCODE; cdecl;
  Tdbcursoropen = function(Proc: PDBPROCESS; Sql: PChar; ScrollOpt,
    ConCurOpt: Integer; nRows: Cardinal; PStatus: PDBINT): PDBCURSOR; cdecl;
  Tdbdata = function(Proc: PDBPROCESS; Column: Integer): PByte; cdecl;
  Tdbdataready = function(Proc: PDBPROCESS): BOOL; cdecl;
  Tdbdatecrack = function(Proc: PDBPROCESS; DateInfo: PDBDATEREC;
    DateType: PDBDATETIME): RETCODE; cdecl;
  Tdbdatlen = function(Proc: PDBPROCESS; Column: Integer): Integer; cdecl;
  Tdbdead = function(Proc: PDBPROCESS): BOOL; cdecl;
  Tdbexit = procedure; cdecl;
//  Tdbenlisttrans = function(PDBPROCESS, LPVOID): RETCODE; cdecl;
//  Tdbenlistxatrans = function(PDBPROCESS, BOOL): RETCODE; cdecl;
//  Tdbfcmd = function(PDBPROCESS, LPCSTR, ...): RETCODE; cdecl;
  Tdbfirstrow = function(Proc: PDBPROCESS): DBINT; cdecl;
  Tdbfreebuf = procedure(Proc: PDBPROCESS); cdecl;
  Tdbfreelogin = procedure(Login: PLOGINREC); cdecl;
  Tdbfreequal = procedure(Ptr: PChar); cdecl;
  Tdbgetchar = function(Proc: PDBPROCESS; N: Integer): PChar; cdecl;
  Tdbgetmaxprocs = function: SmallInt; cdecl;
  Tdbgetoff = function(Proc: PDBPROCESS; OffType: DBUSMALLINT;
    StartFrom: Integer): Integer; cdecl;
  Tdbgetpacket = function(Proc: PDBPROCESS): Cardinal; cdecl;
  Tdbgetrow = function(Proc: PDBPROCESS; Row: DBINT): STATUS; cdecl;
  Tdbgettime = function: Integer; cdecl;
  Tdbgetuserdata = function(Proc: PDBPROCESS): Pointer; cdecl;
  Tdbhasretstat = function(Proc: PDBPROCESS): BOOL; cdecl;
  Tdbinit = function: PChar; cdecl;
  Tdbisavail = function(Proc: PDBPROCESS): BOOL; cdecl;
  Tdbiscount = function(Proc: PDBPROCESS): BOOL; cdecl;
  Tdbisopt = function(Proc: PDBPROCESS; Option: Integer; Param: PChar): BOOL; cdecl;
  Tdblastrow = function(Proc: PDBPROCESS): DBINT; cdecl;
  Tdblogin = function: PLOGINREC; cdecl;
  Tdbmorecmds = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbmoretext = function(Proc: PDBPROCESS; Size: DBINT; Text: PByte): RETCODE; cdecl;
  Tdbname = function(Proc: PDBPROCESS): PChar; cdecl;
  Tdbnextrow = function(Proc: PDBPROCESS): STATUS; cdecl;
  Tdbnullbind = function(Proc: PDBPROCESS; Column: Integer; Indicator: PDBINT):
    RETCODE; cdecl;
  Tdbnumalts = function(Proc: PDBPROCESS; ComputeId: Integer): Integer; cdecl;
  Tdbnumcols = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbnumcompute = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbnumorders = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbnumrets = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbopen = function(Login: PLOGINREC; Host: PChar): PDBPROCESS; cdecl;
  Tdbordercol = function(Proc: PDBPROCESS; Order: Integer): Integer; cdecl;
  Tdbprocinfo = function(Proc: PDBPROCESS; DbProcInfo: PDBPROCINFO): RETCODE; cdecl;
  Tdbprhead = procedure(Proc: PDBPROCESS); cdecl;
  Tdbprrow = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbprtype = function(Token: Integer): PChar; cdecl;
  Tdbqual = function(Proc: PDBPROCESS; TabNum: Integer; TabName: PChar): PChar; cdecl;
//  Tdbreadpage = function(PDBPROCESS, LPCSTR, DBINT, DBINT, LPBYTE): DBINT; cdecl;
  Tdbreadtext = function(Proc: PDBPROCESS; Buf: Pointer; BufSize: DBINT): DBINT; cdecl;
  Tdbresults = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbretdata = function(Proc: PDBPROCESS; RetNum: Integer): PByte; cdecl;
  Tdbretlen = function(Proc: PDBPROCESS; RetNum: Integer): DBINT; cdecl;
  Tdbretname = function(Proc: PDBPROCESS; RetNum: Integer): PChar; cdecl;
  Tdbretstatus = function(Proc: PDBPROCESS): DBINT; cdecl;
  Tdbrettype = function(Proc: PDBPROCESS; RetNum: Integer): Integer; cdecl;
  Tdbrows = function(Proc: PDBPROCESS): RETCODE; cdecl; //!!!
  Tdbrowtype = function(Proc: PDBPROCESS): STATUS; cdecl;
  Tdbrpcinit = function(Proc: PDBPROCESS; ProcName: PChar; Options: DBSMALLINT):
    RETCODE; cdecl; //!!!
  Tdbrpcparam = function(Proc: PDBPROCESS; ParamName: PChar; Status: Byte;
    Typ: Integer; MaxLen, DataLen: DBINT; Value: PByte): RETCODE; cdecl;
  Tdbrpcsend = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbrpcexec = function(Proc: PDBPROCESS): RETCODE; cdecl;

  Tdbrpwclr = procedure(Login: PLOGINREC); cdecl;
//  Tdbrpwset = function(PLOGINREC, LPCSTR, LPCSTR, INT): RETCODE; cdecl;
  Tdbserverenum = function(SearchMode: Word; ServNameBuf: PChar;
    ServNameBufSize: Word; NumEntries: PWord): Integer; cdecl;
  Tdbsetavail = procedure(Proc: PDBPROCESS); cdecl;
  Tdbsetmaxprocs = function(MaxProcs: SmallInt): RETCODE; cdecl;
  Tdbsetlname = function(Login: PLOGINREC; Value: PChar; Item: Integer): RETCODE; cdecl;
  Tdbsetlogintime = function(Seconds: Integer): RETCODE; cdecl;

  Tdbsetlpacket = function(Login: PLOGINREC; PacketSize: Word): RETCODE; cdecl;
  Tdbsetnull = function(Proc: PDBPROCESS; BindType, BindLen: Integer;
    BindVal: PByte): RETCODE; cdecl;
  Tdbsetopt = function(Proc: PDBPROCESS; Option: Integer; Param: PChar):
    RETCODE; cdecl;
  Tdbsettime = function(Seconds: Integer): RETCODE; cdecl;
  Tdbsetuserdata = procedure(Proc: PDBPROCESS; Ptr: Pointer); cdecl;
  Tdbsqlexec = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbsqlok = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbsqlsend = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbstrcpy = function(Proc: PDBPROCESS; Start, NumBytes: Integer; Dest: PChar):
    RETCODE; cdecl;
  Tdbstrlen = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbtabbrowse = function(Proc: PDBPROCESS; TabNum: Integer): BOOL; cdecl;
  Tdbtabcount = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbtabname = function(Proc: PDBPROCESS; Table: Integer): PChar; cdecl;
  Tdbtabsource = function(Proc: PDBPROCESS; Column: Integer; TabNum: PInteger):
    PChar; cdecl;
  Tdbtsnewlen = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbtsnewval = function(Proc: PDBPROCESS): PDBBINARY; cdecl;
  Tdbtsput = function(Proc: PDBPROCESS; NewTs: PDBBINARY; NewTsName,
    TabNum: Integer; TableName: PChar): RETCODE; cdecl;
  Tdbtxptr = function(Proc: PDBPROCESS; Column: Integer): PDBBINARY; cdecl;
  Tdbtxtimestamp = function(Proc: PDBPROCESS; Column: Integer): PDBBINARY; cdecl;
  Tdbtxtsnewval = function(Proc: PDBPROCESS): PDBBINARY; cdecl;
  Tdbtxtsput = function(Proc: PDBPROCESS; NewTxts: PDBBINARY; Column: Integer):
    RETCODE; cdecl;
  Tdbuse = function(Proc: PDBPROCESS; DbName: PChar): RETCODE; cdecl;
  Tdbvarylen = function(Proc: PDBPROCESS; Column: Integer): BOOL; cdecl;
  Tdbwillconvert = function(SrcType, DestType: Integer): BOOL; cdecl;
//  Tdbwritepage = function(PDBPROCESS, LPCSTR, DBINT, DBINT, DBINT, LPBYTE): RETCODE; cdecl;
  Tdbwritetext = function(Proc: PDBPROCESS; ObjName: PChar; TextPtr: PDBBINARY;
    TextPtrLen: DBTINYINT; Timestamp: PDBBINARY; Log: BOOL; Size: DBINT;
    Text: PByte): RETCODE; cdecl;
  Tdbupdatetext = function(Proc: PDBPROCESS; DestObject: PChar; DestTextPtr,
    DestTimestamp: PDBBINARY; UpdateType: Integer; InsertOffset,
    DeleteLength: DBINT; SrcObject: PChar; SrcSize: DBINT; SrcText: PDBBINARY):
    RETCODE; cdecl;

{************* Plain API Function variables definition ************}

var
  dberrhandle           : Tdberrhandle;
  dbmsghandle           : Tdbmsghandle;

  dbprocerrhandle       : Tdbprocerrhandle;
  dbprocmsghandle       : Tdbprocmsghandle;

  { Two-phase commit functions }
  abort_xact            : Tabort_xact;
  build_xact_string     : Tbuild_xact_string;
  close_commit          : Tclose_commit;
  commit_xact           : Tcommit_xact;
  open_commit           : Topen_commit;
  remove_xact           : Tremove_xact;
  scan_xact             : Tscan_xact;
  start_xact            : Tstart_xact;
  stat_xact             : Tstat_xact;

{ BCP functions }
  bcp_batch             : Tbcp_batch;
  bcp_bind              : Tbcp_bind;
  bcp_colfmt            : Tbcp_colfmt;
  bcp_collen            : Tbcp_collen;
  bcp_colptr            : Tbcp_colptr;
  bcp_columns           : Tbcp_columns;
  bcp_control           : Tbcp_control;
  bcp_done              : Tbcp_done;
  bcp_exec              : Tbcp_exec;
  bcp_init              : Tbcp_init;
  bcp_moretext          : Tbcp_moretext;
  bcp_readfmt           : Tbcp_readfmt;
  bcp_sendrow           : Tbcp_sendrow;
  bcp_setl              : Tbcp_setl;
  bcp_writefmt          : Tbcp_writefmt;

{ Standard DB-Library functions }
  dbadata               : Tdbadata;
  dbadlen               : Tdbadlen;
  dbaltbind             : Tdbaltbind;
  dbaltcolid            : Tdbaltcolid;
  dbaltlen              : Tdbaltlen;
  dbaltop               : Tdbaltop;
  dbalttype             : Tdbalttype;
  dbaltutype            : Tdbaltutype;
  dbanullbind           : Tdbanullbind;
  dbbind                : Tdbbind;
  dbbylist              : Tdbbylist;
  dbcancel              : Tdbcancel;
  dbcanquery            : Tdbcanquery;
  dbchange              : Tdbchange;
  dbclose               : Tdbclose;
  dbclrbuf              : Tdbclrbuf;
  dbclropt              : Tdbclropt;
  dbcmd                 : Tdbcmd;
  dbcmdrow              : Tdbcmdrow;
  dbcolbrowse           : Tdbcolbrowse;
  dbcolinfo             : Tdbcolinfo;
  dbcollen              : Tdbcollen;
  dbcolname             : Tdbcolname;
  dbcolsource           : Tdbcolsource;
  dbcoltype             : Tdbcoltype;
  dbcolutype            : Tdbcolutype;
  dbconvert             : Tdbconvert;
  dbcount               : Tdbcount;
  dbcurcmd              : Tdbcurcmd;
  dbcurrow              : Tdbcurrow;

  dbcursor              : Tdbcursor;
  dbcursorbind          : Tdbcursorbind;
  dbcursorclose         : Tdbcursorclose;
  dbcursorcolinfo       : Tdbcursorcolinfo;
  dbcursorfetch         : Tdbcursorfetch;
  dbcursorfetchex       : Tdbcursorfetchex;
  dbcursorinfo          : Tdbcursorinfo;
  dbcursorinfoex        : Tdbcursorinfoex;
  dbcursoropen          : Tdbcursoropen;
  dbdata                : Tdbdata;
  dbdataready           : Tdbdataready;
  dbdatecrack           : Tdbdatecrack;
  dbdatlen              : Tdbdatlen;
  dbdead                : Tdbdead;
  dbexit                : Tdbexit;
//  dbenlisttrans         : Tdbenlisttrans;
//  dbenlistxatrans       : Tdbenlistxatrans;
//  dbfcmd                :Tdbfcmd;
  dbfirstrow            : Tdbfirstrow;
  dbfreebuf             : Tdbfreebuf;
  dbfreelogin           : Tdbfreelogin;
  dbfreequal            : Tdbfreequal;
  dbgetchar             : Tdbgetchar;
  dbgetmaxprocs         : Tdbgetmaxprocs;
  dbgetoff              : Tdbgetoff;
  dbgetpacket           : Tdbgetpacket;
  dbgetrow              : Tdbgetrow;
  dbgettime             : Tdbgettime;
  dbgetuserdata         : Tdbgetuserdata;
  dbhasretstat          : Tdbhasretstat;
  dbinit                : Tdbinit;
  dbisavail             : Tdbisavail;
  dbiscount             : Tdbiscount;
  dbisopt               : Tdbisopt;
  dblastrow             : Tdblastrow;
  dblogin               : Tdblogin;
  dbmorecmds            : Tdbmorecmds;
  dbmoretext            : Tdbmoretext;
  dbname                : Tdbname;
  dbnextrow             : Tdbnextrow;
  dbnullbind            : Tdbnullbind;
  dbnumalts             : Tdbnumalts;
  dbnumcols             : Tdbnumcols;
  dbnumcompute          : Tdbnumcompute;
  dbnumorders           : Tdbnumorders;
  dbnumrets             : Tdbnumrets;
  dbopen                : Tdbopen;
  dbordercol            : Tdbordercol;
  dbprocinfo            : Tdbprocinfo;
  dbprhead              : Tdbprhead;
  dbprrow               : Tdbprrow;
  dbprtype              : Tdbprtype;
  dbqual                : Tdbqual;
//  dbreadpage            : Tdbreadpage;
  dbreadtext            : Tdbreadtext;
  dbresults             : Tdbresults;
  dbretdata             : Tdbretdata;
  dbretlen              : Tdbretlen;
  dbretname             : Tdbretname;
  dbretstatus           : Tdbretstatus;
  dbrettype             : Tdbrettype;
  dbrows                : Tdbrows;
  dbrowtype             : Tdbrowtype;
  dbrpcinit             : Tdbrpcinit;
  dbrpcparam            : Tdbrpcparam;
  dbrpcsend             : Tdbrpcsend;
  dbrpcexec             : Tdbrpcexec;

  dbrpwclr              : Tdbrpwclr;
//  dbrpwset              : Tdbrpwset;
  dbserverenum          : Tdbserverenum;
  dbsetavail            : Tdbsetavail;
  dbsetmaxprocs         : Tdbsetmaxprocs;
  dbsetlname            : Tdbsetlname;
  dbsetlogintime        : Tdbsetlogintime;

  dbsetlpacket          : Tdbsetlpacket;
  dbsetnull             : Tdbsetnull;
  dbsetopt              : Tdbsetopt;
  dbsettime             : Tdbsettime;
  dbsetuserdata         : Tdbsetuserdata;
  dbsqlexec             : Tdbsqlexec;
  dbsqlok               : Tdbsqlok;
  dbsqlsend             : Tdbsqlsend;
  dbstrcpy              : Tdbstrcpy;
  dbstrlen              : Tdbstrlen;
  dbtabbrowse           : Tdbtabbrowse;
  dbtabcount            : Tdbtabcount;
  dbtabname             : Tdbtabname;
  dbtabsource           : Tdbtabsource;
  dbtsnewlen            : Tdbtsnewlen;
  dbtsnewval            : Tdbtsnewval;
  dbtsput               : Tdbtsput;
  dbtxptr               : Tdbtxptr;
  dbtxtimestamp         : Tdbtxtimestamp;
  dbtxtsnewval          : Tdbtxtsnewval;
  dbtxtsput             : Tdbtxtsput;
  dbuse                 : Tdbuse;
  dbvarylen             : Tdbvarylen;
  dbwillconvert         : Tdbwillconvert;
//  dbwritepage           : Tdbwritepage;
  dbwritetext           : Tdbwritetext;
  dbupdatetext          : Tdbupdatetext;

function dbsqlerror: string;
function dboserror: string;
function dbmessage: string;

function MsSqlLoadLib: Boolean;

const
  DLL: string = DEFAULT_DLL_LOCATION;
  hDLL: THandle = 0;
  LibLoaded: Boolean = False;

implementation


uses SysUtils, ZDBaseConst;

{ Handle sql server errors }

const
  DbErrorCode: Integer = 0;
  OsErrorCode: Integer = 0;
  DbMsgCode: Integer = 0;
  DbError: string = '';
  OsError: string = '';
  DbMsg: string = '';

var
  OldErrorHandle: DBERRHANDLE_PROC = nil;
  OldMessageHandle: DBMSGHANDLE_PROC = nil;

{ Handle sql server error messages }
function ErrorHandle(Proc: PDBPROCESS; Severity, DbErr, OsErr: Integer;
  DbErrStr, OsErrStr: PChar): Integer; cdecl;
begin
  DbErrorCode := DbErr;
  OsErrorCode := OsErr;
  DbError := StrPas(DbErrStr);
  OsError := StrPas(OsErrStr);
  Result := 0;
end;

{ Handle sql server messages }
function MessageHandle(Proc: PDBPROCESS; MsgNo: DBINT; MsgState, Severity: Integer;
  MsgText, SrvName, ProcName: PChar; Line: DBUSMALLINT): Integer; cdecl;
begin
  DbMsgCode := MsgNo;
  DbMsg := StrPas(MsgText);
  Result := 0;
end;

function dbsqlerror: string;
begin
  Result := DbError;
end;

function dboserror: string;
begin
  Result := OsError;
end;

function dbmessage: string;
begin
  Result := DbMsg;
end;

{ Initialize MS SQL dynamic library }
function MsSqlLoadLib: Boolean;
begin
  if hDLL = 0 then
  begin
    hDLL := GetModuleHandle(PChar(DLL));
    LibLoaded := False;
    if hDLL = 0 then
    begin
      hDLL := LoadLibrary(PChar(DLL));
      LibLoaded := True;
    end;
  end;

  if hDLL <> 0 then
  begin
    @dberrhandle           := GetProcAddress(hDLL,'dberrhandle');
    @dbmsghandle           := GetProcAddress(hDLL,'dbmsghandle');
    @dbprocerrhandle       := GetProcAddress(hDLL,'dbprocerrhandle');
    @dbprocmsghandle       := GetProcAddress(hDLL,'dbprocmsghandle');
    @abort_xact            := GetProcAddress(hDLL,'abort_xact');
    @build_xact_string     := GetProcAddress(hDLL,'build_xact_string');
    @close_commit          := GetProcAddress(hDLL,'close_commit');
    @commit_xact           := GetProcAddress(hDLL,'commit_xact');
    @open_commit           := GetProcAddress(hDLL,'open_commit');
    @remove_xact           := GetProcAddress(hDLL,'remove_xact');
    @scan_xact             := GetProcAddress(hDLL,'scan_xact');
    @start_xact            := GetProcAddress(hDLL,'start_xact');
    @stat_xact             := GetProcAddress(hDLL,'stat_xact');
    @bcp_batch             := GetProcAddress(hDLL,'bcp_batch');
    @bcp_bind              := GetProcAddress(hDLL,'bcp_bind');
    @bcp_colfmt            := GetProcAddress(hDLL,'bcp_colfmt');
    @bcp_collen            := GetProcAddress(hDLL,'bcp_collen');
    @bcp_colptr            := GetProcAddress(hDLL,'bcp_colptr');
    @bcp_columns           := GetProcAddress(hDLL,'bcp_columns');
    @bcp_control           := GetProcAddress(hDLL,'bcp_control');
    @bcp_done              := GetProcAddress(hDLL,'bcp_done');
    @bcp_exec              := GetProcAddress(hDLL,'bcp_exec');
    @bcp_init              := GetProcAddress(hDLL,'bcp_init');
    @bcp_moretext          := GetProcAddress(hDLL,'bcp_moretext');
    @bcp_readfmt           := GetProcAddress(hDLL,'bcp_readfmt');
    @bcp_sendrow           := GetProcAddress(hDLL,'bcp_sendrow');
    @bcp_setl              := GetProcAddress(hDLL,'bcp_setl');
    @bcp_writefmt          := GetProcAddress(hDLL,'bcp_writefmt');
    @dbadata               := GetProcAddress(hDLL,'dbadata');
    @dbadlen               := GetProcAddress(hDLL,'dbadlen');
    @dbaltbind             := GetProcAddress(hDLL,'dbaltbind');
    @dbaltcolid            := GetProcAddress(hDLL,'dbaltcolid');
    @dbaltlen              := GetProcAddress(hDLL,'dbaltlen');
    @dbaltop               := GetProcAddress(hDLL,'dbaltop');
    @dbalttype             := GetProcAddress(hDLL,'dbalttype');
    @dbaltutype            := GetProcAddress(hDLL,'dbaltutype');
    @dbanullbind           := GetProcAddress(hDLL,'dbanullbind');
    @dbbind                := GetProcAddress(hDLL,'dbbind');
    @dbbylist              := GetProcAddress(hDLL,'dbbylist');
    @dbcancel              := GetProcAddress(hDLL,'dbcancel');
    @dbcanquery            := GetProcAddress(hDLL,'dbcanquery');
    @dbchange              := GetProcAddress(hDLL,'dbchange');
    @dbclose               := GetProcAddress(hDLL,'dbclose');
    @dbclrbuf              := GetProcAddress(hDLL,'dbclrbuf');
    @dbclropt              := GetProcAddress(hDLL,'dbclropt');
    @dbcmd                 := GetProcAddress(hDLL,'dbcmd');
    @dbcmdrow              := GetProcAddress(hDLL,'dbcmdrow');
    @dbcolbrowse           := GetProcAddress(hDLL,'dbcolbrowse');
    @dbcolinfo             := GetProcAddress(hDLL,'dbcolinfo');
    @dbcollen              := GetProcAddress(hDLL,'dbcollen');
    @dbcolname             := GetProcAddress(hDLL,'dbcolname');
    @dbcolsource           := GetProcAddress(hDLL,'dbcolsource');
    @dbcoltype             := GetProcAddress(hDLL,'dbcoltype');
    @dbcolutype            := GetProcAddress(hDLL,'dbcolutype');
    @dbconvert             := GetProcAddress(hDLL,'dbconvert');
    @dbcount               := GetProcAddress(hDLL,'dbcount');
    @dbcurcmd              := GetProcAddress(hDLL,'dbcurcmd');
    @dbcurrow              := GetProcAddress(hDLL,'dbcurrow');
    @dbcursor              := GetProcAddress(hDLL,'dbcursor');
    @dbcursorbind          := GetProcAddress(hDLL,'dbcursorbind');
    @dbcursorclose         := GetProcAddress(hDLL,'dbcursorclose');
    @dbcursorcolinfo       := GetProcAddress(hDLL,'dbcursorcolinfo');
    @dbcursorfetch         := GetProcAddress(hDLL,'dbcursorfetch');
    @dbcursorfetchex       := GetProcAddress(hDLL,'dbcursorfetchex');
    @dbcursorinfo          := GetProcAddress(hDLL,'dbcursorinfo');
    @dbcursorinfoex        := GetProcAddress(hDLL,'dbcursorinfoex');
    @dbcursoropen          := GetProcAddress(hDLL,'dbcursoropen');
    @dbdata                := GetProcAddress(hDLL,'dbdata');
    @dbdataready           := GetProcAddress(hDLL,'dbdataready');
    @dbdatecrack           := GetProcAddress(hDLL,'dbdatecrack');
    @dbdatlen              := GetProcAddress(hDLL,'dbdatlen');
    @dbdead                := GetProcAddress(hDLL,'dbdead');
    @dbexit                := GetProcAddress(hDLL,'dbexit');
//    @dbenlisttrans         := GetProcAddress(hDLL,'dbenlisttrans');
//    @dbenlistxatrans       := GetProcAddress(hDLL,'dbenlistxatrans');
//    @dbfcmd                := GetProcAddress(hDLL,'dbfcmd');
    @dbfirstrow            := GetProcAddress(hDLL,'dbfirstrow');
    @dbfreebuf             := GetProcAddress(hDLL,'dbfreebuf');
    @dbfreelogin           := GetProcAddress(hDLL,'dbfreelogin');
    @dbfreequal            := GetProcAddress(hDLL,'dbfreequal');
    @dbgetchar             := GetProcAddress(hDLL,'dbgetchar');
    @dbgetmaxprocs         := GetProcAddress(hDLL,'dbgetmaxprocs');
    @dbgetoff              := GetProcAddress(hDLL,'dbgetoff');
    @dbgetpacket           := GetProcAddress(hDLL,'dbgetpacket');
    @dbgetrow              := GetProcAddress(hDLL,'dbgetrow');
    @dbgettime             := GetProcAddress(hDLL,'dbgettime');
    @dbgetuserdata         := GetProcAddress(hDLL,'dbgetuserdata');
    @dbhasretstat          := GetProcAddress(hDLL,'dbhasretstat');
    @dbinit                := GetProcAddress(hDLL,'dbinit');
    @dbisavail             := GetProcAddress(hDLL,'dbisavail');
    @dbiscount             := GetProcAddress(hDLL,'dbiscount');
    @dbisopt               := GetProcAddress(hDLL,'dbisopt');
    @dblastrow             := GetProcAddress(hDLL,'dblastrow');
    @dblogin               := GetProcAddress(hDLL,'dblogin');
    @dbmorecmds            := GetProcAddress(hDLL,'dbmorecmds');
    @dbmoretext            := GetProcAddress(hDLL,'dbmoretext');
    @dbname                := GetProcAddress(hDLL,'dbname');
    @dbnextrow             := GetProcAddress(hDLL,'dbnextrow');
    @dbnullbind            := GetProcAddress(hDLL,'dbnullbind');
    @dbnumalts             := GetProcAddress(hDLL,'dbnumalts');
    @dbnumcols             := GetProcAddress(hDLL,'dbnumcols');
    @dbnumcompute          := GetProcAddress(hDLL,'dbnumcompute');
    @dbnumorders           := GetProcAddress(hDLL,'dbnumorders');
    @dbnumrets             := GetProcAddress(hDLL,'dbnumrets');
    @dbopen                := GetProcAddress(hDLL,'dbopen');
    @dbordercol            := GetProcAddress(hDLL,'dbordercol');
    @dbprocinfo            := GetProcAddress(hDLL,'dbprocinfo');
    @dbprhead              := GetProcAddress(hDLL,'dbprhead');
    @dbprrow               := GetProcAddress(hDLL,'dbprrow');
    @dbprtype              := GetProcAddress(hDLL,'dbprtype');
    @dbqual                := GetProcAddress(hDLL,'dbqual');
//    @dbreadpage            := GetProcAddress(hDLL,'dbreadpage');
    @dbreadtext            := GetProcAddress(hDLL,'dbreadtext');
    @dbresults             := GetProcAddress(hDLL,'dbresults');
    @dbretdata             := GetProcAddress(hDLL,'dbretdata');
    @dbretlen              := GetProcAddress(hDLL,'dbretlen');
    @dbretname             := GetProcAddress(hDLL,'dbretname');
    @dbretstatus           := GetProcAddress(hDLL,'dbretstatus');
    @dbrettype             := GetProcAddress(hDLL,'dbrettype');
    @dbrows                := GetProcAddress(hDLL,'dbrows');
    @dbrowtype             := GetProcAddress(hDLL,'dbrowtype');
    @dbrpcinit             := GetProcAddress(hDLL,'dbrpcinit');
    @dbrpcparam            := GetProcAddress(hDLL,'dbrpcparam');
    @dbrpcsend             := GetProcAddress(hDLL,'dbrpcsend');
    @dbrpcexec             := GetProcAddress(hDLL,'dbrpcexec');
    @dbrpwclr              := GetProcAddress(hDLL,'dbrpwclr');
//    @dbrpwset              := GetProcAddress(hDLL,'dbrpwset');
    @dbserverenum          := GetProcAddress(hDLL,'dbserverenum');
    @dbsetavail            := GetProcAddress(hDLL,'dbsetavail');
    @dbsetmaxprocs         := GetProcAddress(hDLL,'dbsetmaxprocs');
    @dbsetlname            := GetProcAddress(hDLL,'dbsetlname');
    @dbsetlogintime        := GetProcAddress(hDLL,'dbsetlogintime');
    @dbsetlpacket          := GetProcAddress(hDLL,'dbsetlpacket');
    @dbsetnull             := GetProcAddress(hDLL,'dbsetnull');
    @dbsetopt              := GetProcAddress(hDLL,'dbsetopt');
    @dbsettime             := GetProcAddress(hDLL,'dbsettime');
    @dbsetuserdata         := GetProcAddress(hDLL,'dbsetuserdata');
    @dbsqlexec             := GetProcAddress(hDLL,'dbsqlexec');
    @dbsqlok               := GetProcAddress(hDLL,'dbsqlok');
    @dbsqlsend             := GetProcAddress(hDLL,'dbsqlsend');
    @dbstrcpy              := GetProcAddress(hDLL,'dbstrcpy');
    @dbstrlen              := GetProcAddress(hDLL,'dbstrlen');
    @dbtabbrowse           := GetProcAddress(hDLL,'dbtabbrowse');
    @dbtabcount            := GetProcAddress(hDLL,'dbtabcount');
    @dbtabname             := GetProcAddress(hDLL,'dbtabname');
    @dbtabsource           := GetProcAddress(hDLL,'dbtabsource');
    @dbtsnewlen            := GetProcAddress(hDLL,'dbtsnewlen');
    @dbtsnewval            := GetProcAddress(hDLL,'dbtsnewval');
    @dbtsput               := GetProcAddress(hDLL,'dbtsput');
    @dbtxptr               := GetProcAddress(hDLL,'dbtxptr');
    @dbtxtimestamp         := GetProcAddress(hDLL,'dbtxtimestamp');
    @dbtxtsnewval          := GetProcAddress(hDLL,'dbtxtsnewval');
    @dbtxtsput             := GetProcAddress(hDLL,'dbtxtsput');
    @dbuse                 := GetProcAddress(hDLL,'dbuse');
    @dbvarylen             := GetProcAddress(hDLL,'dbvarylen');
    @dbwillconvert         := GetProcAddress(hDLL,'dbwillconvert');
//    @dbwritepage           := GetProcAddress(hDLL,'dbwritepage');
    @dbwritetext           := GetProcAddress(hDLL,'dbwritetext');
    @dbupdatetext          := GetProcAddress(hDLL,'dbupdatetext');

    OldErrorHandle := dberrhandle(ErrorHandle);
    OldMessageHandle := dbmsghandle(MessageHandle);

    Result := True;
  end else
    raise Exception.Create(Format(SLibraryNotFound,[DLL]));
end;

function DBSETLHOST(Login: PLOGINREC; ClientHost: PChar): RETCODE;
begin
  Result := dbsetlname(Login, ClientHost, DBSETHOST);
end;

function DBSETLUSER(Login: PLOGINREC; UserName: PChar): RETCODE;
begin
  Result := dbsetlname(Login, UserName, DBSETUSER);
end;

function DBSETLPWD(Login: PLOGINREC; Passwd: PChar): RETCODE;
begin
  Result := dbsetlname(Login, Passwd, DBSETPWD);
end;

function DBSETLAPP(Login: PLOGINREC; AppName: PChar): RETCODE;
begin
  Result := dbsetlname(Login, AppName, DBSETAPP);
end;

function DBSETLNATLANG(Login: PLOGINREC; Lang: PChar): RETCODE;
begin
  Result := dbsetlname(Login, Lang, DBSETLANG);
end;

function DBSETLSECURE(Login: PLOGINREC): RETCODE;
begin
  Result := dbsetlname(Login, nil, DBSETSECURE);
end;

function DBSETLVERSION(Login: PLOGINREC; Version: Byte): RETCODE;
begin
  Result := dbsetlname(Login, nil, Version);
end;

function DBSETLTIME(Login: PLOGINREC; Seconds: DWORD): RETCODE;
begin
  Result := dbsetlname(Login, PChar(Cardinal(Seconds)), DBSET_LOGIN_TIME);
end;

function DBSETLFALLBACK(Login: PLOGINREC; Fallback: PChar): RETCODE;
begin
  Result := dbsetlname(Login, Fallback, DBSETFALLBACK);
end;

function dbrbuf(Proc: PDBPROCESS): DBINT;
begin
  Result := DBINT(dbdataready(Proc));
end;

initialization

finalization
  if hDLL <> 0 then
  begin
    dberrhandle(OldErrorHandle);
    dbmsghandle(OldMessageHandle);
    if LibLoaded then
    begin
      dbexit;
      FreeLibrary(hDLL);
    end;
  end;
end.
