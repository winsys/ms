{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{              Plain interface to db2cli.dll             }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{                                                        }
{********************************************************}

unit ZLibDb2Sql;

interface

uses {$IFNDEF LINUX}Windows,{$ENDIF} Classes, ZSqlTypes;

{//$IFNDEF LINUX}
{//$INCLUDE ..\Zeos.inc}
{//$ELSE}
{//$INCLUDE ../Zeos.inc}
{//$ENDIF}

const
{$IFNDEF LINUX}
  DEFAULT_DLL_LOCATION   = 'db2cli.dll';
{$ELSE}
  DEFAULT_DLL_LOCATION   = 'db2cli.so';
{$ENDIF}

{***************** Plain API Constants definition ****************}

const
{ generally useful constants }
  SQL_MAX_MESSAGE_LENGTH   = 1024; { message buffer size             }
  SQL_MAX_ID_LENGTH        = 128;  { maximum identifier name size,
                                          e.g. cursor names               }

{ date/time length constants }
  SQL_DATE_LEN             = 10;
  SQL_TIME_LEN             = 8;  { add P+1 if precision is nonzero }
  SQL_TIMESTAMP_LEN        = 19;  { add P+1 if precision is nonzero }

{ handle type identifiers }
  SQL_HANDLE_ENV           = 1;
  SQL_HANDLE_DBC           = 2;
  SQL_HANDLE_STMT          = 3;
  SQL_HANDLE_DESC          = 4;

{ RETCODE values             }
  SQL_SUCCESS              = 0;
  SQL_SUCCESS_WITH_INFO    = 1;
  SQL_NEED_DATA            = 99;
  SQL_NO_DATA              = 100;
  SQL_STILL_EXECUTING      = 2;
  SQL_ERROR                = -1;
  SQL_INVALID_HANDLE       = -2;

{ test for SQL_SUCCESS or SQL_SUCCESS_WITH_INFO }
// SQL_SUCCEEDED(rc) (((rc)&(~1))==0)

{ SQLFreeStmt option values  }
  SQL_CLOSE                = 0;
  SQL_DROP                 = 1;
  SQL_UNBIND               = 2;
  SQL_RESET_PARAMS         = 3;

{ SQLTransact option values  }
  SQL_COMMIT               = 0;
  SQL_ROLLBACK             = 1;

{ Standard SQL data types }
  SQL_UNKNOWN_TYPE         = 0;
  SQL_CHAR                 = 1;
  SQL_NUMERIC              = 2;
  SQL_DECIMAL              = 3;
  SQL_INTEGER              = 4;
  SQL_SMALLINT             = 5;
  SQL_FLOAT                = 6;
  SQL_REAL                 = 7;
  SQL_DOUBLE               = 8;
  SQL_DATETIME             = 9;
  SQL_VARCHAR              = 12;
  SQL_WCHAR                = (-8);
  SQL_WVARCHAR             = (-9);
  SQL_WLONGVARCHAR         = (-10);
  SQL_BIGINT               = (-5);
  SQL_BINARY               = (-2);
{ One-parameter shortcuts for date/time data types }
  SQL_TYPE_DATE            = 91;
  SQL_TYPE_TIME            = 92;
  SQL_TYPE_TIMESTAMP       = 93;

{ Statement attribute values for cursor sensitivity }
  SQL_UNSPECIFIED          = 0;
  SQL_INSENSITIVE          = 1;
  SQL_SENSITIVE            = 2;

{ Default conversion code for SQLBindCol(), SQLBindParam() and SQLGetData() }
  SQL_DEFAULT              = 99;

{ SQLGetData() code indicating that the application row descriptor
  specifies the data type }
  SQL_ARD_TYPE             = (-99);

{ SQL date/time type subcodes }
  SQL_CODE_DATE            = 1;
  SQL_CODE_TIME            = 2;
  SQL_CODE_TIMESTAMP       = 3;

{ SQL extended data types }
  SQL_GRAPHIC              = -95;
  SQL_VARGRAPHIC           = -96;
  SQL_LONGVARGRAPHIC       = -97;
  SQL_BLOB                 = -98;
  SQL_CLOB                 = -99;
  SQL_DBCLOB               = -350;
  SQL_DATALINK             = -400;
  SQL_USER_DEFINED_TYPE    = -450;

{ C data type to SQL data type mapping }
  SQL_C_DBCHAR             = SQL_DBCLOB;
  SQL_C_DECIMAL_IBM        = SQL_DECIMAL;
//  SQL_C_DATALINK           = SQL_C_CHAR;
  SQL_C_PTR                = 2463;

{ locator type identifier }

  SQL_BLOB_LOCATOR         = 31;
  SQL_CLOB_LOCATOR         = 41;
  SQL_DBCLOB_LOCATOR       = -351;

{ C Data Type for the LOB locator types }
  SQL_C_BLOB_LOCATOR       = SQL_BLOB_LOCATOR;
  SQL_C_CLOB_LOCATOR       = SQL_CLOB_LOCATOR;
  SQL_C_DBCLOB_LOCATOR     = SQL_DBCLOB_LOCATOR;

{ NULL status defines; these are used in SQLColAttributes, SQLDescribeCol,
  to describe the nullability of a column in a table. }

  SQL_NO_NULLS             = 0;
  SQL_NULLABLE             = 1;
  SQL_NULLABLE_UNKNOWN     = 2;

{ values of UNNAMED field in descriptor used in SQLColAttribute }
 SQL_NAMED                 = 0;
 SQL_UNNAMED               = 1;

{ values of ALLOC_TYPE field in descriptor }
 SQL_DESC_ALLOC_AUTO       = 1;
 SQL_DESC_ALLOC_USER       = 2;

{ values of USER_DEFINED_TYPE_CODE }
  SQL_TYPE_BASE            = 0;
  SQL_TYPE_DISTINCT        = 1;
  SQL_TYPE_STRUCTURED      = 2;
  SQL_TYPE_REFERENCE       = 3;

{ Special length values  }
  SQL_NULL_DATA            = -1;
  SQL_DATA_AT_EXEC         = -2;
  SQL_NTS                  = -3;      { NTS = Null Terminated String    }
  SQL_NTSL                 = -3;      { NTS = Null Terminated String    }

{ SQLColAttributes defines }
  SQL_COLUMN_SCHEMA_NAME    = 16;
  SQL_COLUMN_CATALOG_NAME   = 17;
  SQL_COLUMN_DISTINCT_TYPE  = 1250;
  SQL_DESC_DISTINCT_TYPE    = SQL_COLUMN_DISTINCT_TYPE;
  SQL_COLUMN_REFERENCE_TYPE = 1251;
  SQL_DESC_REFERENCE_TYPE   = SQL_COLUMN_REFERENCE_TYPE;
  SQL_DESC_STRUCTURED_TYPE  = 1252;
  SQL_DESC_USER_TYPE        = 1253;
  SQL_DESC_BASE_TYPE        = 1254;

{ identifiers of fields in the SQL descriptor }
 SQL_DESC_COUNT                  = 1001;
 SQL_DESC_TYPE                   = 1002;
 SQL_DESC_LENGTH                 = 1003;
 SQL_DESC_OCTET_LENGTH_PTR       = 1004;
 SQL_DESC_PRECISION              = 1005;
 SQL_DESC_SCALE                  = 1006;
 SQL_DESC_DATETIME_INTERVAL_CODE = 1007;
 SQL_DESC_NULLABLE               = 1008;
 SQL_DESC_INDICATOR_PTR          = 1009;
 SQL_DESC_DATA_PTR               = 1010;
 SQL_DESC_NAME                   = 1011;
 SQL_DESC_UNNAMED                = 1012;
 SQL_DESC_OCTET_LENGTH           = 1013;
 SQL_DESC_ALLOC_TYPE             = 1099;
 SQL_DESC_USER_DEFINED_TYPE_CODE = 1098;


{ SQLColAttribute defines for SQL_COLUMN_UPDATABLE condition }
  SQL_UPDT_READONLY          = 0;
  SQL_UPDT_WRITE             = 1;
  SQL_UPDT_READWRITE_UNKNOWN = 2;

{ SQLColAttribute defines for SQL_COLUMN_SEARCHABLE condition. }
  SQL_PRED_NONE              = 0;
  SQL_PRED_CHAR              = 1;
  SQL_PRED_BASIC             = 2;

{ NULL handle defines    }
  SQL_NULL_HENV              = 0;
  SQL_NULL_HDBC              = 0;
  SQL_NULL_HSTMT             = 0;
  SQL_NULL_HDESC             = 0;
  SQL_NULL_HANDLE            = 0;

{ identifiers of fields in the diagnostics area }
  SQL_DIAG_RETURNCODE         = 1;
  SQL_DIAG_NUMBER             = 2;
  SQL_DIAG_ROW_COUNT          = 3;
  SQL_DIAG_SQLSTATE           = 4;
  SQL_DIAG_NATIVE             = 5;
  SQL_DIAG_MESSAGE_TEXT       = 6;
  SQL_DIAG_DYNAMIC_FUNCTION   = 7;
  SQL_DIAG_CLASS_ORIGIN       = 8;
  SQL_DIAG_SUBCLASS_ORIGIN    = 9;
  SQL_DIAG_CONNECTION_NAME    = 10;
  SQL_DIAG_SERVER_NAME        = 11;
  SQL_DIAG_DYNAMIC_FUNCTION_CODE = 12;

{ dynamic function codes }
  SQL_DIAG_ALTER_TABLE           = 4;
  SQL_DIAG_CALL                  = 7;
  SQL_DIAG_CREATE_INDEX          = (-1);
  SQL_DIAG_CREATE_TABLE          = 77;
  SQL_DIAG_CREATE_VIEW           = 84;
  SQL_DIAG_DELETE_WHERE          = 19;
  SQL_DIAG_DROP_INDEX            = (-2);
  SQL_DIAG_DROP_TABLE            = 32;
  SQL_DIAG_DROP_VIEW             = 36;
  SQL_DIAG_DYNAMIC_DELETE_CURSOR = 38;
  SQL_DIAG_DYNAMIC_UPDATE_CURSOR = 81;
  SQL_DIAG_GRANT                 = 48;
  SQL_DIAG_INSERT                = 50;
  SQL_DIAG_REVOKE                = 59;
  SQL_DIAG_SELECT_CURSOR         = 85;
  SQL_DIAG_UNKNOWN_STATEMENT     = 0;
  SQL_DIAG_UPDATE_WHERE          = 82;

{ IBM specific SQLGetDiagField values. }

  SQL_DIAG_DEFERRED_PREPARE_ERROR = 1279;

{ SQL_DIAG_ROW_NUMBER values }
  SQL_ROW_NO_ROW_NUMBER          = (-1);
  SQL_ROW_NUMBER_UNKNOWN         = (-2);

{ SQL_DIAG_COLUMN_NUMBER values }
  SQL_COLUMN_NO_COLUMN_NUMBER    = (-1);
  SQL_COLUMN_NUMBER_UNKNOWN      = (-2);

{ NEW PARAMETERS }

{ Options for SQLGetConnectOption/SQLSetConnectOption extensions }
  SQL_WCHARTYPE                  = 1252;
  SQL_LONGDATA_COMPAT            = 1253;
  SQL_CURRENT_SCHEMA             = 1254;
  SQL_DB2EXPLAIN                 = 1258;
  SQL_DB2ESTIMATE                = 1259;
  SQL_PARAMOPT_ATOMIC            = 1260;
  SQL_STMTTXN_ISOLATION          = 1261;
  SQL_MAXCONN                    = 1262;
  SQL_ATTR_CLISCHEMA             = 1280;
  SQL_ATTR_INFO_USERID           = 1281;
  SQL_ATTR_INFO_WRKSTNNAME       = 1282;
  SQL_ATTR_INFO_APPLNAME         = 1283;
  SQL_ATTR_INFO_ACCTSTR          = 1284;
  SQL_ATTR_AUTOCOMMIT_NOCOMMIT   = 2462;
  SQL_ATTR_QUERY_PATROLLER       = 2466;
  SQL_ATTR_CHAINING_BEGIN        = 2464;
  SQL_ATTR_CHAINING_END          = 2465;

  SQL_ATTR_WCHARTYPE            = SQL_WCHARTYPE;
  SQL_ATTR_LONGDATA_COMPAT      = SQL_LONGDATA_COMPAT;
  SQL_ATTR_CURRENT_SCHEMA       = SQL_CURRENT_SCHEMA;
  SQL_ATTR_DB2EXPLAIN           = SQL_DB2EXPLAIN;
  SQL_ATTR_DB2ESTIMATE          = SQL_DB2ESTIMATE;
  SQL_ATTR_PARAMOPT_ATOMIC      = SQL_PARAMOPT_ATOMIC;
  SQL_ATTR_STMTTXN_ISOLATION    = SQL_STMTTXN_ISOLATION;
  SQL_ATTR_MAXCONN              = SQL_MAXCONN;

{ Options for SQLSetConnectOption, SQLSetEnvAttr }
  SQL_CONNECTTYPE                = 1255;
  SQL_SYNC_POINT                 = 1256;
  SQL_MINMEMORY_USAGE            = 1263;
  SQL_CONN_CONTEXT               = 1269;
  SQL_ATTR_INHERIT_NULL_CONNECT  = 1270;
  SQL_ATTR_FORCE_CONVERSION_ON_CLIENT = 1275;

  SQL_ATTR_CONNECTTYPE           = SQL_CONNECTTYPE;
  SQL_ATTR_SYNC_POINT            = SQL_SYNC_POINT;
  SQL_ATTR_MINMEMORY_USAGE       = SQL_MINMEMORY_USAGE;
  SQL_ATTR_CONN_CONTEXT          = SQL_CONN_CONTEXT;

{ connection attributes }
  SQL_ACCESS_MODE                = 101;
  SQL_AUTOCOMMIT                 = 102;
  SQL_LOGIN_TIMEOUT              = 103;
  SQL_OPT_TRACE                  = 104;
  SQL_OPT_TRACEFILE              = 105;
  SQL_TRANSLATE_DLL              = 106;
  SQL_TRANSLATE_OPTION           = 107;
  SQL_TXN_ISOLATION              = 108;
  SQL_CURRENT_QUALIFIER          = 109;
  SQL_ODBC_CURSORS               = 110;
  SQL_QUIET_MODE                 = 111;
  SQL_PACKET_SIZE                = 112;

{ connection attributes with new names }
  SQL_ATTR_ACCESS_MODE           = SQL_ACCESS_MODE;
  SQL_ATTR_AUTOCOMMIT            = SQL_AUTOCOMMIT;
  SQL_ATTR_CONNECTION_TIMEOUT    = 113;
  SQL_ATTR_CURRENT_CATALOG       = SQL_CURRENT_QUALIFIER;
  SQL_ATTR_DISCONNECT_BEHAVIOR   = 114;
  SQL_ATTR_ENLIST_IN_DTC         = 1207;
  SQL_ATTR_ENLIST_IN_XA          = 1208;
  SQL_ATTR_LOGIN_TIMEOUT         = SQL_LOGIN_TIMEOUT;
  SQL_ATTR_ODBC_CURSORS          = SQL_ODBC_CURSORS;
  SQL_ATTR_PACKET_SIZE           = SQL_PACKET_SIZE;
  SQL_ATTR_QUIET_MODE            = SQL_QUIET_MODE;
  SQL_ATTR_TRACE                 = SQL_OPT_TRACE;
  SQL_ATTR_TRACEFILE             = SQL_OPT_TRACEFILE;
  SQL_ATTR_TRANSLATE_LIB         = SQL_TRANSLATE_DLL;
  SQL_ATTR_TRANSLATE_OPTION      = SQL_TRANSLATE_OPTION;
  SQL_ATTR_TXN_ISOLATION         = SQL_TXN_ISOLATION;

{ SQL_AUTOCOMMIT options }
  SQL_AUTOCOMMIT_OFF             = 0;
  SQL_AUTOCOMMIT_ON              = 1;
  SQL_AUTOCOMMIT_DEFAULT         = SQL_AUTOCOMMIT_ON;

{ SQL_TXN_ISOLATION_OPTION masks }
  SQL_TXN_READ_UNCOMMITTED            = #00000001;
  SQL_TRANSACTION_READ_UNCOMMITTED    = SQL_TXN_READ_UNCOMMITTED;
  SQL_TXN_READ_COMMITTED              = #00000002;
  SQL_TRANSACTION_READ_COMMITTED      = SQL_TXN_READ_COMMITTED;
  SQL_TXN_REPEATABLE_READ             = #00000004;
  SQL_TRANSACTION_REPEATABLE_READ     = SQL_TXN_REPEATABLE_READ;
  SQL_TXN_SERIALIZABLE                = #00000008;
  SQL_TRANSACTION_SERIALIZABLE        = SQL_TXN_SERIALIZABLE;
  SQL_TXN_NOCOMMIT                    = #00000020;
  SQL_TRANSACTION_NOCOMMIT            = SQL_TXN_NOCOMMIT;

{ Defines for SQLBindParameter and
                           SQLProcedureColumns (returned in the result set) }
  SQL_PARAM_TYPE_UNKNOWN              = 0;
  SQL_PARAM_INPUT                     = 1;
  SQL_PARAM_INPUT_OUTPUT              = 2;
  SQL_RESULT_COL                      = 3;
  SQL_PARAM_OUTPUT                    = 4;
  SQL_RETURN_VALUE                    = 5;

{***************** Plain API types definition ****************}

type
  { SQL portable types for C  }
//  SQLCHAR = Byte;
  PSQLCHAR = PChar;
//  SQLVARCHAR = Byte;
//  SQLSCHAR = Byte;
  SQLINTEGER = Integer;
  PSQLINTEGER = ^SQLINTEGER;
  SQLSMALLINT = SmallInt;
  PSQLSMALLINT = ^SQLSMALLINT;
  SQLDOUBLE = Double;
  SQLFLOAT = Double;
//  SQLREAL = Real;

  SQLRETURN = SQLSMALLINT;

  SQLUINTEGER = SQLINTEGER;
  PSQLUINTEGER = ^SQLUINTEGER;
  SQLUSMALLINT = SQLSMALLINT;
  SQLPOINTER = Pointer;
  SQLHANDLE = SQLINTEGER;
  PSQLHANDLE = ^SQLHANDLE;
  SQLHENV = SQLINTEGER;
  PSQLHENV = ^SQLHENV;
  SQLHDBC = SQLINTEGER;
  PSQLHDBC = ^SQLHDBC;
  SQLHSTMT = SQLINTEGER;
  PSQLHSTMT = ^SQLHSTMT;
  SQLHDESC = SQLHANDLE;

(*

#if defined(SQL_NO_NATIVE_BIGINT_SUPPORT)
typedef struct
{
        SQLUINTEGER dwLowWord;
        SQLINTEGER  dwHighWord;
} SQLBIGINT;
typedef struct
{
        SQLUINTEGER dwLowWord;
        SQLUINTEGER dwHighWord;
} SQLUBIGINT;
#elif defined(SQL_BIGINT_TYPE)
typedef SQL_BIGINT_TYPE   SQLBIGINT;
typedef SQL_BIGUINT_TYPE  SQLUBIGINT;
#endif
*)
  SQL_DATE_STRUCT = packed record
    year: SQLSMALLINT;
    month: SQLUSMALLINT;
    day: SQLUSMALLINT;
  end;
  PSQL_DATE_STRUCT = ^SQL_DATE_STRUCT;

  SQL_TIME_STRUCT = packed record
    hour: SQLUSMALLINT;
    minute: SQLUSMALLINT;
    second: SQLUSMALLINT;
  end;
  PSQL_TIME_STRUCT = ^SQL_TIME_STRUCT;

  SQL_TIMESTAMP_STRUCT = packed record
    year: SQLSMALLINT;
    month: SQLUSMALLINT;
    day: SQLUSMALLINT;
    hour: SQLUSMALLINT;
    minute: SQLUSMALLINT;
    second: SQLUSMALLINT;
    fraction: SQLUINTEGER;     { fraction of a second }
  end;
  PSQL_TIMESTAMP_STRUCT = ^SQL_TIMESTAMP_STRUCT;

{************** Plain API Function types definition *************}

type

  TSQLAllocConnect = function(henv: SQLHENV; phdbc: PSQLHDBC): SQLRETURN; stdcall;

  TSQLAllocEnv = function(phenv: PSQLHENV): SQLRETURN; stdcall;

  TSQLAllocStmt = function(hdbc: SQLHDBC; phstmt: PSQLHSTMT): SQLRETURN; stdcall;

  TSQLAllocHandle = function(fHandleType: SQLSMALLINT; hInput: SQLHANDLE;
    phOutput: PSQLHANDLE): SQLRETURN; stdcall;

  TSQLBindCol = function(hstmt: SQLHSTMT; icol: SQLUSMALLINT;
    fCType: SQLSMALLINT; rgbValue: SQLPOINTER; cbValueMax: SQLINTEGER;
    pcbValue: PSQLINTEGER): SQLRETURN; stdcall;

  TSQLCancel = function(hstmt: SQLHSTMT): SQLRETURN; stdcall;

  TSQLColAttribute = function(hstmt: SQLHSTMT; icol: SQLUSMALLINT;
    fDescType: SQLUSMALLINT; rgbDesc: SQLPOINTER; cbDescMax: SQLSMALLINT;
    pcbDesc: PSQLSMALLINT; pfDesc: SQLPOINTER): SQLRETURN; stdcall;

  TSQLConnect = function(hdbc: SQLHDBC; szDSN: PSQLCHAR; cbDSN: SQLSMALLINT;
    szUID: PSQLCHAR; cbUID: SQLSMALLINT; szAuthStr: PSQLCHAR;
    cbAuthStr: SQLSMALLINT): SQLRETURN; stdcall;

  TSQLDescribeCol = function(hstmt: SQLHSTMT; icol: SQLUSMALLINT;
    szColName: PSQLCHAR; cbColNameMax: SQLSMALLINT; pcbColName: PSQLSMALLINT;
    pfSqlType: PSQLSMALLINT; pcbColDef: PSQLUINTEGER; pibScale: PSQLSMALLINT;
    pfNullable: PSQLSMALLINT): SQLRETURN; stdcall;

  TSQLDisconnect = function(hdbc: SQLHDBC): SQLRETURN; stdcall;

  TSQLError = function(henv: SQLHENV; hdbc: SQLHDBC; hstmt: SQLHSTMT;
    szSqlState: PSQLCHAR; pfNativeError: PSQLINTEGER; szErrorMsg: PSQLCHAR;
    cbErrorMsgMax: SQLSMALLINT; pcbErrorMsg: PSQLSMALLINT): SQLRETURN; stdcall;

  TSQLExecDirect = function(hstmt: SQLHSTMT; szSqlStr: PSQLCHAR;
    cbSqlStr: SQLINTEGER): SQLRETURN; stdcall;

  TSQLExecute = function(hstmt: SQLHSTMT): SQLRETURN; stdcall;

  TSQLFetch = function(hstmt: SQLHSTMT): SQLRETURN; stdcall;

  TSQLFreeConnect = function(hdbc: SQLHDBC): SQLRETURN; stdcall;

  TSQLFreeEnv = function(henv: SQLHENV): SQLRETURN; stdcall;

  TSQLFreeStmt = function(hstmt: SQLHSTMT; fOption: SQLUSMALLINT): SQLRETURN;
    stdcall;

  TSQLCloseCursor = function(hStmt: SQLHSTMT): SQLRETURN; stdcall;

  TSQLGetCursorName = function(hstmt: SQLHSTMT; szCursor: PSQLCHAR;
    cbCursorMax: SQLSMALLINT; pcbCursor: PSQLSMALLINT): SQLRETURN; stdcall;

  TSQLGetData = function(hstmt: SQLHSTMT; icol: SQLUSMALLINT;
    fCType: SQLSMALLINT; rgbValue: SQLPOINTER; cbValueMax: SQLINTEGER;
    pcbValue: PSQLINTEGER): SQLRETURN; stdcall;

  TSQLNumResultCols = function(hstmt: SQLHSTMT; pccol: PSQLSMALLINT): SQLRETURN;
    stdcall;

  TSQLPrepare = function(hstmt: SQLHSTMT; szSqlStr: PSQLCHAR;
    cbSqlStr: SQLINTEGER): SQLRETURN; stdcall;

  TSQLRowCount = function(hstmt: SQLHSTMT; pcrow: PSQLINTEGER): SQLRETURN; stdcall;

  TSQLSetCursorName = function(hstmt: SQLHSTMT; szCursor: PSQLCHAR;
    cbCursor: SQLSMALLINT): SQLRETURN; stdcall;

  TSQLSetParam = function(hstmt: SQLHSTMT; ipar: SQLUSMALLINT;
    fCType: SQLSMALLINT; fSqlType: SQLSMALLINT; cbParamDef: SQLUINTEGER;
    ibScale: SQLSMALLINT; rgbValue: SQLPOINTER; pcbValue: PSQLINTEGER):
    SQLRETURN; stdcall;

  TSQLTransact = function(henv: SQLHENV; hdbc: SQLHDBC; fType: SQLUSMALLINT):
    SQLRETURN; stdcall;

  TSQLEndTran = function(fHandleType: SQLSMALLINT; hHandle: SQLHANDLE;
    fType: SQLSMALLINT): SQLRETURN; stdcall;

  TSQLFreeHandle = function(fHandleType: SQLSMALLINT; hHandle: SQLHANDLE):
    SQLRETURN; stdcall;

  TSQLGetDiagRec = function(fHandleType: SQLSMALLINT; hHandle: SQLHANDLE;
    iRecNumber: SQLSMALLINT; pszSqlState: PSQLCHAR; pfNativeError: PSQLINTEGER;
    pszErrorMsg: PSQLCHAR; cbErrorMsgMax: SQLSMALLINT;
    pcbErrorMsg: PSQLSMALLINT): SQLRETURN; stdcall;

  TSQLGetDiagField = function(fHandleType: SQLSMALLINT; hHandle: SQLHANDLE;
    iRecNumber: SQLSMALLINT; fDiagIdentifier: SQLSMALLINT; pDiagInfo: SQLPOINTER;
    cbDiagInfoMax: SQLSMALLINT; pcbDiagInfo: PSQLSMALLINT): SQLRETURN; stdcall;

  TSQLCopyDesc = function(hDescSource: SQLHDESC; hDescTarget: SQLHDESC):
    SQLRETURN; stdcall;

  TSQLGetDescField = function(DescriptorHandle: SQLHDESC;
    RecNumber: SQLSMALLINT; FieldIdentifier: SQLSMALLINT; Value: SQLPOINTER;
    BufferLength: SQLINTEGER; StringLength: PSQLINTEGER): SQLRETURN; stdcall;

  TSQLGetDescRec = function(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
    Name: PSQLCHAR; BufferLength: SQLSMALLINT; StringLength: PSQLSMALLINT;
    _Type: PSQLSMALLINT; SubType: PSQLSMALLINT; Length: PSQLINTEGER;
    Precision: PSQLSMALLINT; Scale: PSQLSMALLINT;  Nullable: PSQLSMALLINT):
    SQLRETURN; stdcall;

  TSQLSetDescField = function(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
    FieldIdentifier: SQLSMALLINT; Value: SQLPOINTER; BufferLength: SQLINTEGER):
    SQLRETURN; stdcall;

  TSQLSetDescRec = function(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
    _Type: SQLSMALLINT; SubType: SQLSMALLINT; Length: SQLINTEGER;
    Precision: SQLSMALLINT; Scale: SQLSMALLINT; Data: SQLPOINTER;
    StringLength: PSQLINTEGER; Indicator: PSQLINTEGER): SQLRETURN; stdcall;

  { New added functions }

  TSQLSetConnectAttr = function(hdbc: SQLHDBC; fOption: SQLINTEGER;
    pvParam: SQLPOINTER; fStrLen: SQLINTEGER): SQLRETURN; stdcall;

  TSQLSetConnectOption = function(hdbc: SQLHDBC; fOption: SQLUSMALLINT;
    vParam: SQLUINTEGER): SQLRETURN; stdcall;

  TSQLSetStmtAttr = function(hstmt: SQLHSTMT; fOption: SQLINTEGER;
    pvParam: SQLPOINTER; fStrLen: SQLINTEGER): SQLRETURN; stdcall;

  TSQLSetStmtOption = function(hstmt: SQLHSTMT; fOption: SQLUSMALLINT;
    vParam: SQLUINTEGER): SQLRETURN; stdcall;

  TSQLGetSubString = function(hstmt: SQLHSTMT; LocatorCType: SQLSMALLINT;
    SourceLocator: SQLINTEGER; FromPosition: SQLUINTEGER;
    ForLength: SQLUINTEGER; TargetCType: SQLSMALLINT; rgbValue: SQLPOINTER;
    cbValueMax: SQLINTEGER; StringLength: PSQLINTEGER;
    IndicatorValue: PSQLINTEGER): SQLRETURN; stdcall;

  TSQLGetLength = function(hstmt: SQLHSTMT; LocatorCType: SQLSMALLINT;
    Locator: SQLINTEGER; StringLength: PSQLINTEGER;
    IndicatorValue: PSQLINTEGER): SQLRETURN; stdcall;

  TSQLGetPosition = function(hstmt: SQLHSTMT; LocatorCType: SQLSMALLINT;
    SourceLocator: SQLINTEGER; SearchLocator: SQLINTEGER; SearchLiteral: PSQLCHAR;
    SearchLiteralLength: SQLINTEGER; FromPosition: SQLUINTEGER;
    LocatedAt: PSQLUINTEGER; IndicatorValue: PSQLINTEGER): SQLRETURN; stdcall;

  TSQLBindParameter = function(hstmt: SQLHSTMT; ipar: SQLUSMALLINT;
    fParamType: SQLSMALLINT; fCType: SQLSMALLINT; fSqlType: SQLSMALLINT;
    cbColDef: SQLUINTEGER; ibScale: SQLSMALLINT; rgbValue: SQLPOINTER;
    cbValueMax: SQLINTEGER; pcbValue: PSQLINTEGER): SQLRETURN; stdcall;

  TSQLParamData = function(hstmt: SQLHSTMT; prgbValue: SQLPOINTER):
    SQLRETURN; stdcall;

  TSQLPutData = function(hstmt: SQLHSTMT; rgbValue: SQLPOINTER;
    cbValue: SQLINTEGER): SQLRETURN; stdcall;

{************* Plain API Function variables definition ************}
var
  SQLAllocConnect: TSQLAllocConnect;
  SQLAllocEnv: TSQLAllocEnv;
  SQLAllocStmt: TSQLAllocStmt;
  SQLAllocHandle: TSQLAllocHandle;
  SQLBindCol: TSQLBindCol;
  SQLCancel: TSQLCancel;
  SQLColAttribute: TSQLColAttribute;
  SQLConnect: TSQLConnect;
  SQLDescribeCol: TSQLDescribeCol;
  SQLDisconnect: TSQLDisconnect;
  SQLError: TSQLError;
  SQLExecDirect: TSQLExecDirect;
  SQLExecute: TSQLExecute;
  SQLFetch: TSQLFetch;
  SQLFreeConnect: TSQLFreeConnect;
  SQLFreeEnv: TSQLFreeEnv;
  SQLFreeStmt: TSQLFreeStmt;
  SQLCloseCursor: TSQLCloseCursor;
  SQLGetCursorName: TSQLGetCursorName;
  SQLGetData: TSQLGetData;
  SQLNumResultCols: TSQLNumResultCols;
  SQLPrepare: TSQLPrepare;
  SQLRowCount: TSQLRowCount;
  SQLSetCursorName: TSQLSetCursorName;
  SQLSetParam: TSQLSetParam;
  SQLTransact: TSQLTransact;
  SQLEndTran: TSQLEndTran;
  SQLFreeHandle: TSQLFreeHandle;
  SQLGetDiagRec: TSQLGetDiagRec;
  SQLGetDiagField: TSQLGetDiagField;
  SQLCopyDesc: TSQLCopyDesc;
  SQLGetDescField: TSQLGetDescField;
  SQLGetDescRec: TSQLGetDescRec;
  SQLSetDescField: TSQLSetDescField;
  SQLSetDescRec: TSQLSetDescRec;
  { New added functions }
  SQLSetConnectAttr: TSQLSetConnectAttr;
  SQLSetConnectOption: TSQLSetConnectOption;
  SQLSetStmtAttr: TSQLSetStmtAttr;
  SQLSetStmtOption: TSQLSetStmtOption;
  SQLGetSubString: TSQLGetSubString;
  SQLGetLength: TSQLGetLength;
  SQLGetPosition: TSQLGetPosition;
  SQLBindParameter: TSQLBindParameter;
  SQLParamData: TSQLParamData;
  SQLPutData: TSQLPutData;

{ Library Initialization }
function Db2SqlLoadLib: Boolean;

var
  DLL: string;// = DEFAULT_DLL_LOCATION;
  hDLL: THandle; // = 0;
  DLLVersion: string; // = '';
  LibLoaded: Boolean; // = False;     // must be added

implementation

uses SysUtils, ZExtra, ZDBaseConst;

{ Initialize DB2 dynamic library }
function Db2SqlLoadLib: Boolean;begin
  if hDLL = 0 then
  begin
    DLL := DEFAULT_DLL_LOCATION;
    hDLL := GetModuleHandle(PChar(DLL));
    if hDLL = 0 then
    begin
      hDLL := LoadLibrary(PChar(DLL));
      LibLoaded := True;
    end;
  end;

  if hDLL <> 0 then
  begin
    SQLAllocConnect := GetProcAddress(hDLL, 'SQLAllocConnect');
    SQLAllocEnv := GetProcAddress(hDLL, 'SQLAllocEnv');
    SQLAllocStmt := GetProcAddress(hDLL, 'SQLAllocStmt');
    SQLAllocHandle := GetProcAddress(hDLL, 'SQLAllocHandle');
    SQLBindCol := GetProcAddress(hDLL, 'SQLBindCol');
    SQLCancel := GetProcAddress(hDLL, 'SQLCancel');
    SQLColAttribute := GetProcAddress(hDLL, 'SQLColAttribute');
    SQLConnect := GetProcAddress(hDLL, 'SQLConnect');
    SQLDescribeCol := GetProcAddress(hDLL, 'SQLDescribeCol');
    SQLDisconnect := GetProcAddress(hDLL, 'SQLDisconnect');
    SQLError := GetProcAddress(hDLL, 'SQLError');
    SQLExecDirect := GetProcAddress(hDLL, 'SQLExecDirect');
    SQLExecute := GetProcAddress(hDLL, 'SQLExecute');
    SQLFetch := GetProcAddress(hDLL, 'SQLFetch');
    SQLFreeConnect := GetProcAddress(hDLL, 'SQLFreeConnect');
    SQLFreeEnv := GetProcAddress(hDLL, 'SQLFreeEnv');
    SQLFreeStmt := GetProcAddress(hDLL, 'SQLFreeStmt');
    SQLCloseCursor := GetProcAddress(hDLL, 'SQLCloseCursor');
    SQLGetCursorName := GetProcAddress(hDLL, 'SQLGetCursorName');
    SQLGetData := GetProcAddress(hDLL, 'SQLGetData');
    SQLNumResultCols := GetProcAddress(hDLL, 'SQLNumResultCols');
    SQLPrepare := GetProcAddress(hDLL, 'SQLPrepare');
    SQLRowCount := GetProcAddress(hDLL, 'SQLRowCount');
    SQLSetCursorName := GetProcAddress(hDLL, 'SQLSetCursorName');
    SQLSetParam := GetProcAddress(hDLL, 'SQLSetParam');
    SQLTransact := GetProcAddress(hDLL, 'SQLTransact');
    SQLEndTran := GetProcAddress(hDLL, 'SQLEndTran');
    SQLFreeHandle := GetProcAddress(hDLL, 'SQLFreeHandle');
    SQLGetDiagRec := GetProcAddress(hDLL, 'SQLGetDiagRec');
    SQLGetDiagField := GetProcAddress(hDLL, 'SQLGetDiagField');
    SQLCopyDesc := GetProcAddress(hDLL, 'SQLCopyDesc');
    SQLGetDescField := GetProcAddress(hDLL, 'SQLGetDescField');
    SQLGetDescRec := GetProcAddress(hDLL, 'SQLGetDescRec');
    SQLSetDescField := GetProcAddress(hDLL, 'SQLSetDescField');
    SQLSetDescRec := GetProcAddress(hDLL, 'SQLSetDescRec');
    { New added functions }
    SQLSetConnectAttr := GetProcAddress(hDLL, 'SQLSetConnectAttr');
    SQLSetConnectOption := GetProcAddress(hDLL, 'SQLSetConnectOption');
    SQLSetStmtAttr := GetProcAddress(hDLL, 'SQLSetStmtAttr');
    SQLSetStmtOption := GetProcAddress(hDLL, 'SQLSetStmtOption');
    SQLGetSubString := GetProcAddress(hDLL, 'SQLGetSubString');
    SQLGetLength := GetProcAddress(hDLL, 'SQLGetLength');
    SQLGetPosition := GetProcAddress(hDLL, 'SQLGetPosition');
    SQLBindParameter := GetProcAddress(hDLL, 'SQLBindParameter');
    SQLParamData := GetProcAddress(hDLL, 'SQLParamData');
    SQLPutData := GetProcAddress(hDLL, 'SQLPutData');

    Result := True;
  end
  else
    raise Exception.Create(Format(SLibraryNotFound,[DLL]));
end;

initialization

finalization
  if (hDLL <> 0) and LibLoaded then
    FreeLibrary(hDLL);
end.
