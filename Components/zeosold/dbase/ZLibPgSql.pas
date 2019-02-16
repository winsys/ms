{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{              Plain interface to libpq.dll              }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZLibPgSql;

interface

uses {$IFNDEF LINUX}Windows,{$ENDIF} Classes, ZSqlTypes;

{$IFNDEF LINUX}
{$INCLUDE ..\Zeos.inc}
{$ELSE}
{$INCLUDE ../Zeos.inc}
{$ENDIF}

{***************** Plain API Constants definition ****************}

const
{$IFNDEF LINUX}
  DEFAULT_DLL_LOCATION   = 'libpq.dll';
{$ELSE}
  DEFAULT_DLL_LOCATION   = 'libpq.so';
{$ENDIF}

{ Type Lengths }
  NAMEDATALEN  = 32;
{ OIDNAMELEN should be set to NAMEDATALEN + sizeof(Oid) }
  OIDNAMELEN   = 36;

  INV_WRITE    = $00020000;
  INV_READ     = $00040000;

  SEEK_SET     = 0;
  SEEK_CUR     = 1;
  SEEK_END     = 2;


{****************** Plain API Types definition *****************}

type
  Oid = Integer;

{ Application-visible enum types }
  ConnStatusType = (
    CONNECTION_OK,
    CONNECTION_BAD
  );

  ExecStatusType = (
    PGRES_EMPTY_QUERY,
    PGRES_COMMAND_OK,		{ a query command that doesn't return anything
				  was executed properly by the backend }
    PGRES_TUPLES_OK,		{ a query command that returns tuples
				  was executed properly by the backend,
				  PGresult contains the result tuples }
    PGRES_COPY_OUT,		{ Copy Out data transfer in progress }
    PGRES_COPY_IN,		{ Copy In data transfer in progress }
    PGRES_BAD_RESPONSE,		{ an unexpected response was recv'd from
				  the backend }
    PGRES_NONFATAL_ERROR,
    PGRES_FATAL_ERROR
  );

{ String descriptions of the ExecStatusTypes }
  pgresStatus = array[$00..$ff] of PChar;

{ PGconn encapsulates a connection to the backend.
  The contents of this struct are not supposed to be known to applications.
}
  PGconn = Pointer;
  PPGconn = Pointer;

{ PGresult encapsulates the result of a query (or more precisely, of a single
  SQL command --- a query string given to PQsendQuery can contain multiple
  commands and thus return multiple PGresult objects).
  The contents of this struct are not supposed to be known to applications.
}
  PGresult = Pointer;
  PPGresult = Pointer;

{ PGnotify represents the occurrence of a NOTIFY message.
  Ideally this would be an opaque typedef, but it's so simple that it's
  unlikely to change.
  NOTE: in Postgres 6.4 and later, the be_pid is the notifying backend's,
  whereas in earlier versions it was always your own backend's PID.
}
  PGnotify = packed record
    relname: array [0..NAMEDATALEN-1] of Char; { name of relation containing data }
    be_pid:  Integer;			      { process id of backend }
  end;

  PPGnotify = ^PGnotify;

{ PQnoticeProcessor is the function type for the notice-message callback. }

  PQnoticeProcessor = procedure(arg: Pointer; message: PChar); cdecl;

{ Print options for PQprint() }

{
  We can't use the conventional "bool", because we are designed to be
  included in a user's program, and user may already have that type
  defined.  Pqbool, on the other hand, is unlikely to be used.
}

  PPChar = array[00..$ff] of PChar;

  PQprintOpt = packed record
    header:    TBool;	   { print output field headings and row count }
    align:     TBool;	   { fill align the fields }
    standard:  TBool;	   { old brain dead format }
    html3:     TBool;	   { output html tables }
    expanded:  TBool;	   { expand tables }
    pager:     TBool;	   { use pager for output if needed }
    fieldSep:  PChar;	   { field separator }
    tableOpt:  PChar;      { insert to HTML <table ...> }
    caption:   PChar;	   { HTML <caption> }
    fieldName: PPChar; 	   { null terminated array of repalcement field names }
  end;

  PPQprintOpt = ^PQprintOpt;

{ ----------------
  Structure for the conninfo parameter definitions returned by PQconndefaults
  ----------------
}
  PQconninfoOption = packed record
    keyword:  PChar;	{ The keyword of the option }
    envvar:   PChar;	{ Fallback environment variable name }
    compiled: PChar;	{ Fallback compiled in default value  }
    val:      PChar;	{ Options value	}
    lab:      PChar;	{ Label for field in connect dialog }
    dispchar: PChar;	{ Character to display for this field
			  in a connect dialog. Values are:
			  ""	Display entered value as is
			  "*"	Password field - hide value
			  "D"	Debug options - don't
			  create a field by default }
    dispsize: Integer;	{ Field size in characters for dialog }
  end;

  PPQConninfoOption = ^PQconninfoOption;

{ ----------------
  PQArgBlock -- structure for PQfn() arguments
  ----------------
}
  PQArgBlock = packed record
    len:     Integer;
    isint:   Integer;
    case u: Boolean of
      True:  (ptr: PInteger);	{ can't use void (dec compiler barfs)	 }
      False: (_int: Integer);
  end;

  PPQArgBlock = ^PQArgBlock;


{************** Plain API Function types definition *************}

{ ===	in fe-connect.c === }
  TPQconnectdb     = function(ConnInfo: PChar): PPGconn; cdecl;
  TPQsetdbLogin    = function(Host, Port, Options, Tty, Db, User, Passwd: PChar): PPGconn; cdecl;
  TPQconndefaults  = function: PPQconninfoOption; cdecl;
  TPQfinish        = procedure(Handle: PPGconn); cdecl;
  TPQreset         = procedure(Handle: PPGconn); cdecl;
  TPQrequestCancel = function(Handle: PPGconn): Integer; cdecl;
  TPQdb            = function(Handle: PPGconn): PChar; cdecl;
  TPQuser          = function(Handle: PPGconn): PChar; cdecl;
  TPQpass          = function(Handle: PPGconn): PChar; cdecl;
  TPQhost          = function(Handle: PPGconn): PChar; cdecl;
  TPQport          = function(Handle: PPGconn): PChar; cdecl;
  TPQtty           = function(Handle: PPGconn): PChar; cdecl;
  TPQoptions       = function(Handle: PPGconn): PChar; cdecl;
  TPQstatus        = function(Handle: PPGconn): ConnStatusType; cdecl;
  TPQerrorMessage  = function(Handle: PPGconn): PChar; cdecl;
  TPQsocket        = function(Handle: PPGconn): Integer; cdecl;
  TPQbackendPID    = function(Handle: PPGconn): Integer; cdecl;
  TPQtrace         = procedure(Handle: PPGconn; DebugPort: Pointer); cdecl;
  TPQuntrace       = procedure(Handle: PPGconn); cdecl;
  TPQsetNoticeProcessor = procedure(Handle: PPGconn; Proc: PQnoticeProcessor; Arg: Pointer); cdecl;

{ === in fe-exec.c === }
  TPQexec          = function(Handle: PPGconn; Query: PChar): PPGresult; cdecl;
  TPQnotifies      = function(Handle: PPGconn): PPGnotify; cdecl;
  TPQnotifyFree    = procedure(Handle: PPGnotify);cdecl;
  TPQsendQuery     = function(Handle: PPGconn; Query: PChar): Integer; cdecl;
  TPQgetResult     = function(Handle: PPGconn): PPGresult; cdecl;
  TPQisBusy        = function(Handle: PPGconn): Integer; cdecl;
  TPQconsumeInput  = function(Handle: PPGconn): Integer; cdecl;
  TPQgetline       = function(Handle: PPGconn; Str: PChar; length: Integer): Integer; cdecl;
  TPQputline       = function(Handle: PPGconn; Str: PChar): Integer; cdecl;
  TPQgetlineAsync  = function(Handle: PPGconn; Buffer: PChar; BufSize: Integer): Integer; cdecl;
  TPQputnbytes     = function(Handle: PPGconn; Buffer: PChar; NBytes: Integer): Integer; cdecl;
  TPQendcopy       = function(Handle: PPGconn): Integer; cdecl;
  TPQfn            = function(Handle: PPGconn; fnid: Integer; result_buf, result_len: PInteger; result_is_int: Integer; args: PPQArgBlock; nargs: Integer): PPGresult; cdecl;
  TPQresultStatus  = function(Result: PPGresult): ExecStatusType; cdecl;
  TPQresultErrorMessage = function(Result: PPGresult): PChar; cdecl;
  TPQntuples       = function(Result: PPGresult): Integer; cdecl;
  TPQnfields       = function(Result: PPGresult): Integer; cdecl;
  TPQbinaryTuples  = function(Result: PPGresult): Integer; cdecl;
  TPQfname         = function(Result: PPGresult; field_num: Integer): PChar; cdecl;
  TPQfnumber       = function(Result: PPGresult; field_name: PChar): Integer; cdecl;
  TPQftype         = function(Result: PPGresult; field_num: Integer): Oid; cdecl;
  TPQfsize         = function(Result: PPGresult; field_num: Integer): Integer; cdecl;
  TPQfmod          = function(Result: PPGresult; field_num: Integer): Integer; cdecl;
  TPQcmdStatus     = function(Result: PPGresult): PChar; cdecl;
  TPQoidValue      = function(Result: PPGresult): Oid; cdecl;
  TPQoidStatus     = function(Result: PPGresult): PChar; cdecl;
  TPQcmdTuples     = function(Result: PPGresult): PChar; cdecl;
  TPQgetvalue      = function(Result: PPGresult; tup_num, field_num: Integer): PChar; cdecl;
  TPQgetlength     = function(Result: PPGresult; tup_num, field_num: Integer): Integer; cdecl;
  TPQgetisnull     = function(Result: PPGresult; tup_num, field_num: Integer): Integer; cdecl;
  TPQclear         = procedure(Result: PPGresult); cdecl;
  TPQmakeEmptyPGresult  = function(Handle: PPGconn; status: ExecStatusType): PPGresult; cdecl;

{ === in fe-lobj.c === }
  Tlo_open         = function(Handle: PPGconn; lobjId: Oid; mode: Integer): Integer; cdecl;
  Tlo_close        = function(Handle: PPGconn; fd: Integer): Integer; cdecl;
  Tlo_read         = function(Handle: PPGconn; fd: Integer; buf: PChar; len: Integer): Integer; cdecl;
  Tlo_write        = function(Handle: PPGconn; fd: Integer; buf: PChar; len: Integer): Integer; cdecl;
  Tlo_lseek        = function(Handle: PPGconn; fd, offset, whence: Integer): Integer; cdecl;
  Tlo_creat        = function(Handle: PPGconn; mode: Integer): Oid; cdecl;
  Tlo_tell         = function(Handle: PPGconn; fd: Integer): Integer; cdecl;
  Tlo_unlink       = function(Handle: PPGconn; lobjId: Oid): Integer; cdecl;
  Tlo_import       = function(Handle: PPGconn; filename: PChar): Oid; cdecl;
  Tlo_export       = function(Handle: PPGconn; lobjId: Oid; filename: PChar): Integer; cdecl;


{************* Plain API Function variables definition ************}

var
{ ===	in fe-connect.c === }
  PQconnectdb:     TPQconnectdb;
  PQsetdbLogin:    TPQsetdbLogin;
  PQconndefaults:  TPQconndefaults;
  PQfinish:        TPQfinish;
  PQreset:         TPQreset;
  PQrequestCancel: TPQrequestCancel;
  PQdb:            TPQdb;
  PQuser:          TPQuser;
  PQpass:          TPQpass;
  PQhost:          TPQhost;
  PQport:          TPQport;
  PQtty:           TPQtty;
  PQoptions:       TPQoptions;
  PQstatus:        TPQstatus;
  PQerrorMessage:  TPQerrorMessage;
  PQsocket:        TPQsocket;
  PQbackendPID:    TPQbackendPID;
  PQtrace:         TPQtrace;
  PQuntrace:       TPQuntrace;
  PQsetNoticeProcessor: TPQsetNoticeProcessor;

{ === in fe-exec.c === }
  PQexec:          TPQexec;
  PQnotifies:      TPQnotifies;
  PQnotifyFree:    TPQnotifyFree;
  PQsendQuery:     TPQsendQuery;
  PQgetResult:     TPQgetResult;
  PQisBusy:        TPQisBusy;
  PQconsumeInput:  TPQconsumeInput;
  PQgetline:       TPQgetline;
  PQputline:       TPQputline;
  PQgetlineAsync:  TPQgetlineAsync;
  PQputnbytes:     TPQputnbytes;
  PQendcopy:       TPQendcopy;
  PQfn:            TPQfn;
  PQresultStatus:  TPQresultStatus;
  PQresultErrorMessage: TPQresultErrorMessage;
  PQntuples:       TPQntuples;
  PQnfields:       TPQnfields;
  PQbinaryTuples:  TPQbinaryTuples;
  PQfname:         TPQfname;
  PQfnumber:       TPQfnumber;
  PQftype:         TPQftype;
  PQfsize:         TPQfsize;
  PQfmod:          TPQfmod;
  PQcmdStatus:     TPQcmdStatus;
  PQoidValue:      TPQoidValue;
  PQoidStatus:     TPQoidStatus;
  PQcmdTuples:     TPQcmdTuples;
  PQgetvalue:      TPQgetvalue;
  PQgetlength:     TPQgetlength;
  PQgetisnull:     TPQgetisnull;
  PQclear:         TPQclear;
  PQmakeEmptyPGresult:  TPQmakeEmptyPGresult;

{ === in fe-lobj.c === }
  lo_open:         Tlo_open;
  lo_close:        Tlo_close;
  lo_read:         Tlo_read;
  lo_write:        Tlo_write;
  lo_lseek:        Tlo_lseek;
  lo_creat:        Tlo_creat;
  lo_tell:         Tlo_tell;
  lo_unlink:       Tlo_unlink;
  lo_import:       Tlo_import;
  lo_export:       Tlo_export;

function PgSqlLoadLib: Boolean;

const
  DLL: string = DEFAULT_DLL_LOCATION;
  hDLL: THandle = 0;
  LibLoaded: Boolean = False;

implementation

uses SysUtils, ZDBaseConst;

{ Initialize PostgreSQL dynamic library }
function PgSqlLoadLib: Boolean;
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
{ ===	in fe-connect.c === }
    @PQconnectdb    := GetProcAddress(hDLL,'PQconnectdb');
    @PQsetdbLogin   := GetProcAddress(hDLL,'PQsetdbLogin');
    @PQconndefaults := GetProcAddress(hDLL,'PQconndefaults');
    @PQfinish       := GetProcAddress(hDLL,'PQfinish');
    @PQreset        := GetProcAddress(hDLL,'PQreset');
    @PQrequestCancel := GetProcAddress(hDLL,'PQrequestCancel');
    @PQdb           := GetProcAddress(hDLL,'PQdb');
    @PQuser         := GetProcAddress(hDLL,'PQuser');
    @PQpass         := GetProcAddress(hDLL,'PQpass');
    @PQhost         := GetProcAddress(hDLL,'PQhost');
    @PQport         := GetProcAddress(hDLL,'PQport');
    @PQtty          := GetProcAddress(hDLL,'PQtty');
    @PQoptions      := GetProcAddress(hDLL,'PQoptions');
    @PQstatus       := GetProcAddress(hDLL,'PQstatus');
    @PQerrorMessage := GetProcAddress(hDLL,'PQerrorMessage');
    @PQsocket       := GetProcAddress(hDLL,'PQsocket');
    @PQbackendPID   := GetProcAddress(hDLL,'PQbackendPID');
    @PQtrace        := GetProcAddress(hDLL,'PQtrace');
    @PQuntrace      := GetProcAddress(hDLL,'PQuntrace');
    @PQsetNoticeProcessor := GetProcAddress(hDLL,'PQsetNoticeProcessor');

{ === in fe-exec.c === }
    @PQexec         := GetProcAddress(hDLL,'PQexec');
    @PQnotifies     := GetProcAddress(hDLL,'PQnotifies');
    @PQnotifyFree   := GetProcAddress(hDLL,'PQnotifyFree');
    @PQsendQuery    := GetProcAddress(hDLL,'PQsendQuery');
    @PQgetResult    := GetProcAddress(hDLL,'PQgetResult');
    @PQisBusy       := GetProcAddress(hDLL,'PQisBusy');
    @PQconsumeInput := GetProcAddress(hDLL,'PQconsumeInput');
    @PQgetline      := GetProcAddress(hDLL,'PQgetline');
    @PQputline      := GetProcAddress(hDLL,'PQputline');
    @PQgetlineAsync := GetProcAddress(hDLL,'PQgetlineAsync');
    @PQputnbytes    := GetProcAddress(hDLL,'PQputnbytes');
    @PQendcopy      := GetProcAddress(hDLL,'PQendcopy');
    @PQfn           := GetProcAddress(hDLL,'PQfn');
    @PQresultStatus := GetProcAddress(hDLL,'PQresultStatus');
    @PQresultErrorMessage := GetProcAddress(hDLL,'PQresultErrorMessage');
    @PQntuples      := GetProcAddress(hDLL,'PQntuples');
    @PQnfields      := GetProcAddress(hDLL,'PQnfields');
    @PQbinaryTuples := GetProcAddress(hDLL,'PQbinaryTuples');
    @PQfname        := GetProcAddress(hDLL,'PQfname');
    @PQfnumber      := GetProcAddress(hDLL,'PQfnumber');
    @PQftype        := GetProcAddress(hDLL,'PQftype');
    @PQfsize        := GetProcAddress(hDLL,'PQfsize');
    @PQfmod         := GetProcAddress(hDLL,'PQfmod');
    @PQcmdStatus    := GetProcAddress(hDLL,'PQcmdStatus');
    @PQoidValue     := GetProcAddress(hDLL,'PQoidValue');
    @PQoidStatus    := GetProcAddress(hDLL,'PQoidStatus');
    @PQcmdTuples    := GetProcAddress(hDLL,'PQcmdTuples');
    @PQgetvalue     := GetProcAddress(hDLL,'PQgetvalue');
    @PQgetlength    := GetProcAddress(hDLL,'PQgetlength');
    @PQgetisnull    := GetProcAddress(hDLL,'PQgetisnull');
    @PQclear        := GetProcAddress(hDLL,'PQclear');
    @PQmakeEmptyPGresult := GetProcAddress(hDLL,'PQmakeEmptyPGresult');

{ === in fe-lobj.c === }
    @lo_open        := GetProcAddress(hDLL,'lo_open');
    @lo_close       := GetProcAddress(hDLL,'lo_close');
    @lo_read        := GetProcAddress(hDLL,'lo_read');
    @lo_write       := GetProcAddress(hDLL,'lo_write');
    @lo_lseek       := GetProcAddress(hDLL,'lo_lseek');
    @lo_creat       := GetProcAddress(hDLL,'lo_creat');
    @lo_tell        := GetProcAddress(hDLL,'lo_tell');
    @lo_unlink      := GetProcAddress(hDLL,'lo_unlink');
    @lo_import      := GetProcAddress(hDLL,'lo_import');
    @lo_export      := GetProcAddress(hDLL,'lo_export');

    Result := True;
  end else
    raise Exception.Create(Format(SLibraryNotFound,[DLL]));
end;

initialization

finalization
  if (hDLL <> 0) and LibLoaded then
    FreeLibrary(hDLL);
end.
