{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{                 DB2 direct class API                   }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZDirDb2Sql;

interface

uses Windows, Classes, SysUtils, Db, ZLibDb2Sql, ZDirSql, ZTransact, ZSqlTypes;

{$INCLUDE ..\Zeos.inc}

type
  { Declare the SQL Object }
  TSqlVar = record
    Name:      string;
    Data:      PChar;
    DataType:  SmallInt;
    DataSize:  SmallInt;
    DataLen:   Integer;
    TypeCode:  SmallInt;
    ColType:   TFieldType;
    Scale:     SmallInt;
  end;
  PSqlVar = ^TSqlVar;

  TSqlVars = record
    AllocNum:  SmallInt;
    ActualNum: SmallInt;
    Variables: array[0..0] of TSqlVar;
  end;
  PSqlVars = ^TSqlVars;

  { Direct connection to Oracle database }
  TDirDb2SqlConnect = class (TDirConnect)
  private
    FHandle: SQLHENV;
  public
    procedure Connect; override;
    procedure Disconnect; override;

    property Handle: SQLHENV read FHandle;
  end;

  { Transaction types }
  TZDb2SqlTransIsolation = (dtDefault, dtReadUncommited, dtReadCommited,
    dtRepeatableRead, dtSerializable);

  { Direct DB2 transaction }
  TDirDb2SqlTransact = class (TDirTransact)
  private
    FHandle: SQLHDBC;
    FError: string;
    FTransIsolation: TZDb2SqlTransIsolation;
  protected
    function GetErrorMsg: ShortString; override;
  public
    constructor Create(AConnect: TDirDb2SqlConnect);

    function CheckError(HandleType: Integer; Handle: SQLHANDLE;
      Status: Integer; var Message: string): Boolean;

    procedure Open; override;
    procedure Close; override;
    procedure StartTransaction; override;
    procedure EndTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;

    property Handle: SQLHDBC read FHandle;
    property TransIsolation: TZDb2SqlTransIsolation read FTransIsolation
      write FTransIsolation;
  end;

{ Maximum sqlvar buffer }
const MAX_SQLVAR = 50;

type
  { Direct DB2 Query }
  TDirDb2SqlQuery = class(TDirQuery)
  private
    FHandle: SQLHSTMT;

    FOutSqlVars: PSqlVars;
    FInSqlVars: PSqlVars;
    FError: string;
  protected
    function GetErrorMsg: ShortString; override;
  public
    constructor Create(AConnect: TDirDb2SqlConnect; ATransact: TDirDb2SqlTransact);
    destructor  Destroy; override;

    function  Execute: LongInt; override;
    function  ExecuteParams(Params: TVarRecArray;
      ParamCount: Integer): LongInt; override;
    procedure Open; override;
    procedure Close; override;
    function  CreateBlobObject: TDirBlob; override;

    procedure ShowDatabases(DatabaseName: ShortString); override;
    procedure ShowTables(TableName: ShortString); override;
    procedure ShowColumns(TableName, ColumnName: ShortString); override;
    procedure ShowIndexes(TableName: ShortString); override;

    procedure First; override;
    procedure Last; override;
    procedure Prev; override;
    procedure Next; override;
    procedure Go(Num: Integer); override;

    function  FieldCount: Integer; override;
    function  RecordCount: Integer; override;

    function  FieldName(FieldNum: Integer): ShortString; override;
    function  FieldSize(FieldNum: Integer): Integer; override;
    function  FieldMaxSize(FieldNum: Integer): Integer; override;
    function  FieldDecimals(FieldNum: Integer): Integer; override;
    function  FieldType(FieldNum: Integer): Integer; override;
    function  FieldTypeCode(FieldNum: Integer): Integer;
    function  FieldDataType(FieldNum: Integer): TFieldType; override;
    function  FieldIsNull(FieldNum: Integer): Boolean; override;
    function  Field(FieldNum: Integer): string; override;
    function  FieldBuffer(FieldNum: Integer): PChar; override;

    property Handle: SQLHSTMT read FHandle;
  end;

  { Class for interbase large object }
  TDirDb2SqlBlob = class(TDirBlob)
  private
    FPosition: LongInt;
    FError: string;
    FStatementHandle: SQLHSTMT;
    FBlobType: SmallInt;
    FSize: LongInt;
  protected
    function GetErrorMsg: ShortString; override;
    function GetPosition: LongInt; override;
  public
    constructor Create(AConnect: TDirConnect; ATransact: TDirTransact;
      AHandle: TBlobHandle);

    procedure Open(Mode: Integer); override;
    procedure Close; override;
    procedure CreateBlob; override;
    procedure DropBlob; override;

    function Read(Buffer: PChar; Length: Integer): Integer; override;
    function Write(Buffer: PChar; Length: Integer): Integer; override;

    property StatementHandle: SQLHSTMT read FStatementHandle;
  end;

{ Convert Db2 field types to delphi field types }
function Db2SqlToDelphiType(Value: string; Size, Prec: Integer;
  var BlobType: TBlobType): TFieldType;

{ Monitor list }
var MonitorList: TZMonitorList;

implementation

uses ZExtra, ZDBaseConst, ZSqlExtra;

{**************** TDirDb2SqlConnect implementation ************}

{ Connect to existed database }
procedure TDirDb2SqlConnect.Connect;
begin
  inherited Connect;
  if hDll = 0 then
  begin
    Db2SqlLoadLib;
  end;
  { Connect database }
  FHandle := 0;
  if SqlAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, @FHandle) = SQL_SUCCESS then
  begin
    SetStatus(csOk);
    SetActive(True);
  end;
end;

{ Disconnect from database }
procedure TDirDb2SqlConnect.Disconnect;
begin
  if Active then
  begin
    SqlFreeHandle(SQL_HANDLE_ENV, FHandle);
    FHandle := 0;
    SetActive(False);
  end;
end;

{************* TDirDb2SqlTransact implementation *************}

{ Class constructor }
constructor TDirDb2SqlTransact.Create(AConnect: TDirDb2SqlConnect);
begin
  inherited Create;
  FHandle := 0;
  Connect := AConnect;
end;

{ Get sql-server error message }
function TDirDb2SqlTransact.GetErrorMsg: ShortString;
begin
  if Status <> csOk then
    Result := FError
  else
    Result := '';
end;

{ Process error status }
function TDirDb2SqlTransact.CheckError(HandleType: Integer; Handle: SQLHANDLE;
  Status: Integer; var Message: string): Boolean;
var
  SqlState: array[0..255] of Char;
  SqlCode: Integer;
  ErrorBuf: array[0..1024] of Char;
  I, ErrorBufLen: SmallInt;
begin
  Result := False;
  Message := '';
  case Status of
    SQL_SUCCESS:
      Result := True;
    SQL_SUCCESS_WITH_INFO:
      begin
        SQLGetDiagRec(HandleType, Handle, 1, SqlState, @SqlCode,
          ErrorBuf, 255, @ErrorBufLen);
        Result := True;
        Message := MemPas(ErrorBuf, ErrorBufLen);
      end;
    SQL_NEED_DATA:
      Message := 'SQL_NEED_DATA';
    SQL_NO_DATA:
      begin
        Result := True;
        Message := 'SQL_NO_DATA';
      end;
    SQL_ERROR:
      begin
        I := 1;
        while (SQLGetDiagRec(HandleType, Handle, I, SqlState, @SqlCode,
          ErrorBuf, 1024, @ErrorBufLen) = SQL_SUCCESS) do
        begin
          if Message <> '' then Message := Message + #13;
          Message := Message + MemPas(ErrorBuf, ErrorBufLen);
          Inc(I);
        end;
      end;
    SQL_INVALID_HANDLE:
      Message := 'SQL_INVALID_HANDLE';
    SQL_STILL_EXECUTING:
      Message := 'SQL_STILL_EXECUTING';
  end;
end;

{ Connect transaction }
procedure TDirDb2SqlTransact.Open;
label ErrorProc;
var
  Status: Integer;
  Db2Connect: TDirDb2SqlConnect;
begin
  inherited Open;
  SetStatus(csFail);
  if not Assigned(Connect) or not Connect.Active then
    Exit;
  Db2Connect := TDirDb2SqlConnect(Connect);

  FHandle := 0;
  Status := SqlAllocHandle(SQL_HANDLE_DBC, DB2Connect.Handle, @FHandle);
  if not CheckError(SQL_HANDLE_DBC, FHandle, Status, FError) then
    goto ErrorProc;

  Status := SqlConnect(FHandle, PChar(string(Connect.Database)), SQL_NTS,
    PChar(string(Connect.Login)), SQL_NTS, PChar(string(Connect.Passwd)), SQL_NTS);
  if not CheckError(SQL_HANDLE_DBC, FHandle, Status, FError) then
    goto ErrorProc;

  MonitorList.InvokeEvent(Format('CONNECT %s',[Connect.Database]), 'OK.', False);

  StartTransaction;
  SetActive(inherited Status = csOk);
  Exit;

  { Process error status }
ErrorProc:
  SqlFreeHandle(SQL_HANDLE_DBC, FHandle);
  FHandle := 0;
  MonitorList.InvokeEvent(Format('CONNECT %s',[Connect.Database]), Error, True);
end;

{ Disconnect transaction }
procedure TDirDb2SqlTransact.Close;
var
  Status: Integer;
begin
  EndTransaction;

  Status := SqlDisconnect(FHandle);
  CheckError(SQL_HANDLE_DBC, FHandle, Status, FError);

  SqlFreeHandle(SQL_HANDLE_DBC, FHandle);
  FHandle := 0;

  SetActive(False);
end;

{ Start transaction }
procedure TDirDB2SqlTransact.StartTransaction;
var
  Status: Integer;
begin
  { Set startup values }
  SetStatus(csFail);
  if FHandle <> 0 then
  begin
    if not TransactSafe then
      Status := SqlSetConnectAttr(FHandle, SQL_ATTR_AUTOCOMMIT,
        Pointer(SQL_AUTOCOMMIT_ON), SQL_NTS)
    else begin
      Status := SqlSetConnectAttr(FHandle, SQL_ATTR_AUTOCOMMIT,
        Pointer(SQL_AUTOCOMMIT_OFF), SQL_NTS);
      case TransIsolation of
        dtReadUncommited:
          Status := SqlSetConnectAttr(FHandle, SQL_ATTR_TXN_ISOLATION,
            Pointer(SQL_TXN_READ_UNCOMMITTED), SQL_NTS);
        dtReadCommited:
          Status := SqlSetConnectAttr(FHandle, SQL_ATTR_TXN_ISOLATION,
            Pointer(SQL_TXN_READ_COMMITTED), SQL_NTS);
        dtRepeatableRead:
          Status := SqlSetConnectAttr(FHandle, SQL_ATTR_TXN_ISOLATION,
            Pointer(SQL_TXN_REPEATABLE_READ), SQL_NTS);
        dtSerializable:
          Status := SqlSetConnectAttr(FHandle, SQL_ATTR_TXN_ISOLATION,
            Pointer(SQL_TXN_SERIALIZABLE), SQL_NTS);
      end;
    end;

    if CheckError(SQL_HANDLE_DBC, FHandle, Status, FError) then
      SetStatus(csOk);
    MonitorList.InvokeEvent('BEGIN TRANSACTION', Error, Error <> '');
  end;
end;

{ End transaction }
procedure TDirDb2SqlTransact.EndTransaction;
var
  Status: Integer;
begin
  { Set startup values }
  SetStatus(csFail);
  if FHandle <> 0 then
  begin
    Status := SQLEndTran(SQL_HANDLE_DBC, FHandle, SQL_ROLLBACK);
    if CheckError(SQL_HANDLE_DBC, FHandle, Status, FError) then
      SetStatus(csOk);
    MonitorList.InvokeEvent('END TRANSACTION', Error, Error <> '');
  end;
end;

{ Commit transaction }
procedure TDirDb2SqlTransact.Commit;
var
  Status: Integer;
begin
  SetStatus(csFail);
  if Active then
  begin
    Status := SQLEndTran(SQL_HANDLE_DBC, FHandle, SQL_COMMIT);
    if CheckError(SQL_HANDLE_DBC, FHandle, Status, FError) then
      SetStatus(csOk);
    MonitorList.InvokeEvent('COMMIT', Error, Error <> '');
  end;
end;

{ Rollback transaction }
procedure TDirDb2SqlTransact.Rollback;
var
  Status: Integer;
begin
  SetStatus(csFail);
  if Active then
  begin
    Status := SQLEndTran(SQL_HANDLE_DBC, FHandle, SQL_ROLLBACK);
    if CheckError(SQL_HANDLE_DBC, FHandle, Status, FError) then
      SetStatus(csOk);
    MonitorList.InvokeEvent('ROLLBACK', Error, Error <> '');
  end;
end;

{************* TDirDb2SqlQuery implementation ************}

{ Count length of SqlVars variable }
function SqlVarsLength(Count: Integer): Integer;
begin
  Result := SizeOf(TSqlVars) + Count * SizeOf(TSqlVar);
end;

{ Class constructor }
constructor TDirDb2SqlQuery.Create(AConnect: TDirDb2SqlConnect;
  ATransact: TDirDb2SqlTransact);
begin
  inherited Create;
  Connect := AConnect;
  Transact := ATransact;

  GetMem(FOutSqlVars, SqlVarsLength(MAX_SQLVAR));
  FillChar(FOutSqlVars^, SqlVarsLength(MAX_SQLVAR), 0);
  FOutSqlVars.AllocNum := MAX_SQLVAR;

  GetMem(FInSqlVars, SqlVarsLength(MAX_SQLVAR));
  FillChar(FInSqlVars^, SqlVarsLength(MAX_SQLVAR), 0);
  FInSqlVars.AllocNum := MAX_SQLVAR;
end;

{ Class destructor }
destructor TDirDb2SqlQuery.Destroy;
begin
  inherited Destroy;
  FreeMem(FInSqlVars);
  FreeMem(FOutSqlVars);
end;

{ Get an error message }
function TDirDb2SqlQuery.GetErrorMsg: ShortString;
begin
  if not (Status in [qsCommandOk, qsTuplesOk]) then
    Result := FError
  else
    Result := '';
end;

{ Execute an SQL statement }
function TDirDb2SqlQuery.Execute: LongInt;
label ErrorProc;
var
  Db2Connect: TDirDb2SqlConnect;
  Db2Transact: TDirDb2SqlTransact;
  Status: Integer;
{$IFDEF DELETE_QUERY_SPACES}
  Temp: string;
{$ENDIF}
  TempRows: Integer;
begin
  inherited Execute;

  SetStatus(qsFail);
  Result := 0;
  if Assigned(Connect) and Assigned(Transact) and Transact.Active then
  begin
    Db2Connect := TDirDb2SqlConnect(Connect);
    Db2Transact := TDirDb2SqlTransact(Transact);

    FHandle := 0;
    Status := SqlAllocHandle(SQL_HANDLE_STMT, Db2Connect.Handle, @FHandle);
    if not Db2Transact.CheckError(SQL_HANDLE_STMT, FHandle, Status, FError) then
      goto ErrorProc;

{$IFDEF DELETE_QUERY_SPACES}
    Temp := ClearSpaces(Sql);
    Status := SQLExecDirect(FHandle, PChar(Temp), Length(Temp));
{$ELSE}
    Status := SQLExecDirect(FHandle, PChar(Sql), Length(Sql));
{$ENDIF}
    if not Db2Transact.CheckError(SQL_HANDLE_STMT, FHandle, Status, FError) then
      goto ErrorProc;

    SetStatus(qsCommandOk);

    TempRows := 0;
    SQLRowCount(FHandle, @TempRows);
    SetAffectedRows(TempRows);

ErrorProc:
    MonitorList.InvokeEvent(Sql, Error, Error <> '');
    SqlFreeHandle(SQL_HANDLE_STMT, FHandle);
    FHandle := 0;
  end;
end;

{ Execute an SQL statement with params }
function TDirDb2SqlQuery.ExecuteParams(Params: TVarRecArray; ParamCount: Integer): LongInt;
label ErrorProc;
var
  I: Integer;
  Param: TVarRec;
  RecordBlob: PRecordBlob;
  BlobType: SQLSMALLINT;
  BlobInd: SQLINTEGER;
  Db2Connect: TDirDb2SqlConnect;
  Db2Transact: TDirDb2SqlTransact;
  Status: Integer;
{$IFDEF DELETE_QUERY_SPACES}
  Temp: string;
{$ENDIF}
  TempRows: Integer;
begin
  inherited Execute;

  SetStatus(qsFail);
  Result := 0;
  if not Assigned(Connect) or not Assigned(Transact) or not Transact.Active then
    Exit;

  Db2Connect := TDirDb2SqlConnect(Connect);
  Db2Transact := TDirDb2SqlTransact(Transact);

  FHandle := 0;
  Status := SqlAllocHandle(SQL_HANDLE_STMT, Db2Connect.Handle, @FHandle);
  if not Db2Transact.CheckError(SQL_HANDLE_STMT, FHandle, Status, FError) then
    goto ErrorProc;

{ Prepare an sql statement }
{$IFDEF DELETE_QUERY_SPACES}
  Temp := ClearSpaces(Sql);
  Status := SQLPrepare(FHandle, PChar(Temp), SQL_NTS);
{$ELSE}
  Status := SQLPrepare(FHandle, PChar(Sql), SQL_NTS);
{$ENDIF}
  if not DB2Transact.CheckError(SQL_HANDLE_STMT, FHandle, Status, FError) then
    goto ErrorProc;

  for I := 0 to ParamCount-1 do
  begin
    Param := Params[I];
    RecordBlob := PRecordBlob(Param.VPointer);

    BlobType := RecordBlob.Handle.PtrEx - 1000;
    case BlobType of
      SQL_BLOB_LOCATOR: BlobType := SQL_BLOB;
      SQL_CLOB_LOCATOR: BlobType := SQL_CLOB;
      SQL_DBCLOB_LOCATOR: BlobType := SQL_DBCLOB;
    end;

    BlobInd := SQL_DATA_AT_EXEC;
    Status := SQLBindParameter(FHandle, I+1, SQL_PARAM_INPUT, SQL_BINARY,
      BlobType, RecordBlob.Size, 0, RecordBlob, RecordBlob.Size, @BlobInd);
    if not Db2Transact.CheckError(SQL_HANDLE_STMT, FHandle, Status, FError) then
      goto ErrorProc;
  end;

  Status := SQLExecute(FHandle);

  if Status = SQL_NEED_DATA then
  begin
    Status := SQLParamData(FHandle, @RecordBlob);
    while Status = SQL_NEED_DATA do
    begin
      Status := SQLPutData(FHandle, RecordBlob.Data, RecordBlob.Size);
      if not Db2Transact.CheckError(SQL_HANDLE_STMT, FHandle, Status, FError) then
        goto ErrorProc;
      Status := SQLParamData(FHandle, @RecordBlob);
    end;
  end;

  if not Db2Transact.CheckError(SQL_HANDLE_STMT, FHandle, Status, FError) then
    goto ErrorProc;

  SetStatus(qsCommandOk);

  TempRows := 0;
  SQLRowCount(FHandle, @TempRows);
  SetAffectedRows(TempRows);

ErrorProc:
  MonitorList.InvokeEvent(Sql, Error, Error <> '');
  SqlFreeHandle(SQL_HANDLE_STMT, FHandle);
  FHandle := 0;
end;

{ Open a sql query with result set }
procedure TDirDb2SqlQuery.Open;
label ErrorProc;
var
  I, Status: Integer;
  Db2Connect: TDirDb2SqlConnect;
  Db2Transact: TDirDb2SqlTransact;
  SqlVar: PSqlVar;
  TempBuffer: array[0..255] of Char;
  TempBufferLen: SmallInt;
{$IFDEF DELETE_QUERY_SPACES}
  Temp: string;
{$ENDIF}
  Len: Integer;
begin
  inherited Open;

  { Check connect and transaction status }
  SetStatus(qsFail);
  if not Assigned(Connect) or not Assigned(Transact)
    or not (Connect.Active and Transact.Active) then
    Exit;

  Db2Connect := TDirDb2SqlConnect(Connect);
  Db2Transact := TDirDb2SqlTransact(Transact);

  { Allocate an sql statement }
  FHandle := 0;
  Status := SqlAllocHandle(SQL_HANDLE_STMT, Db2Connect.Handle, @FHandle);
  if not DB2Transact.CheckError(SQL_HANDLE_STMT, FHandle, Status, FError) then
    goto ErrorProc;

  { Prepare an sql statement }
{$IFDEF DELETE_QUERY_SPACES}
  Temp := ClearSpaces(Sql);
  Status := SQLPrepare(FHandle, PChar(Temp), SQL_NTS);
{$ELSE}
  Status := SQLPrepare(FHandle, PChar(Sql), SQL_NTS);
{$ENDIF}
  if not DB2Transact.CheckError(SQL_HANDLE_STMT, FHandle, Status, FError) then
    goto ErrorProc;

  { Resize SQLVERS structure if needed }
  SQLNumResultCols(FHandle, @FOutSqlVars.ActualNum);
  if FOutSqlVars.ActualNum > FOutSqlVars.AllocNum then
  begin
    ReallocMem(FOutSqlVars, SqlVarsLength(FOutSqlVars.ActualNum));
    FOutSqlVars.AllocNum := FOutSqlVars.ActualNum;
  end;

  { Allocate memory for result set }
  for I := 0 to FOutSqlVars.ActualNum-1 do
  begin
    SqlVar := @FOutSqlVars.Variables[I];
    SqlVar.Data := nil;
    SqlVar.DataSize := 0;

    Status := SQLDescribeCol(FHandle, I+1, TempBuffer, 255, @TempBufferLen,
      @SqlVar.DataType, @SqlVar.DataSize, @SqlVar.Scale, nil);
    SqlVar.Name := MemPas(TempBuffer, TempBufferLen);

    case SqlVar.DataType of
      SQL_CHAR, SQL_VARCHAR, SQL_WCHAR, SQL_WVARCHAR, SQL_WLONGVARCHAR:
        begin
          if SqlVar.DataSize < 255 then
            SqlVar.ColType := ftString
          else
            SqlVar.ColType := ftMemo;
        end;
      {$IFNDEF VER100}
      SQL_BIGINT:
        SqlVar.ColType := ftLargeInt;
      {$ENDIF}
      SQL_NUMERIC, SQL_DECIMAL:
        begin
          if (SqlVar.Scale = 0) then
            SqlVar.ColType := ftInteger
          else
            SqlVar.ColType := ftFloat;
        end;
      SQL_INTEGER, SQL_SMALLINT {$IFDEF VER100}, SQL_BIGINT{$ENDIF}:
        SqlVar.ColType := ftInteger;
      SQL_FLOAT, SQL_REAL, SQL_DOUBLE:
        SqlVar.ColType := ftFloat;
      SQL_DATETIME, SQL_TYPE_TIMESTAMP:
        SqlVar.ColType := ftDateTime;
      SQL_TYPE_DATE:
        SqlVar.ColType := ftDate;
      SQL_TYPE_TIME:
        SqlVar.ColType := ftTime;
      SQL_CLOB, SQL_DBCLOB:
        SqlVar.ColType := ftMemo;
      SQL_BLOB:
        SqlVar.ColType := ftBlob;
      else
        SqlVar.ColType := ftUnknown;
    end;

    SqlVar.TypeCode := SqlVar.DataType;
    Len := 0;
    case SqlVar.ColType of
      ftInteger:
        begin
          SqlVar.TypeCode := SQL_INTEGER;
          Len := SizeOf(SQLINTEGER);
        end;
      ftFloat:
        begin
          SqlVar.TypeCode := SQL_DOUBLE;
          Len := SizeOf(SQLDOUBLE);
        end;
(*
      {$IFNDEF VER100}
      ftLargeInt:
        begin
          SqlVar.TypeCode := SQL_BIGINT;
          Len := SizeOf(Int64);
        end;
      {$ENDIF}
*)
      ftDate:
        begin
          SqlVar.TypeCode := SQL_TYPE_DATE;
          Len := SQL_DATE_LEN;
        end;
      ftTime:
        begin
          SqlVar.TypeCode := SQL_TYPE_TIME;
          Len := SQL_TIME_LEN;
        end;
      ftDateTime:
        begin
          SqlVar.TypeCode := SQL_TYPE_TIMESTAMP;
          Len := SQL_TIMESTAMP_LEN;
        end;
      ftString:
        begin
          SqlVar.TypeCode := SQL_CHAR;
          Len := SqlVar.DataSize + 1;
        end;
      ftBlob, ftMemo:
        begin
          case SqlVar.DataType of
            SQL_BLOB: SqlVar.TypeCode := SQL_BLOB_LOCATOR;
            SQL_CLOB: SqlVar.TypeCode := SQL_CLOB_LOCATOR;
            SQL_DBCLOB: SqlVar.TypeCode := SQL_DBCLOB_LOCATOR;
          end;
          Len := SizeOf(SQLINTEGER);
        end;
      ftUnknown:
        Continue;
    end;

    GetMem(SqlVar.Data, Len);

    if Len > 0 then
      Status := SQLBindCol(FHandle, I+1, SqlVar.TypeCode, SqlVar.Data, Len,
        @SqlVar.DataLen);
    if not Db2Transact.CheckError(SQL_HANDLE_STMT, FHandle, Status, FError) then
      goto ErrorProc;
  end;

  Status := SQLExecute(FHandle);
  if not Db2Transact.CheckError(SQL_HANDLE_STMT, FHandle, Status, FError) then
    goto ErrorProc;

  SetActive(True);
  SetStatus(qsTuplesOk);

  SetBOF(Status <> SQL_SUCCESS);
  SetEOF(Status <> SQL_SUCCESS);
  Next;
  Exit;

ErrorProc:

  for I := 0 to FOutSqlVars.ActualNum-1 do
  begin
    SqlVar := @FOutSqlVars.Variables[I];

    FreeMem(SqlVar.Data);
    SqlVar.Data := nil;
  end;

  SqlFreeHandle(SQL_HANDLE_STMT, FHandle);
  FHandle := 0;
end;

{ Close a sql query with result set }
procedure TDirDb2SqlQuery.Close;
var
  I: Integer;
  SqlVar: PSqlVar;
begin
  if not Active then Exit;
  inherited Close;

  { Check connect and transaction status }
  SetStatus(qsTuplesOk);
  if not Assigned(Connect) or not Assigned(Transact)
    or not Transact.Active then Exit;

  { Free sql statement }
  SqlFreeHandle(SQL_HANDLE_STMT, FHandle);
  FHandle := 0;

  { Free allocated memory }
  for I := 0 to FOutSqlVars.ActualNum-1 do
  begin
    SqlVar := @FOutSqlVars.Variables[I];

    FreeMem(SqlVar.Data);
    SqlVar.Data := nil;
  end;
  FOutSqlVars.ActualNum := 0;
end;

{ Create linked blob object }
function TDirDb2SqlQuery.CreateBlobObject: TDirBlob;
var
  TempHandle: TBlobHandle;
begin
  FillChar(TempHandle, SizeOf(TBlobHandle), 0);
  Result := TDirDb2SqlBlob.Create(Connect, Transact, TempHandle);
end;

function TDirDb2SqlQuery.Field(FieldNum: Integer): string;
var
  SqlVar: PSqlVar;
  OldSep: Char;
  TempDate: PSQL_DATE_STRUCT;
  TempTime: PSQL_TIME_STRUCT;
  TempDateTime: PSQL_TIMESTAMP_STRUCT;
  TempDate1: TDateTime;
  TempHandle: TBlobHandle;
begin
  Result := '';

  if not Active or (FieldNum >= FOutSqlVars.ActualNum) then Exit;
  SqlVar := @FOutSqlVars.Variables[FieldNum];

  if (SqlVar.DataLen < 0) or (SqlVar.Data = nil) then Exit;

  OldSep := DecimalSeparator;
  DecimalSeparator := '.';
  case SqlVar.ColType of
    ftInteger:
      Result := IntToStr(PLongInt(SqlVar.Data)^);
    ftFloat:
      Result := FloatToStr(PDouble(SqlVar.Data)^);
    ftString:
      Result := MemPas(SqlVar.Data, SqlVar.DataLen);
    ftDate:
      begin
        TempDate := PSQL_DATE_STRUCT(SqlVar.Data);
        TempDate1 := EncodeDate(TempDate.year, TempDate.month, TempDate.day);
        Result := FormatSqlDate(TempDate1);
      end;
    ftTime:
      begin
        TempTime := PSQL_TIME_STRUCT(SqlVar.Data);
        TempDate1 := EncodeTime(TempTime.hour, TempTime.minute, TempTime.second, 0);
        Result := FormatSqlTime(TempDate1);
      end;
    ftDateTime:
      begin
        TempDateTime := PSQL_TIMESTAMP_STRUCT(SqlVar.Data);
        TempDate1 := EncodeDate(TempDateTime.year, TempDateTime.month,
          TempDateTime.day) + EncodeTime(TempDateTime.hour, TempDateTime.minute,
          TempDateTime.second, 0);
        Result := DateTimeToSqlDate(TempDate1);
      end;
    {$IFNDEF VER100}
    ftLargeInt:
      Result := IntToStr(PInt64(SqlVar.Data)^);
    {$ENDIF}
    ftMemo, ftBlob:
      begin
        TempHandle.Ptr := PInteger(SqlVar.Data)^;
        TempHandle.PtrEx := SqlVar.TypeCode + 1000;
        with TDirDb2SqlBlob.Create(Connect, Transact, TempHandle) do
        try
          Result := Value;
        finally
          Free;
        end;
      end;
  end;
  DecimalSeparator := OldSep;
end;

{ Check if field is null }
function TDirDb2SqlQuery.FieldIsNull(FieldNum: Integer): Boolean;
var
  SqlVar: PSqlVar;
begin
  Result := True;

  if not Active or (FieldNum >= FOutSqlVars.ActualNum) then Exit;
  SqlVar := @FOutSqlVars.Variables[FieldNum];
  Result := (SqlVar.DataLen < 0) and (SqlVar.Data <> nil);
end;

{ Get field buffer }
function TDirDb2SqlQuery.FieldBuffer(FieldNum: Integer): PChar;
var
  SqlVar: PSqlVar;
begin
  Result := nil;

  if not Active or (FieldNum >= FOutSqlVars.ActualNum) then Exit;
  SqlVar := @FOutSqlVars.Variables[FieldNum];

  Result := SqlVar.Data;
end;

{ Get field type }
function TDirDb2SqlQuery.FieldType(FieldNum: Integer): Integer;
begin
  Result := 0;
  if not Active or (FieldNum >= FOutSqlVars.ActualNum) then Exit;
  Result := FOutSqlVars.Variables[FieldNum].DataType;
end;

{ Get field type code }
function TDirDb2SqlQuery.FieldTypeCode(FieldNum: Integer): Integer;
begin
  Result := 0;
  if not Active or (FieldNum >= FOutSqlVars.ActualNum) then Exit;
  Result := FOutSqlVars.Variables[FieldNum].TypeCode;
end;

{ Get field delphi compatible type }
function TDirDb2SqlQuery.FieldDataType(FieldNum: Integer): TFieldType;
begin
  Result := ftUnknown;
  if not Active or (FieldNum >= FOutSqlVars.ActualNum) then Exit;
  Result := FOutSqlVars.Variables[FieldNum].ColType;
end;

{ Get field count }
function TDirDb2SqlQuery.FieldCount: Integer;
begin
  Result := 0;
  if Active then
    Result := FOutSqlVars.ActualNum;
end;

{ Get field maximum size }
function TDirDb2SqlQuery.FieldMaxSize(FieldNum: Integer): Integer;
begin
  Result := 0;
  if not Active or (FieldNum >= FOutSqlVars.ActualNum) then Exit;
  Result := FOutSqlVars.Variables[FieldNum].DataSize;
end;

{ Get fields decimals }
function TDirDb2SqlQuery.FieldDecimals(FieldNum: Integer): Integer;
begin
  Result := 0;
  if not Active or (FieldNum >= FOutSqlVars.ActualNum) then Exit;
  Result := FOutSqlVars.Variables[FieldNum].Scale;
end;

{ Get field name }
function TDirDb2SqlQuery.FieldName(FieldNum: Integer): ShortString;
begin
  Result := '';

  if not Active or (FieldNum >= FOutSqlVars.ActualNum) then Exit;
  Result := FOutSqlVars.Variables[FieldNum].Name;

  if Result = '' then Result := 'Field' + IntToStr(FieldNum+1);
end;

{ Get field size }
function TDirDb2SqlQuery.FieldSize(FieldNum: Integer): Integer;
begin
  Result := 0;
  if not Active or (FieldNum >= FOutSqlVars.ActualNum) then Exit;
  Result := FOutSqlVars.Variables[FieldNum].DataLen;
end;

{ Go to the first row }
procedure TDirDb2SqlQuery.First;
begin
end;

{ Go to specified row }
procedure TDirDb2SqlQuery.Go(Num: Integer);
begin
end;

{ Go to the last row }
procedure TDirDb2SqlQuery.Last;
begin
end;

{ Go to next row }
procedure TDirDb2SqlQuery.Next;
var
  Status: Integer;
begin
  if not Active or EOF then Exit;
  SetStatus(qsFail);
  if not Assigned(Connect) or not Assigned(Transact) then Exit;

  Status := SqlFetch(FHandle);
  case Status of
    SQL_SUCCESS, SQL_SUCCESS_WITH_INFO:
      begin
        SetStatus(qsTuplesOk);
        SetRecNo(RecNo + 1);
        SetEOF(False);
      end;
    SQL_NO_DATA:
      begin
        SetStatus(qsTuplesOk);
        SetEOF(True);
      end;
    else
      begin
        SetEOF(True);
        TDirDb2SqlTransact(Transact).CheckError(SQL_HANDLE_STMT, FHandle, Status, FError);
      end;
  end;
end;

{ Go to prior row }
procedure TDirDb2SqlQuery.Prev;
begin
end;

{ Get rows number }
function TDirDb2SqlQuery.RecordCount: Integer;
begin
  if Active then
    Result := RecNo
  else Result := 0;
end;

{ Showes table columns }
procedure TDirDb2SqlQuery.ShowColumns(TableName, ColumnName: ShortString);
begin
  if Active then Close;
  SQL := 'SELECT COLNO AS Idx, COLNAME AS Fld, TYPENAME AS Typ, LENGTH AS Len,'
    +' NULLS AS Nul, DEFAULT AS Def, SCALE AS Scale, IDENTITY AS Autoinc,'
    +' GENERATED AS AutoGen'
    +' FROM SYSCAT.COLUMNS WHERE'
    +' TABNAME = ''' + UpperCase(TableName) + '''';
  if ColumnName <> '' then
    SQL := SQL + ' AND COLNAME LIKE ''' + UpperCase(ColumnName) + '''';
  SQL := SQL + ' ORDER BY COLNO';
  Open;
end;

{ Show existed databases }
procedure TDirDb2SqlQuery.ShowDatabases(DatabaseName: ShortString);
begin
  inherited;
end;

{ Showes tables indices of database }
procedure TDirDb2SqlQuery.ShowIndexes(TableName: ShortString);
begin
  if Active then Close;
  SQL := 'SELECT A.INDNAME AS Name, TABNAME AS Tbl, '
    + 'UNIQUERULE AS Uni, COLORDER AS Srt, COLNAME AS Fld '
    + 'FROM SYSCAT.INDEXES AS A LEFT JOIN SYSCAT.INDEXCOLUSE AS B '
    + 'ON A.INDSCHEMA=B.INDSCHEMA AND A.INDNAME=B.INDNAME ';
  if TableName <> '' then
    SQL := SQL + 'WHERE TABNAME LIKE ''' + UpperCase(TableName) + ''' ';
  SQL := SQL + ' ORDER BY A.INDNAME, COLSEQ';
  Open;
end;

{ Showes tables of database }
procedure TDirDb2SqlQuery.ShowTables(TableName: ShortString);
begin
  if Active then Close;
  Sql := 'SELECT TABNAME FROM SYSCAT.TABLES WHERE';
  if TableName <> '' then
    Sql := Sql + ' TABNAME LIKE ''' + UpperCase(TableName) + ''''
  else
    Sql := Sql + ' DEFINER <> ''SYSIBM''';
  Sql := Sql + ' ORDER BY TABNAME';
  Open;
end;

{*************** TDirDb2SqlBlob implementation ****************}

{ Class constructor }
constructor TDirDb2SqlBlob.Create(AConnect: TDirConnect; ATransact: TDirTransact;
  AHandle: TBlobHandle);
begin
  inherited Create(AConnect, ATransact, AHandle);
end;

{ Get current blob position }
function TDirDb2SqlBlob.GetPosition: LongInt;
begin
  Result := FPosition;
end;

{ Get blob error message }
function TDirDb2SqlBlob.GetErrorMsg: ShortString;
begin
  Result := '';
  if Status <> bsOk then
    Result := FError;
end;

{ Open a blob }
procedure TDirDb2SqlBlob.Open(Mode: Integer);
var
  Status: SQLINTEGER;
  Indicator: SQLINTEGER;
begin
  SetStatus(bsFail);
  if not Assigned(Connect) or not Connect.Active then Exit;
  if not Assigned(Transact) or not Transact.Active then Exit;
  if Handle.Ptr = 0 then
    CreateBlob;
  if (Handle.Ptr <> 0) and (Mode = 0) then
  begin
    FStatementHandle := 0;
    Status := SqlAllocHandle(SQL_HANDLE_STMT, TDirDb2SqlConnect(Connect).Handle,
      @FStatementHandle);
    if not TDirDb2SqlTransact(Transact).CheckError(SQL_HANDLE_STMT,
      FStatementHandle, Status, FError) then Exit;

    if FHandle.PtrEx < 500 then
      FBlobType := FHandle.PtrEx
    else FBlobType := FHandle.PtrEx - 1000;

    Indicator := 0;

    Status := SQLGetLength(FStatementHandle, FBlobType, FHandle.Ptr,
      @FSize, @Indicator);

    if not TDirDb2SqlTransact(Transact).CheckError(SQL_HANDLE_STMT,
      FStatementHandle, Status, FError) then Exit;

    SetStatus(bsOk);
    SetActive(True);
  end;
  FPosition := 0;
end;

{ Close current blob }
procedure TDirDb2SqlBlob.Close;
begin
  SetStatus(bsFail);
  if FStatementHandle <> 0 then
    SqlFreeHandle(SQL_HANDLE_STMT, FStatementHandle);
  FStatementHandle := 0;
  SetStatus(bsOk);
  SetActive(False);
  FHandle.Ptr := 0;
  FPosition := 0;
end;

{ Create a new blob }
procedure TDirDb2SqlBlob.CreateBlob;
begin
  SetStatus(bsFail);
  if Active then Close;
  if not Assigned(Transact) or not Transact.Active then Exit;
  SetStatus(bsOk);
  SetActive(True);
  FPosition := 0;
end;

{ Delete current blob }
procedure TDirDb2SqlBlob.DropBlob;
begin
  inherited DropBlob;
  if not Assigned(Transact) or not Transact.Active then Exit;
  SetStatus(bsOk);
end;

{ Read segment from open blob }
function TDirDb2SqlBlob.Read(Buffer: PChar; Length: Integer): Integer;
var
  Db2Transact: TDirDb2SqlTransact;
  Affected: SQLINTEGER;
  Indicator: SQLINTEGER;
  Status: SQLINTEGER;
begin
  Result := 0;
  SetStatus(bsFail);
  if not Assigned(Transact) or not Transact.Active then Exit;
  if not Active or (FStatementHandle = 0) then Exit;

  Db2Transact := TDirDb2SqlTransact(Transact);

  Affected := 0;
  Indicator := 0;

  Status := SQLGetSubString(FStatementHandle, FBlobType, FHandle.Ptr,
    FPosition + 1, Min(Length, FSize - FPosition), SQL_BINARY, Buffer, Length, @Affected, @Indicator);

  if Db2Transact.CheckError(SQL_HANDLE_STMT, FStatementHandle, Status, FError) then
  begin
    Result := Affected;
    FPosition := FPosition + Affected;
    SetStatus(bsOk);
  end;
end;

{ Write segment to open blob }
function TDirDb2SqlBlob.Write(Buffer: PChar; Length: Integer): Integer;
begin
  Result := 0;
  SetStatus(bsFail);
  if Handle.Ptr = 0 then Exit;
  if not Assigned(Transact) or not Transact.Active or not Active then Exit;
  SetStatus(bsOk);
end;

{**************** Extra functions *******************}

{ Convert Db2 field types to delphi field types }
function Db2SqlToDelphiType(Value: string; Size, Prec: Integer;
  var BlobType: TBlobType): TFieldType;
begin
  BlobType := btExternal;
  if StrCaseCmp(Value, 'NUMBER') then
  begin
    if Prec = 0 then
      Result := ftInteger
    else Result := ftFloat;
  end
  else if Pos('CHAR', Value) > 0 then
    Result := ftString
  {$IFNDEF VER100}
  else if StrCaseCmp(Value, 'BIGINT') then
    Result := ftLargeInt
  {$ENDIF}
  else if Pos('INT', Value) > 0 then
    Result := ftInteger
  else if StrCaseCmp(Value, 'CLOB') then
  begin
    Result := ftMemo;
    BlobType := btExternal;
  end
  else if StrCaseCmp(Value, 'BLOB') then
  begin
    Result := ftBlob;
    BlobType := btExternal;
  end
  else if StrCaseCmp(Value, 'DOUBLE') or StrCaseCmp(Value, 'REAL') then
    Result := ftFloat
  else if StrCaseCmp(Value, 'DATE') then
    Result := ftDate
  else if StrCaseCmp(Value, 'TIME') then
    Result := ftTime
  else if StrCaseCmp(Value, 'TIMESTAMP') then
    Result := ftDateTime
  else
    Result := ftUnknown;
end;

initialization
  MonitorList := TZMonitorList.Create;
finalization
  MonitorList.Free;
end.
