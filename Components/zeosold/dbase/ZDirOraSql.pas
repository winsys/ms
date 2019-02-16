{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{                Oracle8 direct class API                }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZDirOraSql;

interface

uses Windows, Classes, SysUtils, Db, ZLibOraSql, ZDirSql, ZTransact, ZSqlTypes;

{$INCLUDE ..\Zeos.inc}

type
  { Declare the SQL Object }
  TSqlVar = record
    Handle:    POCIHandle;
    Define:    POCIHandle;
    Data:      PChar;
    DataType:  ub2;
    DataSize:  ub2;
    ColType:   TFieldType;
    TypeCode:  ub2;
    Indicator: sb2;
  end;
  PSqlVar = ^TSqlVar;

  TSqlVars = record
    AllocNum:  ub4;
    ActualNum: ub4;
    Variables: array[0..0] of TSqlVar;
  end;
  PSqlVars = ^TSqlVars;

  { Direct connection to Oracle database }
  TDirOraSqlConnect = class (TDirConnect)
  private
    FHandle: POCIEnv;
  public
    procedure Connect; override;
    procedure Disconnect; override;

    property Handle: POCIEnv read FHandle write FHandle;
  end;

  { Transaction types }
  TZOraSqlTransIsolation = (otDefault, otReadOnly, otReadWrite, otSerializable);

  { Direct Oracle transaction }
  TDirOraSqlTransact = class (TDirTransact)
  private
    FHandle: POCISvcCtx;
    FError: string;
    FErrorHandle: POCIError;
    FServerHandle: POCIServer;
    FSessionHandle: POCISession;
    FTransHandle: POCITrans;
    FTransIsolation: TZOraSqlTransIsolation;
  protected
    function GetErrorMsg: ShortString; override;
  public
    constructor Create(AConnect: TDirOraSqlConnect);

    function CheckError(Handle: POCIError; Status: Integer; var Message: string): Boolean;

    procedure Open; override;
    procedure Close; override;
    procedure StartTransaction; override;
    procedure EndTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;

    property Handle: POCISvcCtx read FHandle write FHandle;
    property ErrorHandle: POCIError read FErrorHandle write FErrorHandle;
    property TransIsolation: TZOraSqlTransIsolation read FTransIsolation
      write FTransIsolation;
  end;

{ Maximum sqlvar buffer }
const MAX_SQLVAR = 50;

type
  { Direct Interbase Query }
  TDirOraSqlQuery = class(TDirQuery)
  private
    FHandle: POCIStmt;
    FErrorHandle: POCIError;
    FOutSqlVars: PSqlVars;
    FInSqlVars: PSqlVars;
    FError: string;
  protected
    function GetErrorMsg: ShortString; override;
  public
    constructor Create(AConnect: TDirOraSqlConnect; ATransact: TDirOraSqlTransact);
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
    function  FieldDataType(FieldNum: Integer): TFieldType; override;
    function  FieldIsNull(FieldNum: Integer): Boolean; override;
    function  Field(FieldNum: Integer): string; override;
    function  FieldBuffer(FieldNum: Integer): PChar; override;

    property Handle: POCIStmt read FHandle;
  end;

  { Abstract class for database binary large object }
  TDirOraSqlBlob = class(TDirBlob)
  private
    FPosition: LongInt;
    FError: string;
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

    function  Read(Buffer: PChar; Length: Integer): Integer; override;
    function  Write(Buffer: PChar; Length: Integer): Integer; override;
  end;

{ Convert postgresql field types to delphi field types }
function OraSqlToDelphiType(Value: string; Size, Prec: Integer;
  var BlobType: TBlobType): TFieldType;

{ Convert oracle internal date to date time }
function OraDateToDateTime(Value: PChar): TDateTime;

{ Monitor list }
var MonitorList: TZMonitorList;

implementation

uses ZExtra, ZDBaseConst, Math, ZSqlExtra;

{**************** TDirOraSqlConnect implementation ************}

{ Connect to existed database }
procedure TDirOraSqlConnect.Connect;
begin
  inherited Connect;
  if hDll = 0 then
  begin
    OraSqlLoadLib;
    OCIInitialize(OCI_THREADED, nil, nil, nil, nil);
  end;
{ Connect database }
  FHandle := nil;
  OCIEnvInit(FHandle, OCI_DEFAULT, 0, nil);
  SetStatus(csOk);
  SetActive(True);
end;

{ Disconnect from database }
procedure TDirOraSqlConnect.Disconnect;
begin
  if Active then
  begin
    OCIHandleFree(FHandle, OCI_HTYPE_ENV);
    FHandle := nil;
    SetActive(False);
  end;
end;

{************* TDirOraSqlTransact implementation *************}

{ Class constructor }
constructor TDirOraSqlTransact.Create(AConnect: TDirOraSqlConnect);
begin
  inherited Create;
  FHandle := nil;
  Connect := AConnect;
end;

{ Get sql-server error message }
function TDirOraSqlTransact.GetErrorMsg: ShortString;
begin
  if Status <> csOk then
    Result := FError
  else
    Result := '';
end;

{ Process error status }
function TDirOraSqlTransact.CheckError(Handle: POCIError; Status: Integer;
  var Message: string): Boolean;
var
  ErrorBuf: array[0..255] of Char;
  ErrorCode: SB4;
begin
  Result := False;
  Message := '';
  case Status of
    OCI_SUCCESS:
      Result := True;
    OCI_SUCCESS_WITH_INFO:
      Message := 'OCI_SUCCESS_WITH_INFO';
    OCI_NEED_DATA:
      Message := 'OCI_NEED_DATA';
    OCI_NO_DATA:
      Message := 'OCI_NO_DATA';
    OCI_ERROR:
      begin
        OCIErrorGet(Handle, 1, nil, ErrorCode, ErrorBuf, 255, OCI_HTYPE_ERROR);
        Message := StrPas(ErrorBuf);
      end;
    OCI_INVALID_HANDLE:
      Message := 'OCI_INVALID_HANDLE';
    OCI_STILL_EXECUTING:
      Message := 'OCI_STILL_EXECUTING';
    OCI_CONTINUE:
      Message := 'OCI_CONTINUE';
  end;
end;

{ Connect transaction }
procedure TDirOraSqlTransact.Open;
label ErrorProc;
var
  Status: Integer;
  OraConnect: TDirOraSqlConnect;
begin
  inherited Open;
  SetStatus(csFail);
  if not Assigned(Connect) or not Connect.Active then
    Exit;
  OraConnect := TDirOraSqlConnect(Connect);

  FErrorHandle := nil;
  OCIHandleAlloc(OraConnect.Handle, FErrorHandle, OCI_HTYPE_ERROR, 0, nil);
  FServerHandle := nil;
  OCIHandleAlloc(OraConnect.Handle, FServerHandle, OCI_HTYPE_SERVER, 0, nil);
  FHandle := nil;
  OCIHandleAlloc(OraConnect.Handle, FHandle, OCI_HTYPE_SVCCTX, 0, nil);

  Status := OCIServerAttach(FServerHandle, FErrorHandle,
    PChar(string(Connect.Database)), Length(Connect.Database), 0);
  if not CheckError(FErrorHandle, Status, FError) then
    goto ErrorProc;

  OCIAttrSet(FHandle, OCI_HTYPE_SVCCTX, FServerHandle, 0, OCI_ATTR_SERVER,
    FErrorHandle);
  OCIHandleAlloc(OraConnect.Handle, FSessionHandle, OCI_HTYPE_SESSION, 0, nil);
  OCIAttrSet(FSessionHandle, OCI_HTYPE_SESSION, PChar(string(Connect.Login)),
    Length(Connect.Login), OCI_ATTR_USERNAME, FErrorHandle);
  OCIAttrSet(FSessionHandle, OCI_HTYPE_SESSION, PChar(string(Connect.Passwd)),
    Length(Connect.Passwd), OCI_ATTR_PASSWORD, FErrorHandle);
  Status := OCISessionBegin(FHandle, FErrorHandle, FSessionHandle,
    OCI_CRED_RDBMS, OCI_DEFAULT);
  if not CheckError(FErrorHandle, Status, FError) then
    goto ErrorProc;
  OCIAttrSet(FHandle, OCI_HTYPE_SVCCTX, FSessionHandle, 0,
    OCI_ATTR_SESSION, FErrorHandle);
  MonitorList.InvokeEvent(Format('CONNECT %s',[Connect.Database]), 'OK.', False);

  StartTransaction;
  SetActive(inherited Status = csOk);
  Exit;

  { Process error status }
ErrorProc:
  OCIHandleFree(FHandle, OCI_HTYPE_SVCCTX);
  FHandle := nil;
  OCIHandleFree(FErrorHandle, OCI_HTYPE_ERROR);
  FErrorHandle := nil;
  OCIHandleFree(FServerHandle, OCI_HTYPE_SERVER);
  FServerHandle := nil;
  MonitorList.InvokeEvent(Format('CONNECT %s',[Connect.Database]), Error, True);
end;

{ Disconnect transaction }
procedure TDirOraSqlTransact.Close;
var
  Status: Integer;
begin
  EndTransaction;

  Status := OCISessionEnd(FHandle, FErrorHandle, FSessionHandle, OCI_DEFAULT);
  CheckError(FErrorHandle, Status, FError);

  Status := OCIServerDetach(FServerHandle, FErrorHandle, OCI_DEFAULT);
  CheckError(FErrorHandle, Status, FError);

  OCIHandleFree(FSessionHandle, OCI_HTYPE_SESSION);
  FSessionHandle := nil;
  OCIHandleFree(FHandle, OCI_HTYPE_SVCCTX);
  FHandle := nil;
  OCIHandleFree(FServerHandle, OCI_HTYPE_SERVER);
  FServerHandle := nil;
  OCIHandleFree(FErrorHandle, OCI_HTYPE_ERROR);
  FErrorHandle := nil;

  SetActive(False);
end;

{ Start transaction }
procedure TDirOraSqlTransact.StartTransaction;
const
  TransIsolationConst: array[TZOraSqlTransIsolation] of Integer = (OCI_DEFAULT,
    OCI_TRANS_READONLY, OCI_TRANS_READWRITE, OCI_TRANS_SERIALIZABLE);
var
  Status: Integer;
  Isolation: Integer;
begin
  { Set startup values }
  SetStatus(csFail);
  if FHandle <> nil then
  begin
    FTransHandle := nil;
    OCIHandleAlloc(TDirOraSqlConnect(Connect).Handle, FTransHandle,
      OCI_HTYPE_TRANS, 0, nil);
    OCIAttrSet(FHandle, OCI_HTYPE_SVCCTX, FTransHandle, 0,
      OCI_ATTR_TRANS, FErrorHandle);

    Isolation := TransIsolationConst[TransIsolation];
    Status := OCITransStart(FHandle, FErrorHandle, 0, Isolation);
    if CheckError(FErrorHandle, Status, FError) then
      SetStatus(csOk);
    MonitorList.InvokeEvent('BEGIN TRANSACTION', Error, Error <> '');
  end;
end;

{ End transaction }
procedure TDirOraSqlTransact.EndTransaction;
var
  Status: Integer;
begin
  { Set startup values }
  SetStatus(csFail);
  if FHandle <> nil then
  begin
    Status := OCITransRollback(FHandle, FErrorHandle, OCI_DEFAULT);
    if CheckError(FErrorHandle, Status, FError) then
      SetStatus(csOk);
    OCIHandleFree(FTransHandle, OCI_HTYPE_TRANS);
    FTransHandle := nil; 
    MonitorList.InvokeEvent('END TRANSACTION', Error, Error <> '');
  end;
end;

{ Commit transaction }
procedure TDirOraSqlTransact.Commit;
var
  Status: Integer;
begin
  SetStatus(csFail);
  if Active then
  begin
    Status := OCITransCommit(FHandle, FErrorHandle, OCI_DEFAULT);
    if CheckError(FErrorHandle, Status, FError) then
      SetStatus(csOk);
    MonitorList.InvokeEvent('COMMIT', Error, Error <> '');
  end;
end;

{ Rollback transaction }
procedure TDirOraSqlTransact.Rollback;
var
  Status: Integer;
begin
  SetStatus(csFail);
  if Active then
  begin
    Status := OCITransRollback(FHandle, FErrorHandle, OCI_DEFAULT);
    if CheckError(FErrorHandle, Status, FError) then
      SetStatus(csOk);
    MonitorList.InvokeEvent('ROLLBACK', Error, Error <> '');
  end;
end;

{************* TDirOraSqlQuery implementation ************}

{ Count length of SqlVars variable }
function SqlVarsLength(Count: Integer): Integer;
begin
  Result := SizeOf(TSqlVars) + Count * SizeOf(TSqlVar);
end;

{ Class constructor }
constructor TDirOraSqlQuery.Create(AConnect: TDirOraSqlConnect;
  ATransact: TDirOraSqlTransact);
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
destructor TDirOraSqlQuery.Destroy;
begin
  inherited Destroy;
  FreeMem(FInSqlVars);
  FreeMem(FOutSqlVars);
end;

{ Get an error message }
function TDirOraSqlQuery.GetErrorMsg: ShortString;
begin
  if not (Status in [qsCommandOk, qsTuplesOk]) then
    Result := FError
  else
    Result := '';
end;

{ Execute an SQL statement }
function TDirOraSqlQuery.Execute: LongInt;
label ErrorProc;
var
  OraConnect: TDirOraSqlConnect;
  OraTransact: TDirOraSqlTransact;
  Status: Integer;
{$IFDEF DELETE_QUERY_SPACES}
  Temp: string;
{$ENDIF}
  TempRows: ub4;
begin
  inherited Execute;

  SetStatus(qsFail);
  Result := 0;
  if Assigned(Connect) and Assigned(Transact) and Transact.Active then
  begin
    OraConnect := TDirOraSqlConnect(Connect);
    OraTransact := TDirOraSqlTransact(Transact);

    FErrorHandle := nil;
    OCIHandleAlloc(OraConnect.Handle, FErrorHandle, OCI_HTYPE_ERROR, 0, nil);
    FHandle := nil;
    OCIHandleAlloc(OraConnect.Handle, FHandle, OCI_HTYPE_STMT, 0, nil);

{$IFDEF DELETE_QUERY_SPACES}
    Temp := ClearSpaces(Sql);
    Status := OCIStmtPrepare(FHandle, FErrorHandle, PChar(Temp), Length(Temp),
      OCI_NTV_SYNTAX, OCI_DEFAULT);
{$ELSE}
    Status := OCIStmtPrepare(FHandle, FErrorHandle, PChar(Sql), Length(Sql),
      OCI_NTV_SYNTAX, OCI_DEFAULT);
{$ENDIF}
    if not OraTransact.CheckError(FErrorHandle, Status, FError) then
      goto ErrorProc;

    Status := OCIStmtExecute(OraTransact.Handle, FHandle, FErrorHandle, 1, 0,
      nil, nil, OCI_DEFAULT);
    if not OraTransact.CheckError(FErrorHandle, Status, FError) then
      goto ErrorProc;

    SetStatus(qsCommandOk);
    TempRows := 0;
    OCIAttrGet(FHandle, OCI_HTYPE_STMT, @TempRows, nil, OCI_ATTR_ROW_COUNT,
      FErrorHandle);
    SetAffectedRows(TempRows);

ErrorProc:
    MonitorList.InvokeEvent(Sql, Error, Error <> '');
    OCIHandleFree(FHandle, OCI_HTYPE_STMT);
    FHandle := nil;
    OCIHandleFree(FHandle, OCI_HTYPE_ERROR);
    FErrorHandle := nil;
  end;
end;

{ Execute an SQL statement with params }
function TDirOraSqlQuery.ExecuteParams(Params: TVarRecArray; ParamCount: Integer): LongInt;
label ErrorProc;
var
  I: Integer;
  SqlVar: PSqlVar;
  OraConnect: TDirOraSqlConnect;
  OraTransact: TDirOraSqlTransact;
  Status: Integer;
{$IFDEF DELETE_QUERY_SPACES}
  Temp: string;
{$ENDIF}
  BindHandle: POCIBind;
  TempRows: ub4;
begin
  inherited Execute;

  SetStatus(qsFail);
  Result := 0;
  if Assigned(Connect) and Assigned(Transact) and Transact.Active then
  begin
    OraConnect := TDirOraSqlConnect(Connect);
    OraTransact := TDirOraSqlTransact(Transact);

    FErrorHandle := nil;
    OCIHandleAlloc(OraConnect.Handle, FErrorHandle, OCI_HTYPE_ERROR, 0, nil);
    FHandle := nil;
    OCIHandleAlloc(OraConnect.Handle, FHandle, OCI_HTYPE_STMT, 0, nil);

{$IFDEF DELETE_QUERY_SPACES}
    Temp := ClearSpaces(Sql);
    Status := OCIStmtPrepare(FHandle, FErrorHandle, PChar(Temp), Length(Temp),
      OCI_NTV_SYNTAX, OCI_DEFAULT);
{$ELSE}
    Status := OCIStmtPrepare(FHandle, FErrorHandle, PChar(Sql), Length(Sql),
      OCI_NTV_SYNTAX, OCI_DEFAULT);
{$ENDIF}
    if not OraTransact.CheckError(FErrorHandle, Status, FError) then
      goto ErrorProc;

    { Resize SQLVERS structure if needed }
    if ParamCount > FInSqlVars.AllocNum then
    begin
      ReallocMem(FInSqlVars, SqlVarsLength(FInSqlVars.ActualNum));
      FInSqlVars.AllocNum := FInSqlVars.ActualNum;
    end;

    { Allocate memory for result set }
    for I := 0 to ParamCount-1 do
    begin
(*
      case VarType(Params[I]) of
        varInteger, varSmallInt:
          Status := OCIBindByPos(FHandle, BindHandle, FErrorHandle, I+1,
            @Params[I], 0, SQLT_INT, nil, nil, nil, 0, nil, OCI_DATA_AT_EXEC);
        varDouble:
          Status := OCIBindByPos(FHandle, BindHandle, FErrorHandle, I+1,
            @Params[I], 0, SQLT_FLT, nil, nil, nil, 0, nil, OCI_DATA_AT_EXEC);
        varOleStr, varString:
          Status := OCIBindByPos(FHandle, BindHandle, FErrorHandle, I+1,
            PChar(Params[I]), Length(Params[I]), SQLT_STR, nil, nil, nil,
            0, nil, OCI_DATA_AT_EXEC);
        else
          Status := OCIBindByPos(FHandle, BindHandle, FErrorHandle, I+1,
            @Params[I], 0, SQLT_BLOB, nil, nil, nil, 0, nil, OCI_DATA_AT_EXEC);
      end;
*)
      SqlVar := @FInSqlVars.Variables[I];
      GetMem(SqlVar.Data, SizeOf(POCILobLocator));

      Status := OCIDescriptorAlloc(OraConnect.Handle, PPOCIDescriptor(SqlVar.Data)^,
        OCI_DTYPE_LOB, 0, nil);
      if Status <> OCI_SUCCESS then
        goto ErrorProc;
      Inc(FInSqlVars.ActualNum);

      TempRows := 0;
      Status := OCIAttrSet(PPOCIDescriptor(SqlVar.Data)^, OCI_DTYPE_LOB, @TempRows,
        0, OCI_ATTR_LOBEMPTY, OraTransact.ErrorHandle);
      if not OraTransact.CheckError(FErrorHandle, Status, FError) then
        goto ErrorProc;

      Status := OCIBindByPos(FHandle, BindHandle, FErrorHandle, I+1,
        PPOCIDescriptor(SqlVar.Data)^, 0, SQLT_BLOB, nil, nil, nil, 0,
        nil, OCI_DATA_AT_EXEC);
      if not OraTransact.CheckError(FErrorHandle, Status, FError) then
        goto ErrorProc;

      TempRows := Length(string(Params[I].VAnsiString));
      Status := OCILobWrite(OraTransact.Handle, OraTransact.ErrorHandle,
        PPOCIDescriptor(SqlVar.Data)^, TempRows, 1,
        PChar(string(Params[I].VAnsiString)), TempRows, OCI_ONE_PIECE, nil,
        nil, 0, SQLCS_IMPLICIT);
      if not OraTransact.CheckError(OraTransact.ErrorHandle, Status, FError) then
        goto ErrorProc;
    end;

    Status := OCIStmtExecute(OraTransact.Handle, FHandle, FErrorHandle, 1, 0,
      nil, nil, OCI_DEFAULT);
    if not OraTransact.CheckError(FErrorHandle, Status, FError) then
      goto ErrorProc;

    SetStatus(qsCommandOk);
    TempRows := 0;
    OCIAttrGet(FHandle, OCI_HTYPE_STMT, @TempRows, nil, OCI_ATTR_ROW_COUNT,
      FErrorHandle);
    SetAffectedRows(TempRows);

ErrorProc:
    for I := 0 to FInSqlVars.ActualNum-1 do
    begin
      SqlVar := @FInSqlVars.Variables[I];
      OCIDescriptorFree(PPOCIDescriptor(SqlVar.Data)^, OCI_DTYPE_LOB);
      FreeMem(SqlVar.Data);
    end;
    MonitorList.InvokeEvent(Sql, Error, Error <> '');
    OCIHandleFree(FHandle, OCI_HTYPE_STMT);
    FHandle := nil;
    OCIHandleFree(FHandle, OCI_HTYPE_ERROR);
    FErrorHandle := nil;
  end;
end;

{ Open a sql query with result set }
procedure TDirOraSqlQuery.Open;
label ErrorProc;
var
  I, Status: Integer;
  OraConnect: TDirOraSqlConnect;
  OraTransact: TDirOraSqlTransact;
  SqlVar: PSqlVar;
{$IFDEF DELETE_QUERY_SPACES}
  Temp: string;
{$ENDIF}
  Prec, Scale, Len: Integer;
begin
  inherited Open;

  { Check connect and transaction status }
  SetStatus(qsFail);
  if not Assigned(Connect) or not Assigned(Transact)
    or not (Connect.Active and Transact.Active) then
    Exit;

  OraConnect := TDirOraSqlConnect(Connect);
  OraTransact := TDirOraSqlTransact(Transact);

  { Allocate an sql statement }
  FHandle := nil;
  OCIHandleAlloc(OraConnect.Handle, FHandle, OCI_HTYPE_STMT, 0, nil);
  FErrorHandle := nil;
  OCIHandleAlloc(OraConnect.Handle, FErrorHandle, OCI_HTYPE_ERROR, 0, nil);

  { Prepare an sql statement }
{$IFDEF DELETE_QUERY_SPACES}
  Temp := ClearSpaces(Sql);
  Status := OCIStmtPrepare(FHandle, FErrorHandle, PChar(Temp), Length(Temp),
    OCI_NTV_SYNTAX, OCI_DEFAULT);
{$ELSE}
  Status := OCIStmtPrepare(FHandle, FErrorHandle, PChar(Sql), Length(Sql),
    OCI_NTV_SYNTAX, OCI_DEFAULT);
{$ENDIF}
  if not OraTransact.CheckError(FErrorHandle, Status, FError) then
    goto ErrorProc;

  Status := OCIStmtExecute(OraTransact.Handle, FHandle, FErrorHandle, 1, 0,
    nil, nil, OCI_DESCRIBE_ONLY);
  if not OraTransact.CheckError(FErrorHandle, Status, FError) then
    goto ErrorProc;

  { Resize SQLVERS structure if needed }
  OCIAttrGet(FHandle, OCI_HTYPE_STMT, @FOutSqlVars.ActualNum, nil,
    OCI_ATTR_PARAM_COUNT, FErrorHandle);
  if FOutSqlVars.ActualNum > FOutSqlVars.AllocNum then
  begin
    ReallocMem(FOutSqlVars, SqlVarsLength(FOutSqlVars.ActualNum));
    FOutSqlVars.AllocNum := FOutSqlVars.ActualNum;
  end;

  { Allocate memory for result set }
  for I := 0 to FOutSqlVars.ActualNum-1 do
  begin
    SqlVar := @FOutSqlVars.Variables[I];
    SqlVar.Handle := nil;

    OCIParamGet(FHandle, OCI_HTYPE_STMT, FErrorHandle, SqlVar.Handle, I+1);
    OCIAttrGet(SqlVar.Handle, OCI_DTYPE_PARAM, @SqlVar.DataSize, nil,
      OCI_ATTR_DATA_SIZE, FErrorHandle);
    OCIAttrGet(SqlVar.Handle, OCI_DTYPE_PARAM, @SqlVar.DataType, nil,
      OCI_ATTR_DATA_TYPE, FErrorHandle);

    case SqlVar.DataType of
      SQLT_CHR, SQLT_VCS, SQLT_AFC:
        begin
          if SqlVar.DataSize < 255 then
            SqlVar.ColType := ftString
          else
            SqlVar.ColType := ftMemo;
        end;
      SQLT_NUM:
        begin
          Prec := 0;
          OCIAttrGet(SqlVar.Handle, OCI_DTYPE_PARAM, @Prec, nil,
            OCI_ATTR_PRECISION, FErrorHandle);
          Scale := 0;
          OCIAttrGet(SqlVar.Handle, OCI_DTYPE_PARAM, @Scale, nil,
            OCI_ATTR_SCALE, FErrorHandle);

          if (Scale = 0) and (Prec <> 0) and (Prec <= 38) then
            SqlVar.ColType := ftInteger
          else
            SqlVar.ColType := ftFloat;
        end;
      SQLT_INT, _SQLT_PLI:
        SqlVar.ColType := ftInteger;
      SQLT_LNG:
        SqlVar.ColType := ftMemo;
      SQLT_RID, SQLT_RDD:
        begin
          SqlVar.ColType := ftString;
          SqlVar.DataSize := 20;
        end;
      SQLT_DAT:
        SqlVar.ColType := ftDateTime;
      SQLT_BIN, SQLT_LBI:
        SqlVar.ColType := ftBlob;
      SQLT_CLOB:
        SqlVar.ColType := ftMemo;
      SQLT_BLOB:
        SqlVar.ColType := ftBlob;
      else
        SqlVar.ColType := ftUnknown;
    end;

    SqlVar.TypeCode := SqlVar.DataType;
    Len := 0;
    case SqlVar.ColType of
      ftInteger:
        begin
          SqlVar.TypeCode := SQLT_INT;
          Len := SizeOf(LongInt);
        end;
      ftFloat:
        begin
          SqlVar.TypeCode := SQLT_FLT;
          Len := SizeOf(Double);
        end;
      ftDateTime:
        Len := 7;
      ftString:
        begin
          SqlVar.TypeCode := SQLT_STR;
          Len := SqlVar.DataSize + 1;
        end;
      ftBlob, ftMemo:
        if not (SqlVar.TypeCode in [SQLT_CLOB, SQLT_BLOB]) then
        begin
          if SqlVar.ColType = ftMemo then
            SqlVar.TypeCode := SQLT_LVC
          else SqlVar.TypeCode := SQLT_LVB;
          if SqlVar.DataSize = 0 then
            Len := 1024 * 128 + SizeOf(Integer)
          else Len := SqlVar.DataSize + SizeOf(Integer);
        end else
          Len := SizeOf(POCILobLocator);
      ftUnknown:
        Continue;
    end;

    GetMem(SqlVar.Data, Len);
    if SqlVar.TypeCode in [SQLT_BLOB, SQLT_CLOB] then
      OCIDescriptorAlloc(OraConnect.Handle, PPOCIDescriptor(SqlVar.Data)^, OCI_DTYPE_LOB, 0, nil);
    Status := OCIDefineByPos(FHandle, SqlVar.Define, FErrorHandle, I+1,
      SqlVar.Data, Len, SqlVar.TypeCode, @SqlVar.Indicator, nil, nil, OCI_DEFAULT);
    if not OraTransact.CheckError(FErrorHandle, Status, FError) then
      goto ErrorProc;
  end;

  { Execute a query }
  Status := OCIStmtExecute(OraTransact.Handle, FHandle, FErrorHandle, 1, 0,
    nil, nil, OCI_DEFAULT);
  if not (Status in [OCI_SUCCESS, OCI_NO_DATA]) then
  begin
    OraTransact.CheckError(FErrorHandle, Status, FError);
    goto ErrorProc;
  end;

  SetActive(True);
  SetStatus(qsTuplesOk);
  SetBOF(Status <> OCI_SUCCESS);
  SetEOF(Status <> OCI_SUCCESS);
  if Status = OCI_SUCCESS then
    SetRecNo(1);
  Exit;

ErrorProc:
  for I := 0 to FOutSqlVars.ActualNum-1 do
  begin
    SqlVar := @FOutSqlVars.Variables[I];
    if SqlVar.TypeCode in [SQLT_BLOB, SQLT_CLOB] then
      OCIDescriptorFree(PPOCIDescriptor(SqlVar.Data)^, OCI_DTYPE_LOB);
    FreeMem(SqlVar.Data);
    SqlVar.Data := nil;
  end;

  OCIHandleFree(FHandle, OCI_HTYPE_STMT);
  FHandle := nil;
  OCIHandleFree(FErrorHandle, OCI_HTYPE_ERROR);
  FErrorHandle := nil;
end;

{ Close a sql query with result set }
procedure TDirOraSqlQuery.Close;
var
  I: Integer;
  SqlVar: PSqlVar;
begin
  if not Active then
    Exit;
  inherited Close;

  { Check connect and transaction status }
  SetStatus(qsTuplesOk);
  if not Assigned(Connect) or not Assigned(Transact)
    or not Transact.Active then Exit;

  { Free sql statement }
  OCIHandleFree(FHandle, OCI_HTYPE_STMT);
  FHandle := nil;
  OCIHandleFree(FErrorHandle, OCI_HTYPE_ERROR);
  FErrorHandle := nil;

  { Free allocated memory }
  for I := 0 to FOutSqlVars.ActualNum-1 do
  begin
    SqlVar := @FOutSqlVars.Variables[I];
    if SqlVar.TypeCode in [SQLT_BLOB, SQLT_CLOB] then
      OCIDescriptorFree(PPOCIDescriptor(SqlVar.Data)^, OCI_DTYPE_LOB);
    FreeMem(SqlVar.Data);
    SqlVar.Data := nil;
  end;
  FOutSqlVars.ActualNum := 0;
end;

{ Create linked blob object }
function TDirOraSqlQuery.CreateBlobObject: TDirBlob;
var
  TempHandle: TBlobHandle;
begin
  FillChar(TempHandle, SizeOf(TBlobHandle), 0);
  Result := TDirOraSqlBlob.Create(Connect, Transact, TempHandle);
end;

function TDirOraSqlQuery.Field(FieldNum: Integer): string;
var
  SqlVar: PSqlVar;
  OldSep: Char;
begin
  Result := '';
  if not Active or (FieldNum >= FOutSqlVars.ActualNum) then Exit;
  SqlVar := @FOutSqlVars.Variables[FieldNum];

  if (SqlVar.Indicator < 0) or (SqlVar.Data = nil) then Exit;
  OldSep := DecimalSeparator;
  DecimalSeparator := '.';
  case SqlVar.TypeCode of
    SQLT_INT:
      Result := IntToStr(PLongInt(SqlVar.Data)^);
    SQLT_FLT:
      Result := FloatToStr(PDouble(SqlVar.Data)^);
    SQLT_STR:
      Result := StrPas(SqlVar.Data);
    SQLT_LVB, SQLT_LVC:
      Result := MemPas(SqlVar.Data + SizeOf(Integer), PInteger(SqlVar.Data)^);
    SQLT_DAT:
      Result := DateTimeToSqlDate(OraDateToDateTime(SqlVar.Data));
    SQLT_BLOB, SQLT_CLOB:
      begin
        with TDirOraSqlBlob.Create(Connect, Transact,
          PBlobHandle(SqlVar.Data)^) do
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
function TDirOraSqlQuery.FieldIsNull(FieldNum: Integer): Boolean;
var
  SqlVar: PSqlVar;
begin
  Result := True;
  if not Active or (FieldNum >= FOutSqlVars.ActualNum) then Exit;
  SqlVar := @FOutSqlVars.Variables[FieldNum];
  Result := (SqlVar.Indicator < 0);
end;

{ Get field buffer }
function TDirOraSqlQuery.FieldBuffer(FieldNum: Integer): PChar;
var
  SqlVar: PSqlVar;
begin
  Result := nil;
  if not Active or (FieldNum >= FOutSqlVars.ActualNum) then Exit;
  SqlVar := @FOutSqlVars.Variables[FieldNum];
  if SqlVar.Indicator >= 0 then
    Result := SqlVar.Data;
end;

{ Get field type }
function TDirOraSqlQuery.FieldType(FieldNum: Integer): Integer;
begin
  Result := 0;
  if not Active or (FieldNum >= FOutSqlVars.ActualNum) then Exit;
  Result := FOutSqlVars.Variables[FieldNum].TypeCode;
end;

{ Get field delphi compatible type }
function TDirOraSqlQuery.FieldDataType(FieldNum: Integer): TFieldType;
begin
  Result := ftUnknown;
  if not Active or (FieldNum >= FOutSqlVars.ActualNum) then Exit;
  Result := FOutSqlVars.Variables[FieldNum].ColType;
end;

{ Get field count }
function TDirOraSqlQuery.FieldCount: Integer;
begin
  Result := 0;
  if Active then
    Result := FOutSqlVars.ActualNum;
end;

{ Get field maximum size }
function TDirOraSqlQuery.FieldMaxSize(FieldNum: Integer): Integer;
begin
  Result := 0;
  if not Active or (FieldNum >= FOutSqlVars.ActualNum) then Exit;
  Result := FOutSqlVars.Variables[FieldNum].DataSize;
end;

{ Get fields decimals }
function TDirOraSqlQuery.FieldDecimals(FieldNum: Integer): Integer;
begin
  Result := 0;
  if not Active or (FieldNum >= FOutSqlVars.ActualNum) then Exit;
  Result := 0;
end;

{ Get field name }
function TDirOraSqlQuery.FieldName(FieldNum: Integer): ShortString;
var
  SqlVar: PSqlVar;
  Temp: PChar;
  TempLen: Integer;
begin
  Result := '';
  if not Active or (FieldNum >= FOutSqlVars.ActualNum) then Exit;

  SqlVar := @FOutSqlVars.Variables[FieldNum];
  Temp := nil;
  OCIAttrGet(SqlVar.Handle, OCI_DTYPE_PARAM, @Temp, @TempLen,
    OCI_ATTR_NAME, FErrorHandle);
  if Temp <> nil then
    Result := MemPas(Temp, TempLen);

  if Result = '' then Result := 'Field' + IntToStr(FieldNum+1);
end;

{ Get field size }
function TDirOraSqlQuery.FieldSize(FieldNum: Integer): Integer;
begin
  Result := 0;
end;

{ Go to the first row }
procedure TDirOraSqlQuery.First;
begin
end;

{ Go to specified row }
procedure TDirOraSqlQuery.Go(Num: Integer);
begin
end;

{ Go to the last row }
procedure TDirOraSqlQuery.Last;
begin
end;

{ Go to next row }
procedure TDirOraSqlQuery.Next;
var
  Status: Integer;
begin
  if not Active or EOF then Exit;
  SetStatus(qsFail);
  if not Assigned(Connect) or not Assigned(Transact) then Exit;

  Status := OCIStmtFetch(FHandle, FErrorHandle, 1, OCI_FETCH_NEXT, OCI_DEFAULT);
  case Status of
    OCI_SUCCESS:
      begin
        SetStatus(qsTuplesOk);
        SetRecNo(RecNo + 1);
        SetEOF(False);
      end;
    OCI_NO_DATA:
      begin
        SetStatus(qsTuplesOk);
        SetEOF(True);
      end;
    else
      begin
        SetEOF(True);
        TDirOraSqlTransact(Transact).CheckError(FErrorHandle, Status, FError);
      end;
  end;
end;

{ Go to prior row }
procedure TDirOraSqlQuery.Prev;
begin
end;

{ Get rows number }
function TDirOraSqlQuery.RecordCount: Integer;
begin
  if Active then
    Result := RecNo
  else Result := 0;
end;

{ Showes table columns }
procedure TDirOraSqlQuery.ShowColumns(TableName, ColumnName: ShortString);
begin
  if Active then Close;
  SQL := 'SELECT COLUMN_ID AS Idx, COLUMN_NAME AS Fld,'
    +' DATA_TYPE AS Typ, DATA_LENGTH AS Len,'
    +' NULLABLE AS Nul, DATA_DEFAULT AS Def,'
    +' DATA_SCALE AS Scale'
    +' FROM ALL_TAB_COLUMNS WHERE'
    +' TABLE_NAME='''+UpperCase(TableName)+'''';
  if ColumnName <> '' then
    SQL := SQL + ' AND COLUMN_NAME LIKE '''+UpperCase(ColumnName)+'''';
  SQL := SQL + ' ORDER BY COLUMN_ID';
  Open;
end;

{ Show existed databases }
procedure TDirOraSqlQuery.ShowDatabases(DatabaseName: ShortString);
begin
  inherited;
end;

{ Showes tables indices of database }
procedure TDirOraSqlQuery.ShowIndexes(TableName: ShortString);
begin
  if Active then Close;
  SQL := 'SELECT A.INDEX_NAME AS Name, A.TABLE_NAME AS Tbl,'
    +' A.UNIQUENESS AS Uni, A.GENERATED AS Gen, B.COLUMN_NAME AS Fld'
    +' FROM USER_INDEXES A, USER_IND_COLUMNS B'
    +' WHERE A.INDEX_NAME=B.INDEX_NAME AND A.TABLE_NAME=B.TABLE_NAME';
  if TableName <> '' then
    SQL := SQL + ' AND A.TABLE_NAME LIKE '''+ UpperCase(TableName)+'''';
  Open;
end;

{ Showes tables of database }
procedure TDirOraSqlQuery.ShowTables(TableName: ShortString);
begin
  if Active then Close;
  Sql := 'SELECT TABLE_NAME FROM ALL_TABLES WHERE';
  if TableName <> '' then
    Sql := Sql + ' TABLE_NAME LIKE '''+UpperCase(TableName)+''''
  else
    Sql := Sql + ' OWNER <> ''SYS'' AND OWNER <> ''SYSTEM''';
  Sql := Sql + ' ORDER BY TABLE_NAME';
  Open;
end;

{*************** TDirOraSqlBlob implementation ****************}

{ Class constructor }
constructor TDirOraSqlBlob.Create(AConnect: TDirConnect; ATransact: TDirTransact;
  AHandle: TBlobHandle);
begin
  inherited Create(AConnect, ATransact, AHandle);
end;

{ Get current blob position }
function TDirOraSqlBlob.GetPosition: LongInt;
begin
  Result := FPosition;
end;

{ Get blob error message }
function TDirOraSqlBlob.GetErrorMsg: ShortString;
begin
  Result := '';
  if Status <> bsOk then
    Result := FError;
end;

{ Open a blob }
procedure TDirOraSqlBlob.Open(Mode: Integer);
begin
  SetStatus(bsFail);
  if not Assigned(Connect) or not Assigned(Transact) then Exit;
  if not Connect.Active or not Transact.Active then Exit;
  if Handle.Ptr = 0 then
    CreateBlob;
  if Handle.Ptr <> 0 then
  begin
    SetStatus(bsOk);
    SetActive(True);
  end;
  FPosition := 0;
end;

{ Close current blob }
procedure TDirOraSqlBlob.Close;
begin
  SetStatus(bsFail);
  if not Assigned(Connect) or not Connect.Active then Exit;
  SetStatus(bsOk);
  SetActive(False);
//  FHandle.Ptr := 0;
  FPosition := 0;
end;

{ Create a new blob }
procedure TDirOraSqlBlob.CreateBlob;
(*
var
  OraConnect: TDirOraSqlConnect;
  OraTransact: TDirOraSqlTransact;
  LobEmpty: ub4;
  Status: Integer;
*)
begin
  SetStatus(bsFail);
  if Active then Close;
  if not Assigned(Connect) or not Connect.Active then Exit;
  if not Assigned(Transact) or not Transact.Active then Exit;
(*
  FHandle.Ptr := 0;
  OraConnect := TDirOraSqlConnect(Connect);
  OraTransact := TDirOraSqlTransact(Transact);
  Status := OCIDescriptorAlloc(OraConnect.Handle, POCIDescriptor(FHandle.Ptr),
    OCI_DTYPE_LOB, 0, nil);
  if Status = OCI_SUCCESS then
  begin
    LobEmpty := 0;
    Status := OCIAttrSet(POCIDescriptor(FHandle.Ptr), OCI_DTYPE_LOB, @LobEmpty,
      0, OCI_ATTR_LOBEMPTY, OraTransact.ErrorHandle);
    OraTransact.CheckError(OraTransact.ErrorHandle, Status, FError);
*)
    SetStatus(bsOk);
    SetActive(True);
//  end;
  FPosition := 0;
end;

{ Delete current blob }
procedure TDirOraSqlBlob.DropBlob;
(*
var
  OraTransact: TDirOraSqlTransact;
  Status: Integer;
*)
begin
  inherited DropBlob;
  if not Assigned(Transact) then Exit;
(*
  OraTransact := TDirOraSqlTransact(Transact);
  Status := OCILobTrim(OraTransact.Handle, OraTransact.ErrorHandle,
    POCIDescriptor(Handle.Ptr), 0);
  OraTransact.CheckError(OraTransact.ErrorHandle, Status, FError);
  if Status = OCI_SUCCESS then
    Close;
*)
  SetStatus(bsOk);
end;

{ Read segment from open blob }
function TDirOraSqlBlob.Read(Buffer: PChar; Length: Integer): Integer;
var
  OraTransact: TDirOraSqlTransact;
  Affected: ub4;
  Status: Integer;
begin
  Result := 0;
  SetStatus(bsFail);
  if not Assigned(Transact) then Exit;
  if not Transact.Active or not Active then Exit;

  OraTransact := TDirOraSqlTransact(Transact);
  Affected := Length;
  Status := OCILobRead(OraTransact.Handle, OraTransact.ErrorHandle,
    POCIDescriptor(Handle.Ptr), Affected, FPosition+1, Buffer, Length, nil, nil,
    0, SQLCS_IMPLICIT);
  OraTransact.CheckError(OraTransact.ErrorHandle, Status, FError);

  if Status = OCI_SUCCESS then
  begin
    Result := Affected;
    FPosition := FPosition + Affected;
    SetStatus(bsOk);
  end;
end;

{ Write segment to open blob }
function TDirOraSqlBlob.Write(Buffer: PChar; Length: Integer): Integer;
(*
var
  Affected: ub4;
  OraTransact: TDirOraSqlTransact;
  Status: Integer;
*)
begin
  Result := 0;
  SetStatus(bsFail);
  if Handle.Ptr = 0 then Exit;
  if not Assigned(Transact) then Exit;
  if not Transact.Active or not Active then Exit;
(*
  OraTransact := TDirOraSqlTransact(Transact);
  Affected := Length;
  Status := OCILobWrite(OraTransact.Handle, OraTransact.ErrorHandle,
    POCIDescriptor(Handle.Ptr), Affected, FPosition+1, Buffer, Length,
    OCI_ONE_PIECE, nil, nil, 0, SQLCS_IMPLICIT);
  OraTransact.CheckError(OraTransact.ErrorHandle, Status, FError);

  if Status = OCI_SUCCESS then
  begin
    Result := Affected;
    FPosition := FPosition + Affected;
*)
    SetStatus(bsOk);
//  end;
end;

{**************** Extra functions *******************}

{ Convert Oracle field types to delphi field types }
function OraSqlToDelphiType(Value: string; Size, Prec: Integer;
  var BlobType: TBlobType): TFieldType;
begin
  BlobType := btInternal;
  if StrCaseCmp(Value, 'NUMBER') then
  begin
    if Prec = 0 then
      Result := ftInteger
    else Result := ftFloat;
  end
  else if StrCaseCmp(Value, 'VARCHAR2') or StrCaseCmp(Value, 'CHAR')
    or StrCaseCmp(Value, 'NCHAR') or StrCaseCmp(Value, 'NVARCHAR2') then
    Result := ftString
  else if StrCaseCmp(Value, 'LONG') then
    Result := ftMemo
  else if StrCaseCmp(Value, 'CLOB') then
  begin
    Result := ftMemo;
    BlobType := btExternal;
  end
  else if StrCaseCmp(Value, 'RAW') or StrCaseCmp(Value, 'LONG RAW') then
    Result := ftBlob
  else if StrCaseCmp(Value, 'BLOB') then
  begin
    Result := ftBlob;
    BlobType := btExternal;
  end
  else if StrCaseCmp(Value, 'FLOAT') then
    Result := ftFloat
  else if StrCaseCmp(Value, 'DATE') then
    Result := ftDateTime
  else
    Result := ftUnknown;
end;

{ Convert oracle internal date to date time }
function OraDateToDateTime(Value: PChar): TDateTime;
type
  TOraDate = array[1..7] of Byte;
  POraDate = ^TOraDate;
var
  Ptr: POraDate;
begin
  Ptr := POraDate(Value);
  Result := EncodeDate((Ptr[1] - 100) * 100 + Ptr[2] - 100, Ptr[3], Ptr[4]) +
    EncodeTime(Ptr[5]-1, Ptr[6]-1, Ptr[7]-1, 0);
end;

initialization
  MonitorList := TZMonitorList.Create;
finalization
  MonitorList.Free;
end.

