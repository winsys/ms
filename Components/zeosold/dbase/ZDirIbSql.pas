{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{              Interbase direct class API                }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZDirIbSql;

interface

{$IFNDEF LINUX}
{$INCLUDE ..\ZeosDef.inc}
{$ELSE}
{$INCLUDE ../ZeosDef.inc}
{$ENDIF}

uses {$IFNDEF LINUX}Windows,{$ENDIF}{$IFDEF VERCLX}Variants,{$ENDIF} Classes,
  SysUtils, DB, ZLibIbSql, ZDirSql, ZSqlTypes, ZTransact
  {$IFDEF VER100}, DbTables{$ENDIF};

{$IFNDEF LINUX}
{$INCLUDE ..\Zeos.inc}
{$ELSE}
{$INCLUDE ../Zeos.inc}
{$ENDIF}

type
  { Interbase Parameter Block class }
  TIbParamList = class
  private
    FList: TStringList;

    function GetParam(Index: Word): string;
    procedure SetParam(Index: Word; Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    function IndexOf(Index: Word): Integer;
    procedure Add(Index: Word; Value: string);
    procedure AddParams(Value: TIbParamList);
    procedure Delete(Index: Word);
    procedure Clear;

    procedure GenerateDPB(var DPB: string; var DPBLength: Word);
    procedure GenerateTPB(var TPB: string; var TPBLength: Word);

    property Params[Index: Word]: string read GetParam write SetParam;
  end;

  { Interbase status array }
  ARRAY_ISC_STATUS = array[0..20] of ISC_STATUS;
  PARRAY_ISC_STATUS = ^ARRAY_ISC_STATUS;

  { Direct connection to Interbase database }
  TDirIbSqlConnect = class(TDirConnect)
  private
    FHandle: TISC_DB_HANDLE;
    FStatusVector: ARRAY_ISC_STATUS;
    FParams: TIbParamList;
    FDialect: Word;
    FCharSet: string;
    FSqlRole: string;
    function HasError: Boolean;
  protected
    function GetErrorMsg: ShortString; override;
    function GetFullDbName: string;
    function CheckResult(Cmd: string): Boolean;
    function GetStatusVector(Index: Word): ISC_STATUS;
    procedure SetStatusVector(Index: Word; Value: ISC_STATUS);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Connect; override;
    procedure Disconnect; override;
    procedure CreateDatabase(Params: string); override;
    procedure DropDatabase; override;

    property Handle: TISC_DB_HANDLE read FHandle;
    property StatusVector[Index: Word]: ISC_STATUS read GetStatusVector
      write SetStatusVector;
    property Params: TIbParamList read FParams;
    property Dialect: Word read FDialect write FDialect;
    property CharSet: string read FCharSet write FCharSet;
    property SqlRole: string read FSqlRole write FSqlRole;
  end;

  { Transaction types }
  TZIbSqlTransIsolation = (itDefault, itConcurrency, itConsistency,
    itReadCommitted, itReadCommittedRec);

  { Direct Interbase transaction }
  TDirIbSqlTransact = class(TDirTransact)
  private
    FHandle: TISC_TR_HANDLE;
    FParams: TIbParamList;
    FTransIsolation: TZIbSqlTransIsolation;
  public
    constructor Create(AConnect: TDirIbSqlConnect);
    destructor Destroy; override;

    procedure Open; override;
    procedure Close; override;
    procedure StartTransaction; override;
    procedure EndTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;

    property Handle: TISC_TR_HANDLE read FHandle;
    property Params: TIbParamList read FParams;
    property TransIsolation: TZIbSqlTransIsolation read FTransIsolation
      write FTransIsolation;
  end;

  { Interbase Statement Type }
  TIbSqlStmtType = (stUnknown, stSelect, stInsert, stUpdate, stDelete, stDDL,
    stGetSegment, stPutSegment, stExecProc, stStartTrans, stCommit, stRollback,
    stSelectForUpdate, stSetGenerator);

  TProcParamType = (ppInput, ppOutput);
  TProcParamTypes = set of TProcParamType;

{ Maximum xsqlvar buffer }
const
  MAX_XSQLVAR = 3;
  NULL_FLAG: SmallInt = ISC_NULL;

type

  { Direct Interbase Query }
  TDirIbSqlQuery = class(TDirQuery)
  private
    FHandle: TISC_STMT_HANDLE;
    FOutSqlDa: PXSQLDA;
    FInSqlDa: PXSQLDA;

    FPrepared: Boolean;
    FStatementType: TIbSqlStmtType;

    function GetPlan: string;
    function SqlStatementType: Boolean;
    function SqlAffectedRows: Integer;
    procedure AbortOnError;

    function FreeStatement: Boolean;
  protected
    function GetErrorMsg: ShortString; override;

    procedure UpdateParams(Params: TParams);
    function PrepareStatement: Boolean;
    function ExecStatement: Boolean;
  public
    constructor Create(AConnect: TDirIbSqlConnect; ATransact: TDirIbSqlTransact);
    destructor Destroy; override;

    function Execute: LongInt; override;
    function ExecuteImmediate: LongInt;

    function ExecuteParams(Params: TVarRecArray;
      ParamCount: Integer): LongInt; override;

    procedure Open; override;
    procedure Close; override;
    function CreateBlobObject: TDirBlob; override;

    procedure ShowDatabases(DatabaseName: ShortString); override;
    procedure ShowTables(TableName: ShortString); override;
    procedure ShowColumns(TableName, ColumnName: ShortString); override;
    procedure ShowIndexes(TableName: ShortString); override;
    procedure ShowProcs(ProcName: ShortString);
    procedure ShowProcsParams(ProcName: ShortString);

    procedure First; override;
    procedure Last; override;
    procedure Prev; override;
    procedure Next; override;
    procedure Go(Num: Integer); override;

    function FieldCount: Integer; override;
    function RecordCount: Integer; override;

    function FieldName(FieldNum: Integer): ShortString; override;
    function FieldSize(FieldNum: Integer): Integer; override;
    function FieldMaxSize(FieldNum: Integer): Integer; override;
    function FieldPrecision(FieldNum: Integer): Integer; override;
    function FieldDecimals(FieldNum: Integer): Integer; override;
    function FieldType(FieldNum: Integer): Integer; override;
    function FieldDataType(FieldNum: Integer): TFieldType; override;
    function FieldIsNull(FieldNum: Integer): Boolean; override;
    function FieldReadOnly(FieldNum: Integer): Boolean; override;
    function Field(FieldNum: Integer): string; override;
    function FieldBuffer(FieldNum: Integer): PChar; override;

    function FieldSubType(FieldNum: Integer): Integer;
    function FieldValue(FieldNum: Integer): Variant;
    function GetFieldValue(FieldNum: Integer; var Buffer): boolean;

    function StringToSql(Value: string): string; override;

    property Handle: TISC_STMT_HANDLE read FHandle;
    property Prepared: Boolean read FPrepared;
    property Plan: string read GetPlan;
  end;

  { Class for interbase large object }
  TDirIbSqlBlob = class(TDirBlob)
  private
    FBlobHandle: TISC_BLOB_HANDLE;
  public
    constructor Create(AConnect: TDirConnect; ATransact: TDirTransact;
      AHandle: TBlobHandle);

    procedure Open(Mode: Integer); override;
    procedure Close; override;
    procedure CreateBlob; override;
    procedure DropBlob; override;

    function Read(Buffer: PChar; Length: Integer): Integer; override;
    function Write(Buffer: PChar; Length: Integer): Integer; override;

    property BlobHandle: TISC_BLOB_HANDLE read FBlobHandle;
  end;

  TDirIbSqlArray = class(TDirBlob)
  private
    FArrayDesc: TISC_ARRAY_DESC;
    FSQLVAR: PXSQLVAR;
  public
    constructor Create(AConnect: TDirConnect; ATransact: TDirTransact;
      AHandle: TBlobHandle; ASQLVAR: PXSQLVAR);

    procedure Open(Mode: Integer); override;
    procedure Close; override;
    procedure CreateBlob; override;
    procedure DropBlob; override;

    function Read(Buffer: PChar; Length: Integer): Integer; override;
    function Write(Buffer: PChar; Length: Integer): Integer; override;
  end;

  {TDirNotify}
  TDirIbSqlNotify = class(TDirNotify)
  private
    { IB API call parameters }
    WhichEvent: Integer;
    EventID: ISC_LONG;
    EventBuffer: PChar;
    EventBufferLen: SmallInt;
    ResultBuffer: PChar;

    FEvents: TStrings;
    FParent: TZNotify;
    EventCount: Integer;

    AStatusVector: ARRAY_ISC_STATUS;

    //procedure ProcessEvents;
    procedure UpdateResultBuffer(Length: Short; Updated: PChar);
    //procedure DoQueueEvents;
  protected
    function GetErrorMsg: ShortString; override;

    procedure RegisterEvents; virtual;
    procedure UnRegisterEvents; virtual;
  public
    constructor Create(AParent: TZNotify; AConnect: TDirIbSqlConnect; ATransact: TDirIbSqlTransact);
    destructor Destroy; override;

    procedure ListenTo(Event: string); override;
    procedure UnlistenTo(Event: string); override;
    procedure DoNotify(Event: string); override;
    function CheckEvents: string; override;

    property Parent: TZNotify read FParent;
  end;


{ Convert interbase field types to delphi field types }
function IbSqlToDelphiType(Value, SubType, Decimals: Integer): TFieldType;

function QuoteIdentifier(Dialect: Integer; Value: string): string;

procedure IBReAllocMem(var P; OldSize, NewSize: Integer);

{ Monitor list }
var
  MonitorList: TZMonitorList;

implementation

uses ZExtra, ZDBaseConst, Math, ZSqlExtra;

{*************** TIbParamList class implementation ***************}

{ Class constructor }
constructor TIbParamList.Create;
begin
  FList := TStringList.Create;
end;

{ Class destructor }
destructor TIbParamList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

{ Get parameter }
function TIbParamList.GetParam(Index: Word): string;
var
  N: Integer;
begin
  N := IndexOf(Index);
  if N >= 0 then
    Result := FList[N]
  else
    Result := '';
end;

{ Set parameter }
procedure TIbParamList.SetParam(Index: Word; Value: string);
begin
  Add(Index, Value);
end;

{ Assign a value }
procedure TIbParamList.AddParams(Value: TIbParamList);
begin
  FList.AddStrings(Value.FList);
end;


{ Get param index }
function TIbParamList.IndexOf(Index: Word): Integer;
begin
  Result := FList.IndexOfObject(TObject(Index));
end;

{ Add new parameter }
procedure TIbParamList.Add(Index: Word; Value: string);
var
  N: Integer;
begin
  N := IndexOf(Index);
  if N >= 0 then
    FList[N] := Value
  else
    FList.AddObject(Value, TObject(Index));
end;

{ Delete parameter }
procedure TIbParamList.Delete(Index: Word);
var
  N: Integer;
begin
  N := IndexOf(Index);
  if N >= 0 then
    FList.Delete(N);
end;

{ Clear param list }
procedure TIbParamList.Clear;
begin
  FList.Clear;
end;

{ Fill database parameter block }
procedure TIbParamList.GenerateDPB(var DPB: string; var DPBLength: Word);
var
  I, PValue: Integer;
  ParamNo: Word;
  ParamValue: string;
begin
  DPBLength := 1;
  DPB := Char(isc_dpb_version1);

  for I := 0 to FList.Count - 1 do
  begin
    ParamNo := Word(FList.Objects[I]);
    ParamValue := FList[I];

    case ParamNo of
      isc_dpb_user_name, isc_dpb_password, isc_dpb_password_enc,
      isc_dpb_sys_user_name, isc_dpb_license, isc_dpb_encrypt_key,
      isc_dpb_lc_messages, isc_dpb_lc_ctype, isc_dpb_sql_role_name:
        begin
          DPB := DPB + Char(ParamNo) + Char(Length(ParamValue)) + ParamValue;
          Inc(DPBLength, 2 + Length(ParamValue));
        end;
      isc_dpb_num_buffers, isc_dpb_dbkey_scope, isc_dpb_force_write,
      isc_dpb_no_reserve, isc_dpb_damaged, isc_dpb_verify:
        begin
          DPB := DPB + Char(ParamNo) + #1 + Char(StrToInt(ParamValue));
          Inc(DPBLength, 3);
        end;
      isc_dpb_sweep:
        begin
          DPB := DPB + Char(ParamNo) + #1 + Char(isc_dpb_records);
          Inc(DPBLength, 3);
        end;
      isc_dpb_sweep_interval:
        begin
          PValue := StrToInt(ParamValue);
          DPB := DPB + Char(ParamNo) + #4 + PChar(@PValue)[0] + PChar(@PValue)[1] +
            PChar(@PValue)[2] + PChar(@PValue)[3];
          Inc(DPBLength, 6);
        end;
      isc_dpb_activate_shadow, isc_dpb_delete_shadow, isc_dpb_begin_log,
      isc_dpb_quit_log:
        begin
          DPB := DPB + Char(ParamNo) + #1 + #0;
          Inc(DPBLength, 3);
        end;
    end;
  end;
end;

{ Fill transaction parameter block }
procedure TIbParamList.GenerateTPB(var TPB: string; var TPBLength: Word);
var
  I: Integer;
  ParamNo: Word;
  ParamValue: string;
begin
  if FList.Count = 0 then
  begin
    TPB := '';
    TPBLength := 0;
    Exit;
  end
  else
  begin
    TPB := Char(isc_tpb_version3);
    TPBLength := 1;
  end;

  for I := 0 to FList.Count - 1 do
  begin
    ParamNo := Word(FList.Objects[I]);
    ParamValue := FList[I];

    case ParamNo of
      isc_tpb_consistency, isc_tpb_exclusive, isc_tpb_protected,
      isc_tpb_concurrency, isc_tpb_shared, isc_tpb_wait, isc_tpb_nowait,
      isc_tpb_read, isc_tpb_write, isc_tpb_ignore_limbo,
      isc_tpb_read_committed, isc_tpb_rec_version, isc_tpb_no_rec_version:
        begin
          TPB := TPB + Char(ParamNo);
          Inc(TPBLength, 1);
        end;
      isc_tpb_lock_read, isc_tpb_lock_write:
        begin
          TPB := TPB + Char(ParamNo) + Char(Length(ParamValue)) + ParamValue;
          Inc(TPBLength, Length(ParamValue) + 2);
        end;
    end;
  end;
end;

{**************** TDirIbSqlConnect implementation ************}

{ Class constructor }
constructor TDirIbSqlConnect.Create;
begin
  inherited Create;
  FParams := TIbParamList.Create;
  FHandle := nil;
  FDialect := 1;
end;

{ Class destructor }
destructor TDirIbSqlConnect.Destroy;
begin
  inherited Destroy;
  FParams.Free;
end;

{ Get status vector item }
function TDirIbSqlConnect.GetStatusVector(Index: Word): ISC_STATUS;
begin
  Result := FStatusVector[Index];
end;

{ Set status vector item }
procedure TDirIbSqlConnect.SetStatusVector(Index: Word; Value: ISC_STATUS);
begin
  FStatusVector[Index] := Value;
end;

function TDirIbSqlConnect.HasError: Boolean;
begin
  Result := (StatusVector[0] = 1) and (StatusVector[1] > 0);
end;

{ Check result code }
function TDirIbSqlConnect.CheckResult(Cmd: string): Boolean;
begin
  if (StatusVector[0] = 1) and (StatusVector[1] > 0) then
    SetStatus(csFail)
  else
    SetStatus(csOk);
  Result := (Status = csOk);
  if Cmd <> '' then
    MonitorList.InvokeEvent(Cmd, Error, not Result);
end;

{ Get full database name with host name }
function TDirIbSqlConnect.GetFullDbName: string;
begin
  Result := Trim(Database);
  HostName := Trim(HostName);
  if HostName <> '' then
    Result := HostName + ':' + Result;
end;

{ Connect to existed database }
procedure TDirIbSqlConnect.Connect;
var
  Temp: string;
  DBName: array[0..512] of Char;
  DPB: PChar;
  DPBLen: Word;
begin
  inherited Connect;
  CheckIbSqlLoaded;

  if Login <> '' then
    Params.Add(isc_dpb_user_name, Login);
  if Passwd <> '' then
    Params.Add(isc_dpb_password, Passwd);
  if FCharSet <> '' then
    Params.Add(isc_dpb_lc_ctype, FCharSet);
  if FSqlRole <> '' then
    Params.Add(isc_dpb_sql_role_name, FSqlRole);

  Params.GenerateDPB(Temp, DPBLen);
  DPBLen := Length(Temp);
  DPB := StrAlloc(DPBLen + 1);
  StrPCopy(DPB, Temp);
  StrPCopy(DBName, GetFullDbName);

  { Connect database }
  FHandle := nil;
  isc_attach_database(@FStatusVector, StrLen(DBName), DBName, @Handle,
    DPBLen, DPB);
  StrDispose(DPB);

  SetActive(CheckResult(Format('CONNECT ''%s''', [GetFullDbName])));
end;

{ Disconnect from database }
procedure TDirIbSqlConnect.Disconnect;
begin
  if Active then
  begin
    isc_detach_database(@FStatusVector, @Handle);
    CheckResult(Format('DISCONNECT ''%s''', [GetFullDbName]));
    SetActive(False);
  end;
end;

{ Create new database }
procedure TDirIbSqlConnect.CreateDatabase(Params: string);
var
  TrHandle: TISC_TR_HANDLE;
  Buffer: string;
begin
  inherited CreateDatabase(Params);
  CheckIbSqlLoaded;
  Disconnect;
  TrHandle := nil;

  Buffer := Format('CREATE DATABASE ''%s'' USER ''%s'' PASSWORD ''%s'' %s',
    [GetFullDbName, Login, Passwd, Params]);
  isc_dsql_execute_immediate(@FStatusVector, @Handle, @TrHandle, 0,
    PChar(Buffer), Dialect, nil);
  if CheckResult(Buffer) then
    isc_detach_database(@FStatusVector, @Handle);
end;

{ Drop existed database }
procedure TDirIbSqlConnect.DropDatabase;
begin
  if not Active then Connect;
  if Active then
  begin
    isc_drop_database(@FStatusVector, @Handle);
    CheckResult(Format('DROP DATABASE ''%s''', [GetFullDbName]));
    SetActive(False);
  end;
end;

{ Get Interbase Database Error }
function TDirIbSqlConnect.GetErrorMsg: ShortString;
var
  PStatusVector: PISC_STATUS;
  Msg: array[0..1024] of Char;
begin
  if (FStatusVector[0] = 1) and (FStatusVector[1] > 0) then
  begin
    PStatusVector := @FStatusVector;
    isc_interprete(Msg, @PStatusVector);
    Result := StrPas(Msg);
  end else if not Active then
    Result := SNotConnected
  else Result := '';
end;

{************* TDirIbSqlTransact implementation *************}

{ Class constructor }
constructor TDirIbSqlTransact.Create(AConnect: TDirIbSqlConnect);
begin
  inherited Create;
  FParams := TIbParamList.Create;
  FHandle := nil;
  Connect := AConnect;
end;

{ Class destructor }
destructor TDirIbSqlTransact.Destroy;
begin
  inherited Destroy;
  FParams.Free;
end;

{ Connect transaction }
procedure TDirIbSqlTransact.Open;
begin
  inherited Open;
  SetStatus(csFail);
  StartTransaction;
  SetActive(Status = csOk);
end;

{ Disconnect transaction }
procedure TDirIbSqlTransact.Close;
begin
  EndTransaction;
  SetActive(False);
end;

{ Start transaction }
procedure TDirIbSqlTransact.StartTransaction;
var
  Temp: string;
  PTPB: PChar;
  TPBLen: Word;
  PTEB: PISC_TEB;
  TempParams: TIbParamList;
begin
  SetStatus(csFail);
  if not Assigned(Connect) or not Connect.Active then
    Exit;

  TempParams := TIbParamList.Create;
  try
    case TransIsolation of
      itConsistency:
        begin
          TempParams.Add(isc_tpb_consistency, '');
        end;
      itConcurrency :
        begin
          TempParams.Add(isc_tpb_concurrency, '');
        end;
      itReadCommitted:
        begin
          TempParams.Add(isc_tpb_read_committed, '');
          TempParams.Add(isc_tpb_no_rec_version, '');
        end;
      itReadCommittedRec:
        begin
          TempParams.Add(isc_tpb_read_committed, '');
          TempParams.Add(isc_tpb_rec_version, '');
        end;
    end;
    TempParams.AddParams(Params);
    TempParams.Add(isc_tpb_nowait, '');
    TempParams.GenerateTPB(Temp, TPBLen);
  finally
    TempParams.Free;
  end;
  TPBLen := Length(Temp);
  if TPBLen > 0 then
  begin
    PTPB := StrAlloc(TPBLen + 1);
    PTPB := StrPCopy(PTPB, Temp);
  end else
    PTPB := nil;

  FHandle := nil;
  PTEB := AllocMem(Sizeof(TISC_TEB));
  try
    {
     //problem with GDS32.dll ---> ib6
     isc_start_transaction(@TDirIbSqlConnect(Connect).FStatusVector, @FHandle, 1,
       @TDirIbSqlConnect(Connect).Handle, TPBLen, PTPB);
    }

    //ajouter par fourat-----> isc_start_multiple
    with PTEB^ do
    begin
      db_handle := @TDirIbSqlConnect(Connect).FHandle;
      tpb_length := TPBLen;
      tpb_address := PTPB;
    end;

    isc_start_multiple(@TDirIbSqlConnect(Connect).FStatusVector, @FHandle, 1, PTEB);
  finally
    StrDispose(PTPB);
    FreeMem(PTEB);
  end;

  TDirIbSqlConnect(Connect).CheckResult('START TRANSACTION');
  SetStatus(Connect.Status);
end;

{ End transaction }
procedure TDirIbSqlTransact.EndTransaction;
begin
  SetStatus(csFail);
  if Active then
  begin
    isc_rollback_transaction(@TDirIbSqlConnect(Connect).FStatusVector, @Handle);
    TDirIbSqlConnect(Connect).CheckResult('END TRANSACTION');
    SetStatus(Connect.Status);
    FHandle := nil;
  end else
    SetStatus(csOk);
end;

{ Commit transaction }
procedure TDirIbSqlTransact.Commit;
begin
  SetStatus(csFail);
  if Active then
  begin
    isc_commit_retaining(@TDirIbSqlConnect(Connect).FStatusVector, @Handle);
    TDirIbSqlConnect(Connect).CheckResult('COMMIT');
    SetStatus(Connect.Status);
  end;
end;

{ Rollback transaction }
procedure TDirIbSqlTransact.Rollback;
begin
  SetStatus(csFail);
  if Active then
  begin
    if GetIbSqlClientVersion >= 6 then
    begin
      isc_rollback_retaining(@TDirIbSqlConnect(Connect).FStatusVector, @Handle);
      TDirIbSqlConnect(Connect).CheckResult('ROLLBACK');
      SetStatus(Connect.Status);
    end
    else
    begin
      isc_rollback_transaction(@TDirIbSqlConnect(Connect).FStatusVector, @Handle);
      TDirIbSqlConnect(Connect).CheckResult('ROLLBACK');
      SetStatus(Connect.Status);
      if Connect.Status = csOk then
        StartTransaction;
    end;
  end;
end;

{************* TDirIbSqlQuery implementation ************}

{ Class constructor }
constructor TDirIbSqlQuery.Create(AConnect: TDirIbSqlConnect;
  ATransact: TDirIbSqlTransact);
begin
  inherited Create;
  Connect := AConnect;
  Transact := ATransact;

  FOutSqlDa := AllocMem(XSQLDA_LENGTH(MAX_XSQLVAR));
  FOutSqlDa.version := SQLDA_VERSION1;
  FOutSqlDa.sqln := MAX_XSQLVAR;

  FInSqlDa := AllocMem(XSQLDA_LENGTH(MAX_XSQLVAR));
  FInSqlDa.version := SQLDA_VERSION1;
  FInSqlDa.sqln := MAX_XSQLVAR;

  FHandle := nil;
  FPrepared := False;
  FStatementType := stUnknown;
end;

{ Class destructor }
destructor TDirIbSqlQuery.Destroy;
begin
  inherited Destroy;
  FreeMem(FInSqlDa);
  FreeMem(FOutSqlDa);
  FreeStatement;
end;

{ Get an error message }
function TDirIbSqlQuery.GetErrorMsg: ShortString;
var
  Msg: array[0..1024] of Char;
  SqlCode: LongInt;
  LastError: string;
  PStatusVector: PISC_STATUS;
  IbConnect: TDirIbSqlConnect;
begin
  Result := 'Connection is not defined';
  if not Assigned(Connect) then Exit;
  IbConnect := TDirIbSqlConnect(Connect);

  if IbConnect.HasError then
  begin
    PStatusVector := @IbConnect.FStatusVector;
    while isc_interprete(Msg, @PStatusVector) <> 0 do
    begin
      LastError := Trim(StrPas(Msg));
      if LastError <> '' then
      begin
        if Result <> '' then
          Result := Result + #13#10;
          Result := Result + LastError;
      end;
    end;

    SqlCode := isc_sqlcode(@IbConnect.FStatusVector);
    isc_sql_interprete(SqlCode, Msg, 1024);

    if Result <> '' then
      Result := Result + #13#10;
    Result := Result + Trim(StrPas(Msg));
  end;
end;

procedure TDirIbSqlQuery.AbortOnError;
var
  IbConnect: TDirIbSqlConnect;
begin
  IbConnect := TDirIbSqlConnect(Connect);
  IbConnect.CheckResult('');
  if IbConnect.HasError then
  begin
    SetStatus(qsFail);
    Abort;
  end;
end;

function TDirIbSqlQuery.GetPlan: string;
var
  Out_buffer: array[0..16384] of Char;
  Out_length: Integer;
  req_info: Char;
  IbConnect: TDirIbSqlConnect;
begin
  IbConnect := TDirIbSqlConnect(Connect);
  if (FHandle = nil) or (not FPrepared) or
    not (FStatementType in [stSelect, stSelectForUpdate,
    {stExecProc,} stUpdate, stDelete]) then
    Result := ''
  else
  begin
    req_info := Char(isc_info_sql_get_plan);

    isc_dsql_sql_info(@IbConnect.FStatusVector, @FHandle,
      1, @req_info, SizeOf(Out_buffer), Out_buffer);

    if Out_buffer[0] <> Char(isc_info_sql_get_plan) then
      DatabaseError('Unknown Plan');

    Out_length := isc_vax_integer(@Out_buffer[1], 2);

    SetString(result, nil, Out_length);
    Move(Out_buffer[3], Result[1], Out_length);
    Result := Trim(result);
  end;
end;

function TDirIbSqlQuery.SqlAffectedRows: Integer;
var
  OutBuffer: array[0..255] of Char;
  ReqInfo: Char;
  IbConnect: TDirIbSqlConnect;
begin
  Result := -1;
  if (FHandle = nil) or not FPrepared then Exit;
  IbConnect := TDirIbSqlConnect(Connect);

  ReqInfo := Char(isc_info_sql_records);
  if isc_dsql_sql_info(@IbConnect.FStatusVector, @FHandle, 1, @ReqInfo,
    SizeOf(OutBuffer), OutBuffer) > 0 then
    Exit;
  if OutBuffer[0] = Char(isc_info_sql_records) then
  begin
    case FStatementType of
      {
      stSelectForUpdate,
      stSelect: Result := isc_vax_integer(@Out_buffer[1], 4);
      }
      stUpdate: Result := isc_vax_integer(@OutBuffer[6], 4);
      stDelete: Result := isc_vax_integer(@OutBuffer[13], 4);
      stInsert: Result := isc_vax_integer(@OutBuffer[27], 4);
      else Result := -1;
    end;
  end;
end;

function TDirIbSqlQuery.SqlStatementType: Boolean;
var
  StatementLen: Integer;
  StatementBuffer: array[0..7] of Char;
  TypeItem: Char;
  IbConnect: TDirIbSqlConnect;
begin
  Result := False;
  FStatementType := stUnknown;
  if (FHandle = nil) or not FPrepared then Exit;
  IbConnect := TDirIbSqlConnect(Connect);

  TypeItem := Char(isc_info_sql_stmt_type);
  isc_dsql_sql_info(@IbConnect.FStatusVector, @FHandle, 1, @TypeItem,
    SizeOf(StatementBuffer), StatementBuffer);

  if StatementBuffer[0] = Char(isc_info_sql_stmt_type) then
  begin
    StatementLen := isc_vax_integer(@StatementBuffer[1], 2);
    FStatementType := TIbSqlStmtType(isc_vax_integer(@StatementBuffer[3],
      StatementLen));
    Result := True;
  end;
end;

function TDirIbSqlQuery.ExecuteImmediate: LongInt;
begin
  inherited Execute;

  SetStatus(qsFail);
  Result := 0;
  if Assigned(Connect) and Assigned(Transact) then
  begin
    isc_dsql_execute_immediate(@TDirIbSqlConnect(Connect).FStatusVector,
      @TDirIbSqlConnect(Connect).Handle, @TDirIbSqlTransact(Transact).Handle,
{$IFDEF DELETE_QUERY_SPACES}
      0, PChar(ClearSpaces(Sql)), TDirIbSqlConnect(Connect).Dialect, nil);
{$ELSE}
      0, PChar(Sql), TDirIbSqlConnect(Connect).Dialect, nil);
{$ENDIF}

    TDirIbSqlConnect(Connect).CheckResult('');
    if Connect.Status = csOk then
      SetStatus(qsCommandOk);
  end;
end;

{ Execute an SQL statement }
function TDirIbSqlQuery.Execute: LongInt;
var
  IbConnect: TDirIbSqlConnect;
  IbTransact: TDirIbSqlTransact;
begin
  inherited Execute;
  FPrepared := False;

  SetStatus(qsFail);
  Result := 0;
  if Assigned(Connect) and Assigned(Transact) then
  try
    IbConnect := TDirIbSqlConnect(Connect);
    IbTransact := TDirIbSqlTransact(Transact);

    if Active then Close;
    FHandle := nil;
    isc_dsql_alloc_statement2(@ibConnect.FStatusVector, @ibConnect.Handle, @FHandle);
    AbortOnError;

    isc_dsql_prepare(@ibConnect.FStatusVector, @ibTransact.Handle, @FHandle, 0,
{$IFDEF DELETE_QUERY_SPACES}
      PChar(ClearSpaces(Sql)), IbConnect.Dialect, nil);
{$ELSE}
      PChar(Sql), IbConnect.Dialect, nil);
{$ENDIF}
    AbortOnError;

    FPrepared := True;
    { Statement information }
    SQLStatementType;

    { Execute a query }
    isc_dsql_execute(@IbConnect.FStatusVector, @IbTransact.Handle, @FHandle,
      IbConnect.Dialect, nil);
    AbortOnError;

    if Connect.Status = csOk then
      SetStatus(qsCommandOk);
    SetAffectedRows(SQLAffectedRows);
    Result := AffectedRows;
    FreeStatement;
  except
    FreeStatement;
  end;
  TDirIbSqlConnect(Connect).CheckResult(Sql);
end;

{ Execute an SQL statement with params }
function TDirIbSqlQuery.ExecuteParams(Params: TVarRecArray; ParamCount: Integer): LongInt;
var
  I: Short;
  IbConnect: TDirIbSqlConnect;
  IbTransact: TDirIbSqlTransact;
  SqlVar: PXSQLVAR;
begin
  FStatementType := stUnknown;
  FPrepared := False;

  { Check connect and transaction status }
  Result := inherited Execute;
  SetStatus(qsFail);
  if not Assigned(Connect) or not Assigned(Transact)
    or not (Connect.Active and Transact.Active) then
  begin
    TDirIbSqlConnect(Connect).CheckResult(Sql);
    Exit;
  end;

  IbConnect := TDirIbSqlConnect(Connect);
  IbTransact := TDirIbSqlTransact(Transact);

  { Allocate an sql statement }
  FHandle := nil;

  try
    isc_dsql_alloc_statement2(@IbConnect.FStatusVector, @IbConnect.Handle, @FHandle);
    AbortOnError;

    { Prepare an sql statement }
    isc_dsql_prepare(@IbConnect.FStatusVector, @IbTransact.Handle, @FHandle, 0,
{$IFDEF DELETE_QUERY_SPACES}
      PChar(ClearSpaces(Sql)), IbConnect.Dialect, nil);
{$ELSE}
      PChar(Sql), IbConnect.Dialect, nil);
{$ENDIF}
    AbortOnError;

    FPrepared := True;

    { Statement information}
    SqlStatementType;

    isc_dsql_describe_bind(@IbConnect.FStatusVector, @FHandle, IbConnect.Dialect,
      FInSqlDa);
    AbortOnError;

    { Resize XSQLDA structure if needed }
    if FInSqlDa.sqld > FInSqlDa.sqln then
    begin
      IbReallocMem(FInSqlDa, XSQLDA_LENGTH(FInSqlDa.sqln), XSQLDA_LENGTH(FInSqlDa.sqld));
      FInSqlDa.sqln := FInSqlDa.sqld;
      isc_dsql_describe_bind(@IbConnect.FStatusVector, @FHandle, IbConnect.Dialect,
        FInSqlDa);
      AbortOnError;
    end;

    { Allocate memory for result set }
    for I := 0 to FInSqlDa.sqld - 1 do
    begin
      SqlVar := @FInSqlDa.SqlVar[I];
      if (I > ParamCount) or (I > High(Params)) then
      begin
        SqlVar.sqldata := nil;
        SqlVar.sqltype := SqlVar.sqltype or 1;
        SqlVar.sqlind := @NULL_FLAG;
      end
      else
      begin
        SqlVar.sqltype := SqlVar.sqltype and (not 1);
        case SqlVar.sqltype of
          SQL_VARYING, SQL_TEXT:
            begin
              SqlVar.sqldata := @Params[I];
              //SqlVar.sqllen := Length(string(Params[I]));
            end
          else
            SqlVar.sqldata := @Params[I];
          end;
          SqlVar.sqlind := nil;
        end;
      end;

    { Execute a query }
    isc_dsql_execute(@IbConnect.FStatusVector, @IbTransact.Handle, @FHandle,
      IbConnect.Dialect, FInSqlDa);
    AbortOnError;

    SetStatus(qsCommandOk);
    SetAffectedRows(SqlAffectedRows);
    Result := AffectedRows;
    FreeStatement;
  except
    FreeStatement;
  end;
  TDirIbSqlConnect(Connect).CheckResult(Sql);
end;

function DateTimeToIbTimeStamp(Value: TDateTime): TISC_TIMESTAMP;
var
  TmDate: TCTimeStructure;
  Yr, Mn, Dy, Hr, Mt, S, Ms: Word;
begin
  DecodeDate(Value, Yr, Mn, Dy);
  DecodeTime(Value, Hr, Mt, S, Ms);
  with TmDate do
  begin
    tm_sec := S;
    tm_min := Mt;
    tm_hour := Hr;
    tm_mday := Dy;
    tm_mon := Mn - 1;
    tm_year := Yr - 1900;
  end;
  isc_encode_date(@TmDate, @Result);
end;

procedure TDirIbSqlQuery.UpdateParams(Params: TParams);
var
  TmpDateTime: TDateTime;
  TmpTimestamp: TISC_TIMESTAMP;
  TmpSmallint: Smallint;
  TmpInteter: integer;
{$IFNDEF VER100}
  TmpInt64: Int64;
{$ENDIF}

  I, Len: Integer;
  SqlVar: PXSQLVAR;
  Parami: TParam;

  function GetParamName(FieldNum: integer): string;
  begin
    Result := MemPas(FInSqlDa.SqlVar[FieldNum].aliasname,
      FInSqlDa.SqlVar[FieldNum].aliasname_length);
  end;

begin
  for I := 0 to FInSqlDa.sqld - 1 do
  begin
    SqlVar := @FInSqlDa.SqlVar[I];
    try
      Parami := Params.ParamByName(GetParamName(I));
    except
      Parami := nil;
    end;

    if (Parami = nil) or (Parami.Value = Null) then
    begin
      SqlVar.sqldata := nil;
      SqlVar.sqltype := SqlVar.sqltype or 1;
      SqlVar.sqlind := @NULL_FLAG;
    end
    else
    begin
      SqlVar.sqlind := nil;

      case SqlVar.sqltype and (not 1) of
        SQL_VARYING:
          begin //?????????
            if SqlVar.sqllen > length(Parami.AsString) then
              Len := length(Parami.AsString)
            else Len := SqlVar.sqllen;
            ReallocMem(SqlVar.sqldata, SqlVar.sqllen + 2 + 1);
            PShort(SqlVar.sqldata)^ := SqlVar.sqllen;
            StrLCopy(SqlVar.sqldata + 2, Pchar(Parami.AsString), Len);
          end;
        SQL_TEXT: //?????????
          begin
            if SqlVar.sqllen > length(Parami.AsString) then
              Len := Length(Parami.AsString)
            else Len := SqlVar.sqllen;

            ReallocMem(SqlVar.sqldata, SqlVar.sqllen + 1);
            StrLCopy(SqlVar.sqldata, Pchar(Parami.AsString), Len);
          end;
        SQL_TYPE_DATE:
          begin
            ReallocMem(SqlVar.sqldata, Sizeof(ISC_DATE));
            TmpDateTime := Parami.AsDateTime;
            TmptimeStamp := DateTimeToIBTimeStamp(TmpDateTime);
            PISC_DATE(SqlVar.sqldata)^ := TmptimeStamp.timestamp_date;
          end;
        SQL_TYPE_TIME:
          begin
            ReallocMem(SqlVar.sqldata, Sizeof(ISC_TIME));
            TmpDateTime := Parami.AsDateTime;
            TmptimeStamp := DateTimeToIBTimeStamp(TmpDateTime);
            PISC_TIME(SqlVar.sqldata)^ := TmptimeStamp.timestamp_time;
          end;
        SQL_DATE:
          begin
            ReallocMem(SqlVar.sqldata, SizeOf(TISC_TIMESTAMP));
            TmpDateTime := Parami.AsDateTime;
            TmptimeStamp := DateTimeToIBTimeStamp(TmpDateTime);
            PISC_TIMESTAMP(SqlVar.sqldata)^ := TmptimeStamp;
          end;
        SQL_SHORT:
          begin
            ReallocMem(SqlVar.sqldata, Sizeof(Smallint));
            TmpSmallint := Trunc(Parami.AsFloat * IntPower(10, -SqlVar.sqlscale));
            PSmallInt(SqlVar.sqldata)^ := TmpSmallInt;
          end;
        SQL_LONG{$IFDEF VER100}, SQL_INT64{$ENDIF}:
          begin
            ReallocMem(SqlVar.sqldata, Sizeof(Longint));
            TmpInteter := Trunc(Parami.AsFloat * IntPower(10, -SqlVar.sqlscale));
            PLongInt(SqlVar.sqldata)^ := TmpInteter;
          end;
{$IFNDEF VER100}
        SQL_INT64:
          begin
            ReallocMem(SqlVar.sqldata, Sizeof(Int64));
            TmpInt64 := Trunc(Parami.AsFloat * IntPower(10, -SqlVar.sqlscale));
            PInt64(SqlVar.sqldata)^ := TmpInt64;
          end;
{$ENDIF}
        SQL_DOUBLE, SQL_D_FLOAT:
          begin
            ReallocMem(SqlVar.sqldata, Sizeof(double));
            PDouble(SqlVar.sqldata)^ := Parami.AsFloat;
          end;
        SQL_FLOAT:
          begin
            ReallocMem(SqlVar.sqldata, Sizeof(single));
            PSingle(SqlVar.sqldata)^ := Parami.AsFloat;
          end;
        SQL_QUAD, SQL_ARRAY, SQL_BLOB: //??????????
          begin
            SqlVar.sqllen := Length(Parami.AsString);
            ReallocMem(SqlVar.sqldata, SqlVar.sqllen + 1);
            StrLCopy(SqlVar.sqldata, Pchar(Parami.AsString), SqlVar.sqllen);
            SqlVar.sqltype := SQL_TEXT;
          end;
      end;
    end;
  end;
end;

function TDirIbSqlQuery.PrepareStatement{(Params: TParams)}: Boolean;
var
  I: Integer;
  IbConnect: TDirIbSqlConnect;
  IbTransact: TDirIbSqlTransact;
  SqlVar: PXSQLVAR;
begin
  FStatementType := stUnknown;
  FPrepared := False;
  Result := False;

  { Check connect and transaction status }
  SetStatus(qsFail);
  if not Assigned(Connect) or not Assigned(Transact)
    or not (Connect.Active and Transact.Active) then
  begin
    TDirIbSqlConnect(Connect).CheckResult('');
    Exit;
  end;

  IbConnect := TDirIbSqlConnect(Connect);
  IbTransact := TDirIbSqlTransact(Transact);

  try
    { Allocate an sql statement }
    FHandle := nil;

    isc_dsql_alloc_statement2(@IbConnect.FStatusVector, @IbConnect.Handle, @FHandle);
    AbortOnError;

    { Prepare an sql statement }
    isc_dsql_prepare(@IbConnect.FStatusVector, @IbTransact.Handle, @FHandle, 0,
      {$IFDEF DELETE_QUERY_SPACES}
      PChar(ClearSpaces(Sql)), IbConnect.Dialect, nil {FOutSqlDa});
    {$ELSE}
      PChar(Sql), IbConnect.Dialect, nil {FOutSqlDa});
    {$ENDIF}
    AbortOnError;


    FPrepared := True;
    { Statement information }
    SqlStatementType;
    {valid statement ????????????}
    if FStatementType in [stUnknown, stGetSegment, stPutSegment, stStartTrans] then
      DatabaseError('Statement Not Permitted'); //????


    (*
    { Initialise input param }
    if (Params <> nil) and (Params.Count > 0) then
    begin
      isc_dsql_describe_bind(@IbConnect.FStatusVector, @FHandle, IbConnect.Dialect,
        FInSqlDa);
      AbortOnError;


      { Resize XSQLDA structure if needed }
      if FInSqlDa.sqld > FInSqlDa.sqln then
      begin
        IbReallocMem(FInSqlDa, XSQLDA_LENGTH(FInSqlDa.sqln), XSQLDA_LENGTH(FInSqlDa.sqld));
        FInSqlDa.sqln := FInSqlDa.sqld;
        isc_dsql_describe_bind(@IbConnect.FStatusVector, @FHandle, IbConnect.Dialect,
          FInSqlDa);
        AbortOnError;
      end;

      { assign params values }
      if Params.Count > 0 then
        UpdateParams(Params);
    end;
    *)

    { Initialise ouput param or Fields }
    if FStatementType in [stSelect, stSelectForUpdate, stExecProc] then
    begin
      isc_dsql_describe(@IbConnect.FStatusVector, @FHandle, IbConnect.Dialect, FOutSqlDa);
      AbortOnError;

        { Resize XSQLDA structure if needed }
      if FOutSqlDa.sqld > FOutSqlDa.sqln then
      begin
        IbReallocMem(FOutSqlDa, XSQLDA_LENGTH(FOutSqlDa.sqln),
          XSQLDA_LENGTH(FOutSqlDa.sqld));
        FOutSqlDa.sqln := FOutSqlDa.sqld;
        isc_dsql_describe(@IbConnect.FStatusVector, @FHandle,
          IbConnect.Dialect, FOutSqlDa);
        AbortOnError;
      end;

      { Inialise Fields }
      { Allocate memory for result set }
      for I := 0 to FOutSqlDa.sqld - 1 do
      begin
        SqlVar := @FOutSqlDa.SqlVar[I];
        case SqlVar.sqltype and (not 1) of
          SQL_TEXT, SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_DATE,
          SQL_BLOB, SQL_ARRAY, SQL_QUAD, SQL_SHORT,
          SQL_LONG, SQL_INT64, SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
            begin
              if SqlVar.sqllen = 0 then
             { Make sure you get a valid pointer anyway select '' from ibtable }
                GetMem(SqlVar.sqldata, 1)
              else
                GetMem(SqlVar.sqldata, SqlVar.sqllen)
            end;
          SQL_VARYING:
            begin
              GetMem(SqlVar.sqldata, SqlVar.sqllen + 2);
            end;
        end;

        if (SqlVar.sqltype and 1) <> 0 then
          GetMem(SqlVar.sqlind, SizeOf(Short))
        else
          SqlVar.sqlind := nil;
      end;
    end;
    Result := True;
  except
    FreeStatement;
  end;
end;

function TDirIbSqlQuery.ExecStatement: Boolean;
var
  IbConnect: TDirIbSqlConnect;
  IbTransact: TDirIbSqlTransact;
//  SqlVar: PXSQLVAR;
  D_res: ISC_STATUS;
begin
  Result := False;
  { Check connect and transaction status }
  SetStatus(qsFail);
  if not Assigned(Connect) or not Assigned(Transact)
    or not (Connect.Active and Transact.Active) then
    Exit;

  PrepareStatement{(Params)};
  if not FPrepared then Exit;

  IbConnect := TDirIbSqlConnect(Connect);
  IbTransact := TDirIbSqlTransact(Transact);

  { Execute statement }
  try
    case FStatementType of
      stExecProc:
        begin
          D_res := isc_dsql_execute2(@IbConnect.FStatusVector, @IbTransact.Handle, @FHandle,
            IbConnect.Dialect, FInSqlDa, FOutSqlDa);

          //For (pre V6.0) only
          //if error --> try prepare with dialect 1 and ReExecute
          if (D_res <> 0) and (D_res <> 335544345) then //335544345 = isc_lock_conflict
          begin
            isc_dsql_prepare(@IbConnect.FStatusVector, @IbTransact.Handle, @FHandle, 0,
              PChar(Sql), 1 {IbConnect.Dialect}, nil);

            isc_dsql_execute2(@IbConnect.FStatusVector, @IbTransact.Handle, @FHandle,
              IbConnect.Dialect, FInSqlDa, FOutSqlDa);
          end;

          SetBOF(True);
          SetRecNo(0);
        end;
      stSelect, stSelectForUpdate:
        begin
          isc_dsql_execute(@IbConnect.FStatusVector, @IbTransact.Handle, @FHandle,
            IbConnect.Dialect, FInSqlDa);
          SetBOF(False);
        end;
      else
        begin
          isc_dsql_execute(@IbConnect.FStatusVector, @IbTransact.Handle, @FHandle,
            IbConnect.Dialect, FInSqlDa);
          Abort; //go to except for freestatement
        end;
    end;
    AbortOnError;

    SetEOF(False);
    SetActive(True);
    SetStatus(qsTuplesOk);
    if Status <> qsTuplesOk then
      SetActive(False);

    if FStatementType in [stSelect, stSelectForUpdate] then
      Next;
  except
    FreeStatement;
  end;
  Result := (Status <> qsFail);
end;

procedure TDirIbSqlQuery.Open;
begin
  inherited Open;
  ExecStatement;
  if Assigned(Connect) then
    TDirIbSqlConnect(Connect).CheckResult(Sql)
  else
    MonitorList.InvokeEvent(Sql, Error, True);
end;

(*
{ Open a sql query with result set }
procedure TDirIbSqlQuery.Open;
var
  I: Integer;
  IbConnect: TDirIbSqlConnect;
  IbTransact: TDirIbSqlTransact;
  SqlVar: PXSQLVAR;
begin
  inherited Open;
  FPrepared := False;

  { Check connect and transaction status }
  SetStatus(qsFail);
  if not Assigned(Connect) or not Assigned(Transact)
    or not (Connect.Active and Transact.Active) then
  begin
    TDirIbSqlConnect(Connect).CheckResult(Sql);
    Exit;
  end;

  IbConnect := TDirIbSqlConnect(Connect);
  IbTransact := TDirIbSqlTransact(Transact);

  try
    { Allocate an sql statement }
    FHandle := nil;
    isc_dsql_alloc_statement2(@IbConnect.FStatusVector, @IbConnect.Handle, @FHandle);
    AbortOnError;

    { Prepare an sql statement }
    isc_dsql_prepare(@IbConnect.FStatusVector, @IbTransact.Handle, @FHandle, 0,
      {$IFDEF DELETE_QUERY_SPACES}
      PChar(ClearSpaces(Sql)), IbConnect.Dialect, FOutSqlDa);
    {$ELSE}
      PChar(Sql), IbConnect.Dialect, FOutSqlDa);
    {$ENDIF}
    AbortOnError;

    FPrepared := True;
    SqlStatementType;

    { Resize XSQLDA structure if needed }
    if FOutSqlDa.sqld > FOutSqlDa.sqln then
    begin
      IbReallocMem(FOutSqlDa, XSQLDA_LENGTH(FOutSqlDa.sqln), XSQLDA_LENGTH(FOutSqlDa.sqld));
      FOutSqlDa.sqln := FOutSqlDa.sqld;
      isc_dsql_describe(@IbConnect.FStatusVector, @FHandle, IbConnect.Dialect,
        FOutSqlDa);
      AbortOnError;
    end;

    { Allocate memory for result set }
    for I := 0 to FOutSqlDa.sqld - 1 do
    begin
      SqlVar := @FOutSqlDa.SqlVar[I];

      case SqlVar.sqltype and (not 1) of
        SQL_TEXT, SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_DATE,
        SQL_BLOB, SQL_ARRAY, SQL_QUAD, SQL_SHORT,
        SQL_LONG, SQL_INT64, SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT:
          begin
            if (SqlVar.sqllen = 0) then
              { Make sure you get a valid pointer anyway select '' from sqltable }
              GetMem(SqlVar.sqldata, 1)
            else
              GetMem(SqlVar.sqldata, SqlVar.sqllen)
          end;
        SQL_VARYING:
          begin
            GetMem(SqlVar.sqldata, SqlVar.sqllen + 2);
          end;
      end;
      if (SqlVar.sqltype and 1) <> 0 then
        GetMem(SqlVar.sqlind, SizeOf(Short))
      else
        SqlVar.sqlind := nil;
    end;

    { Execute a query }
    isc_dsql_execute(@IbConnect.FStatusVector, @IbTransact.Handle, @FHandle,
      IbConnect.Dialect, nil);
    AbortOnError;

    SetActive(True);
    SetStatus(qsTuplesOk);
    SetBOF(False);
    SetEOF(False);
    Next;
    if Status <> qsTuplesOk then
      SetActive(False);
  except
    FreeStatement;
  end;
end;
*)

{ Free allocated statement }
function TDirIbSqlQuery.FreeStatement: Boolean;
var
  FStatusVector: ARRAY_ISC_STATUS;
begin
  Result := True;
  if FHandle <> nil then
  begin
    isc_dsql_free_statement(@FStatusVector, @FHandle, DSQL_drop);
    if (FStatusVector[0] = 1) and (FStatusVector[0] > 0) then
      Result := False;
    FHandle := nil;
    FPrepared := False;
    TDirIbSqlConnect(Connect).CheckResult('');
  end;
end;

{ Close a sql query with result set }
procedure TDirIbSqlQuery.Close;
var
  I: Integer;
  SqlVar: PXSQLVAR;
begin
  if not Active then Exit;
  inherited Close;

  { Check connect and transaction status }
  SetStatus(qsTuplesOk);
  if not Assigned(Connect) then Exit;

  { Free sql statement }
  FreeStatement;

  { Free output allocated memory }
  for I := 0 to FOutSqlDa.sqld - 1 do
  begin
    SqlVar := @FOutSqlDa.SqlVar[I];
    FreeMem(SqlVar.sqldata);
    FreeMem(SqlVar.sqlind);
    SqlVar.sqldata := nil;
    SqlVar.sqlind := nil;
  end;
  FOutSqlDa.sqld := 0;
end;

{ Create linked blob object }
function TDirIbSqlQuery.CreateBlobObject: TDirBlob;
var
  TempHandle: TBlobHandle;
begin
  FillChar(TempHandle, SizeOf(TBlobHandle), 0);
  Result := TDirIbSqlBlob.Create(Connect, Transact, TempHandle);
end;

function TDirIbSqlQuery.Field(FieldNum: Integer): string;
var
  SqlVar: PXSQLVAR;
  VarType: Short;
  OldSep: Char;
  TempDate: TCTimeStructure;
begin
  Result := '';
  if not Active then Exit;
  SqlVar := @FOutSqlDa.SqlVar[FieldNum];
  VarType := SqlVar.sqltype;
  if ((VarType and 1) <> 0) and ((SqlVar.sqlind^) = -1) then
    Exit;
  OldSep := DecimalSeparator;
  DecimalSeparator := '.';
  case (VarType and (not 1)) of
    SQL_VARYING:
      Result := MemPas(SqlVar.sqldata + 2, PSmallInt(SqlVar.sqldata)^);
    SQL_TEXT:
      Result := TrimRight(MemPas(SqlVar.sqldata, SqlVar.sqllen));
    SQL_LONG {$IFDEF VER100}, SQL_INT64{$ENDIF}:
      begin
        if SqlVar.sqlscale = 0 then
          Result := IntToStr(PLongInt(SqlVar.sqldata)^)
        else if Abs(SqlVar.sqlscale) <= 4 then
          Result := CurrToStr(PLongInt(SqlVar.sqldata)^
            * IntPower(10, SqlVar.sqlscale))
        else
          Result := FloatToStr(PLongInt(SqlVar.sqldata)^
            * IntPower(10, SqlVar.sqlscale));
      end;
    SQL_SHORT:
      begin
        if SqlVar.sqlscale = 0 then
          Result := IntToStr(PSmallInt(SqlVar.sqldata)^)
        else
          Result := CurrToStr(PSmallInt(SqlVar.sqldata)^
            * IntPower(10, SqlVar.sqlscale));
      end;
    SQL_D_FLOAT,
    SQL_DOUBLE:
      Result := FloatToStr(PDouble(SqlVar.sqldata)^);
    SQL_FLOAT:
      Result := FloatToStr(PSingle(SqlVar.sqldata)^);
    SQL_TYPE_TIME:
      begin
        isc_decode_sql_time(PISC_TIME(SqlVar.sqldata), @TempDate);
        Result := DateTimeToSqlDate(
          EncodeTime(Word(TempDate.tm_hour), Word(TempDate.tm_min),
          Word(TempDate.tm_sec), 0));
      end;
    SQL_TYPE_DATE:
      begin
        isc_decode_sql_date(PISC_DATE(SqlVar.sqldata), @TempDate);
        Result := DateTimeToSqlDate(
          EncodeDate(Word(TempDate.tm_year + 1900), Word(TempDate.tm_mon + 1),
          Word(TempDate.tm_mday)));
      end;
{$IFNDEF VER100}
    SQL_INT64:
      begin
        if SqlVar.sqlscale = 0 then
          Result := IntToStr(PInt64(SqlVar.sqldata)^)
        else if Abs(SqlVar.sqlscale) <= 4 then
          Result := CurrToStr(PInt64(SqlVar.sqldata)^
            * IntPower(10, SqlVar.sqlscale))
        else
          Result := FloatToStr(PInt64(SqlVar.sqldata)^
            * IntPower(10, SqlVar.sqlscale));
      end;
{$ENDIF}
    SQL_ARRAY:
      Result := '(ARRAY)';
    SQL_QUAD:
      Result := '(QUAD)';
    SQL_DATE:
      begin
        isc_decode_date(PISC_QUAD(SqlVar.sqldata), @TempDate);
        Result := DateTimeToSqlDate(EncodeDate(TempDate.tm_year + 1900,
          TempDate.tm_mon + 1, TempDate.tm_mday) + EncodeTime(TempDate.tm_hour,
          TempDate.tm_min, TempDate.tm_sec, 0));
      end;
    SQL_BLOB:
      begin
        with TDirIbSqlBlob.Create(Connect, Transact,
          PBlobHandle(SqlVar.sqldata)^) do
        try
          Result := Value;
        finally
          Free;
        end;
      end;
  end;
  DecimalSeparator := OldSep;
end;

function TDirIbSqlQuery.FieldValue(FieldNum: Integer): Variant;
var
  SqlVar: PXSQLVAR;
  VarType: Short;
  OldSep: Char;
  TempDate: TCTimeStructure;
  tmpCurrency: System.Currency;
  tmpDouble: Double;
begin
  Result := Null;
  if not Active then Exit;
  SqlVar := @FOutSqlDa.SqlVar[FieldNum];
  VarType := SqlVar.sqltype;
  if ((VarType and 1) <> 0) and ((SqlVar.sqlind^) = -1) then
    Exit;
  OldSep := DecimalSeparator;
  DecimalSeparator := '.';
  case (VarType and (not 1)) of
    SQL_VARYING:
      Result := MemPas(SqlVar.sqldata + 2, PSmallInt(SqlVar.sqldata)^);
    SQL_TEXT:
      Result := TrimRight(MemPas(SqlVar.sqldata, SqlVar.sqllen));
    SQL_LONG {$IFDEF VER100}, SQL_INT64{$ENDIF}:
      begin
        if SqlVar.sqlscale = 0 then
          Result := PLongInt(SqlVar.sqldata)^
        else
          if abs(SqlVar.sqlscale) <= 4 then
          begin
            tmpCurrency := PLongInt(SqlVar.sqldata)^
              * IntPower(10, SqlVar.sqlscale);
            Result := tmpCurrency;
          end
        else
          begin
            tmpDouble := PLongInt(SqlVar.sqldata)^
              * IntPower(10, SqlVar.sqlscale);
            Result := tmpDouble;
          end
      end;
    SQL_SHORT:
      begin
        if SqlVar.sqlscale = 0 then
          Result := PSmallInt(SqlVar.sqldata)^
        else
        begin
          tmpCurrency := PSmallInt(SqlVar.sqldata)^
            * IntPower(10, SqlVar.sqlscale);
          Result := tmpCurrency;
        end;
      end;
    SQL_D_FLOAT,
    SQL_DOUBLE:
      Result := PDouble(SqlVar.sqldata)^;
    SQL_FLOAT:
      Result := PSingle(SqlVar.sqldata)^;
    SQL_TYPE_TIME:
      begin
        isc_decode_sql_time(PISC_TIME(SqlVar.sqldata), @TempDate);
        Result := EncodeTime(Word(TempDate.tm_hour), Word(TempDate.tm_min),
          Word(TempDate.tm_sec), 0);
      end;
    SQL_TYPE_DATE:
      begin
        isc_decode_sql_date(PISC_DATE(SqlVar.sqldata), @TempDate);
        Result :=
          EncodeDate(Word(TempDate.tm_year + 1900), Word(TempDate.tm_mon + 1),
          Word(TempDate.tm_mday));
      end;
{$IFNDEF VER100}
    SQL_INT64:
      begin
        if SqlVar.sqlscale = 0 then
{$IFDEF VERCLX}
          Result := PInt64(SqlVar.sqldata)^
{$ELSE}
          Result := InttoStr(PInt64(SqlVar.sqldata)^)
{$ENDIF}
        else if Abs(SqlVar.sqlscale) <= 4 then
        begin
          tmpCurrency := PInt64(SqlVar.sqldata)^
            * IntPower(10, SqlVar.sqlscale);
          Result := tmpCurrency;
        end
        else
        begin
          tmpDouble := PInt64(SqlVar.sqldata)^
            * IntPower(10, SqlVar.sqlscale);
          Result := tmpDouble;
        end;
      end;
{$ENDIF}
    //SQL_ARRAY: Result := '(ARRAY)';
    //SQL_QUAD: Result := '(QUAD)';
    SQL_DATE:
      begin
        isc_decode_date(PISC_QUAD(SqlVar.sqldata), @TempDate);
        Result := EncodeDate(TempDate.tm_year + 1900,
          TempDate.tm_mon + 1, TempDate.tm_mday) + EncodeTime(TempDate.tm_hour,
          TempDate.tm_min, TempDate.tm_sec, 0);
      end;
    SQL_BLOB:
      begin
        with TDirIbSqlBlob.Create(Connect, Transact,
          PBlobHandle(SqlVar.sqldata)^) do
          try
            Result := Value;
          finally
            Free;
          end;
      end;
  end;
  DecimalSeparator := OldSep;
end;

{}
function TDirIbSqlQuery.GetFieldValue(FieldNum: Integer; var Buffer): Boolean;
var
  SqlVar: PXSQLVAR;
  VarType: Short;
  OldSep: Char;
  TempTime: TDateTime;
  TempDate: TCTimeStructure;
  tmpCurrency: System.Currency;
  tmpDouble: Double;
begin
  Result := Null;
  if not Active then Exit;
  SqlVar := @FOutSqlDa.SqlVar[FieldNum];
  VarType := SqlVar.sqltype;
  if ((VarType and 1) <> 0) and ((SqlVar.sqlind^) = -1) then
    Exit;
  OldSep := DecimalSeparator;
  DecimalSeparator := '.';
  case (VarType and (not 1)) of
    SQL_VARYING:
      string(Buffer) := MemPas(SqlVar.sqldata + 2, PSmallInt(SqlVar.sqldata)^);
    SQL_TEXT:
      string(Buffer) := TrimRight(MemPas(SqlVar.sqldata, SqlVar.sqllen));
    SQL_LONG {$IFDEF VER100}, SQL_INT64{$ENDIF}:
      begin
        if SqlVar.sqlscale = 0 then
          LongInt(Buffer) := PLongInt(SqlVar.sqldata)^
        else
          if abs(SqlVar.sqlscale) <= 4 then
          begin
            tmpCurrency := PLongInt(SqlVar.sqldata)^
              * IntPower(10, SqlVar.sqlscale);
            Currency(Buffer) := tmpCurrency;
          end
        else
          begin
            tmpDouble := PLongInt(SqlVar.sqldata)^
              * IntPower(10, SqlVar.sqlscale);
            Double(Buffer) := tmpDouble;
          end
      end;
    SQL_SHORT:
      begin
        if SqlVar.sqlscale = 0 then
          SmallInt(Buffer) := PSmallInt(SqlVar.sqldata)^
        else
        begin
          tmpCurrency := PSmallInt(SqlVar.sqldata)^
            * IntPower(10, SqlVar.sqlscale);
          Currency(Buffer) := tmpCurrency;
        end;
      end;
    SQL_D_FLOAT,
    SQL_DOUBLE:
      Double(Buffer) := PDouble(SqlVar.sqldata)^;
    SQL_FLOAT:
      Single(Buffer) := PSingle(SqlVar.sqldata)^;
    SQL_TYPE_TIME:
      begin
        isc_decode_sql_time(PISC_TIME(SqlVar.sqldata), @TempDate);
        TempTime := EncodeTime(Word(TempDate.tm_hour), Word(TempDate.tm_min),
          Word(TempDate.tm_sec), 0);
        TDateTime(Buffer) := TempTime;
      end;
    SQL_TYPE_DATE:
      begin
        isc_decode_sql_date(PISC_DATE(SqlVar.sqldata), @TempDate);
        TempTime :=
          EncodeDate(Word(TempDate.tm_year + 1900), Word(TempDate.tm_mon + 1),
          Word(TempDate.tm_mday));
       TDateTime(Buffer) := TempTime;
      end;
{$IFNDEF VER100}
    SQL_INT64:
      begin
        if SqlVar.sqlscale = 0 then
          Int64(Buffer) := PInt64(SqlVar.sqldata)^
        else if Abs(SqlVar.sqlscale) <= 4 then
        begin
          tmpCurrency := PInt64(SqlVar.sqldata)^
            * IntPower(10, SqlVar.sqlscale);
          Currency(Buffer) := tmpCurrency;
        end
        else
        begin
          tmpDouble := PInt64(SqlVar.sqldata)^
            * IntPower(10, SqlVar.sqlscale);
          Double(Buffer) := tmpDouble;
        end;
      end;
{$ENDIF}
    SQL_QUAD: TISC_QUAD(Buffer) := PISC_QUAD(SqlVar.sqldata)^;
    SQL_DATE:
      begin
        isc_decode_date(PISC_QUAD(SqlVar.sqldata), @TempDate);
        TempTime := EncodeDate(TempDate.tm_year + 1900,
          TempDate.tm_mon + 1, TempDate.tm_mday) + EncodeTime(TempDate.tm_hour,
          TempDate.tm_min, TempDate.tm_sec, 0);
        TDateTime(Buffer) := TempTime;
      end;
    //SQL_ARRAY: Result := '(ARRAY)';
    SQL_BLOB:
      begin
       TISC_QUAD(Buffer) := PISC_QUAD(SqlVar.sqldata)^;
      end;
  end;
  DecimalSeparator := OldSep;
end;

{ Check if field is Null }
function TDirIbSqlQuery.FieldIsNull(FieldNum: Integer): Boolean;
var
  SqlVar: PXSQLVAR;
  VarType: Short;
begin
  Result := True;
  SqlVar := @FOutSqlDa.SqlVar[FieldNum];
  if not Active then Exit;
  VarType := SqlVar.sqltype;
  Result := ((VarType and 1) <> 0) and (SqlVar.sqlind <> nil)
    and ((SqlVar.sqlind^) = ISC_NULL);
end;

{ Check if field is ReadOnly }
function TDirIbSqlQuery.FieldReadOnly(FieldNum: Integer): boolean;
begin
  Result := false;
  if not Active then Exit;

  with FOutSqlDa.SqlVar[FieldNum] do
    Result := (RelName = '') or (SqlName = '') or (SqlName = 'RDB$DB_KEY');
end;

{ Get field buffer }
function TDirIbSqlQuery.FieldBuffer(FieldNum: Integer): PChar;
var
  SqlVar: PXSQLVAR;
begin
  Result := nil;
  if not Active then Exit;
  SqlVar := @FOutSqlDa.SqlVar[FieldNum];
  if ((SqlVar.sqltype and 1) = 0) or ((SqlVar.sqlind^) <> -1) then
    Result := SqlVar.sqldata;
end;

{ Get field type }
function TDirIbSqlQuery.FieldType(FieldNum: Integer): Integer;
begin
  Result := 0;
  if not Active then Exit;
  Result := FOutSqlDa.SqlVar[FieldNum].sqltype and (not 1);
end;

{ Get field subtype }
function TDirIbSqlQuery.FieldSubType(FieldNum: Integer): Integer;
begin
  Result := 0;
  if not Active then Exit;
  Result := FOutSqlDa.SqlVar[FieldNum].sqlsubtype;
end;

{ Get field delphi compatible type }
function TDirIbSqlQuery.FieldDataType(FieldNum: Integer): TFieldType;
var
  ASqlScale: Integer;
  ASqlSubType: Integer;
begin
  Result := ftUnknown;
  if not Active then Exit;

  ASqlScale := FOutSqlDa.SqlVar[FieldNum].sqlscale;
  ASqlSubType := FOutSqlDa.SqlVar[FieldNum].sqlsubtype;

  case (FOutSqlDa.SqlVar[FieldNum].sqltype and (not 1)) of
    SQL_VARYING, SQL_TEXT: Result := ftString;
    SQL_LONG {$IFDEF VER100}, SQL_INT64{$ENDIF}:
      begin
        if ASqlScale = 0 then
          Result := ftInteger
        else if Abs(ASqlScale) <= 4 then
          Result := ftBCD
        else
          Result := ftFloat;
      end;
    SQL_SHORT:
      begin
        if ASqlScale = 0 then
          Result := ftSmallInt
        else Result := ftBCD;
      end;
    SQL_FLOAT: Result := ftFloat;
    SQL_DOUBLE:
      begin
        {$IFDEF ENABLE_BCD}
        if Abs(ASqlScale) in [1..4] then
          Result := ftBCD
        else
       {$ENDIF}
          Result := ftFloat;
      end;
    SQL_DATE: Result := ftDateTime;
    SQL_TYPE_TIME: Result := ftTime;
    SQL_TYPE_DATE: Result := ftDate;
{$IFNDEF VER100}
    SQL_INT64:
      begin
        if ASqlScale = 0 then
          Result := ftLargeInt
        else if Abs(ASqlScale) <= 4 then
          Result := ftBCD
        else
          Result := ftFloat;
      end;
{$ENDIF}
    SQL_BLOB:
      begin
        if ASqlSubType = isc_blob_text then
          Result := ftMemo
        else Result := ftBlob;
      end;
    //SQL_ARRAY: Result := ftArray;
    else Result := ftString;
  end;
end;

{ Get field count }
function TDirIbSqlQuery.FieldCount: Integer;
begin
  Result := 0;
  if not Active then Exit;
  Result := FOutSqlDa.sqld;
end;

{ Get field maximum size }
function TDirIbSqlQuery.FieldMaxSize(FieldNum: Integer): Integer;
begin
  Result := 0;
  if not Active then Exit;
  Result := FOutSqlDa.SqlVar[FieldNum].sqllen;
end;

function TDirIbSqlQuery.FieldPrecision(FieldNum: Integer): Integer;
begin
  Result := 0;
  if not Active then Exit;

  if FOutSqlDa.SqlVar[FieldNum].sqlscale <> 0 then
  case (FOutSqlDa.SqlVar[FieldNum].sqltype and (not 1)) of
    SQL_LONG :
       Result := 9;
    SQL_SHORT:
       Result := 4;
    SQL_DOUBLE:
       Result := 18;
    SQL_INT64:
       Result := 18;
  end;
end;

{ Get fields decimals }
function TDirIbSqlQuery.FieldDecimals(FieldNum: Integer): Integer;
begin
  Result := 0;
  if not Active then Exit;
  Result := -FOutSqlDa.SqlVar[FieldNum].sqlscale;
end;

{ Get field name }
function TDirIbSqlQuery.FieldName(FieldNum: Integer): ShortString;
begin
  Result := '';
  if not Active then Exit;
  Result := MemPas(FOutSqlDa.SqlVar[FieldNum].aliasname,
    FOutSqlDa.SqlVar[FieldNum].aliasname_length);
  if Result = '' then Result := 'Field' + IntToStr(FieldNum + 1);
end;

{ Get field size }
function TDirIbSqlQuery.FieldSize(FieldNum: Integer): Integer;
begin
  Result := 0;
  if not Active then Exit;
  Result := FOutSqlDa.SqlVar[FieldNum].sqllen;
  if (FOutSqlDa.SqlVar[FieldNum].sqltype and (not 1)) = SQL_VARYING then
    Result := PSmallInt(FOutSqlDa.SqlVar[FieldNum].sqldata)^;
end;

{ Go to the first row }
procedure TDirIbSqlQuery.First;
begin
end;

{ Go to specified row }
procedure TDirIbSqlQuery.Go(Num: Integer);
begin
end;

{ Go to the last row }
procedure TDirIbSqlQuery.Last;
begin
end;

{ Go to next row }
procedure TDirIbSqlQuery.Next;
var
  FetchStat: Integer;
  IbConnect: TDirIbSqlConnect;
begin
  if not Active or EOF then Exit;
  SetStatus(qsFail);
  if not Assigned(Connect) or not Assigned(Transact) then
    Exit;

  IbConnect := TDirIbSqlConnect(Connect);
  FetchStat := isc_dsql_fetch(@IbConnect.FStatusVector, @FHandle,
    IbConnect.Dialect, FOutSqlDa);

  if not IbConnect.HasError then
  begin
    SetStatus(qsTuplesOk);
    SetEOF(FetchStat <> 0);
    if FetchStat = 0 then
      SetRecNo(RecNo + 1);
  end else
    SetEOF(True);
end;

{ Go to prior row }
procedure TDirIbSqlQuery.Prev;
begin
end;

{ Get rows number }
function TDirIbSqlQuery.RecordCount: Integer;
var
  IbConnect: TDirIbSqlConnect;
  InBuf: Char;
  OutBuf: array[0..10] of Char;
begin
  Result := 0;
  if Active then
  begin
    IbConnect := TDirIbSqlConnect(Connect);
    InBuf := Chr(isc_info_sql_records);
    isc_dsql_sql_info(@IbConnect.FStatusVector, @FHandle, 1, @InBuf,
      SizeOf(OutBuf), OutBuf);
    if OutBuf[0] = Chr(isc_info_sql_records) then
      Result := isc_vax_integer(OutBuf + 1, 4)
    else
      Result := RecNo;
  end;
end;

{ Showes table columns }
procedure TDirIbSqlQuery.ShowColumns(TableName, ColumnName: ShortString);
begin
  if Active then Close;
  SQL := 'SELECT RDB$FIELD_POSITION AS Idx, A.RDB$FIELD_NAME AS Fld,'
    + ' B.RDB$FIELD_TYPE AS Typ, B.RDB$FIELD_LENGTH AS Len,'
    + ' A.RDB$NULL_FLAG AS Nul, A.RDB$DEFAULT_SOURCE AS Def,'
    + ' -B.RDB$FIELD_SCALE AS Scale, B.RDB$FIELD_SUB_TYPE SubType,'
    + ' RDB$COMPUTED_BLR COMPFLD,'
    + ' A.RDB$QUERY_NAME DL'
    + ' FROM RDB$RELATION_FIELDS A LEFT JOIN RDB$FIELDS B'
    + ' ON A.RDB$FIELD_SOURCE=B.RDB$FIELD_NAME WHERE'
    + ' A.RDB$RELATION_NAME=''' + UpperCase(TableName) + '''';
  if ColumnName <> '' then
    SQL := SQL + ' AND A.RDB$FIELD_NAME LIKE ''' + UpperCase(ColumnName) + '''';
  SQL := SQL + ' ORDER BY RDB$FIELD_POSITION';
  Open;
end;

{ Show existed databases }
procedure TDirIbSqlQuery.ShowDatabases(DatabaseName: ShortString);
begin
  inherited;
end;

{ Showes tables indices of database }
procedure TDirIbSqlQuery.ShowIndexes(TableName: ShortString);
begin
  if Active then Close;
  SQL := 'SELECT RDB$INDEX_ID AS Idx, A.RDB$INDEX_NAME AS Name, RDB$RELATION_NAME'
    + ' AS Tbl, RDB$UNIQUE_FLAG AS Uni, RDB$INDEX_TYPE AS Srt, RDB$FIELD_NAME AS Fld'
    + ' FROM RDB$INDICES A LEFT JOIN RDB$INDEX_SEGMENTS B'
    + ' ON A.RDB$INDEX_NAME=B.RDB$INDEX_NAME WHERE';
  if TableName <> '' then
    //SQL := SQL + ' RDB$RELATION_NAME LIKE ''' + UpperCase(TableName) + ' %'''
    SQL := SQL + ' RDB$RELATION_NAME = ''' + UpperCase(TableName)+''''
  else
    SQL := SQL + ' RDB$INDEX_NAME NOT LIKE ''RDB$%''';
  Open;
end;

{ Showes tables of database }
procedure TDirIbSqlQuery.ShowTables(TableName: ShortString);
begin
  if Active then Close;
  Sql := 'SELECT RDB$RELATION_ID AS Idx, RDB$RELATION_NAME AS TableName'
    + ' FROM RDB$RELATIONS WHERE';
  if TableName <> '' then
    Sql := Sql + ' RDB$RELATION_NAME LIKE ''' + UpperCase(TableName) + ''''
  else
    Sql := Sql + ' RDB$RELATION_NAME NOT LIKE ''RDB$%''';
  Sql := Sql + ' ORDER BY RDB$RELATION_NAME';
  Open;
end;

{ Showes Procs of database }
procedure TDirIbSqlQuery.ShowProcs(ProcName: ShortString);
begin
  if Active then Close;
  Sql := 'SELECT RDB$PROCEDURE_ID AS Idx, RDB$PROCEDURE_NAME AS ProcName'
    + ' FROM RDB$PROCEDURES WHERE';
  if ProcName <> '' then
    Sql := Sql + ' RDB$PROCEDURE_NAME LIKE ''' + UpperCase(ProcName) + ''''
  else
    Sql := Sql + ' RDB$PROCEDURE_NAME NOT LIKE ''RDB$%''';
  Sql := Sql + ' ORDER BY RDB$PROCEDURE_NAME';
  Open;
end;

{ Showes Proc Params }
procedure TDirIbSqlQuery.ShowProcsParams(ProcName: ShortString);
begin
  if Active then Close;
  SQL := 'SELECT A.RDB$PARAMETER_NUMBER AS Idx, A.RDB$PARAMETER_NAME AS Fld,'
    + ' A.RDB$PARAMETER_TYPE PTyp,'
    + ' B.RDB$FIELD_TYPE AS Typ, B.RDB$FIELD_LENGTH AS Len,'
    + ' B.RDB$NULL_FLAG AS Nul, B.RDB$DEFAULT_SOURCE AS Def,'
    + ' -B.RDB$FIELD_SCALE AS Scale,'
    + ' B.RDB$FIELD_SUB_TYPE SubType,'
    + ' B.RDB$QUERY_NAME DL'
    + ' FROM RDB$PROCEDURE_PARAMETERS A LEFT JOIN RDB$FIELDS B'
    + ' ON A.RDB$FIELD_SOURCE=B.RDB$FIELD_NAME WHERE'
    + ' A.RDB$PROCEDURE_NAME=''' + UpperCase(ProcName) + '''';
  SQL := SQL + ' ORDER BY A.RDB$PARAMETER_TYPE,A.RDB$PARAMETER_NUMBER';
  Open;
end;

{ Convert string to sql format }
function TDirIbSqlQuery.StringToSql(Value: string): string;
begin
  Result := Value;
end;

{*************** TDirIbSqlBlob implementation ****************}

{ Class constructor }
constructor TDirIbSqlBlob.Create(AConnect: TDirConnect; ATransact: TDirTransact;
  AHandle: TBlobHandle);
begin
  inherited Create(AConnect, ATransact, AHandle);
end;

{ Open a blob }
procedure TDirIbSqlBlob.Open(Mode: Integer);
var
  IbConnect: TDirIbSqlConnect;
  IbTransact: TDirIbSqlTransact;
begin
  SetStatus(bsFail);
  if not Assigned(Connect) or not Assigned(Transact) then
    Exit;
  if not Connect.Active or not Transact.Active then
    Exit;

  if Active then Close;
  IbConnect := TDirIbSqlConnect(Connect);
  IbTransact := TDirIbSqlTransact(Transact);

  if Mode = fmOpenRead then
  begin
    FBlobHandle := nil;
    isc_open_blob2(@IbConnect.FStatusVector, @IbConnect.FHandle, @IbTransact.FHandle,
      @FBlobHandle, @Handle, 0, nil);
    if not IbConnect.HasError then
    begin
      SetStatus(bsOk);
      SetActive(True);
    end;
  end
  else if Mode = fmOpenWrite then
    CreateBlob;
end;

{ Close current blob }
procedure TDirIbSqlBlob.Close;
var
  IbConnect: TDirIbSqlConnect;
begin
  SetStatus(bsFail);
  if not Assigned(Connect) or not Connect.Active then
    Exit;

  if Active then
  begin
    IbConnect := TDirIbSqlConnect(Connect);
    isc_close_blob(@IbConnect.FStatusVector, @FBlobHandle);
    if not IbConnect.HasError then
      SetStatus(bsOk);
  end;
  SetActive(False);
end;

{ Create a new blob }
procedure TDirIbSqlBlob.CreateBlob;
var
  IbConnect: TDirIbSqlConnect;
  IbTransact: TDirIbSqlTransact;
begin
  if Active then Close;

  IbConnect := TDirIbSqlConnect(Connect);
  IbTransact := TDirIbSqlTransact(Transact);

  FBlobHandle := nil;
  isc_create_blob2(@IbConnect.FStatusVector, @IbConnect.FHandle, @IbTransact.FHandle,
    @FBlobHandle, @Handle, 0, nil);
  if not IbConnect.HasError then
  begin
    SetStatus(bsOk);
    SetActive(True);
  end;
end;

{ Delete current blob }
procedure TDirIbSqlBlob.DropBlob;
begin
  inherited DropBlob;
  if Assigned(Connect) and (FBlobHandle <> nil) then
    isc_cancel_blob(@TDirIbSqlConnect(Connect).FStatusVector, @FBlobHandle);
  SetStatus(bsOk);
  SetActive(False);
  PBlobHandle(@Handle).Ptr := 0;
  PBlobHandle(@Handle).PtrEx := 0;
  FBlobHandle := nil;
end;

{ Read segment from open blob }
function TDirIbSqlBlob.Read(Buffer: PChar; Length: Integer): Integer;
var
  TempLen: Short;
  IbConnect: TDirIbSqlConnect;
  BlobStat: LongInt;
begin
  Result := 0;
  SetStatus(bsFail);
  if not Assigned(Connect) or not Assigned(Transact) then
    Exit;
  if not Connect.Active or not Transact.Active or not Active then
    Exit;

  IbConnect := TDirIbSqlConnect(Connect);
  repeat
    BlobStat := isc_get_segment(@IbConnect.FStatusVector, @FBlobHandle,
      @TempLen, Length - Result, Buffer + Result);
    Inc(Result, TempLen);
  until (BlobStat = 335544367) or (Result >= Length);

  if not IbConnect.HasError then
    SetStatus(bsOk);
end;

{ Write segment to open blob }
function TDirIbSqlBlob.Write(Buffer: PChar; Length: Integer): Integer;
var
  IbConnect: TDirIbSqlConnect;
begin
  Result := 0;
  SetStatus(bsFail);

  if ((Handle.Ptr = 0) and (Handle.PtrEx = 0)) or (FBlobHandle = nil) then
    Exit;
  if not Assigned(Connect) or not Assigned(Transact) then
    Exit;
  if not Connect.Active or not Transact.Active or not Active then
    Exit;

  IbConnect := TDirIbSqlConnect(Connect);
  isc_put_segment(@IbConnect.FStatusVector, @FBlobHandle, Length, Buffer);
  if not IbConnect.HasError then
  begin
    SetStatus(bsOk);
    Result := Length;
  end;
end;

{**************** Extra functions *******************}

{ Convert interbase field types to delphi field types }
function IbSqlToDelphiType(Value, SubType, Decimals: Integer): TFieldType;
begin
  case Value of
    7:
      begin
        if Decimals = 0 then
          Result := ftSmallInt
        else Result := ftBCD;
      end;
    8 {$IFDEF VER100}, 16{$ENDIF}:
      begin
        if Decimals = 0 then
          Result := ftInteger
        else if Decimals <= 4 then
          Result := ftBCD
        else
          Result := ftFloat;
      end;
    10, 11: Result := ftFloat;
    27:
      begin
        {$IFDEF ENABLE_BCD}
        if Decimals in [1..4] then
          Result := ftBCD
        else
       {$ENDIF}
          Result := ftFloat;
      end;
    14, 37: Result := ftString;
    12: Result := ftDate;
    13: Result := ftTime;
{$IFNDEF VER100}
    16:
      begin
        if Decimals = 0 then
          Result := ftLargeInt
        else if Decimals <= 4 then
          Result := ftBCD
        else
          Result := ftFloat;
      end;
{$ENDIF}
    35: Result := ftDateTime;
    261:
      if SubType = isc_blob_text then
        Result := ftMemo
      else Result := ftBlob;
    else Result := ftUnknown;
  end;
end;

procedure IBReAllocMem(var P; OldSize, NewSize: Integer);
begin
  ReallocMem(Pointer(P), NewSize);

  if NewSize > OldSize then
    Fillchar((Pchar(P) + OldSize)^, NewSize - OldSize, #0);
end;

function QuoteIdentifier(Dialect: Integer; Value: string): string;
begin
  //if Dialect = 1 then
  Value := AnsiUpperCase(Trim(Value))
    {
 else
 Result := '"' + StringReplace (Value, '"', '""', [rfReplaceAll]) + '"';
 }
end;

{***************** TDirIbSqlArray implementation ****************}

function GetArrayBufferLength(ArrayDesc: PISC_ARRAY_DESC): LongInt;
var
  I: Integer;
  Elements: Integer;
  ElementSize: Integer;
begin
  Elements := 1;
  for I := 0 to ArrayDesc.array_desc_dimensions - 1 do
    with ArrayDesc.array_desc_bounds[I] do
      Elements := Elements * (array_bound_upper - array_bound_lower + 1);

  ElementSize := ArrayDesc.array_desc_length;
  case byte(ArrayDesc.array_desc_dtype) of
    37, 38: Inc(ElementSize, 2);
  end;
  Result := Elements * ElementSize;
end;

constructor TDirIbSqlArray.Create(AConnect: TDirConnect; ATransact: TDirTransact;
  AHandle: TBlobHandle; ASQLVAR: PXSQLVAR);
begin
  inherited Create(AConnect, ATransact, AHandle);
  FSQLVAR := ASQLVAR;
end;

procedure TDirIbSqlArray.Open(Mode: Integer);
var
  IbConnect: TDirIbSqlConnect;
  IbTransact: TDirIbSqlTransact;
begin
  SetStatus(bsFail);
  if not Assigned(Connect) or not Assigned(Transact) then
    Exit;
  if not Connect.Active or not Transact.Active then
    Exit;

  if Active then Close;
  IbConnect := TDirIbSqlConnect(Connect);
  IbTransact := TDirIbSqlTransact(Transact);

  if Mode = fmOpenRead then
  begin
    isc_array_lookup_bounds(
      @IbConnect.FStatusVector, @IbConnect.FHandle, @IbTransact.FHandle,
      FSQLVAR.RelName, FSQLVAR.SQLName, @FArrayDesc);

    if not IbConnect.HasError then
    begin
      SetStatus(bsOk);
      SetActive(True);
    end;
  end 
  else if Mode = fmOpenWrite then
    CreateBlob;
end;

procedure TDirIbSqlArray.Close;
begin
  SetStatus(bsOk);
  SetActive(False);
end;

procedure TDirIbSqlArray.CreateBlob;
var
  IbConnect: TDirIbSqlConnect;
  IbTransact: TDirIbSqlTransact;
begin
  if Active then Close;
  IbConnect := TDirIbSqlConnect(Connect);
  IbTransact := TDirIbSqlTransact(Transact);

  isc_array_lookup_bounds(
    @IbConnect.FStatusVector, @IbConnect.FHandle, @IbTransact.FHandle,
    FSQLVAR.RelName, FSQLVAR.SQLName, @FArrayDesc);

  if not IbConnect.HasError then
  begin
    SetStatus(bsOk);
    SetActive(True);
  end;
end;

procedure TDirIbSqlArray.DropBlob;
begin
  inherited DropBlob;
  SetStatus(bsOk);
  SetActive(False);
  PBlobHandle(@Handle).Ptr := 0;
  PBlobHandle(@Handle).PtrEx := 0;
end;

function TDirIbSqlArray.Read(Buffer: PChar; Length: Integer): Integer;
var
  ArraySize: ISC_LONG;
  IbConnect: TDirIbSqlConnect;
  IbTransact: TDirIbSqlTransact;
begin
  SetStatus(bsFail);

  ArraySize := GetArrayBufferLength(@FArrayDesc);
  if Length < ArraySize then
  begin
    Length := ArraySize;
    ReallocMem(Buffer, Length);
  end;

  IbConnect := TDirIbSqlConnect(Connect);
  IbTransact := TDirIbSqlTransact(Transact);

  Result := isc_array_get_slice(
    @IbConnect.FStatusVector, @IbConnect.FHandle, @IbTransact.FHandle,
      @Handle, @FArrayDesc, @Buffer, Length);

  if not IbConnect.HasError then
  begin
    SetStatus(bsOk);
    Result := Length;
  end;
end;

function TDirIbSqlArray.Write(Buffer: PChar; Length: Integer): Integer;
var
  IbConnect: TDirIbSqlConnect;
  IbTransact: TDirIbSqlTransact;
begin
  SetStatus(bsFail);

  IbConnect := TDirIbSqlConnect(Connect);
  IbTransact := TDirIbSqlTransact(Transact);

  if Length = 0 then
  begin
    FillChar(FSQLVAR.SQLData^, FSQLVAR.SQLLen, #0);
    FSQLVAR.SQLInd := @NULL_FLAG;
    Result := 0;
  end
  else
  begin
    Result := isc_array_put_slice(
      @IbConnect.FStatusVector, @IbConnect.FHandle, @IbTransact.FHandle,
      @Handle, @FArrayDesc, @Buffer, @Length);
  end;

  if not IbConnect.HasError then
  begin
    SetStatus(bsOk);
    Result := Length;
  end;
end;

{****************  TDirIbSqlNotify implementation *****************}
const
  IB_MAX_EVENT_BLOCK = 15;

type
  PPChar = ^PChar;

  Tsib_event_block = function(EventBuffer, ResultBuffer: PPChar; IDCount: Word;
    Event1, Event2, Event3, Event4, Event5, Event6, Event7, Event8, Event9,
    Event10, Event11, Event12, Event13, Event14, Event15: PChar): ISC_LONG; cdecl;

constructor TDirIbSqlNotify.Create(AParent: TZNotify; AConnect: TDirIbSqlConnect;
  ATransact: TDirIbSqlTransact);
begin
  inherited Create;
  FParent := AParent;
  Connect := AConnect;
  Transact := ATransact;
  //???????????????
  FEvents := TStringList.Create;
  with TStringList(FEvents) do
  begin
    Sorted := true;
    Duplicates := dupIgnore;
  end;

  WhichEvent := 0;
  EventID := -1;
  EventBuffer := nil;
  EventBufferLen := 0;
  ResultBuffer := nil;
  EventCount := 0;
end;

destructor TDirIbSqlNotify.Destroy;
begin
  //?????????????????
  FEvents.Free;
  inherited Destroy;
end;

function TDirIbSqlNotify.GetErrorMsg: ShortString;
var
  Msg: array[0..1024] of Char;
  SqlCode: LongInt;
  IbConnect: TDirIbSqlConnect;
begin
  Result := 'Connection not defined';
  if not Assigned(Connect) then Exit;
  IbConnect := TDirIbSqlConnect(Connect);

  if IbConnect.HasError then
  begin
    SqlCode := isc_sqlcode(@IbConnect.FStatusVector);
    isc_sql_interprete(SqlCode, @Msg, 1024);
    Result := Result + #13#10 + Trim(StrPas(Msg));
  end;
end;

procedure TDirIbSqlNotify.ListenTo(Event: string);
begin
  if EventBufferLen > 0 then Exit;
  if FEvents.IndexOf(Event) >= 0 then Exit;

  UnRegisterEvents;
  RegisterEvents;

  SetActive(True);
end;

procedure TDirIbSqlNotify.UnlistenTo(Event: string);
var
  I: integer;
  OldActive:boolean;
begin
  I := FEvents.IndexOf(Event);
  if I < 0 then Exit;
  FEvents.Delete(I);

  SetStatus(nsFail);

  OldActive := Active;
  UnRegisterEvents;
  RegisterEvents;

  SetStatus(nsOK);
  SetActive(OldActive);
end;

procedure TDirIbSqlNotify.DoNotify(Event: string);
begin
  //cause Event
end;

procedure TDirIbSqlNotify.UpdateResultBuffer(Length: Short; Updated: PChar);
begin
  Move(Updated[0], ResultBuffer[0], Length);
end;

procedure ZEventCallback(P: Pointer; Length: Short; Updated: PChar); cdecl;
begin
  if (Assigned(P) and Assigned(Updated)) then
  begin
    TDirIbSqlNotify(P).UpdateResultBuffer(Length, Updated);
    //Found:=True;
  end;
end;

function TDirIbSqlNotify.CheckEvents: string;
var
  I: Integer;
  IbConnect: TDirIbSqlConnect;
begin
  SetStatus(nsFail);
  Result := '';

  if not Assigned(FTransact) or not FTransact.Active then Exit;
  if not Active then Exit;

  IbConnect := TDirIbSqlConnect(Connect);

  for I := 0 to (EventCount - 1) do
  begin
    if AStatusVector[I] <> 0 then
    begin
      Result := Parent.EventsList[I];
      SetStatus(nsOK);
      Exit;
    end;
  end;

  isc_event_counts(@IbConnect.FStatusVector,
    EventBufferLen, EventBuffer, ResultBuffer);

  for I := 0 to (EventCount - 1) do
  begin
    if IbConnect.FStatusVector[I] <> 0 then
    begin
      WhichEvent := I;
      AStatusVector[I] := 1;
    end else
      AStatusVector[I] := 0;
  end;

  for I := 0 to (EventCount - 1) do
  begin
    if AStatusVector[I] <> 0 then
    begin
      Result := Parent.EventsList[I];
      Exit;
    end;
  end;

  isc_que_events(@IbConnect.FStatusVector, @IbConnect.Handle,
    @EventID, EventBufferLen, EventBuffer, TISC_CALLBACK(@ZEventCallback),
    PVoid(Self));

  SetStatus(nsOK);
end;

procedure TDirIbSqlNotify.RegisterEvents;

  function EBP(Index: Integer): PChar;
  begin
    if Index > Parent.EventsList.Count then
      Result := nil
    else Result := PChar(Parent.EventsList[Index - 1]);
  end;

begin
  if Active then Exit;

  EventBuffer := nil;
  ResultBuffer := nil;
  EventBufferLen := 0;
  EventCount := Parent.EventsList.Count;

  if EventCount > IB_MAX_EVENT_BLOCK then
    EventCount := IB_MAX_EVENT_BLOCK;

  EventBufferLen := Tsib_event_block(isc_event_block)(@EventBuffer,
    @ResultBuffer, EventCount, EBP(1), EBP(2), EBP(3), EBP(4), EBP(5), EBP(6),
    EBP(7), EBP(8), EBP(9), EBP(10), EBP(11), EBP(12), EBP(13), EBP(14), EBP(15));

  SetActive(True);
end;

procedure TDirIbSqlNotify.UnRegisterEvents;
var
  IbConnect: TDirIbSqlConnect;
begin
  if not Active then Exit;

  IbConnect := TDirIbSqlConnect(Connect);
  isc_cancel_events(@IbConnect.FStatusVector, @IbConnect.Handle, @EventID);

  if (IbConnect.FStatusVector[0] = 1) and (IbConnect.FStatusVector[1] > 0) then
    DatabaseError(GetErrorMsg);

  isc_free(EventBuffer);
  EventBuffer := nil;
  isc_free(ResultBuffer);
  ResultBuffer := nil;

  SetActive(False);
end;

initialization
  MonitorList := TZMonitorList.Create;
finalization
  MonitorList.Free;
end.

