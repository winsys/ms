{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{              PostgreSql direct class API               }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZDirPgSql;

interface

uses SysUtils, Classes, ZDirSql, ZLibPgSql, DB, ZTransact, ZSqlTypes,
  ZSqlExtra, ZToken;

{$IFNDEF LINUX}
{$INCLUDE ..\Zeos.inc}
{$ELSE}
{$INCLUDE ../Zeos.inc}
{$ENDIF}

type
  { Direct PostgreSql connection }
  TDirPgSqlConnect = class (TDirConnect)
  private
    FError: string;
  protected
    function GetErrorMsg: ShortString; override;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Connect; override;
    procedure Disconnect; override;
    procedure CreateDatabase(Params: string); override;
    procedure DropDatabase; override;

    function GetConnectStr(Db: string): string;
  end;

  { Transaction types }
  TZPgSqlTransIsolation = (ptDefault, ptReadCommitted, ptRepeatableRead);

  { Direct PostgreSql transaction }
  TDirPgSqlTransact = class (TDirTransact)
  private
    FHandle: PPGconn;
    FError: string;
    FTypeList: TStringList;
    FNotice: AnsiString;
    FTransIsolation: TZPgSqlTransIsolation;
  protected
    function GetErrorMsg: ShortString; override;
    function GetTypeName(TypeNum: Oid): ShortString;
    function GetPid: Integer;
    function GetStatus: TDirStatus; override;
  public
    constructor Create(AConnect: TDirPgSqlConnect);
    destructor Destroy; override;

    procedure Open; override;
    procedure Close; override;
    procedure StartTransaction; override;
    procedure EndTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;

    procedure Reset;

    property Handle: PPGconn read FHandle;
    property Pid: Integer read GetPid;
    property Notice: AnsiString read FNotice write FNotice;
    property TransIsolation: TZPgSqlTransIsolation read FTransIsolation
      write FTransIsolation;
  end;

  { Direct PostgreSql query }
  TDirPgSqlQuery = class (TDirQuery)
  protected
    FHandle: PPGresult;
    FLastInsertOid: Oid;
    FCursorName: string;
    function GetErrorMsg: ShortString; override;
  public
    constructor Create(AConnect: TDirPgSqlConnect; ATransact: TDirPgSqlTransact);

    function  Execute: LongInt; override;
    procedure Open; override;
    procedure Close; override;
    function  CreateBlobObject: TDirBlob; override;

    procedure First; override;
    procedure Last; override;
    procedure Prev; override;
    procedure Next; override;
    procedure Go(Num: Integer); override;

    procedure ShowDatabases(DatabaseName: ShortString); override;
    procedure ShowTables(TableName: ShortString); override;
    procedure ShowColumns(TableName, ColumnName: ShortString); override;
    procedure ShowIndexes(TableName: ShortString); override;

    function  FieldCount: Integer; override;
    function  RecordCount: Integer; override;

    function  FieldName(FieldNum: Integer): ShortString; override;
    function  FieldSize(FieldNum: Integer): Integer; override;
    function  FieldMaxSize(FieldNum: Integer): Integer; override;
    function  FieldType(FieldNum: Integer): Integer; override;
    function  FieldDataType(FieldNum: Integer): TFieldType; override;
    function  FieldIsNull(FieldNum: Integer): Boolean; override;
    function  Field(FieldNum: Integer): string; override;
    function  FieldBuffer(FieldNum: Integer): PChar; override;

    function  FieldMinSize(FieldNum: Integer): Integer;
    function  FieldTypeName(FieldNum: Integer): ShortString;
    function  StringToSql(Value: string): string; override;

    property  Handle: PPGresult read FHandle;
    property  LastInsertOid: Oid read FLastInsertOid;
    property  CursorName: string read FCursorName write FCursorName;
  end;

  { PostgreSql large object }
  TDirPgSqlBlob = class (TDirBlob)
  private
    FBlobHandle: Integer;
  protected
    function GetPosition: LongInt; override;
  public
    constructor Create(AConnect: TDirPgSqlConnect; ATransact: TDirPgSqlTransact;
      AHandle: TBlobHandle);

    procedure Open(Mode: Integer); override;
    procedure Close; override;
    procedure CreateBlob; override;
    procedure DropBlob; override;

    function  Read(Buffer: PChar; Length: Integer): Integer; override;
    function  Write(Buffer: PChar; Length: Integer): Integer; override;
    procedure Seek(Offset: LongInt; Origin: Integer); override;

    procedure ImportFile(FileName: ShortString); override;
    procedure ExportFile(FileName: ShortString); override;

    property BlobHandle: Integer read FBlobHandle;
  end;

  { PostgreSQL array class }
  (*
  TDirPgSqlArray = class(TDirArray)
    protected
      function GetAsString: string; override;
      procedure SetAsString(Value: string); override;
    public
      function Slice(Start, End: Integer) Variant;
      function Value: Variant;
      function
  end;
  *)

  { PostgreSQL class for asynchrounous notifying}
  TDirPgSqlNotify = class(TDirNotify)
  protected
    FHandle: PPGnotify;
    FQuery: TDirPgSqlQuery;
    procedure InternalExec(Sql: string);
  public
    constructor Create(AConnect: TDirPgSqlConnect; ATransact: TDirPgSqlTransact);
    destructor Destroy; override;

    procedure ListenTo(Event: string); override;
    procedure UnlistenTo(Event: string); override;
    procedure DoNotify(Event: string); override;
    function CheckEvents: string; override;

    property Handle: PPGnotify read fHandle;
  end;

{ Convert postgresql field types to delphi field types }
function PgSqlToDelphiType(Value: string; var Size: Integer;
  var ArraySubType: TFieldType; var BlobType: TBlobType): TFieldType;

{ Monitor list }
var
  MonitorList: TZMonitorList;

implementation

uses ZDBaseConst, ZExtra {$IFNDEF LINUX}, ActiveX{$ENDIF};

{***************** TDirPgSqlConnect implementation *****************}

{ Class constructor }
constructor TDirPgSqlConnect.Create;
begin
  inherited Create;
  Port := '5432';
end;

{ Class destructor }
destructor TDirPgSqlConnect.Destroy;
begin
  inherited Destroy;
end;

{ Get an error message }
function TDirPgSqlConnect.GetErrorMsg: ShortString;
begin
  if Status <> csOk then
    Result := FError
  else
    Result := '';
end;

{ Construct postgresql connect string }
function TDirPgSqlConnect.GetConnectStr(Db: string): string;

  function CheckAddr(Value: string): Boolean;
  var
    I, N: Integer;
  begin
    Result := False;
    N := 0;
    for I := 1 to Length(Value) do
    begin
      if Value[I] = '.' then
        Inc(N)
      else if not (Value[I] in ['0'..'9']) then
        Exit;
    end;
    Result := (N = 3);
  end;

begin
  HostName := Trim(HostName);
  if CheckAddr(HostName) then
    Result := 'hostaddr='
  else
    Result := 'host=';
  Result := Result + HostName + ' port=' + Port +
    ' dbname=' + Db + ' user=' + Login + ' password=' + Passwd;
end;

{ Connect to database }
procedure TDirPgSqlConnect.Connect;
begin
  inherited Connect;
  if hDll = 0 then PgSqlLoadLib;
  SetStatus(csOk);
  SetActive(True);
end;

{ Disconnect from database }
procedure TDirPgSqlConnect.Disconnect;
begin
  SetStatus(csOk);
  SetActive(False);
end;

{ Create and connect to database }
procedure TDirPgSqlConnect.CreateDatabase(Params: string);
var
  Handle: PPGconn;
  Result: PPGresult;
  Buffer: string;
begin
  if Active then Disconnect;
  if hDll = 0 then PgSqlLoadLib;
  SetStatus(csFail);
  FError := SDbCreateError;

  Handle := PQconnectdb(PChar(GetConnectStr('template1')));

  if not Assigned(Handle) then Exit;

  Buffer := 'CREATE DATABASE ' + Database + ' ' + Params;
  Result := PQexec(Handle, PChar(Buffer));
  if Assigned(Result) then
  begin
    SetStatus(csOk);
    PQclear(Result);
  end;
  MonitorList.InvokeEvent(Buffer, 'Fail.', Status <> csOk);
end;

{ Drop current database }
procedure TDirPgSqlConnect.DropDatabase;
var
  Handle: PPGconn;
  Result: PPGresult;
  Buffer: string;
begin
  if Active then Disconnect;
  SetStatus(csFail);
  FError := SConnectError;

  Buffer := 'DROP DATABASE ' + Database;
  Handle := PQconnectdb(PChar(GetConnectStr('template1')));
  if Assigned(Handle) then
  begin
    Result := PQexec(Handle, PChar(Buffer));
    if Assigned(Result) then
      SetStatus(csOk);
    PQclear(Result);
    PQfinish(Handle);
  end;

  MonitorList.InvokeEvent(Buffer, 'Fail.', Status <> csOk);
end;

{*********** TDirPgSqlTransact implementation *********}

{ NoticeProcessor handler }
procedure NoticeProc(Arg: Pointer; Msg: PChar); cdecl;
begin
  if Assigned(Arg) then
    TDirPgSqlTransact(Arg).Notice := TDirPgSqlTransact(Arg).Notice + Msg;
end;

{ Class constructor }
constructor TDirPgSqlTransact.Create(AConnect: TDirPgSqlConnect);
begin
  inherited Create;
  Connect := AConnect;
  FHandle := nil;
end;

{ Class destructor }
destructor TDirPgSqlTransact.Destroy;
begin
  inherited Destroy;
  if Assigned(FTypeList) then
    FTypeList.Free;
end;

{ Get error message }
function TDirPgSqlTransact.GetErrorMsg: ShortString;
begin
  if Assigned(Handle) and Assigned(Connect) then
    FError := Trim(StrPas(PQerrorMessage(Handle)));
  Result := FError;
end;

{ Get field type by oid }
function TDirPgSqlTransact.GetTypeName(TypeNum: Oid): ShortString;
var
  I: Integer;
  AResult: PPGresult;
begin
  Result := '';
  if not Assigned(Handle) then Exit;
  if not Assigned(FTypeList) then
  begin
    FTypeList := TStringList.Create;
    AResult := PQexec(Handle, 'SELECT oid, typname FROM pg_type WHERE oid<10000');
    if not Assigned(AResult) then Exit;
    for I := 0 to PQntuples(AResult)-1 do
      FTypeList.AddObject(StrPas(PQgetvalue(AResult,I,1)),
        TObject(StrToIntDef(StrPas(PQgetvalue(AResult,I,0)),0)));
    PQclear(AResult);
  end;
  I := FTypeList.IndexOfObject(TObject(TypeNum));
  if I >= 0 then
    Result := FTypeList.Strings[I];
end;

{ Retrieve backend server's process id (PID }
function TDirPgSqlTransact.GetPID: Integer;
begin
  Result := PQbackendPID(Handle);
end;

{ Get status directly from the PQlib interface}
function TDirPgSqlTransact.GetStatus: TDirStatus;
begin
  Result := inherited GetStatus;
  if Handle <> nil then
  begin
     case PQstatus(Handle) of
       CONNECTION_OK:
         Result := csOK;
       CONNECTION_BAD:
         Result := csFail
     end;
  end;
end;

{ Connect to database }
procedure TDirPgSqlTransact.Open;
begin
  inherited Open;
  SetStatus(csFail);
  if not Assigned(Connect) or not Connect.Active then
    Exit;

  FHandle := PQconnectdb(PChar(TDirPgSqlConnect(Connect).
    GetConnectStr(Connect.Database)));

  if PQstatus(Handle) = CONNECTION_BAD then
  begin
    if not Assigned(Handle) then
      FError := SConnectError;
    MonitorList.InvokeEvent(Format('CONNECT %s',[Connect.Database]), FError, True);
    Exit;
  end;
  MonitorList.InvokeEvent(Format('CONNECT %s',[Connect.Database]), 'OK.', False);
  StartTransaction;
  PQsetNoticeProcessor(FHandle, NoticeProc, Self);
  SetActive(Status = csOk);
end;

{ Disconnect from database }
procedure TDirPgSqlTransact.Close;
begin
  EndTransaction;
  if Active then
  begin
    PQfinish(Handle);
    FHandle := nil;
    if Assigned(FTypeList) then
    begin
      FTypeList.Free;
      FTypeList := nil;
    end;
    if Assigned(Connect) then
      MonitorList.InvokeEvent(Format('DISCONNECT %s',[Connect.Database]), 'OK.', False);
  end;
  SetActive(False);
end;

{ Start transaction }
procedure TDirPgSqlTransact.StartTransaction;
var
  Result: PPGresult;
  Temp: string;
begin
  SetStatus(csFail);
  if not Assigned(Connect) or not Connect.Active then
    Exit;

  if TransactSafe then
  begin
    Result := PQexec(Handle, 'BEGIN');
    PQclear(Result);
    { Set isolation level }
    if TransIsolation <> ptDefault then
    begin
      Temp := 'SET TRANSACTION ISOLATION LEVEL ';
      case TransIsolation of
        ptReadCommitted:
          Temp := Temp + 'READ COMMITED';
        ptRepeatableRead:
          Temp := Temp + 'SERIALIZABLE';
        else
          Temp := '';
      end;
      if Temp <> '' then
      begin
        Result := PQexec(Handle, PChar(Temp));
        PQclear(Result);
        MonitorList.InvokeEvent(Temp, Error, Error <> '');
      end;
    end;

    MonitorList.InvokeEvent('BEGIN', Error, Error <> '');
  end;
  SetStatus(csOk);
end;

{ End transaction and disconnect from database }
procedure TDirPgSqlTransact.EndTransaction;
var
  Result: PPGresult;
begin
  if Active and TransactSafe then
  begin
    Result := PQexec(Handle, 'END');
    PQclear(Result);
    MonitorList.InvokeEvent('END', Error, Error <> '');
  end;
  SetStatus(csOk);
end;

{ Commit transaction }
procedure TDirPgSqlTransact.Commit;
var
  Result: PPGresult;
begin
  SetStatus(csFail);
  if not Active or not Assigned(Handle) then Exit;
  SetStatus(csOk);
  if TransactSafe then
  begin
    Result := PQexec(Handle, 'COMMIT');
    PQclear(Result);
    MonitorList.InvokeEvent('COMMIT', Error, Error <> '');

    Result := PQexec(Handle, 'BEGIN');
    PQclear(Result);
    MonitorList.InvokeEvent('BEGIN', Error, Error <> '');
    if Error <> ''  then
      SetStatus(csFail);
  end;
end;

{ Rollback transaction }
procedure TDirPgSqlTransact.Rollback;
var
  Result: PPGresult;
begin
  SetStatus(csFail);
  if not Active or not Assigned(Handle) then Exit;
  SetStatus(csOk);
  if TransactSafe then
  begin
    Result := PQexec(Handle, 'ROLLBACK');
    PQclear(Result);
    MonitorList.InvokeEvent('ROLLBACK', Error, Error <> '');

    Result := PQexec(Handle, 'BEGIN');
    PQclear(Result);
    MonitorList.InvokeEvent('BEGIN', Error, Error <> '');
    if Error <> ''  then
      SetStatus(csFail);
  end;
end;

{ Reset the connection to the backend server, i.e., disconnects then }
{ reconnects using the same parameters                               }
procedure TDirPgSqlTransact.Reset;
begin
{$IFDEF PGSQL7}
  PQresetStart(Handle)
{$ELSE}
  PQreset(Handle);
{$ENDIF}
end;

{******************* TDirPgSqlQuery implementation **********************}

{ Class constructor }
constructor TDirPgSqlQuery.Create(AConnect: TDirPgSqlConnect;
  ATransact: TDirPgSqlTransact);
begin
  inherited Create;
  Connect := AConnect;
  Transact := ATransact;
  FHandle := nil;
end;

{ Get an error message }
function TDirPgSqlQuery.GetErrorMsg: ShortString;
begin
  Result := '';
  if not (Status in [qsTuplesOk, qsCommandOk]) and Assigned(Transact) and
    Assigned(Connect) then
    Result := Trim(StrPas(PQerrorMessage(TDirPgSqlTransact(Transact).Handle)));
end;

{ Close open query }
procedure TDirPgSqlQuery.Close;
begin
  inherited Close;
  { Closing the cursor, if there is come }
  if Active and (FCursorName <> '') and Assigned(Handle) then
  begin
    PQexec(TDirPgSqlTransact(Transact).Handle,
      PChar(Format('CLOSE %s', [FCursorName])));
    FCursorName := '';
  end;

  if Assigned(Handle) then
    PQclear(Handle);
  FHandle := nil;
  SetActive(False);
  SetStatus(qsCommandOk);
end;

{ Execute the query }
function TDirPgSqlQuery.Execute: LongInt;
begin
  Result := inherited Execute;
  SetStatus(qsFail);
  FLastInsertOid := 0;
  if not Assigned(Connect) or not Assigned(Transact)
    or not (Connect.Active and Transact.Active) then
    Exit;

  FHandle := PQexec(TDirPgSqlTransact(Transact).Handle,
    PChar(ClearSpaces(Trim(Sql))));
  if Assigned(Handle) and (Transact.Error = '')  then
  begin
    SetAffectedRows(StrToIntDef(StrPas(PQcmdTuples(Handle)),0));
    Result := AffectedRows;
    FLastInsertOid := PQoidValue(Handle);
    PQclear(Handle);
    FHandle := nil;
    SetStatus(qsCommandOk);
  end
  else
  begin
    PQclear(Handle);
    FHandle := nil;
  end;
  MonitorList.InvokeEvent(Sql, Transact.Error, Status <> qsCommandOk);
end;

{ Generate a Ñursor Number }
function CursorGenerator: string;
var
  I: Integer;

{$IFNDEF LINUX}
function CreateNewGUID: string;
var
  ClassID: TCLSID;
  P: PWideChar;
begin
  CoCreateGuid(ClassID);
  StringFromCLSID(ClassID, P);
  Result := P;
  CoTaskMemFree(P);
end;
{$ELSE}
function CreateNewGUID: string;
var
  Temp: TGUID;
begin
  CreateGUID(Temp);
  Result := GUIDToString(Temp);
end;
{$ENDIF}

begin
  Result := CreateNewGUID;
  Delete(Result, 1, 20);
  for I := Length(Result) downto 1 do
  begin
    if Result[I] = '-' then
      Result[I] := '_';
    if Result[I] in ['{', '}'] then
      Delete(Result, I, 1);
  end;
end;

{ Open the query with result set }
procedure TDirPgSqlQuery.Open;
var
  Temp: string;
begin
  inherited Open;
  SetStatus(qsFail);
  FLastInsertOid := 0;
  if not Assigned(Connect) then
    DatabaseError(SConnectNotDefined);
  if not Assigned(Transact) then
    DatabaseError(STransactNotDefined);
  if not (Connect.Active and Transact.Active) then
    Exit;

  { If it's a select query in Cursor mode, declare a new cursor }
  FCursorName := '';
  if UseCursor then
  begin
    Temp := Sql;
    if StrCaseCmp(StrTok(Temp, ' '#9#13#10), 'SELECT') then
      FCursorName := 'ZeosCursor_' + CursorGenerator;
  end;

  if FCursorName <> '' then
  begin
    Temp := ClearSpaces(Format('DECLARE %s CURSOR FOR %s', [FCursorName, Trim(Sql)]));
    FHandle := PQexec(TDirPgSqlTransact(Transact).Handle, PChar(Temp));
    MonitorList.InvokeEvent(Temp, Transact.Error, not Active);

    Temp := Format('FETCH FORWARD 1 FROM %s', [FCursorName]);
    FHandle := PQexec(TDirPgSqlTransact(Transact).Handle, PChar(Temp));
    MonitorList.InvokeEvent(Temp, Transact.Error, not Active);
  end
  else
  begin
    FHandle := PQexec(TDirPgSqlTransact(Transact).Handle,
      PChar(ClearSpaces(Trim(Sql))));
  end;

  if Assigned(FHandle) and (Transact.Error = '') then
  begin
    SetActive(True);
    SetStatus(qsTuplesOk);
    inherited First;
  end else
    PQclear(FHandle);
  MonitorList.InvokeEvent(Sql, Transact.Error, not Active);
end;

{ Go to the first row }
procedure TDirPgSqlQuery.First;
begin
  if FCursorName = '' then
    inherited First;
end;

{ Go to specified row }
procedure TDirPgSqlQuery.Go(Num: Integer);
begin
  if FCursorName = '' then
    inherited Go(Num);
end;

{ Go to the last row }
procedure TDirPgSqlQuery.Last;
begin
  if FCursorName = '' then
    inherited Last;
end;

{ Go to next row }
procedure TDirPgSqlQuery.Next;
var
  Temp: string;
begin
  if not Active or EOF then Exit;
  SetStatus(qsFail);
  if not Assigned(Connect) or not Assigned(Transact) then
    Exit;

  if FCursorName = '' then
    inherited Next
  else begin
    Temp := Format('FETCH FORWARD 1 FROM %s', [FCursorName]);
    FHandle := PQexec(TDirPgSqlTransact(Transact).Handle, PChar(Temp));
    MonitorList.InvokeEvent(Temp, Transact.Error, not Active);

    if Assigned(FHandle) and (PQntuples(FHandle) <> 0) then
      SetRecNo(RecNo + 1)
    else SetEOF(True);
    if Transact.Error = '' then
      SetStatus(qsTuplesOk);
  end;
end;

{ Go to prior row }
procedure TDirPgSqlQuery.Prev;
begin
  if FCursorName = '' then
    inherited Prev;
end;

{ Create linked blob object }
function TDirPgSqlQuery.CreateBlobObject: TDirBlob;
var
  TempHandle: TBlobHandle;
begin
  FillChar(TempHandle, SizeOf(TBlobHandle), 0);
  Result := TDirPgSqlBlob.Create(TDirPgSqlConnect(Connect),
    TDirPgSqlTransact(Transact), TempHandle);
end;

{ Get record quantity }
function TDirPgSqlQuery.RecordCount: Integer;
begin
  if not Assigned(FHandle) then Result := 0
  else begin
    if FCursorName = '' then
      Result := PQntuples(FHandle)
    else Result := RecNo + 1;
  end;
end;

{ Get fields quantity }
function TDirPgSqlQuery.FieldCount: Integer;
begin
  if not Assigned(FHandle) then Result := 0
  else Result := PQnfields(FHandle)
end;

{ Get field name }
function TDirPgSqlQuery.FieldName(FieldNum: Integer): ShortString;
begin
  if not Assigned(FHandle) then
    Result := ''
  else
    Result := StrPas(PQfname(FHandle, FieldNum));
end;

{ Get field size }
function TDirPgSqlQuery.FieldSize(FieldNum: Integer): Integer;
begin
  if Assigned(FHandle) and (PQntuples(FHandle) > 0) then
    Result := PQgetlength(FHandle, Recno, FieldNum)
  else Result := 0;
end;

{ Get maximum field size }
function TDirPgSqlQuery.FieldMaxSize(FieldNum: Integer): Integer;
begin
  if not Assigned(FHandle) then Result := 0
  else begin
    Result := Max(PQfmod(FHandle, FieldNum)-4, 0);
    if Result = 0 then
      Result := FieldMinSize(FieldNum);
  end;
end;

{ Get minumum accepted field size }
function TDirPgSqlQuery.FieldMinSize(FieldNum: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  if not Assigned(FHandle) then Exit;
  for I := 0 to PQntuples(FHandle)-1 do
    if PQgetlength(FHandle, I, FieldNum) > Result then
      Result := PQgetlength(FHandle, I, FieldNum);
end;

{ Get field type }
function TDirPgSqlQuery.FieldType(FieldNum: Integer): Integer;
begin
  if not Assigned(FHandle) then Result := 0
  else Result := PQftype(FHandle, FieldNum);
end;

{ Get field type name }
function TDirPgSqlQuery.FieldTypeName(FieldNum: Integer): ShortString;
begin
  if not Assigned(Transact) then
    Result := ''
  else
    Result := TDirPgSqlTransact(Transact).GetTypeName(FieldType(FieldNum));
end;

{ Define field type }
function TDirPgSqlQuery.FieldDataType(FieldNum: Integer): TFieldType;
var
  Size: Integer;
  BlobType: TBlobType;
  ArraySubType: TFieldType;
begin
  Size := 0;
  Result := PgSqlToDelphiType(FieldTypeName(FieldNum), Size, ArraySubType, BlobType);
end;

{ Get field value }
function TDirPgSqlQuery.Field(FieldNum: Integer): string;
var
  TempRecno: Integer;
begin
  if FCursorName = '' then TempRecno := Recno
  else TempRecno := 0;

  if not Assigned(FHandle) or Eof or Bof then
    Result := ''
  else begin
    Result := StrPas(PQgetvalue(FHandle, TempRecno, FieldNum));
    if FieldType(FieldNum) = 1042 then
      Result := TrimRight(Result);
  end;
end;

{ Get field buffer }
function TDirPgSqlQuery.FieldBuffer(FieldNum: Integer): PChar;
var
  TempRecno: Integer;
begin
  if FCursorName = '' then TempRecno := Recno
  else TempRecno := 0;

  if not Assigned(FHandle) or Eof or Bof
    or (PQgetisnull(FHandle, TempRecno, FieldNum) <> 0) then
    Result := nil
  else
    Result := PQgetvalue(FHandle, TempRecno, FieldNum);
end;

{ Is field null }
function TDirPgSqlQuery.FieldIsNull(FieldNum: Integer): Boolean;
var
  TempRecno: Integer;
begin
  if FCursorName = '' then TempRecno := Recno
  else TempRecno := 0;

  if not Assigned(FHandle) or Eof or Bof then
    Result := True
  else
    Result := PQgetisnull(FHandle, TempRecno, FieldNum) <> 0;
end;

{ Showes databases }
procedure TDirPgSqlQuery.ShowDatabases(DatabaseName: ShortString);
begin
  if Active then Close;
  Sql := 'SELECT datname as DatabaseName FROM pg_database';
  if DatabaseName <> '' then
    Sql := Sql + ' WHERE datname LIKE '''+DatabaseName+'''';
  Sql := Sql + ' ORDER BY datname';
  Open;
end;

{ Showes tables of the database }
procedure TDirPgSqlQuery.ShowTables(TableName: ShortString);
begin
  if Active then Close;
  Sql := 'SELECT pg_class.oid as Index, relname as TableName FROM pg_class WHERE'
    +' relkind = ''r'' AND relname !~''^pg_''';
  if TableName <> '' then
    Sql := Sql + ' AND lower(relname) LIKE '''+LowerCase(TableName)+'''';
  Sql := Sql + ' ORDER BY relname';
  Open;
end;

{ Showes columns of the table }
procedure TDirPgSqlQuery.ShowColumns(TableName, ColumnName: ShortString);
begin
  if Active then Close;
  { Select all columns with defaults }
  Sql := 'SELECT pg_attribute.attnum AS index, attname AS field,'
    +' typname AS type, atttypmod-4 as length, NOT attnotnull AS "null",'
    +' adsrc AS def FROM pg_attribute, pg_class, pg_type, pg_attrdef WHERE'
    +' pg_class.oid=attrelid AND pg_type.oid=atttypid AND attnum>0'
    +' AND pg_class.oid=adrelid AND adnum=attnum AND atthasdef=''t'''
    +' AND lower(relname)='''+LowerCase(TableName)+'''';
  if ColumnName <> '' then
    Sql := Sql + ' AND lower(attname) LIKE '''+LowerCase(ColumnName)+'''';

  { Select all columns without defaults }
  Sql := Sql + ' UNION SELECT pg_attribute.attnum AS index, attname AS field,'
    +' typname AS type, atttypmod-4 as length, NOT attnotnull AS "null",'
    +' '''' AS def FROM pg_attribute, pg_class, pg_type WHERE'
    +' pg_class.oid=attrelid AND pg_type.oid=atttypid AND attnum>0'
    +' AND atthasdef=''f'' AND lower(relname)='''+LowerCase(TableName)+'''';
  if ColumnName <> '' then
    Sql := Sql + ' AND lower(attname) LIKE '''+LowerCase(ColumnName)+'''';
  Open;
end;

{ Showes indexes of the table }
procedure TDirPgSqlQuery.ShowIndexes(TableName: ShortString);
begin
  if Active then Close;
  Sql := 'SELECT i.oid AS index, t1.relname AS name, t2.relname AS table,'
    +' indisunique AS "unique", indkey AS fields'
    +' FROM pg_index AS i, pg_class AS t1, pg_class AS t2 WHERE'
    +' i.indexrelid=t1.oid AND i.indrelid=t2.oid'
    +' AND lower(t2.relname)='''+LowerCase(TableName)+'''';
  Open;
end;

{ Convert string to sql format }
function TDirPgSqlQuery.StringToSql(Value: string): string;
begin
  Result := ZSqlTypes.StringToSql(Value);
end;

{**************** TDirPgSqlBlob implementation *************}

{ Class constructor }
constructor TDirPgSqlBlob.Create(AConnect: TDirPgSqlConnect;
  ATransact: TDirPgSqlTransact; AHandle: TBlobHandle);
begin
  inherited Create(AConnect, ATransact, AHandle);
  FBlobHandle := -1;
end;

{ Get current position }
function TDirPgSqlBlob.GetPosition: LongInt;
begin
  SetStatus(bsFail);
  Result := 0;
  if not Assigned(Connect) or not Assigned(Transact)
    or not (Connect.Active and Transact.Active) then
    Exit;

  Result  := lo_tell(TDirPgSqlTransact(Transact).Handle, FBlobHandle);
  SetStatus(bsOk);
end;

{ Open large object }
procedure TDirPgSqlBlob.Open(Mode: Integer);
begin
  // force read only if mode is zero
  if Mode = 0 then Mode := INV_READ;
  inherited Open(Mode);
  SetStatus(bsFail);
  if not Assigned(Connect) or not Assigned(Transact)
    or not (Connect.Active and Transact.Active) then
    Exit;

  if Handle.Ptr <> 0 then
  begin
    FBlobHandle := lo_open(TDirPgSqlTransact(Transact).Handle,
      Handle.Ptr, Mode);
    if FBlobHandle >= 0 then
    begin
      SetStatus(bsOk);
      SetActive(True);
    end;
  end else
    CreateBlob;
end;

{ Close large object }
procedure TDirPgSqlBlob.Close;
begin
  inherited Close;
  SetStatus(bsFail);
  if not Assigned(Connect) or not Assigned(Transact)
    or not (Connect.Active and Transact.Active) then
    Exit;

  if FBlobHandle >= 0 then
  begin
    lo_close(TDirPgSqlTransact(Transact).Handle, FBlobHandle);
    SetStatus(bsOk);
    FBlobHandle := -1;
  end;
  SetActive(False);
end;

{ Read from large object }
function TDirPgSqlBlob.Read(Buffer: PChar; Length: Integer): Integer;
begin
  if Assigned(Transact) and (FBlobHandle >= 0) then
  begin
    Result  := lo_read(TDirPgSqlTransact(Transact).Handle,
      FBlobHandle, Buffer, Length);
    SetStatus(bsOk);
  end
  else
  begin
    Result  := 0;
    SetStatus(bsFail);
  end;
end;

{ Write to large object }
function TDirPgSqlBlob.Write(Buffer: PChar; Length: Integer): Integer;
begin
  if Assigned(Transact) and (FBlobHandle >= 0) then
  begin
    Result  := lo_write(TDirPgSqlTransact(Transact).Handle,
      FBlobHandle, Buffer, Length);
    SetStatus(bsOk);
  end
  else
  begin
    Result  := 0;
    SetStatus(bsFail);
  end;
end;

{ Seek new position }
procedure TDirPgSqlBlob.Seek(Offset: LongInt; Origin: Integer);
begin
  if Assigned(Transact) and (FBlobHandle >= 0) then
  begin
    lo_lseek(TDirPgSqlTransact(Transact).Handle, FBlobHandle, Offset, Origin);
    SetStatus(bsOk);
  end else
    SetStatus(bsFail);
end;

{ Create new large object }
procedure TDirPgSqlBlob.CreateBlob;
var
  TempHandle: TBlobHandle;
begin
  inherited CreateBlob;
  SetStatus(bsFail);
  if not Assigned(Connect) or not Assigned(Transact)
    or not (Connect.Active and Transact.Active) then
    Exit;

  FBlobHandle := -1;
  TempHandle.Ptr := lo_creat(TDirPgSqlTransact(Transact).Handle,
    INV_WRITE or INV_READ);
  Handle := TempHandle;
  if Handle.Ptr <> 0 then
    Open(INV_WRITE);
end;

{ Unlink large object }
procedure TDirPgSqlBlob.DropBlob;
begin
  inherited DropBlob;
  SetStatus(bsFail);
  if not Assigned(Connect) or not Assigned(Transact)
    or not (Connect.Active and Transact.Active) then
    Exit;

  if Handle.Ptr = 0 then Exit;
  lo_unlink(TDirPgSqlTransact(Transact).Handle, Handle.Ptr);
  SetStatus(bsOk);
end;

{ Import from file to large object }
procedure TDirPgSqlBlob.ImportFile(FileName: ShortString);
var
  TempHandle: TBlobHandle;
begin
  SetStatus(bsFail);
  if not Assigned(Connect) or not Assigned(Transact)
    or not (Connect.Active and Transact.Active) then
    Exit;

  TempHandle.Ptr := lo_import(TDirPgSqlTransact(Transact).Handle,
    PChar(string(FileName)));
  Handle := TempHandle;
  if Handle.Ptr <> 0 then
    SetStatus(bsOk);
end;

{ Export to file from large object }
procedure TDirPgSqlBlob.ExportFile(FileName: ShortString);
begin
  SetStatus(bsFail);
  if not Assigned(Connect) or not Assigned(Transact)
    or not (Connect.Active and Transact.Active) then
    Exit;

  if Handle.Ptr <> 0 then
  begin
    lo_export(TDirPgSqlTransact(Transact).Handle, Handle.Ptr, PChar(string(FileName)));
    SetStatus(bsOk);
  end;
end;

{ **************** TDirPgSqlArray implementation *************}
(*
function TDirPgSqlArray.GetAsString: string;
begin
  Result:=''
end;

procedure TDirPgSqlArray.SetAsString(Value: string);
begin
end;
*)

{**************** TDirPgSqlNotify implementation *************}

{ Class constructor }
constructor TDirPgSqlNotify.Create(AConnect: TDirPgSqlConnect;
  ATransact: TDirPgSqlTransact);
begin
  FConnect := AConnect;
  FTransact := FTransact;
  FHandle := nil;
  FQuery := TDirPgSqlQuery.Create(AConnect, ATransact)
end;

{ Class destructor }
destructor TDirPgSqlNotify.Destroy;
begin
  FQuery.Free;
  if Assigned(FHandle) then
     PQnotifyFree(FHandle);
  inherited Destroy;
end;

{ Execute a sql query }
procedure TDirPgSqlNotify.InternalExec(Sql: string);
begin
  FQuery.Connect := FConnect;
  FQuery.Transact := FTransact;
  FQuery.Sql := Sql;
  FQuery.Execute;
  if FQuery.Status <> qsCommandOk then
    SetStatus(nsFail)
  else SetStatus(nsOk);
end;

{ Listen to a specific event }
procedure TDirPgSqlNotify.ListenTo(Event: string);
begin
  Event := Trim(Event);
  { This check is needed because otherwise the string will be silently truncated } 
  if Length(Event) > NAMEDATALEN then
    DatabaseErrorFmt(SEventLength, [Length(Event), NAMEDATALEN]);
  if Event <> '' then
    InternalExec('LISTEN ' + Event);
end;

{ Stop listening to a specific event }
procedure TDirPgSqlNotify.UnlistenTo(Event: string);
begin
  Event := Trim(Event);
  if Event <> '' then
    InternalExec('UNLISTEN ' + Event);
end;

{ Generate a notify event }
procedure TDirPgSqlNotify.DoNotify(Event: string);
begin
  Event := Trim(Event);
  if Event <> '' then
    InternalExec('NOTIFY ' + Event);
end;

{ Checks for any pending events }
function TDirPgSqlNotify.CheckEvents: string;
begin
  SetStatus(nsFail);
  Result := '';

  if not Assigned(FTransact) or not FTransact.Active then
    Exit;

  if Assigned(FHandle) then
    PQnotifyFree(FHandle);

  { Collect any asynchronous backend messages }
  PQconsumeInput(TDirPgSqlTransact(FTransact).Handle);
  FHandle := PQnotifies(TDirPgSqlTransact(FTransact).Handle);

  if Assigned(FHandle) then
    Result := StrPas(FHandle^.relname);
  SetStatus(nsOK);  
end;

{*************** Extra functions implementation ****************}

{ Convert postgresql field types to delphi field types }
function PgSqlToDelphiType(Value: string; var Size: Integer;
  var ArraySubType: TFieldType; var BlobType: TBlobType): TFieldType;
var
  IsArray: Boolean;
begin
  BlobType := btInternal;
  IsArray := False;

  { If the field name starts with '_', should an array }
  if Value[1] = '_' then
  begin
    IsArray := True;
    Delete(Value, 1, 1);
  end;
  if Size < 0 then Size := 0;
  if (Value = 'interval') or (Value = 'char')
    or (Value = 'varchar') or ((Value = 'text') and (Size > 0)) then
    Result := ftString
  else if Value = 'text' then
    Result := ftMemo
  else if Value = 'oid' then
  begin
    Result := ftBlob;
    BlobType := btExternal;
  end
  else if Value = 'int2' then
    Result := ftInteger
  else if Value = 'int4' then
    Result := ftInteger
  else if Value = 'int8' then
{$IFNDEF VER100}
    Result := ftLargeInt
{$ELSE}
    Result := ftInteger
{$ENDIF}
  else if (Value = 'float4') or (Value = 'float8')
    or (Value = 'decimal') or (Value = 'numeric') then
    Result := ftFloat
  else if Value = 'money' then
    Result := ftCurrency
  else if Value = 'bool' then
    Result := ftBoolean
  else if (Value = 'datetime') or (Value = 'timestamp') or (Value = 'abstime') then
    Result := ftDateTime
  else if Value = 'date' then
    Result := ftDate
  else if (Value = 'time') then
    Result := ftTime
  else if Value = 'name' then
  begin
    Result := ftString;
    Size := 32;
  end else if Value = 'regproc' then
  begin
    Result := ftString;
    Size := 10;
  end
  else
  begin
    Result := ftString;
    if Size <= 0 then
      Size := DEFAULT_STRING_SIZE;
  end;
  { Fixing array type and subtype }
  if IsArray then
  begin
    ArraySubType := Result;
{$IFNDEF VER100}
    Result := ftArray
{$ENDIF}
  end;
  if (Result = ftString) and (Size = 0) then
    Size := DEFAULT_STRING_SIZE;
  if Result <> ftString then Size := 0;
end;

initialization
  MonitorList := TZMonitorList.Create;
finalization
  MonitorList.Free;
end.

