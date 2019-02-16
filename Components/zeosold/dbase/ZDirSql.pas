{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{              Abstract direct class API                 }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZDirSql;

interface

uses SysUtils, Classes, DB, ZToken, ZSqlTypes {$IFDEF VER100}, DbTables{$ENDIF};

type
  { Database status }
  TDirStatus = (csNone, csOk, csFail, csNotImplemented);

  { Abstract class for direct database connection }
  TDirConnect = class
  private
    FHost: ShortString;
    FPort: ShortString;
    FDatabase: ShortString;
    FLogin: ShortString;
    FPasswd: ShortString;
    FActive: Boolean;
    FStatus: TDirStatus;
  protected
    function  GetErrorMsg: ShortString; virtual;
    procedure SetActive(Value: Boolean);
    procedure SetStatus(Value: TDirStatus);
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Connect; virtual;
    procedure Disconnect; virtual;
    procedure CreateDatabase(Params: string); virtual;
    procedure DropDatabase; virtual;

    property  HostName: ShortString read FHost write FHost;
    property  Port: ShortString read FPort write FPort;
    property  Database: ShortString read FDatabase write FDatabase;
    property  Login: ShortString read FLogin write FLogin;
    property  Passwd: ShortString read FPasswd write FPasswd;

    property  Active: Boolean read FActive;
    property  Error: ShortString read GetErrorMsg;
    property  Status: TDirStatus read FStatus;
  end;

  { Abstract class for database transaction }
  TDirTransact = class
  private
    FStatus: TDirStatus;
    FConnect: TDirConnect;
    FActive: Boolean;
    FTransactSafe: Boolean;
  protected
    function  GetErrorMsg: ShortString; virtual;
    function  GetStatus: TDirStatus; virtual;
    procedure SetStatus(Value: TDirStatus); virtual;
    procedure SetActive(Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Open; virtual;
    procedure Close; virtual;
    procedure StartTransaction; virtual;
    procedure EndTransaction; virtual;
    procedure Commit; virtual;
    procedure Rollback; virtual;

    property Connect: TDirConnect read FConnect write FConnect;
    property TransactSafe: Boolean read FTransactSafe write FTransactSafe;

    property Active: Boolean read FActive;
    property Error: ShortString read GetErrorMsg;
    property Status: TDirStatus read GetStatus;
 end;

  { Query status }
  TDirQueryStatus = (qsNone, qsTuplesOk, qsCommandOk, qsFail, qsNotImplemented);

  TDirBlob = class;

  { Abstract class for database query }
  TDirQuery = class
  private
    FLocFields: TStringList;
    FLocValues: TStringList;
    FUseCursor: Boolean;
    FConnect: TDirConnect;
    FTransact: TDirTransact;
    FStatus: TDirQueryStatus;

    FActive: Boolean;
    FRecno: LongInt;
    FAffectedRows: LongInt;
    FBof: Boolean;
    FEof: Boolean;
    FSql: string;
  protected
    function GetBof: Boolean; virtual;
    procedure SetBof(Value: Boolean);
    function GetEof: Boolean; virtual;
    procedure SetEof(Value: Boolean);
    procedure SetRecNo(Value: LongInt);
    procedure SetActive(Value: Boolean);
    procedure SetStatus(Value: TDirQueryStatus);
    procedure SetAffectedRows(Value: LongInt);
    function GetErrorMsg: ShortString; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function  ExecuteParams(Params: TVarRecArray;
      ParamCount: Integer): LongInt; virtual;
    function Execute: LongInt; virtual;
    procedure Open; virtual;
    procedure Close; virtual;
    function CreateBlobObject: TDirBlob; virtual;

    procedure ShowDatabases(DatabaseName: ShortString); virtual;
    procedure ShowTables(TableName: ShortString); virtual;
    procedure ShowColumns(TableName, ColumnName: ShortString); virtual;
    procedure ShowIndexes(TableName: ShortString); virtual;

    procedure First; virtual;
    procedure Last; virtual;
    procedure Prev; virtual;
    procedure Next; virtual;
    procedure Go(Num: Integer); virtual;
    function Locate(Params: string): Boolean;
    function FindNext: Boolean;

    function FieldCount: Integer; virtual;
    function RecordCount: Integer; virtual;

    function FieldName(FieldNum: Integer): ShortString; virtual;
    function FieldAlias(FieldNum: Integer): ShortString; virtual;
    function FieldIndex(FieldName: ShortString): Integer; virtual;
    function FieldSize(FieldNum: Integer): Integer; virtual;
    function FieldMaxSize(FieldNum: Integer): Integer; virtual;
    function FieldPrecision(FieldNum: Integer): Integer; virtual;
    function FieldDecimals(FieldNum: Integer): Integer; virtual;
    function FieldType(FieldNum: Integer): Integer; virtual;
    function FieldDataType(FieldNum: Integer): TFieldType; virtual;
    function FieldIsNull(FieldNum: Integer): Boolean; virtual;
    function FieldReadOnly(FieldNum: Integer): Boolean; virtual;
    function Field(FieldNum: Integer): string; virtual;
    function FieldBuffer(FieldNum: Integer): PChar; virtual;
    function FieldByName(FieldName: ShortString): string;

    function StringToSql(Value: string): string; virtual;

    property Connect: TDirConnect read FConnect write FConnect;
    property Transact: TDirTransact read FTransact write FTransact;
    property Sql: string read FSql write FSql;
    property Active: Boolean read FActive;
    property Status: TDirQueryStatus read FStatus;
    property Error: ShortString read GetErrorMsg;
    property UseCursor: Boolean read FUseCursor write FUseCursor;

    property Bof: Boolean read GetBof;
    property Eof: Boolean read GetEof;
    property RecNo: LongInt read FRecno;
    property AffectedRows: LongInt read FAffectedRows;
  end;

  { Blob status }
  TDirBlobStatus = (bsNone, bsOk, bsFail, bsNotImplemented);

  { Abstract class for database binary large object }
  TDirBlob = class
  protected
    FStatus: TDirBlobStatus;
    FActive: Boolean;
    FHandle: TBlobHandle;
    FConnect: TDirConnect;
    FTransact: TDirTransact;
  protected
    procedure SetStatus(Value: TDirBlobStatus);
    procedure SetActive(Value: Boolean);
    procedure SetHandle(Value: TBlobHandle);
    function  GetErrorMsg: ShortString; virtual;
    function  GetPosition: LongInt; virtual;
    function  GetValue: string;
    procedure SetValue(Value: string);
  public
    constructor Create(AConnect: TDirConnect; ATransact: TDirTransact;
      AHandle: TBlobHandle);
    destructor Destroy; override;

    procedure Open(Mode: Integer); virtual;
    procedure Close; virtual;
    procedure CreateBlob; virtual;
    procedure DropBlob; virtual;

    function  Read(Buffer: PChar; Length: Integer): Integer; virtual;
    function  Write(Buffer: PChar; Length: Integer): Integer; virtual;
    procedure Seek(Offset: LongInt; Origin: Integer); virtual;

    procedure ImportFile(FileName: ShortString); virtual;
    procedure ExportFile(FileName: ShortString); virtual;

    property  Connect: TDirConnect read FConnect write FConnect;
    property  Transact: TDirTransact read FTransact write FTransact;
    property  Status: TDirBlobStatus read FStatus;
    property  Active: Boolean read FActive;
    property  Error: ShortString read GetErrorMsg;

    property  Handle: TBlobHandle read FHandle write FHandle;
    property  Position: LongInt read GetPosition;
    property  Value: string read GetValue write SetValue;
  end;

  TDirNotifyStatus = (nsNone, nsOk, nsFail, nsNotImplemented);

  { Abstract class for asynchrounous notifying}
  TDirNotify = class
  protected
    FActive: Boolean;
    FConnect: TDirConnect;
    FTransact: TDirTransact;
    FStatus: TDirNotifyStatus;

    procedure SetStatus(Value: TDirNotifyStatus);
    procedure SetActive(Value: Boolean);
    function GetErrorMsg: ShortString; virtual;
  public
    procedure ListenTo(Event: string); virtual;
    procedure UnlistenTo(Event: string); virtual;
    procedure DoNotify(Event: string); virtual;
    function CheckEvents: string; virtual;

    property Connect: TDirConnect read FConnect write FConnect;
    property Transact: TDirTransact read FTransact write FTransact;
    property Active: Boolean read FActive;
    property Status: TDirNotifyStatus read FStatus;
    property Error: ShortString read GetErrorMsg;
  end;

  { Abstract class for database stored procedure }
  TDirStoredProc = class
  private
    FLocFields: TStringList;  // Locate
    FLocValues: TStringList;  // Locate
    FConnect: TDirConnect;
    FTransact: TDirTransact;
    FStatus: TDirQueryStatus;

    FActive: Boolean;
    FRecno: LongInt;
    FAffectedRows: LongInt;
    FBof: Boolean;
    FEof: Boolean;
    FPrepared: Boolean;
    FStoredProcName: string;
  protected
    function GetBof: Boolean; virtual;
    procedure SetBof(Value: Boolean);
    function GetEof: Boolean; virtual;
    procedure SetEof(Value: Boolean);
    procedure SetRecNo(Value: LongInt);
    procedure SetActive(Value: Boolean);
    procedure SetStatus(Value: TDirQueryStatus);
    procedure SetAffectedRows(Value: LongInt);
    function GetErrorMsg: ShortString; virtual;
    function GetPrepared: Boolean; virtual;
    procedure SetPrepared(const Value: Boolean); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ExecProc; virtual;
    procedure Open; virtual;
    procedure Close; virtual;
    function CreateBlobObject: TDirBlob; virtual;
    procedure Prepare(Params: TParams); virtual;
    procedure UnPrepare; virtual;
    function GetReturnValue: string; virtual;

    procedure ShowStoredProcs; virtual;
    procedure ShowParams(StoredProcedureName: ShortString); virtual;

    procedure First; virtual;
    procedure Last; virtual;
    procedure Prev; virtual;
    procedure Next; virtual;
    procedure Go(Num: Integer); virtual;
    function Locate(Params: string): Boolean;
    function FindNext: Boolean;

    function FieldCount: Integer; virtual;
    function RecordCount: Integer; virtual;
    function ParamCount: Integer; virtual;

    function FieldName(FieldNum: Integer): ShortString; virtual;
    function FieldAlias(FieldNum: Integer): ShortString; virtual;
    function FieldIndex(FieldName: ShortString): Integer; virtual;
    function FieldSize(FieldNum: Integer): Integer; virtual;
    function FieldMaxSize(FieldNum: Integer): Integer; virtual;
    function FieldDecimals(FieldNum: Integer): Integer; virtual;
    function FieldType(FieldNum: Integer): Integer; virtual;
    function FieldDataType(FieldNum: Integer): TFieldType; virtual;
    function FieldIsNull(FieldNum: Integer): Boolean; virtual;
    function Field(FieldNum: Integer): string; virtual;
    function FieldBuffer(FieldNum: Integer): PChar; virtual;
    function FieldByName(FieldName: ShortString): string;

    function ParamName(ParamNum: Integer): ShortString; virtual;
    function ParamSize(ParamNum: Integer): Integer; virtual;
    function ParamAlias(ParamNum: Integer): ShortString; virtual;
    function ParamMaxSize(ParamNum: Integer): Integer; virtual;
    function ParamDecimals(ParamNum: Integer): Integer; virtual;
    function ParamIndex(ParamName: ShortString): Integer; virtual;
    function ParamType(ParamNum: Integer): Integer; virtual;
    function ParamDataType(ParamNum: Integer): TFieldType; virtual;
    function ParamIsNull(ParamNum: Integer): Boolean; virtual;
    function Param(ParamNum: Integer): string; virtual;
    function ParamBuffer(ParamNum: Integer): PChar; virtual;
    function ParamByName(ParamName: ShortString): string;

    function StringToSql(Value: string): string; virtual;

    property Connect: TDirConnect read FConnect write FConnect;
    property Transact: TDirTransact read FTransact write FTransact;
    property Active: Boolean read FActive;
    property Status: TDirQueryStatus read FStatus;
    property Error: ShortString read GetErrorMsg;

    property Bof: Boolean read GetBof;
    property Eof: Boolean read GetEof;
    property RecNo: LongInt read FRecno;
    property AffectedRows: LongInt read FAffectedRows;
    property Prepared: Boolean read GetPrepared write SetPrepared;

    property StoredProcName: string read FStoredProcName write FStoredProcName;
  end;

implementation

{********************* TDirConnect implementation *********************}

{ Class constructor }
constructor TDirConnect.Create;
begin
end;

{ Class destructor }
destructor TDirConnect.Destroy;
begin
  if Active then Disconnect;
  inherited;
end;

{ Set active connect property }
procedure TDirConnect.SetActive(Value: Boolean);
begin
  FActive := Value;
end;

{ Set connect status }
procedure TDirConnect.SetStatus(Value: TDirStatus);
begin
  FStatus := Value;
end;

{ Get an error message }
function TDirConnect.GetErrorMsg: ShortString;
begin
  Result := 'Not connected';
end;

{ Connect to the database }
procedure TDirConnect.Connect;
begin
  if Active then Disconnect;
  SetStatus(csNotImplemented);
end;

{ Disconnect from the database }
procedure TDirConnect.Disconnect;
begin
  SetActive(False);
  SetStatus(csNotImplemented);
end;

{ Create new database }
procedure TDirConnect.CreateDatabase(Params: string);
begin
  if Active then Disconnect;
  SetStatus(csNotImplemented);
end;

{ Drop connected database }
procedure TDirConnect.DropDatabase;
begin
  if Active then Disconnect;
  SetStatus(csNotImplemented);
end;

{******************** TDirTransact implementation ****************}

{ Class constructor }
constructor TDirTransact.Create;
begin
  FActive := False;
  FTransactSafe := True;
end;

{ Class destructor }
destructor TDirTransact.Destroy;
begin
  if Active then EndTransaction;
  inherited;
end;

{ Set active transaction property }
procedure TDirTransact.SetActive(Value: Boolean);
begin
  FActive := Value;
end;

{ Get status transaction property }
function TDirTransact.GetStatus: TDirStatus;
begin
  Result := FStatus;
end;

{ Set status transaction property }
procedure TDirTransact.SetStatus(Value: TDirStatus);
begin
  FStatus := Value;
end;

{ Get error message }
function TDirTransact.GetErrorMsg: ShortString;
begin
  Result := '';
 if (Status <> csOk) and Assigned(Connect) then
   Result := Connect.Error
 else
   Result := '';
end;

{ Abstract connect transaction }
procedure TDirTransact.Open;
begin
  if Active then Close;
  SetStatus(csNotImplemented);
end;

{ Abstract disconnect transaction }
procedure TDirTransact.Close;
begin
//  SetActive(False);
  SetStatus(csNotImplemented);
end;

{ Abstract start transaction }
procedure TDirTransact.StartTransaction;
begin
  SetStatus(csNotImplemented);
end;

{ Abtract end transaction }
procedure TDirTransact.EndTransaction;
begin
  SetStatus(csNotImplemented);
end;

{ Abstract commit transaction }
procedure TDirTransact.Commit;
begin
  SetStatus(csNotImplemented);
end;

{ Astract rollback transaction }
procedure TDirTransact.Rollback;
begin
  SetStatus(csNotImplemented);
end;

{******************** TDirQuery implementation ******************}

function IIF(A: Boolean; B, C: Integer): Integer;
begin
  if A then Result := B
  else Result := C;
end;

{ Class constructor }
constructor TDirQuery.Create;
begin
  FLocFields := TStringList.Create;
  FLocValues := TStringList.Create;
  FBof := True;
  FEof := True;
end;

{ Class destructor }
destructor TDirQuery.Destroy;
begin
  if Active then Close;
  FLocFields.Free;
  FLocValues.Free;
  Finalize(FSql);
  inherited Destroy;
end;

{ Set active property }
procedure TDirQuery.SetActive(Value: Boolean);
begin
  FActive := Value;
end;

{ Set rows affected property }
procedure TDirQuery.SetAffectedRows(Value: Integer);
begin
  FAffectedRows := Value;
end;

{ Is begin of rows }
function TDirQuery.GetBof: Boolean;
begin
  Result := FBof;
end;

{ Set begin of rows property }
procedure TDirQuery.SetBof(Value: Boolean);
begin
  FBof := Value;
end;

{ Is end of rows }
function TDirQuery.GetEof: Boolean;
begin
  Result := FEof;
end;

{ Set end of rows property }
procedure TDirQuery.SetEof(Value: Boolean);
begin
  FEof := Value;
end;

{ Set current row number }
procedure TDirQuery.SetRecNo(Value: Integer);
begin
  FRecNo := Value;
end;

{ Set query status }
procedure TDirQuery.SetStatus(Value: TDirQueryStatus);
begin
  FStatus := Value;
end;

{ Get an error message }
function TDirQuery.GetErrorMsg: ShortString;
begin
  Result := '';
  if not (FStatus in [qsTuplesOk, qsCommandOk]) and Assigned(Transact) then
    Result := Transact.Error;
end;

{ Execute a query without rows returning }
function TDirQuery.Execute: LongInt;
begin
  if Active then Close;
  SetStatus(qsNotImplemented);
  FAffectedRows := 0;
  Result := 0;
end;

{ Execute a query with parameters }
function TDirQuery.ExecuteParams(Params: TVarRecArray;
  ParamCount: Integer): LongInt;
begin
  Result := Execute;
end;

{ Open a query }
procedure TDirQuery.Open;
begin
  if Active then Close;
  FAffectedRows := 0;
  SetStatus(qsNotImplemented);
end;

{ Close an open query }
procedure TDirQuery.Close;
begin
  FLocFields.Clear;
  FLocValues.Clear;
  FActive := False;
  FAffectedRows := 0;
  FBof := True;
  FEof := True;
  FRecNo := 0;
  SetStatus(qsNotImplemented);
end;

{ Create connected bob stream }
function TDirQuery.CreateBlobObject: TDirBlob;
begin
  Result := nil;
end;

{ Go to the first row }
procedure TDirQuery.First;
begin
  FRecno := 0;
  FBof := (RecordCount <= 0);
  FEof := FBof;
end;

{ Go to the last row }
procedure TDirQuery.Last;
begin
  FRecno := IIF(RecordCount>0, RecordCount-1, 0);
  FBof := (RecordCount <= 0);
  FEof := FBof;
end;

{ Go to prior row }
procedure TDirQuery.Prev;
begin
  FEof := False;
  if FRecno > 0 then
  begin
    Dec(FRecno);
    FBof := False;
  end else
    FBof := True;
  if RecordCount <= 0 then
  begin
    FBof := True;
    FEof := True;
  end;
end;

{ Go to next row }
procedure TDirQuery.Next;
begin
  FBof := False;
  if FRecno < (RecordCount-1) then
  begin
    Inc(FRecno);
    FEof := False;
  end else
    FEof := True;
  if RecordCount <= 0 then
  begin
    FBof := True;
    FEof := True;
  end;
end;

{ Go to Num row }
procedure TDirQuery.Go(Num: Integer);
begin
  FRecno := IIF(Num < (RecordCount-1), Num, RecordCount-1);
  FRecno := IIF(FRecno < 0, 0, FRecno);
  FBof   := (FRecno < 0);
  FEof   := (FRecno >= RecordCount);
end;

{ Get field quantity in a query }
function TDirQuery.FieldCount: Integer;
begin
  Result := 0;
end;

{ Get a record quantity in a query }
function TDirQuery.RecordCount: Integer;
begin
  Result := 0;
end;

{ Get a field name by it number }
function TDirQuery.FieldName(FieldNum: Integer): ShortString;
begin
  Result := '';
end;

{ Get field alias }
function TDirQuery.FieldAlias(FieldNum: Integer): ShortString;
var
  I, P:  Integer;
begin
  Result := FieldName(FieldNum);
  P := 0;
  for I := 0 to FieldNum-1 do
  begin
    if FieldName(I) = Result then
      Inc(P);
  end;
  if P <> 0 then
    Result := Result + '_' + IntToStr(P);
end;

{ Get field number by it name }
function TDirQuery.FieldIndex(FieldName: ShortString): Integer;
var
  I, P: Integer;
  Name, Num: string;
begin
  Result := -1;
  if FieldCount = 0 then Exit;
  
  for I := 0 to FieldCount-1 do
    if CompareText(FieldName, Self.FieldName(I)) = 0 then
    begin
      Result := I;
      Break;
    end;

  if Result <> -1 then Exit;

  Name := '';
  Num  := '';
  P := LastDelimiter('_', FieldName);
  if P > 0 then
  begin
    Name := Copy(FieldName, 1, P-1);
    Num  := Copy(FieldName, P+1, 10);
  end else
    Exit;

  P := StrToIntDef(Num, 0) + 1;
  if P <= 1 then Exit;

  for I := 0 to FieldCount-1 do
  begin
    if CompareText(Name, Self.FieldName(I)) = 0 then Dec(P);
    if P = 0 then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

{ Get a field size }
function TDirQuery.FieldSize(FieldNum: Integer): Integer;
begin
  Result := 0;
end;

{ Get a maximum field size }
function TDirQuery.FieldMaxSize(FieldNum: Integer): Integer;
begin
  Result := 0;
end;

{ Get field Precision }
function TDirQuery.FieldPrecision(FieldNum: Integer): Integer;
begin
  Result := 0;
end;

{ Get field decimals }
function TDirQuery.FieldDecimals(FieldNum: Integer): Integer;
begin
  Result := 0;
end;

{ Get a field value by it number }
function TDirQuery.Field(FieldNum: Integer): string;
begin
  Result := '';
end;

{ Get a field value by it name }
function TDirQuery.FieldByName(FieldName: ShortString): string;
begin
  Result := Field(FieldIndex(FieldName));
end;

{ Check if field is null }
function TDirQuery.FieldIsNull(FieldNum: Integer): Boolean;
begin
  Result := True;
end;

{ Check if field is ReadOnly }
function TDirQuery.FieldReadOnly(FieldNum: Integer): boolean;
begin
  Result := False;
end;

{ Get field buffer }
function TDirQuery.FieldBuffer(FieldNum: Integer): PChar;
begin
  Result := nil;
end;

{ Get field type }
function TDirQuery.FieldType(FieldNum: Integer): Integer;
begin
  Result := 0;
end;

{ Get field delphi compatible type }
function TDirQuery.FieldDataType(FieldNum: Integer): TFieldType;
begin
  Result := ftUnknown;
end;

{ Find a first row equal to params }
{ Params - params string as "field=value..." }
function TDirQuery.Locate(Params: string): Boolean;
var
  I, N: Integer;
begin
  Result := False;
  SplitParams(Params, FLocFields, FLocValues);
  if FLocValues.Count = 0 then Exit;

  for I := FLocValues.Count-1 downto 0 do
  begin
    if IsDigit(FLocFields[I][1]) then
      N := StrToIntDef(FLocFields[I], -1)
    else
      N := FieldIndex(FLocFields[I]);
    if (N < 0) or (N >= FieldCount) then
    begin
      FLocFields.Delete(I);
      FLocValues.Delete(I);
    end else
      FLocFields.Objects[I] := TObject(N);
  end;

  First;
  while not Eof do
  begin
    Result := True;
    for I := 0 to FLocValues.Count-1 do
      if Field(Integer(FLocFields.Objects[I])) <> FLocValues[I] then
      begin
        Result := False;
        Break;
      end;
    if Result then Break;
    Next;
  end;
end;

{ Find a next by locate row }
function TDirQuery.FindNext: Boolean;
var
  I: Integer;
begin
  Result := False;
  if FLocValues.Count = 0 then Exit;

  Next;
  while not Eof do
  begin
    Result := True;
    for I := 0 to FLocValues.Count-1 do
      if Field(Integer(FLocValues.Objects[I])) <> FLocValues[I] then
        Result := False;
    if Result then Break;
    Next;
  end;
end;

{ Showes all databases }
procedure TDirQuery.ShowDatabases(DatabaseName: ShortString);
begin
  if Active then Close;
  SetStatus(qsNotImplemented);
end;

{ Showes tables of the database }
procedure TDirQuery.ShowTables(TableName: ShortString);
begin
  if Active then Close;
  SetStatus(qsNotImplemented);
end;

{ Showes columns of the table }
procedure TDirQuery.ShowColumns(TableName, ColumnName: ShortString);
begin
  if Active then Close;
  SetStatus(qsNotImplemented);
end;

{ Showes indexes of the table }
procedure TDirQuery.ShowIndexes(TableName: ShortString);
begin
  if Active then Close;
  SetStatus(qsNotImplemented);
end;

{ Convert string to Sql string }
function TDirQuery.StringToSql(Value: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Value) do
    if Value[I] = '''' then
      Result := Result + ''''''
    else Result := Result + Value[I];
end;

{***************** TDirBlob implementation ****************}

{ Class constructor }
constructor TDirBlob.Create(AConnect: TDirConnect; ATransact: TDirTransact;
  AHandle: TBlobHandle);
begin
  FConnect := AConnect;
  FTransact := ATransact;
  FHandle := AHandle;
end;

{ Class destructor }
destructor TDirBlob.Destroy;
begin
  if Active then Close;
  inherited Destroy;
end;

{ Set blob active status }
procedure TDirBlob.SetActive(Value: Boolean);
begin
  FActive := Value;
end;

{ Set blob status }
procedure TDirBlob.SetStatus(Value: TDirBlobStatus);
begin
  FStatus := Value;
end;

{ Set blob handle }
procedure TDirBlob.SetHandle(Value: TBlobHandle);
begin
  FHandle := Value;
end;

{ Get error message }
function TDirBlob.GetErrorMsg: ShortString;
begin
  Result := '';
  if (Status <> bsOk) and Assigned(Transact) then
    Result := Transact.Error;
end;

{ Get current blob position }
function TDirBlob.GetPosition: LongInt;
begin
  Result := 0;
end;

{ Open new blob for any mode }
procedure TDirBlob.Open(Mode: Integer);
begin
  FStatus := bsNotImplemented;
end;

{ Close open blob }
procedure TDirBlob.Close;
begin
  FActive := False;
  FStatus := bsNotImplemented;
end;

{ Create new blob }
procedure TDirBlob.CreateBlob;
begin
  FStatus := bsNotImplemented;
end;

{ Delete open blob }
procedure TDirBlob.DropBlob;
begin
  FStatus := bsNotImplemented;
  FActive := False;
end;

{ Read blob segment }
function TDirBlob.Read(Buffer: PChar; Length: Integer): Integer;
begin
  Result := 0;
end;

{ Write blob segment }
function TDirBlob.Write(Buffer: PChar; Length: Integer): Integer;
begin
  Result := 0;
end;

{ Seek new blob position }
procedure TDirBlob.Seek(Offset: Integer; Origin: Integer);
begin
  FStatus := bsNotImplemented;
end;

{ Export blob to file }
procedure TDirBlob.ExportFile(FileName: ShortString);
begin
  FStatus := bsNotImplemented;
end;

{ Import blob from file }
procedure TDirBlob.ImportFile(FileName: ShortString);
begin
  FStatus := bsNotImplemented;
end;

{ Get blob as string value }
function TDirBlob.GetValue: string;
var
  Buffer: array[0..512] of Char;
  N: Integer;
begin
  Result := '';
  Open(fmOpenRead);
  if Status = bsOk then
    repeat
      N := Read(Buffer, 512);
      Result := Result + Copy(Buffer, 1, N);
    until N = 0;
end;

{ Set blob as string value }
procedure TDirBlob.SetValue(Value: string);
begin
  DropBlob;
  CreateBlob;
  Write(PChar(Value), Length(Value));
end;

{***************** TDirNotify implementation ****************}

{ Set instance status }
procedure TDirNotify.SetStatus(Value: TDirNotifyStatus);
begin
  FStatus := Value;
end;

{ Set notify active state }
procedure TDirNotify.SetActive(Value: Boolean);
begin
  FActive := Value;
end;

{ Get an internal error }
function TDirNotify.GetErrorMsg: ShortString;
begin
  Result := '';
  if Status <> nsOk then
  begin
    if Assigned(Transact) then
      Result := Transact.Error
    else Result := 'Transact object isn''t defined';
  end;
end;

{ Listen to a specific event }
procedure TDirNotify.ListenTo(Event: string);
begin
  SetStatus(nsNotImplemented);
end;

{ Stop listening to a specific event }
procedure TDirNotify.UnlistenTo(Event: string);
begin
  SetStatus(nsNotImplemented);
end;

{ Generate a notify event }
procedure TDirNotify.DoNotify(Event: string);
begin
  SetStatus(nsNotImplemented);
end;

{ Check a notifycation message }
function TDirNotify.CheckEvents: string;
begin
  SetStatus(nsNotImplemented);
  Result := '';
end;

{ TDirStoredProc }

constructor TDirStoredProc.Create;
begin
  FLocFields := TStringList.Create;
  FLocValues := TStringList.Create;
  FBof := true;
  FEof := true;
end;

destructor TDirStoredProc.Destroy;
begin
  if Active then
    Close;
  FLocFields.Free;
  FLocValues.Free;

  inherited Destroy;
end;

function TDirStoredProc.CreateBlobObject: TDirBlob;
begin
  Result := nil;
end;

procedure TDirStoredProc.Close;
begin
  FLocFields.Clear;
  FLocValues.Clear;
  FActive := False;
  FAffectedRows := 0;
  FBof := true;
  FEof := true;
  FRecNo := 0;
  SetStatus(qsNotImplemented);
end;

procedure TDirStoredProc.ExecProc;
begin
  if Active then
    Close;
  SetStatus(qsNotImplemented);
  FAffectedRows := 0;
end;

function TDirStoredProc.Field(FieldNum: Integer): string;
begin
  Result := '';
end;

function TDirStoredProc.FieldAlias(FieldNum: Integer): ShortString;
var
  P, I: Integer;
begin
  Result := FieldName(FieldNum);
  P := 0;
  for I := 0 to FieldNum-1 do
    if FieldName(I) = Result then
      inc(P);
  if P <> 0 then
    Result := Result + '_' + IntToStr(P);
end;

function TDirStoredProc.FieldBuffer(FieldNum: Integer): PChar;
begin
  Result := nil;
end;

function TDirStoredProc.FieldByName(FieldName: ShortString): string;
begin
  Result := Field(FieldIndex(FieldName));
end;

function TDirStoredProc.FieldCount: Integer;
begin
  Result := 0;
end;

function TDirStoredProc.FieldDataType(FieldNum: Integer): TFieldType;
begin
  Result := ftUnknown;
end;

function TDirStoredProc.FieldDecimals(FieldNum: Integer): Integer;
begin
  Result := 0;
end;

function TDirStoredProc.FieldIndex(FieldName: ShortString): Integer;
var
  I, P: Integer;
  Name, Num: string;
begin
  Result := -1;
  if FieldCount = 0 then
    Exit;

  for I := 0 to FieldCount-1 do
    if FieldName = Self.FieldName(I) then
    begin
      Result := I;
      Break;
    end;

  if Result <> -1 then
    Exit;

  Name := '';
  Num := '';

  P := LastDelimiter('_', FieldName);
  if P > 0 then
  begin
    Name := Copy(FieldName,1,P-1);
    Num := Copy(FieldName,P+1,10);
  end else
    Exit;

  P := StrToIntDef(Num,0) + 1;
  if P < 1 then
    Exit;

  for I := 0 to FieldCount-1 do
  begin
    if Name = Self.FieldName(I) then
      Dec(P);
    if P=0 then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function TDirStoredProc.FieldIsNull(FieldNum: Integer): Boolean;
begin
  Result := True;
end;

function TDirStoredProc.FieldMaxSize(FieldNum: Integer): Integer;
begin
  Result := 0;
end;

function TDirStoredProc.FieldName(FieldNum: Integer): ShortString;
begin
  Result := '';
end;

function TDirStoredProc.FieldSize(FieldNum: Integer): Integer;
begin
  Result := 0;
end;

function TDirStoredProc.FieldType(FieldNum: Integer): Integer;
begin
  Result := 0;
end;

function TDirStoredProc.FindNext: Boolean;
var
  I: Integer;
begin
  Result := False;
  if FLocValues.Count = 0 then
    Exit;

  Next;
  while not Eof do
  begin
    Result := True;
    for I := 0 to FLocValues.Count-1 do
      if Field(Integer(FLocValues.Objects[I])) <> FLocValues[I] then
        Result := False;

    if Result = False then
      Break;
    Next;
  end;
end;

procedure TDirStoredProc.First;
begin
  FRecNo := 0;
  FBof := (RecordCount <= 0);
  FEof := FBof;
end;

function TDirStoredProc.GetBof: Boolean;
begin
  Result := FBof;
end;

function TDirStoredProc.GetEof: Boolean;
begin
  Result := FEof;
end;

function TDirStoredProc.GetErrorMsg: ShortString;
begin
  Result := '';
  if not (FStatus in [qsTuplesOk, qsCommandOk]) and Assigned(Transact) then
    Result := Transact.Error;
end;

function TDirStoredProc.GetPrepared: Boolean;
begin
  Result := FPrepared;
end;

procedure TDirStoredProc.Go(Num: Integer);
begin
  FRecNo := IIF(Num < (RecordCount-1), Num, RecordCount-1);
  FRecNo := IIF(FRecno < 0, 0, FRecno);
  FBof := (FRecNo < 0);
  FEof := (FRecNo >= RecordCount);
end;

procedure TDirStoredProc.Last;
begin
  FRecNo := IIF(RecordCount > 0, RecordCount-1, 0);
  FBof := (RecordCount <= 0);
  FEof := FBof;
end;

function TDirStoredProc.Locate(Params: string): Boolean;
var
  I, N: Integer;
begin
  Result := False;
  SplitParams(Params, FLocFields, FLocValues);
  if FLocValues.Count = 0 then
    Exit;

  for I := FLocValues.Count-1 downto 0 do
  begin
    if IsDigit(FLocFields[I][1]) then
      N := StrToIntDef(FLocFields[I],-1)
    else
      N := FieldIndex(FLocFields[I]);
    if (N < 0) or (N >= FieldCount) then
    begin
      FLocFields.Delete(I);
      FLocValues.Delete(I);
    end
    else
      FLocFields.Objects[I] := TObject(N);
  end;

  First;
  while not Eof do
  begin
    Result := True;
    for I := 0 to FLocValues.Count-1 do
    if Field(Integer(FLocFields.Objects[I])) <> FLocValues[I] then
    begin
      Result := False;
      Break;
    end;
    if Result then
      Break;
    Next;
  end;
end;

procedure TDirStoredProc.Next;
begin
  FBof := False;
  if FRecNo < (RecordCount-1) then
  begin
    Inc(FRecNo);
    FEof := False;
  end
  else
    FBof := True;

  if RecordCount <= 0 then
  begin
    FBof := True;
    FEof := True;
  end;
end;

procedure TDirStoredProc.Open;
begin
  if Active then
    Close;
  FAffectedRows := 0;
  SetStatus(qsNotImplemented);
end;

function TDirStoredProc.Param(ParamNum: Integer): string;
begin
  Result := '';
end;

function TDirStoredProc.ParamAlias(ParamNum: Integer): ShortString;
var
  I, P: Integer;
begin
  Result := ParamName(ParamNum);
  P := 0;
  for I := 0 to ParamNum-1 do
    if ParamName(I) = Result then
      Inc(P);
  if P <> 0 then
    Result := Result + '_' + IntToStr(P);
end;

function TDirStoredProc.ParamBuffer(ParamNum: Integer): PChar;
begin
  Result := nil;
end;

function TDirStoredProc.ParamByName(ParamName: ShortString): string;
begin
  Result := Param(ParamIndex(ParamName));
end;

function TDirStoredProc.ParamCount: Integer;
begin
  Result := 0;
end;

function TDirStoredProc.ParamDataType(ParamNum: Integer): TFieldType;
begin
  Result := ftUnknown;
end;

function TDirStoredProc.ParamDecimals(ParamNum: Integer): Integer;
begin
  Result := 0;
end;

function TDirStoredProc.ParamIndex(ParamName: ShortString): Integer;
var
  I, P: Integer;
  Name, Num: string;
begin
  Result := -1;
  if ParamCount = 0 then
    Exit;

  for I := 0 to ParamCount-1 do
    if ParamName = Self.ParamName(I) then
    begin
      Result := I;
      Break;
    end;

  if Result <> -1 then
    Exit;

  Name := '';
  Num := '';
  P := LastDelimiter('_', ParamName);
  if P > 0 then
  begin
    Name := Copy(ParamName,1,P-1);
    Num := Copy(ParamName,P+1,10);
  end else
    Exit;

  P := StrToIntDef(Num,0) + 1;
  if P <= 1 then
    Exit;

  for I := 0 to ParamCount-1 do
  begin
    if Name = Self.ParamName(I) then
      Dec(P);
    if P = 0 then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function TDirStoredProc.ParamIsNull(ParamNum: Integer): Boolean;
begin
  Result := True;
end;

function TDirStoredProc.ParamMaxSize(ParamNum: Integer): Integer;
begin
  Result := 0;
end;

function TDirStoredProc.ParamName(ParamNum: Integer): ShortString;
begin
  Result := '';
end;

function TDirStoredProc.ParamSize(ParamNum: Integer): Integer;
begin
  Result := 0;
end;

function TDirStoredProc.ParamType(ParamNum: Integer): Integer;
begin
  Result := 0;
end;

procedure TDirStoredProc.Prepare(Params: TParams);
begin
  SetPrepared(True);
end;

procedure TDirStoredProc.Prev;
begin
  FEof := False;
  if FRecNo > 0 then
  begin
    Dec(FRecNo);
    FBof := False;
  end
  else
    FBof := True;
  if RecordCount <= 0 then
  begin
    FBof := True;
    FEof := True;
  end;
end;

function TDirStoredProc.RecordCount: Integer;
begin
  Result := 0;
end;

procedure TDirStoredProc.SetActive(Value: Boolean);
begin
  FActive := Value;
end;

procedure TDirStoredProc.SetAffectedRows(Value: Integer);
begin
  FAffectedRows := Value;
end;

procedure TDirStoredProc.SetBof(Value: Boolean);
begin
  FBof := Value;
end;

procedure TDirStoredProc.SetEof(Value: Boolean);
begin
  FEof := Value;
end;

procedure TDirStoredProc.SetPrepared(const Value: Boolean);
begin
  FPrepared := Value;
end;

procedure TDirStoredProc.SetRecNo(Value: Integer);
begin
  FRecNo := Value;
end;

procedure TDirStoredProc.SetStatus(Value: TDirQueryStatus);
begin
  FStatus := Value;
end;

procedure TDirStoredProc.ShowParams(StoredProcedureName: ShortString);
begin
  if Active then
    Close;
  SetStatus(qsNotImplemented);
end;

procedure TDirStoredProc.ShowStoredProcs;
begin
  if Active then
    Close;
  SetStatus(qsNotImplemented);
end;

function TDirStoredProc.StringToSql(Value: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Value) do
    if Value[I] = '''' then
      Result := Result + ''''''
    else
      Result := Result + Value[I];
end;

procedure TDirStoredProc.UnPrepare;
begin
  SetPrepared(False);
end;

function TDirStoredProc.GetReturnValue: string;
begin
  Result := '';
end;

end.
