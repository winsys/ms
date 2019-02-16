{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{              Abstract Database component               }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZConnect;

interface

{$IFNDEF LINUX}
{$INCLUDE ..\ZeosDef.inc}
{$ELSE}
{$INCLUDE ../ZeosDef.inc}
{$ENDIF}

uses
  SysUtils, {$IFDEF WINDOWS}Windows,{$ENDIF} Classes, ZToken, ZDirSql, ZConvert;

{$INCLUDE ../Zeos.inc}

type
  { Database options }
  TZDatabaseOption = (coHourGlass);
  TZDatabaseOptions = set of TZDatabaseOption;

  { Abstract database component }
  TZDatabase = class(TComponent)
  protected
    FDatabase: ShortString;
    FLogin: ShortString;
    FPasswd: ShortString;
    FHost: ShortString;
    FPort: ShortString;
    FDatasets: TList;
    FTransacts: TList;
    FConnected: Boolean;
    FLoginPrompt: Boolean;
    FEncoding: TEncodingType;
    FHandle: TDirConnect;
    FOptions: TZDatabaseOptions;
    FVersion: Integer;

    FBeforeConnect: TNotifyEvent;
    FBeforeDisconnect: TNotifyEvent;
    FBeforeCreate: TNotifyEvent;
    FBeforeDrop: TNotifyEvent;
    FAfterDisconnect: TNotifyEvent;
    FAfterConnect: TNotifyEvent;
    FAfterCreate: TNotifyEvent;
    FAfterDrop: TNotifyEvent;

    procedure SetConnected(Value: Boolean);
    procedure SetHost(Value: ShortString);
    procedure SetDatabase(Value: ShortString);
    function  GetTransacts(Index: Integer): TObject;
    function  GetTransactCount: Integer;
    function  GetDefaultTransact: TComponent;
    function  GetDatasets(Index: Integer): TObject;
    function  GetDatasetCount: Integer;

    procedure Loaded; override;

    procedure DoBeforeConnect; virtual;
    procedure DoAfterConnect; virtual;
    procedure DoBeforeDisconnect; virtual;
    procedure DoAfterDisconnect; virtual;
    procedure DoBeforeCreate; virtual;
    procedure DoAfterCreate; virtual;
    procedure DoBeforeDrop; virtual;
    procedure DoAfterDrop; virtual;

    property  Port: ShortString read FPort write FPort;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Connect; virtual;
    procedure Disconnect; virtual;
    procedure CreateDatabase(Params: string); virtual;
    procedure DropDatabase; virtual;
    procedure GetTableNames(Pattern: string;
      SystemTables: Boolean; List: TStrings); virtual;
    procedure GetFieldNames(const TableName: string; List: TStrings); virtual;

    procedure AddTransaction(Transact: TObject);
    procedure RemoveTransaction(Transact: TObject);
    procedure OpenTransactions;
    procedure CloseTransactions;

    procedure AddDataset(Dataset: TObject);
    procedure RemoveDataset(Dataset: TObject);
    procedure OpenActiveDatasets;
    procedure CloseDatasets;

    property  Handle: TDirConnect read FHandle;

    property  Host: ShortString read FHost write SetHost;
    property  Database: ShortString read FDatabase  write SetDatabase;
    property  Login: ShortString read FLogin write FLogin;
    property  Password: ShortString read FPasswd write FPasswd;
    property  LoginPrompt: Boolean read FLoginPrompt write FLoginPrompt;
    property  Options: TZDatabaseOptions read FOptions write FOptions;
    property  Connected: Boolean read FConnected write SetConnected;
    property  Encoding: TEncodingType read FEncoding write FEncoding;

    property  Transactions[Index: Integer]: TObject read GetTransacts;
    property  TransactionCount: Integer read GetTransactCount;
    property  DefaultTransaction: TComponent read GetDefaultTransact;
    property  Datasets[Index: Integer]: TObject read GetDatasets;
    property  DatasetCount: Integer read GetDatasetCount;
  published
    property Version: Integer read FVersion;

    property  BeforeConnect: TNotifyEvent read FBeforeConnect write FBeforeConnect;
    property  AfterConnect: TNotifyEvent read FAfterConnect write FAfterConnect;
    property  BeforeDisconnect: TNotifyEvent read FBeforeDisconnect write FBeforeDisconnect;
    property  AfterDisconnect: TNotifyEvent read FAfterDisconnect write FAfterDisconnect;
  end;

implementation

uses {$IFNDEF NO_GUI}{$IFNDEF LINUX}Controls, Forms,{$ELSE}QControls, QForms,{$ENDIF}{$ENDIF}
  ZDBaseConst, DB, ZTransact, ZQuery, {#$IFNDEF VERCLX}DBLogDlg(*{$ELSE}QDBLogDlg{$ENDIF}*),
  ZSqlScript;

{***************** TZDatabase implementation *****************}

{ Class constructor }
constructor TZDatabase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDatasets  := TList.Create;
  FTransacts := TList.Create;
  FConnected := False;
  FLoginPrompt := False;
  FEncoding  := etNone;
  FOptions := [coHourGlass];
  FVersion := ZDBO_VERSION;
end;

{ Class destructor }
destructor TZDatabase.Destroy;
begin
  inherited Destroy;
  Disconnect;
  FDatasets.Free;
  FTransacts.Free;
  FHandle.Free;
end;

{ Set connected prop }
procedure TZDatabase.SetConnected(Value: Boolean);
begin
  if Value <> FConnected then
    if Value then Connect
    else Disconnect;
end;

{ Set new host name }
procedure TZDatabase.SetHost(Value: ShortString);
begin
  if FHost <> Value then
  begin
    Disconnect;
    FHost := Value;
  end;
end;

{ Set new database name }
procedure TZDatabase.SetDatabase(Value: ShortString);
begin
  if FDatabase <> Value then
  begin
    Disconnect;
    FDatabase := Value;
  end;
end;

{ Get datasets count }
function TZDatabase.GetDatasetCount: Integer;
begin
  Result := FDatasets.Count;
end;

{ Get dataset by index }
function TZDatabase.GetDatasets(Index: Integer): TObject;
begin
  Result := FDatasets[Index];
end;

{ Get default transaction }
function TZDatabase.GetDefaultTransact: TComponent;
begin
  Result := nil;
  if FTransacts.Count > 0 then
    Result := FTransacts[0];
end;

{ Get transactions count }
function TZDatabase.GetTransactCount: Integer;
begin
  Result := FTransacts.Count;
end;

{ Get transaction by index }
function TZDatabase.GetTransacts(Index: Integer): TObject;
begin
  Result := FTransacts[Index];
end;

{ Before connect settings }
procedure TZDatabase.DoBeforeConnect;
begin
  if Assigned(FBeforeConnect) then
    FBeforeConnect(Self);
end;

{ After connect settings }
procedure TZDatabase.DoAfterConnect;
begin
  if Assigned(FAfterConnect) then
    FAfterConnect(Self);
end;

{ Before disconnect settings }
procedure TZDatabase.DoBeforeDisconnect;
begin
  if Assigned(FBeforeDisconnect) then
    FBeforeDisconnect(Self);
end;

{ After disconnect settings }
procedure TZDatabase.DoAfterDisconnect;
begin
  if Assigned(FAfterDisconnect) then
    FAfterDisconnect(Self);
end;

{ After database created }
procedure TZDatabase.DoAfterCreate;
begin
  if Assigned(FAfterCreate) then
    FAfterCreate(Self);
end;

{ After dropped database }
procedure TZDatabase.DoAfterDrop;
begin
  if Assigned(FAfterDrop) then
    FAfterDrop(Self);
end;

{ Before created database }
procedure TZDatabase.DoBeforeCreate;
begin
  if Assigned(FBeforeCreate) then
    FBeforeCreate(Self);
end;

{ Before droped database }
procedure TZDatabase.DoBeforeDrop;
begin
  if Assigned(FBeforeDrop) then
    FBeforeDrop(Self);
end;

{ Connect to Sql database }
procedure TZDatabase.Connect;
var
  ALogin, APasswd: string;
{$IFNDEF NO_GUI}
  OldCursor: TCursor;
{$ENDIF}
begin
  if FConnected then Exit;
{$IFNDEF NO_GUI}
  OldCursor := Screen.Cursor;
{$ENDIF}
  try
{$IFNDEF NO_GUI}
    if coHourGlass in FOptions then
      Screen.Cursor := crSqlWait;
{$ENDIF}

    DoBeforeConnect;
    if FLoginPrompt then
    begin
      ALogin := FLogin;
      APasswd := FPasswd;
      if not LoginDialog(FDatabase, ALogin, APasswd) then
        Abort;
      FLogin := ALogin;
      FPasswd := APasswd;
   end;

    FHandle.HostName := FHost;
    FHandle.Database := FDatabase;
    FHandle.Port     := FPort;
    FHandle.Login    := FLogin;
    FHandle.Passwd   := FPasswd;

    FHandle.Connect;
    if FHandle.Status <> csOk then
      DatabaseError(Convert(FHandle.Error, FEncoding, etNone));

    FConnected := FHandle.Active;
    if FConnected then
      try
        OpenTransactions;
      except
        CloseTransactions;
        raise;
      end;

    if FConnected then
      DoAfterConnect;
  except
    FConnected := False;
{$IFNDEF NO_GUI}
    Screen.Cursor := OldCursor;
{$ENDIF}
    raise;
  end;
{$IFNDEF NO_GUI}
  Screen.Cursor := OldCursor;
{$ENDIF}
end;

{ Disconnect from Sql database }
procedure TZDatabase.Disconnect;
{$IFNDEF NO_GUI}
var
  OldCursor: TCursor;
{$ENDIF}
begin
  if not FConnected then Exit;
{$IFNDEF NO_GUI}
  OldCursor := Screen.Cursor;
{$ENDIF}
  try
{$IFNDEF NO_GUI}
    if coHourGlass in FOptions then
      Screen.Cursor := crSqlWait;
{$ENDIF}

    DoBeforeDisconnect;

    CloseDatasets;
    CloseTransactions;
    FConnected := False;

    FHandle.Disconnect;
    if FHandle.Status <> csOk then
      DatabaseError(Convert(FHandle.Error, FEncoding, etNone));

    DoAfterDisconnect;
  finally
{$IFNDEF NO_GUI}
    Screen.Cursor := OldCursor;
{$ENDIF}
  end;
end;

{ Create new database }
procedure TZDatabase.CreateDatabase(Params: string);
{$IFNDEF NO_GUI}
var
  OldCursor: TCursor;
{$ENDIF}
begin
{$IFNDEF NO_GUI}
  OldCursor := Screen.Cursor;
{$ENDIF}
  try
{$IFNDEF NO_GUI}
    if coHourGlass in FOptions then
      Screen.Cursor := crSqlWait;
{$ENDIF}

    if Connected then Disconnect;
    DoBeforeCreate;

    FHandle.HostName := FHost;
    FHandle.Database := FDatabase;
    FHandle.Port     := FPort;
    FHandle.Login    := FLogin;
    FHandle.Passwd   := FPasswd;

    FHandle.CreateDatabase(Params);
    if FHandle.Status <> csOk then
      DatabaseError(Convert(FHandle.Error, FEncoding, etNone));
    Connect;

    DoAfterCreate;
  finally
{$IFNDEF NO_GUI}
    Screen.Cursor := OldCursor;
{$ENDIF}
  end;
end;

{ Drop exist database }
procedure TZDatabase.DropDatabase;
{$IFNDEF NO_GUI}
var
  OldCursor: TCursor;
{$ENDIF}
begin
{$IFNDEF NO_GUI}
  OldCursor := Screen.Cursor;
{$ENDIF}
  try
{$IFNDEF NO_GUI}
    if coHourGlass in FOptions then
      Screen.Cursor := crSqlWait;
{$ENDIF}

    DoBeforeDrop;

    Disconnect;

    FHandle.HostName := FHost;
    FHandle.Database := FDatabase;
    FHandle.Port     := FPort;
    FHandle.Login    := FLogin;
    FHandle.Passwd   := FPasswd;

    FHandle.DropDatabase;
    if FHandle.Status <> csOk then
      DatabaseError(Convert(FHandle.Error, FEncoding, etNone));

    DoAfterDrop;
  finally
{$IFNDEF NO_GUI}
    Screen.Cursor := OldCursor;
{$ENDIF}
  end;
end;

procedure TZDatabase.GetTableNames(Pattern: string;
  SystemTables: Boolean; List: TStrings);
begin
  List.Clear;
  if DefaultTransaction = nil then Exit;

  with TZTransact(DefaultTransaction).QueryHandle do
  begin
    ShowTables(Pattern);
    while not Eof do
    begin
      List.Add(Field(1));
      Next;
    end;
    Close;
  end;
end;

{ Retrieve a list of fields in the associated table }
procedure TZDatabase.GetFieldNames(const TableName: string; List: TStrings);
begin
  List.Clear;
  if DefaultTransaction = nil then Exit;

  with TZTransact(DefaultTransaction).QueryHandle do
  begin
    ShowColumns(TableName, '');
    while not Eof do
    begin
      List.Add(Field(1));
      Next;
    end;
    Close;
  end;
end;

{ Add new query to query's list }
procedure TZDatabase.AddDataset(Dataset: TObject);
begin
  if FDatasets.IndexOf(Dataset) >= 0 then Exit;
  FDatasets.Add(Dataset);
end;

{ Remove query from query's list }
procedure TZDatabase.RemoveDataset(Dataset: TObject);
var
  N: Integer;
begin
  N := FDatasets.IndexOf(Dataset);
  if N >= 0 then
  try
    TDataset(FDatasets[N]).Close;
  finally
    FDatasets.Delete(N);
  end;
end;

{ Close all queries from qiery's list }
procedure TZDatabase.CloseDatasets;
var
  I: Integer;
begin
  for I := 0 to FDatasets.Count-1 do
    try
      TDataset(FDatasets[I]).Close;
    except
    end;
end;

{ Add new transactions to list }
procedure TZDatabase.AddTransaction(Transact: TObject);
begin
  if FTransacts.IndexOf(Transact) < 0 then
    FTransacts.Add(Transact);
end;

{ Remove transaction from list }
procedure TZDatabase.RemoveTransaction(Transact: TObject);
var
  N: Integer;
begin
  N := FTransacts.IndexOf(Transact);
  if N >= 0 then
  try
    TZTransact(FTransacts[N]).Disconnect;
  finally
    FTransacts.Delete(N);
  end;
end;

{ Open all transactions from list }
procedure TZDatabase.OpenTransactions;
var
  I: Integer;
begin
  for I := 0 to FTransacts.Count-1 do
    TZTransact(FTransacts[I]).Connect;
end;

{ Close all transactions from list }
procedure TZDatabase.CloseTransactions;
var
  I: Integer;
begin
  for I := 0 to FTransacts.Count-1 do
  try
    if TZTransact(FTransacts[I]).Connected then
      TZTransact(FTransacts[I]).Disconnect;
  except
  end;
end;

{ Open active datasets }
procedure TZDatabase.Loaded;
begin
  inherited Loaded;
  OpenActiveDatasets;
end;

{ Open autoactivated datasets }
procedure TZDatabase.OpenActiveDatasets;
var
  I: Integer;
begin
  for I := 0 to FDatasets.Count-1 do
    if Assigned(TZDataset(FDatasets[I]).Database)
      and Assigned(TZDataset(FDatasets[I]).Transaction)
      and TZDataset(FDatasets[I]).AutoOpen
      and not TZDataset(FDatasets[I]).Active then
      TDataset(FDatasets[I]).Open;
end;

end.
