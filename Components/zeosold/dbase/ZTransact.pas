{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{             Abstract Transaction component             }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZTransact;

interface

{$R *.dcr}

uses
  {$IFNDEF LINUX} ExtCtrls, {$ELSE} QExtCtrls, {$ENDIF}
  SysUtils, DB, Classes, ZToken, ZConnect, ZDirSql, ZSqlTypes, ZSqlScanner;

{$INCLUDE ../Zeos.inc}

type
  { Transact options }
  TZTransactOption = (toHourGlass);
  TZTransactOptions = set of TZTransactOption;
  TZMonitor = class;

  { BatchExecSql event handlers }
  TOnBeforeBatchExec = procedure (Sender: TObject; var Sql: string) of object;
  TOnAfterbatchExec = procedure (Sender: TObject; var Res: Integer) of object;
  TOnBatchError = procedure (Sender: TObject; const E: Exception; var Stop: Boolean) of object; 

  { Abstract transaction component }
  TZTransact = class(TComponent)
  protected
    FConnected: Boolean;
    FAutoCommit: Boolean;
    FAutoRecovery: Boolean;
    FNotifies: TList;
    FOptions: TZTransactOptions;
    FDatabase: TZDatabase;
    FDatabaseType: TDatabaseType;
    FHandle: TDirTransact;
    FQuery: TDirQuery;
    FOnDataChange: TNotifyEvent;
    FOnApplyUpdates: TNotifyEvent;
    FOnCommit: TNotifyEvent;
    FOnRollback: TNotifyEvent;
    FOnBeforeConnect: TNotifyEvent;
    FOnAfterConnect: TNotifyEvent;
    FOnBeforeDisconnect: TNotifyEvent;
    FOnAfterDisconnect: TNotifyEvent;
    FVersion: Integer;
    FBatchCurPos, FBatchCurLen, FBatchCurrentLine: Integer;
    FOnBeforeBatchExec: TOnBeforeBatchExec;
    FOnAfterBatchExec: TOnAfterBatchExec;
    FOnBatchError: TOnBatchError;

    procedure SetConnected(Value: Boolean);
    procedure SetDatabase(Value: TZDatabase);
    function  GetTransactSafe: Boolean;
    procedure SetTransactSafe(Value: Boolean);
    function  GetNotifies(Index: Integer): TObject;
    function  GetNotifyCount: Integer;

    procedure Loaded; override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoDataChange(Sql: string);
    procedure DoCommit;
    procedure DoRollback;

    property AutoRecovery: Boolean read FAutoRecovery write FAutoRecovery;
    property DatabaseType: TDatabaseType read FDatabaseType;
    property TransactSafe: Boolean read GetTransactSafe write SetTransactSafe;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Connect; virtual;
    procedure Disconnect; virtual;
    function ExecSql(Sql: String): LongInt; virtual;
    function ExecSqlParams(Sql: WideString; Params: TVarRecArray;
      ParamCount: Integer): LongInt; virtual;
    function BatchExecSql(Sql: String): LongInt;
    function ExecFunc(Func: WideString): WideString; virtual;
    procedure Commit; virtual;
    procedure Rollback; virtual;
    procedure Recovery(Force: Boolean); virtual;
    procedure DoApplyUpdates;

    procedure AddMonitor(Monitor: TZMonitor); virtual; abstract;
    procedure DeleteMonitor(Monitor: TZMonitor); virtual; abstract;

    procedure AddNotify(Notify: TObject);
    procedure RemoveNotify(Notify: TObject);
    procedure CloseNotifies;

    property Database: TZDatabase read FDatabase write SetDatabase;
    property Connected: Boolean read FConnected write SetConnected;
    property Handle: TDirTransact read FHandle;
    property QueryHandle: TDirQuery read FQuery;

    property Notifies[Index: Integer]: TObject read GetNotifies;
    property NotifyCount: Integer read GetNotifyCount;
  published
    property Options: TZTransactOptions read FOptions write FOptions;
    property AutoCommit: Boolean read FAutoCommit write FAutoCommit;
    property Version: Integer read FVersion;
    property BatchCurPos: Integer read FBatchCurPos;
    property BatchCurLen: Integer read FBatchCurLen;
    property BatchCurrentLine: Integer read FBatchCurrentLine;

    property OnBeforeConnect: TNotifyEvent read FOnBeforeConnect write FOnBeforeConnect;
    property OnAfterConnect: TNotifyEvent read FOnAfterConnect write FOnAfterConnect;
    property OnBeforeDisconnect: TNotifyEvent read FOnBeforeDisconnect write FOnBeforeDisconnect;
    property OnAfterDisconnect: TNotifyEvent read FOnAfterDisconnect write FOnAfterDisconnect;
    property OnBeforeBatchExec: TOnBeforeBatchExec read FOnBeforeBatchExec write FOnBeforeBatchExec;
    property OnAfterBatchExec: TOnAfterBatchExec read FOnAfterBatchExec write FOnAfterBatchExec;
    property OnBatchError: TOnBatchError read FOnBatchError write FOnBatchError;
    property OnDataChange: TNotifyEvent read FOnDataChange write FOnDataChange;
    property OnApplyUpdates: TNotifyEvent read FOnApplyUpdates write FOnApplyUpdates;
    property OnCommit: TNotifyEvent read FOnCommit write FOnCommit;
    property OnRollback: TNotifyEvent read FOnRollback write FOnRollback;
  end;

  { Event on post sql query }
  TMonitorEvent = procedure(Sql, Result: string) of object;

  { Abstract component for monitoring outgoing queries }
  TZMonitor = class (TComponent)
  private
    FTransact: TZTransact;
    FMonitorEvent: TMonitorEvent;
    procedure SetTransact(const Value: TZTransact);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
  published
    property Transaction: TZTransact read FTransact write SetTransact;
    property OnMonitorEvent: TMonitorEvent read FMonitorEvent write FMonitorEvent;
  end;

  { Monitors list class }
  TZMonitorList = class (TList)
  private
    function GetMonitor(Index: Integer): TZMonitor;
  public
    procedure AddMonitor(Value: TZMonitor);
    procedure DeleteMonitor(Value: TZMonitor);
    procedure InvokeEvent(Sql, Result: WideString; Error: Boolean);

    property Monitors[Index: Integer]: TZMonitor read GetMonitor;
  end;

  { Sql statements executing component }
  TZBatchSql = class(TComponent)
  private
    FTransact: TZTransact;
    FAffectedRows: LongInt;
    FBeforeExecute: TNotifyEvent;
    FAfterExecute: TNotifyEvent;
    FSql: TStringList;

    procedure SetSql(Value: TStringList);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure ExecSql;
  published
    property Transaction: TZTransact read FTransact write FTransact;
    property Sql: TStringList read FSql write SetSql;
    property RowsAffected: LongInt read FAffectedRows;

    property OnBeforeExecute: TNotifyEvent read FBeforeExecute write FBeforeExecute;
    property OnAfterExecute: TNotifyEvent read FAfterExecute write FAfterExecute;
  end;

  { Custom Notify event handler type }
  TZNotifyEvent = procedure (Sender: TObject; Event: string) of object;

  { Asynchronous notifying}
  TZNotify = class (TComponent)
  private
    FActive: Boolean;
    FAutoOpen: Boolean;
    FEventsList: TStringList;
    FTimer: TTimer;
    FFirstConnect: Boolean;

    FBeforeRegister: TZNotifyEvent;
    FBeforeUnregister: TZNotifyEvent;
    FAfterRegister: TZNotifyEvent;
    FAfterUnregister: TZNotifyEvent;
    FNotifyFired: TZNotifyEvent;
  protected
    FTransact: TZTransact;
    FHandle: TDirNotify;
    FBackEventsList: TStringList;

    procedure SetActive(Value: Boolean);
    function GetInterval: Cardinal; virtual;
    procedure SetInterval(Value: Cardinal); virtual;
    procedure SetEventsList(Value: TStringList); virtual;
    procedure SetTransact(Value: TZTransact);

    procedure TimerProc(Sender: TObject); virtual;
    procedure CheckEvents; virtual;
    procedure EventsChange(Sender: TObject); virtual;
    procedure EventsChanging(Sender: TObject); virtual;
    procedure CheckActive; virtual;

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Open; virtual;
    procedure Close; virtual;

    procedure ListenTo(Event: string);
    procedure DoNotify(Event: string);
    procedure UnlistenTo(Event: string);

    property Handle: TDirNotify read FHandle;
  published

    property Active: Boolean read FActive write SetActive;
    property EventsList: TStringList read FEventsList write SetEventsList;
    property Interval: Cardinal read GetInterval write SetInterval;

    property OnBeforeRegister: TZNotifyEvent read FBeforeRegister write FBeforeRegister;
    property OnAfterRegister: TZNotifyEvent read FAfterRegister write FAfterRegister;
    property OnBeforeUnregister: TZNotifyEvent read FBeforeUnregister write FBeforeUnregister;
    property OnAfterUnregister: TZNotifyEvent read FAfterUnregister write FAfterUnregister;
    property OnNotify: TZNotifyEvent read FNotifyFired write FNotifyFired;
  end;

  { Custom TThread descendent to allow true asynchronous notify processing }
implementation

uses ZDbaseConst, ZConvert, ZSqlScript
{$IFNDEF NO_GUI}{$IFNDEF LINUX}
  ,Windows, Controls, Forms
{$ELSE}
  ,QControls, QForms
{$ENDIF}{$ENDIF};

{***************** TZTransact implementation *****************}

{ Class constructor }
constructor TZTransact.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnected  := False;
  FAutoCommit := True;
  FOptions := [toHourGlass];
  FVersion := ZDBO_VERSION;
  FNotifies := TList.Create;
  FDatabaseType := dtUnknown;
end;

{ Class destructor }
destructor TZTransact.Destroy;
begin
  if Connected then
    Disconnect;
  CloseNotifies;
  
  if Assigned(FDatabase) then
    FDatabase.RemoveTransaction(Self);
  FQuery.Free;
  FHandle.Free;
  FNotifies.Free;
  inherited Destroy;
end;

{ Set connected prop }
procedure TZTransact.SetConnected(Value: Boolean);
begin
  if Value <> FConnected then
    if Value then Connect
    else Disconnect;
end;

{ Set database connection prop }
procedure TZTransact.SetDatabase(Value: TZDatabase);
begin
  Disconnect;
  try
    if Assigned(FDatabase) then
      FDatabase.RemoveTransaction(Self);
    if Assigned(Value) then
      Value.AddTransaction(Self);
  finally
    FDatabase := Value;
    if Assigned(FDatabase) then
    begin
      FHandle.Connect := FDatabase.Handle;
      FQuery.Connect := FDatabase.Handle;
    end
    else
    begin
      FHandle.Connect := nil;
      FQuery.Connect := nil;
    end;
  end;
end;

{ Get transaction safe property }
function TZTransact.GetTransactSafe: Boolean;
begin
  Result := Handle.TransactSafe;
end;

{ Set transaction safe property}
procedure TZTransact.SetTransactSafe(Value: Boolean);
begin
  if Handle.TransactSafe <> Value then
  begin
    if Connected then
    begin
      if Handle.TransactSafe then
        Handle.EndTransaction
      else
        Handle.StartTransaction;
    end;
    Handle.TransactSafe := Value;
  end;
end;

{ Process notification method }
procedure TZTransact.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FDatabase ) and (Operation = opRemove) then
  begin
    Disconnect;
    FDatabase := nil;
    FHandle.Connect := nil;
    FQuery.Connect := nil;
  end;
end;

{ Connect to Sql-database }
procedure TZTransact.Connect;
{$IFNDEF NO_GUI}
var
  OldCursor: TCursor;
{$ENDIF}
begin
  if FConnected then Exit;
{$IFNDEF NO_GUI}
  OldCursor := Screen.Cursor;
{$ENDIF}
  try
{$IFNDEF NO_GUI}
    if toHourGlass in FOptions then
      Screen.Cursor := crSqlWait;
{$ENDIF}

    if not Assigned(FDatabase) then
      DatabaseError(SConnectNotDefined);

    if Assigned(OnBeforeConnect) then
      OnBeforeConnect(Self);
    FDatabase.Connect;
    if not FDatabase.Connected then
      DatabaseError(SConnectError);

    if not FHandle.Active then
    begin
      FHandle.Open;
      if FHandle.Status <> csOk then
        DatabaseError(Convert(FHandle.Error, FDatabase.Encoding, etNone));
    end;
    FConnected := FHandle.Active;
    if Assigned(OnAfterConnect) then
      OnAfterConnect(Self);
  finally
{$IFNDEF NO_GUI}
    Screen.Cursor := OldCursor;
{$ENDIF}
  end;
end;

{ Disconnect from database }
procedure TZTransact.Disconnect;
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
    if toHourGlass in FOptions then
      Screen.Cursor := crSqlWait;
{$ENDIF}

    //CloseNotifies;
    if Assigned(OnBeforeDisconnect) then
      OnBeforeDisconnect(Self);
    FConnected := False;

    FHandle.Close;
    if FHandle.Status <> csOk then
      DatabaseError(Convert(FHandle.Error, FDatabase.Encoding, etNone));
  finally
{$IFNDEF NO_GUI}
    Screen.Cursor := OldCursor;
{$ENDIF}
  end;
  if Assigned(OnAfterDisconnect) then
    OnAfterDisconnect(Self);
end;

{ Call OnDataChange event }
procedure TZTransact.DoDataChange(Sql: string);
var
  Token: string;
begin
  if Assigned(FOnDataChange) then
  begin
    Token := UpperCase(StrTok(Sql,' '#9#10#13));
    if (Token = 'UPDATE') or (Token = 'INSERT') or (Token = 'DELETE') then
      FOnDataChange(Self);
  end;
end;

{ Invoke OnApplyUpdates event }
procedure TZTransact.DoApplyUpdates;
begin
  if Assigned(FOnApplyUpdates) then
    FOnApplyUpdates(Self);
end;

{ Invoke OnCommit event }
procedure TZTransact.DoCommit;
begin
  if Assigned(FOnCommit) then
    FOnCommit(Self);
end;

{ Invoke OnRollback event }
procedure TZTransact.DoRollback;
begin
  if Assigned(FOnRollback) then
    FOnRollback(Self);
end;

{ Execute a query }
function TZTransact.ExecSql(Sql: string): LongInt;
var
{$IFNDEF NO_GUI}
  OldCursor: TCursor;
{$ENDIF}
  Error: string;
begin
  if not FConnected then
    DatabaseError(SNotConnected);
{$IFNDEF NO_GUI}
  OldCursor := Screen.Cursor;
{$ENDIF}
  try
{$IFNDEF NO_GUI}
    if toHourGlass in FOptions then
      Screen.Cursor := crSqlWait;
{$ENDIF}

    FQuery.Sql := Sql;
    Result := FQuery.Execute;
    if FQuery.Status <> qsCommandOk then
    begin
      Error := Convert(SQL+#13#10#13#10+FQuery.Error, FDatabase.Encoding, etNone);
      Recovery(False);
      DatabaseError(Error);
    end else
      DoDataChange(Sql);

    if FAutoCommit then Commit;
  finally
    FQuery.Sql := '';
{$IFNDEF NO_GUI}
    Screen.Cursor := OldCursor;
{$ENDIF}
  end;
end;

{ Execute an sql statement with parameters }
function TZTransact.ExecSqlParams(Sql: WideString; Params: TVarRecArray;
  ParamCount: Integer): LongInt;
var
{$IFNDEF NO_GUI}
  OldCursor: TCursor;
{$ENDIF}
  Error: string;
begin
  if not FConnected then
    DatabaseError(SNotConnected);
{$IFNDEF NO_GUI}
  OldCursor := Screen.Cursor;
{$ENDIF}
  try
{$IFNDEF NO_GUI}
    if toHourGlass in FOptions then
      Screen.Cursor := crSqlWait;
{$ENDIF}

    FQuery.Sql := Sql;
    Result := FQuery.ExecuteParams(Params, ParamCount);
    if FQuery.Status <> qsCommandOk then
    begin
      Error := Convert(FQuery.Error, FDatabase.Encoding, etNone);
      Recovery(False);
      DatabaseError(Error);
    end else
      DoDataChange(Sql);

    if FAutoCommit then Commit;
  finally
{$IFNDEF NO_GUI}
    Screen.Cursor := OldCursor;
{$ENDIF}
  end;
end;

{ Execute a function with params and return a result }
function TZTransact.ExecFunc(Func: WideString): WideString;
var
{$IFNDEF NO_GUI}
  OldCursor: TCursor;
{$ENDIF}
  Error: string;
begin
  if not FConnected then
    DatabaseError(SNotConnected);
{$IFNDEF NO_GUI}
  OldCursor := Screen.Cursor;
{$ENDIF}
  try
{$IFNDEF NO_GUI}
    if toHourGlass in FOptions then
      Screen.Cursor := crSqlWait;
{$ENDIF}

    FQuery.Sql := 'SELECT '+Func;
    FQuery.Open;
    if FQuery.Status <> qsTuplesOk then
    begin
      Error := Convert(FQuery.Error, FDatabase.Encoding, etNone);
      Recovery(False);
      DatabaseError(Error);
    end;

    Result := FQuery.Field(0);
    FQuery.Close;
    if FAutoCommit then Commit;
  finally
{$IFNDEF NO_GUI}
    Screen.Cursor := OldCursor;
{$ENDIF}
  end;
end;

{ Commit transaction }
procedure TZTransact.Commit;
{$IFNDEF NO_GUI}
var
  OldCursor: TCursor;
{$ENDIF}
begin
  if not FConnected then
    DatabaseError(SNotConnected);
{$IFNDEF NO_GUI}
  OldCursor := Screen.Cursor;
{$ENDIF}
  try
{$IFNDEF NO_GUI}
    if toHourGlass in FOptions then
      Screen.Cursor := crSqlWait;
{$ENDIF}

    FHandle.Commit;
    if FHandle.Status <> csOk then
      DatabaseError(Convert(FHandle.Error, FDatabase.Encoding, etNone));
    DoCommit;
  finally
{$IFNDEF NO_GUI}
    Screen.Cursor := OldCursor;
{$ENDIF}
  end;
end;

{ Rollback transaction }
procedure TZTransact.Rollback;
{$IFNDEF NO_GUI}
var
  OldCursor: TCursor;
{$ENDIF}
begin
  if not FConnected then
    DatabaseError(SNotConnected);
{$IFNDEF NO_GUI}
  OldCursor := Screen.Cursor;
{$ENDIF}
  try
{$IFNDEF NO_GUI}
    if toHourGlass in FOptions then
      Screen.Cursor := crSqlWait;
{$ENDIF}

    FHandle.Rollback;
    if FHandle.Status <> csOk then
      DatabaseError(Convert(FHandle.Error, FDatabase.Encoding, etNone));
    DoRollback;
  finally
{$IFNDEF NO_GUI}
    Screen.Cursor := OldCursor;
{$ENDIF}
  end;
end;

{ Execute a multiple queries }
function TZTransact.BatchExecSql(Sql: String): LongInt;
var
  Text: string;
  Scanner: TZSqlScanner;
  Stop: Boolean;
begin
  Scanner := ZSqlScanner.CreateSqlScanner(DatabaseType);
  Scanner.ShowEOL := True;
  FBatchCurPos := 0;
  FBatchCurLen := 0;
  FBatchCurrentLine := 1;
  Result := 0;
  try
    Scanner.Buffer := Sql;
    while True do
    begin
      Text := Scanner.ExtractStatement(FBatchCurPos, FBatchCurLen, FBatchCurrentLine);
      if Text='' then
        Break;
      try
        if Assigned(FOnBeforeBatchExec) then
          FOnBeforeBatchExec(Self, Text);
        Result := ExecSql(Text);
        if Assigned(FOnAfterBatchExec) then
          FOnAfterBatchExec(Self, Result);
      except
        on E: EDatabaseError do
        begin
          if Assigned(FOnBatchError) then
          begin
            Stop := True;
            FOnBatchError(Self, E, Stop);
            if Stop then
              raise;
          end
          else
            raise;
        end;
      end;
    end;
  finally
    Scanner.Free;
    if Assigned(FOnAfterBatchExec) then
      FOnAfterBatchExec(Self, Result);
  end;
end;

{ Recovery after error }
procedure TZTransact.Recovery(Force: Boolean);
begin
end;

{ Open autoactivated datasets }
procedure TZTransact.Loaded;
begin
  inherited Loaded;
  if Assigned(Database) then
    Database.OpenActiveDatasets;
end;

{ Get notify listener by index }
function TZTransact.GetNotifies(Index: Integer): TObject;
begin
  Result := FNotifies[Index];
end;

{ Get notifies count }
function TZTransact.GetNotifyCount: Integer;
begin
  Result := FNotifies.Count;
end;

{ Add new notify listener }
procedure TZTransact.AddNotify(Notify: TObject);
begin
  if FNotifies.IndexOf(Notify) >= 0 then Exit;
  FNotifies.Add(Notify);
end;

{ Delete notify listener from list }
procedure TZTransact.RemoveNotify(Notify: TObject);
var
  N: Integer;
begin
  N := FNotifies.IndexOf(Notify);
  if N >= 0 then
  try
    TZNotify(FNotifies[N]).Close;
  finally
    FNotifies.Delete(N);
  end;
end;

{ Close all notify listeners }
procedure TZTransact.CloseNotifies;
var
  I: Integer;
begin
  for I := 0 to FNotifies.Count-1 do
    try
      TZNotify(FNotifies[I]).Close;
    except
    end;
end;

{*********** TZMonitorList implementation ************}

{ Get monitor }
function TZMonitorList.GetMonitor(Index: Integer): TZMonitor;
begin
  Result := TZMonitor(Items[Index]);
end;

{ Add new monitor }
procedure TZMonitorList.AddMonitor(Value: TZMonitor);
begin
  Add(Pointer(Value));
end;

{ Delete existed monitor }
procedure TZMonitorList.DeleteMonitor(Value: TZMonitor);
var
  N: Integer;
begin
  N := IndexOf(Pointer(Value));
  if N >= 0 then
    Delete(N);
end;

{ Invoke SqlEvent in all connected monitors }
procedure TZMonitorList.InvokeEvent(Sql, Result: WideString; Error: Boolean);
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    try
      if not Error then
        Result := 'OK.';
      if Assigned(Monitors[I].OnMonitorEvent) then
        Monitors[I].OnMonitorEvent(Sql, Result);
    except
    end;
end;

{*************** TZBatchSql implementation **************}

{ Class constructor }
constructor TZBatchSql.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSql := TStringList.Create;
end;

{ Class destructor }
destructor  TZBatchSql.Destroy;
begin
  FSql.Free;
  inherited Destroy;
end;

{ Set new sql value }
procedure TZBatchSql.SetSql(Value: TStringList);
begin
  FSql.Assign(Value);
end;

{ Process notification messages }
procedure TZBatchSql.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FTransact ) and (Operation = opRemove) then
    FTransact   := nil;
end;

{ Execute sql statements }
procedure TZBatchSql.ExecSql;
begin
  if Assigned(FTransact) then
  begin
    if Assigned(FBeforeExecute) then
      FBeforeExecute(Self);
    FTransact.Connected := True;
    FAffectedRows := FTransact.BatchExecSql(FSql.Text);
    if Assigned(FAfterExecute) then
      FAfterExecute(Self);
  end else
    DatabaseError(STransactNotDefined);
end;

{******************** TZMonitor implementation ****************}

{ Class destructor }
destructor TZMonitor.Destroy;
begin
  if Assigned(FTransact) and not (csDestroying in ComponentState) then
    FTransact.DeleteMonitor(Self);
  inherited Destroy;
end;

{ Process notification events }
procedure TZMonitor.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FTransact) and (Operation = opRemove) then
  begin
    if Assigned(FTransact) then
      FTransact.DeleteMonitor(Self);
    FTransact := nil;
  end;
end;

{ Set new transaction }
procedure TZMonitor.SetTransact(const Value: TZTransact);
begin
  if FTransact <> Value then
  begin
    if Assigned(FTransact) then
      FTransact.DeleteMonitor(Self);
    FTransact := Value;
    if Assigned(FTransact) then
      FTransact.AddMonitor(Self);
  end;
end;

{******************** TZNotify implementation ****************}

{ TZNotify class constructor }
constructor TZNotify.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FEventsList := TStringList.Create;
  with FEventsList do
  begin
    Duplicates := dupIgnore;
    OnChange := EventsChange;
    OnChanging := EventsChanging;
  end;
  FBackEventsList := TStringList.Create;

  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 250;
  FTimer.OnTimer := TimerProc;

  FActive := False;
  FFirstConnect := True;
end;

{ TZNotify destructor }
destructor TZNotify.Destroy;
begin
  Close;
  FEventsList.Free;
  FBackEventsList.Free;
  FTimer.Free;
  FHandle.Free;
  inherited Destroy;
end;

{ Set check interval }
procedure TZNotify.SetInterval(Value: Cardinal);
begin
  FTimer.Interval := Value;
end;

{ Retrieve check interval }
function TZNotify.GetInterval;
begin
  Result := FTimer.Interval;
end;

{ Update the events list and sends register events at the server }
procedure TZNotify.SetEventsList(Value: TStringList);
var
  I: Integer;
begin
  FEventsList.Assign(Value);
  for I := 0 to FEventsList.Count -1 do
    FEventsList[I] := Trim(FEventsList[I]);
end;

{ Activate component }
procedure TZNotify.SetActive(Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then Open
    else Close;
  end;
end;

{ Set new transaction object }
procedure TZNotify.SetTransact(Value: TZTransact);
begin
  if FTransact <> Value then
  begin
    Close;
    if FTransact <> nil then
      FTransact.RemoveNotify(Self);
    FTransact := Value;
    if FTransact <> nil then
      FTransact.AddNotify(Self);
  end;
end;

{ Process notification messages }
procedure TZNotify.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FTransact ) and (Operation = opRemove) then
    SetTransact(nil);
end;

{ Process events list changing }
procedure TZNotify.EventsChanging(Sender: TObject);
begin
  if not Active then Exit;
  FBackEventsList.Text:=FEventsList.Text;
end;

{ Process events list changing }
procedure TZNotify.EventsChange(Sender: TObject);
var
  I: Integer;
begin
  if not Active then Exit;
  with TStringList(FEventsList) do
  begin
    OnChange := nil;
    OnChanging := nil;
  end;

  try
    { Unregistering old events }
    for I := 0 to FBackEventsList.Count-1 do
    begin
      if FEventsList.IndexOf(FBackEventsList[I]) = -1 then
      begin
        FHandle.UnListenTo(Trim(FBackEventsList[I]));
        if FHandle.Status <> nsOK then
          DatabaseError(SNotifyRegister);
      end;
    end;

    { Registering new events }
    for I := 0 to FEventsList.Count-1 do
    begin
      if FBackEventsList.IndexOf(FEventsList[I])=-1 then
      begin
        FHandle.ListenTo(Trim(FEventsList[I]));
        if FHandle.Status <> nsOK then
          DatabaseError(SNotifyRegister);
      end;
    end;
  { Restoring change event handlers }
  finally
    with TStringList(FEventsList) do
    begin
      OnChange := EventsChange;
      OnChanging := EventsChanging;
    end;
    FBackEventsList.Clear;
  end;
end;

{ Internal procedure that will be called at each timer trigger }
procedure TZNotify.TimerProc(Sender: TObject);
begin
  if not Active then
    FTimer.Enabled := False
  else CheckEvents;
end;

{ Raise exception if notify isn't active }
procedure TZNotify.CheckActive;
begin
  if not Assigned(FTransact) then
    DatabaseError(STransactNotDefined);
  if not Active then
    DatabaseError('TZNotify not in active mode');
  if not FTransact.Connected then
     DatabaseError(SNotConnected);
end;

{ Check autoopen property }
procedure TZNotify.Loaded;
begin
  inherited Loaded;
  if FAutoOpen then
  begin
    FAutoOpen := False;
    Open;
  end;
end;

{ Start the events listener }
procedure TZNotify.Open;
var
  I: Integer;
begin
  if Active then Exit;

  if not Assigned(FTransact) and (csLoading in ComponentState) then
  begin
    FAutoOpen := True;
    Exit;
  end;

  if not Assigned(FTransact) then
    DatabaseError(STransactNotDefined);
  if not FTransact.Connected then
    FTransact.Connect;

  FHandle.Connect := FTransact.Handle.Connect;
  FHandle.Transact := FTransact.Handle;

  { Registering events }
  for I := 0 to FEventsList.Count-1 do
  begin
    FHandle.ListenTo(FEventsList[I]);
    if FHandle.Status <> nsOk then
      DatabaseError(FHandle.Error);
  end;

  FActive := True;
  FTimer.Enabled := True;
end;

{ Stop the events listener }
procedure TZNotify.Close;
var
  I: Integer;
begin
  if not Active then Exit;

  FTimer.Enabled := False;

  { Unregistering events }
  for I:= 0 to FEventsList.Count-1 do
  begin
    FHandle.UnlistenTo(FEventsList[I]);
    if FHandle.Status <> nsOk then
      DatabaseError(FHandle.Error);
  end;
  FTransact.Disconnect;
  FActive := False;
end;

{ Listen to a specific event }
procedure TZNotify.ListenTo(Event: string);
begin
  if Assigned(FBeforeRegister) then
    FBeforeRegister(Self, Event);

  CheckActive;
  FHandle.ListenTo(Trim(Event));
  if FHandle.Status <> nsOk then
    DatabaseError(FHandle.Error);

  { Adding event to list }
  with FEventsList do
  begin
    OnChange := nil;
    OnChanging := nil;
    if IndexOf(Event) = -1 then
      Append(Event);
    OnChange := EventsChange;
    OnChanging := EventsChanging;
  end;

  if Assigned(FAfterRegister) then
    FAfterRegister(Self, Event);
end;

{ Generate a notify event }
procedure TZNotify.DoNotify(Event: string);
begin
  CheckActive;
  FHandle.DoNotify(Event);
  if FHandle.Status <> nsOk then
    DatabaseError(FHandle.Error);
end;

{ Stop listening to a specific event }
procedure TZNotify.UnlistenTo(Event: string);
begin
  if Assigned(FBeforeRegister) then
    FBeforeUnregister(Self, Event);

  CheckActive;
  FHandle.UnlistenTo(Trim(Event));
  if FHandle.Status <> nsOk then
    DatabaseError(FHandle.Error);

  { Removing event from list }
  with FEventsList do
  begin
    OnChange := nil;
    OnChanging := nil;
    Delete(IndexOf(Event));
    OnChange := EventsChange;
    OnChanging := EventsChanging;
  end;

  if Assigned(FAfterRegister) then
    FAfterUnregister(Self, Event);
end;

{ Checks for any pending events }
procedure TZNotify.CheckEvents;
var
  Notify: string;
begin                         
  CheckActive;
  while True do
  begin
    Notify := Trim(FHandle.CheckEvents);
    if FHandle.Status<>nsOK then
      Exit;
    if Notify = '' then Break;
    if FEventsList.IndexOf(Notify) >= 0 then
    begin
      if Assigned(FNotifyFired) then
        FNotifyFired(Self, Notify);
    end;
  end;
end;

end.
