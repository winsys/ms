{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{            Interbase Notification Component            }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZIbSqlNotify;

interface

uses
  SysUtils, Classes, DB, ZLibIbSql, ZDirIbSql, ZIbSqlCon, ZIbSqlTr;

type

  TZEventAlert = procedure(Sender: TObject; EventName: string; EventCount: LongInt;
    var CancelAlerts: Boolean) of object;
  TZErrorEvent = procedure(Sender: TObject; ErrorCode: Integer) of object;

  TZIbSqlNotify = class(TComponent)
  private
    FEvents: TStrings;
    FOnEventAlert: TZEventAlert;
    FThreads: TList;
    FNativeHandle: TISC_DB_HANDLE;
    ThreadException: Boolean;
    FDatabase: TZIbSqlDatabase;
    FOnError: TZErrorEvent;
    FAutoRegister: Boolean;
    FRegistered: Boolean;

    FStatusVector: ARRAY_ISC_STATUS;

    procedure SetDatabase(value: TZIbSqlDatabase);
    procedure SetEvents(Value: TStrings);
    function GetRegistered: Boolean;
    procedure SetRegistered(const Value: Boolean);
  protected
    //FTransact: TZIbSqlTransact;

    function GetNativeHandle: TISC_DB_HANDLE; virtual;
    procedure EventChange(Sender: TObject); virtual;
    procedure ThreadEnded(Sender: TObject); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ValidateDatabase(DataBase: TZIbSqlDatabase); virtual;

    function GetErrorMsg: ShortString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RegisterEvents; virtual;
    procedure UnRegisterEvents; virtual;
    property NativeHandle: TISC_DB_HANDLE read GetNativeHandle;
    procedure SetAutoRegister(const Value: Boolean);
    function GetAutoRegister: Boolean;
  published
    property AutoRegister: Boolean read GetAutoRegister write SetAutoRegister;
    property Database: TZIbSqlDatabase read FDatabase write SetDatabase;
    property Events: TStrings read FEvents write SetEvents;
    property Registered: Boolean read GetRegistered write SetRegistered;
    property OnEventAlert: TZEventAlert read FOnEventAlert write FOnEventAlert;
    property OnError: TZErrorEvent read FOnError write FOnError;
  end;

implementation

uses SyncObjs;

const
  IB_MAX_EVENT_BLOCK = 15; // maximum events handled per block by InterBase
  IB_MAX_EVENT_LENGTH = 64; // maximum event name length

type
  { TZIbSqlNotifyTread }
  TZIbSqlNotifyTread = class(TThread)
  private
    { IB API call parameters }
    WhichEvent: Integer;
    EventID: ISC_LONG;
    EventBuffer: PChar;
    EventBufferLen: SmallInt;
    ResultBuffer: PChar;

    { Local use variables }
    Signal: TSimpleEvent;
    EventsReceived, FirstTime: Boolean;
    EventGroup, EventCount: Integer;
    Parent: TZIbSqlNotify;
    FExceptObject: TObject;
    FExceptAddr: Pointer;
    FCancelAlerts: Boolean;
  protected
    procedure Execute; override;
    procedure SignalEvent; virtual;
    procedure SignalTerminate; virtual;
    procedure RegisterEvents; virtual;
    procedure UnRegisterEvents; virtual;
    procedure QueueEvents; virtual;
    procedure SQueEvents;
    procedure ProcessEvents; virtual;
    procedure DoEvent;
    procedure DoHandleException;
    function HandleException: Boolean; virtual;
    procedure UpdateResultBuffer(Length: UShort; Updated: PChar);
  public
    constructor Create(Owner: TZIbSqlNotify; EventGrp: Integer; TermEvent: TNotifyEvent); virtual;
    destructor Destroy; override;
  end;

  Tsib_event_block = function(EventBuffer, ResultBuffer: PPChar; IDCount: UShort;
    Event1, Event2, Event3, Event4, Event5, Event6, Event7, Event8, Event9,
    Event10, Event11, Event12, Event13, Event14, Event15: PChar): ISC_LONG; cdecl;

function TZIbSqlNotify.GetNativeHandle: TISC_DB_HANDLE;
begin
  ValidateDatabase(FDatabase);
  Result := TDirIbSqlConnect(FDatabase.Handle).Handle;
end;

procedure TZIbSqlNotify.ValidateDatabase(Database: TZIbSqlDatabase);
begin
  if not Assigned(Database) then
    DataBaseError('Database Name Missing');
  if not Database.Connected then
    DataBaseError('Database Closed');
end;

{ TZIbSqlNotify }

constructor TZIbSqlNotify.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CheckIbSqlLoaded;

  //FTransact:= TZIbSqlTransact.Create(nil) ;//added by faraj
  //FTransact.TransactSafe:=False;//faraj

  ThreadException := False;
  FOnEventAlert := nil;
  FNativeHandle := nil;
  FDatabase := nil;
  FAutoRegister := False;
  FEvents := TStringList.Create;
  with TStringList(FEvents) do
  begin
    Sorted := True; // dupIgnore only works when the TStringList is sorted
    OnChange := EventChange; // assign the routine which validates the event lenghts
    Duplicates := dupIgnore; // don't allow duplicate events
  end;
  FThreads := TList.Create;
end;


destructor TZIbSqlNotify.Destroy;
begin
  try
    if Registered then
      UnRegisterEvents;
  except
   // silence any exceptions which might be raised
   // by UnRegisterEvents during destruction
  end;

 {
  If Assigned(FTransact) then //faraj
    FTransact.RemoveNotify(Self);
 }

  FThreads.Free;
  TStringList(FEvents).OnChange := nil;
  FEvents.Free;

  //FTransact.Free;//Faraj
  inherited Destroy;
end;

procedure TZIbSqlNotify.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDatabase) then
  begin
    if Registered then
      UnRegisterEvents;
    FDatabase := nil;

    //faraj
    //FTransact.Database := nil;
  end;
end;

function TZIbSqlNotify.GetErrorMsg: ShortString;
var
  PStatusVector: PISC_STATUS;
  Msg: array[0..1024] of Char;
begin
  if (FStatusVector[0] = 1) and (FStatusVector[1] > 0) then
  begin
    PStatusVector := @FStatusVector;
    isc_interprete(Msg, @PStatusVector);
    Result := StrPas(Msg);
  end
  else if not Registered then
    Result := 'Not Registered'
  else
    Result := '';
end;


procedure TZIbSqlNotify.RegisterEvents;
var
  i: Integer;
begin
  if csDesigning in ComponentState then Exit;
  if FThreads.Count = 0 then
  begin
    {
    if not FTransact.Connected then
      FTransact.Connect; //Faraj
    }

    if FEvents.Count > 0 then
    begin
      for I := 0 to ((FEvents.Count - 1) div IB_MAX_EVENT_BLOCK) do
        FThreads.Add(TZIbSqlNotifyTread.Create(Self, i, ThreadEnded));
    end;
  end else
    DataBaseError('Event Already Registered');
end;

procedure TZIbSqlNotify.SetEvents(value: TStrings);
begin
  FEvents.Assign(value);
end;

procedure TZIbSqlNotify.SetDatabase(value: TZIbSqlDatabase);
var
  WasRegistered: Boolean;
begin
  if (Value <> FDatabase) then
  begin
    //FTransact.Database:=Value; //Faraj

    if (csDesigning in ComponentState) then
      FDatabase := Value
    else
    begin
      WasRegistered := Registered;
      if WasRegistered then
        UnRegisterEvents;
      try
      {
        if Assigned(FTransact) then  //faraj
          FTransact.RemoveNotify(Self);
      }
          FDatabase := Value;
      {
        if Assigned(FTransact) then  //faraj
          FTransact.AddNotify(Self);
      }
      finally
        if WasRegistered then
          RegisterEvents;
      end;
    end;
  end;
end;

procedure TZIbSqlNotify.SetRegistered(const Value: Boolean);
begin
  FRegistered := Value;
  if csDesigning in ComponentState then Exit;
  if Value then
    RegisterEvents
  else UnRegisterEvents;
end;

procedure TZIbSqlNotify.UnregisterEvents;
var
  I: Integer;
  Temp: TZIbSqlNotifyTread;
begin
  if csDesigning in ComponentState then Exit;
  if (FThreads.Count > 0) then
  begin
    for I := (FThreads.Count - 1) downto 0 do
    begin
      Temp := TZIbSqlNotifyTread(FThreads[I]);
      FThreads.Delete(I);
      Temp.SignalTerminate;
      Temp.WaitFor;
    end;

    //FTransact.Disconnect;//Faraj
  end
end;

procedure TZIbSqlNotify.EventChange(Sender: TObject);
var
  i: Integer;
  TooLong, AnyEmpty, WasRegistered: Boolean;
  ErrorStr: string;
begin
  ErrorStr := EmptyStr;
  WasRegistered := Registered;
  try
    if WasRegistered then
      UnRegisterEvents;
    TStringList(FEvents).OnChange := nil;
    try
      TooLong := False;
      AnyEmpty := False;
      for i := (FEvents.Count - 1) downto 0 do
      begin
        if (FEvents[i] = EmptyStr) then
        begin
          AnyEmpty := True;
          FEvents.Delete(i);
        end
        else if (Length(FEvents[i]) > (IB_MAX_EVENT_LENGTH - 1)) then
        begin
          TooLong := True;
          FEvents[i] := Copy(FEvents[i], 1, (IB_MAX_EVENT_LENGTH - 1));
        end;
      end;
      if AnyEmpty then
        DataBaseError('Invalid Event');
      if TooLong then
        DatabaseError('Invalid Event');
    finally
      TStringList(FEvents).OnChange := EventChange;
    end;
  finally
    if WasRegistered then
      RegisterEvents;
  end;
end;

function TZIbSqlNotify.GetRegistered: Boolean;
begin
 Result := FRegistered;
end;

procedure TZIbSqlNotify.ThreadEnded(Sender: TObject);
var
 ThreadIdx: Integer;
begin
 if (Sender is TZIbSqlNotifyTread) then
  begin
   ThreadIdx := FThreads.IndexOf(Sender);
   if (ThreadIdx > -1) then
    FThreads.Delete(ThreadIdx);
   if (TZIbSqlNotifyTread(Sender).ReturnValue = 1) then
    begin
     if Registered then
      UnRegisterEvents;
     ThreadException := False;
    end
  end;
end;

procedure TZIbSqlNotify.SetAutoRegister(const Value: Boolean);
begin
 if FAutoRegister <> Value then
  begin
   FAutoRegister := Value;
   if FAutoRegister and (not Registered) and
    Assigned(FDatabase) and FDatabase.Connected then
    RegisterEvents;
  end;
end;

function TZIbSqlNotify.GetAutoRegister: Boolean;
begin
 Result := FAutoRegister;
end;

{ TZIbSqlNotifyTread }

procedure EventCallback(P: Pointer; Length: SmallInt; Updated: PChar); cdecl;
begin
  if (Assigned(P) and Assigned(Updated)) then
  begin
    TZIbSqlNotifyTread(P).UpdateResultBuffer(Length, Updated);
    TZIbSqlNotifyTread(P).SignalEvent;
  end;
end;

procedure TZIbSqlNotifyTread.DoEvent;
begin
  Parent.FOnEventAlert(Parent, Parent.FEvents[((EventGroup * IB_MAX_EVENT_BLOCK) + WhichEvent)],
    Parent.FStatusVector[WhichEvent], FCancelAlerts)
end;

procedure TZIbSqlNotifyTread.UpdateResultBuffer(Length: UShort; Updated: PChar);
begin
  Move(Updated[0], ResultBuffer[0], Length);
end;

procedure TZIbSqlNotifyTread.QueueEvents;
begin
  EventsReceived := False;
  Signal.ResetEvent;
  Synchronize(SQueEvents);
end;

procedure TZIbSqlNotifyTread.ProcessEvents;
var
  i: Integer;
begin
  isc_event_counts(@Parent.FStatusVector, EventBufferLen, EventBuffer, ResultBuffer);
  if (Assigned(Parent.FOnEventAlert) and (not FirstTime)) then
  begin
    FCancelAlerts := false;
    for i := 0 to (EventCount - 1) do
    begin
      if (Parent.FStatusVector[i] <> 0) then
      begin
        WhichEvent := i;
        Synchronize(DoEvent)
      end;
    end;
  end;
  FirstTime := False;
end;


procedure TZIbSqlNotifyTread.UnRegisterEvents;
begin
  isc_cancel_events(@Parent.FStatusVector, @TDirIbSqlConnect(Parent.Database.Handle).Handle,
    @EventID);

  if (Parent.FStatusVector[0] = 1) and (Parent.FStatusVector[1] > 0) then
    raise EDatabaseError.Create(Parent.GetErrorMsg);

  isc_free(EventBuffer);
  EventBuffer := nil;
  isc_free(ResultBuffer);
  ResultBuffer := nil;
end;


procedure TZIbSqlNotifyTread.RegisterEvents;
  function EBP(Index: Integer): PChar;
  begin
    Inc(Index, (EventGroup * IB_MAX_EVENT_BLOCK));
    if (Index > Parent.FEvents.Count) then
      Result := nil
    else
     Result := PChar(Parent.FEvents[Index - 1]);
  end;
begin
  EventBuffer := nil;
  ResultBuffer := nil;
  EventBufferLen := 0;
  FirstTime := True;
  EventCount := (Parent.FEvents.Count - (EventGroup * IB_MAX_EVENT_BLOCK));
  if EventCount > IB_MAX_EVENT_BLOCK then
    EventCount := IB_MAX_EVENT_BLOCK;
  EventBufferLen := Tsib_event_block(isc_event_block)(@EventBuffer,
    @ResultBuffer, EventCount, EBP(1), EBP(2), EBP(3), EBP(4), EBP(5), EBP(6),
    EBP(7), EBP(8), EBP(9), EBP(10), EBP(11), EBP(12), EBP(13), EBP(14), EBP(15));
end;

procedure TZIbSqlNotifyTread.SignalEvent;
begin
  EventsReceived := True;
  Signal.SetEvent;
end;

procedure TZIbSqlNotifyTread.SignalTerminate;
begin
  if not Terminated then
  begin
    Terminate;
    Signal.SetEvent;
  end;
end;

procedure TZIbSqlNotifyTread.DoHandleException;
begin
  SysUtils.ShowException(FExceptObject, FExceptAddr);
end;

function TZIbSqlNotifyTread.HandleException: Boolean;
begin
  if not Parent.ThreadException then
  begin
    Result := True;
    Parent.ThreadException := True;
    FExceptObject := ExceptObject;
    FExceptAddr := ExceptAddr;
    try
      if not (FExceptObject is EAbort) then
        Synchronize(DoHandleException);
    finally
      FExceptObject := nil;
      FExceptAddr := nil;
    end;
  end else
    Result := False;
end;

procedure TZIbSqlNotifyTread.Execute;
begin
  try
    repeat
      Signal.WaitFor(0); //!! Doesn't compile under Linux INFINITE);
      if EventsReceived then
      begin
        ProcessEvents;
        QueueEvents;
      end;
    until Terminated;
    ReturnValue := 0;
  except
    if HandleException then
      ReturnValue := 1
    else ReturnValue := 0;
  end;
end;

constructor TZIbSqlNotifyTread.Create(Owner: TZIbSqlNotify; EventGrp: Integer; TermEvent: TNotifyEvent);
begin
  inherited Create(True);
  FCancelAlerts := false;
  Signal := TSimpleEvent.Create;
  Parent := Owner;
  EventGroup := EventGrp;
  OnTerminate := TermEvent;
  FreeOnTerminate := True;
  RegisterEvents;
  QueueEvents;
  Signal.WaitFor(0); //!! Doesn't compile under Linux INFINITE);
  ProcessEvents;
  QueueEvents;
  Resume;
end;

destructor TZIbSqlNotifyTread.Destroy;
begin
  try
    UnRegisterEvents;
  except
    if HandleException then
      ReturnValue := 1
    else
      ReturnValue := 0;
  end;
  Signal.Free;
  inherited Destroy;
end;

procedure TZIbSqlNotifyTread.SQueEvents;
begin
  try
    isc_que_events(@Parent.FStatusVector, @TDirIbSqlConnect(Parent.Database.Handle).Handle,
      @EventID, EventBufferLen, EventBuffer, TISC_CALLBACK(@EventCallback),
      PVoid(Self));
    if (Parent.FStatusVector[0] = 1) and (Parent.FStatusVector[1] > 0) then
      raise EDatabaseError.Create(Parent.GetErrorMsg);
  except
    on E: Exception do
      if Assigned(Parent.OnError) then
        if E is EDataBaseError then
          Parent.OnError(Parent, Parent.FStatusVector[1]) //faraj
        else
          Parent.OnError(Parent, 0);
  end;
end;

end.

