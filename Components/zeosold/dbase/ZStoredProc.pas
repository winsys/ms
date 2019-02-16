unit ZStoredProc;

interface

uses Classes, SysUtils, ZConnect, ZTransact, ZQuery, ZDirSql, ZSqlTypes, DB,
  ZSqlItems, ZSqlParser, ZSqlBuffer {$IFDEF VER100}, DBTables{$ENDIF};

{$INCLUDE ../Zeos.inc}

type

  TZParamBindMode = (zpbByName, zpbByNumber);

  { Abstract storedprocedure with descendant of TZDataSet }
  TZStoredProc = class(TZDataSet)
  private
    FStoredProc: TDirStoredProc;
    FPrepared: Boolean;
    FStoredProcName: String;
    FParamBindMode: TZParamBindMode;

    FDatabase: TZDatabase;
    FTransact: TZTransact;
//    FDefaultFields: Boolean;

    procedure QueryRecords(Force: Boolean);
//    procedure ParamsRequery;
    function GetPrepared: Boolean;
    procedure SetPrepared(const Value: Boolean);
    procedure SetStoredProcName(const Value: string);
//    procedure ShortRefresh;
  protected
    procedure SetDatabase(Value: TZDatabase);
    procedure SetTransact(Value: TZTransact);

    procedure AutoFillObjects;
    procedure CreateConnections; override;

    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalLast; override;
    procedure InternalRefresh; override;
    procedure InternalSort(Fields: string; SortType: TSortType);

    procedure SetRecNo(Value: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoProgress(Stage: TZProgressStage; Proc: TZProgressProc;
      Position: Integer);

    procedure QueryParams; virtual;
    procedure GetAllRecords; virtual;
    procedure GetAllParams(const spName: String); virtual; abstract;
    function GetRecordCount: Integer; override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean):
      TGetResult; override;

    function FindRecord(Restart, GoForward: Boolean): Boolean; override;

    function IsCursorOpen: Boolean; override;

    property DatabaseObj: TZDatabase read FDatabase write FDatabase;
    property TransactObj: TZTransact read FTransact write FTransact;
    property StoredProc: TDirStoredProc read FStoredProc write FStoredProc;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Prepare; virtual;
    procedure UnPrepare; virtual;
    procedure ExecProc; virtual;

    property Prepared: Boolean read GetPrepared write SetPrepared;
    property StoredProcName: string read FStoredProcName write SetStoredProcName;
    property ParamBindMode: TZParamBindMode read FParamBindMode write FParamBindMode;
  end;

implementation

uses ZDBaseConst, ZExtra {$IFNDEF NO_GUI}, Forms, Controls{$ENDIF};

{ TZStoredProc }

{ Class Constructor }
procedure TZStoredProc.AutoFillObjects;
begin
  if Assigned(TransactObj) and not Assigned(TransactObj.Database) then
    TransactObj.Database := Database;
  if not Assigned(DatabaseObj) and Assigned(TransactObj) then
    DatabaseObj := TransactObj.Database;
  if Assigned(DatabaseObj) and not Assigned(TransactObj) then
    TransactObj := TZTransact(DatabaseObj.DefaultTransaction);
end;

constructor TZStoredProc.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParamBindMode := zpbByName;
end;

{ Class Destructor }
procedure TZStoredProc.CreateConnections;
begin
  { Check database and transact components }
  if not Assigned(DatabaseObj) then
    DatabaseError(SConnectNotDefined);
  if not Assigned(TransactObj) then
    DatabaseError(STransactNotDefined);
  { Check connect }
  TransactObj.Connect;
  if not TransactObj.Connected then
    DatabaseError(SConnectTransactError);
end;

destructor TZStoredProc.Destroy;
begin
  if Active then
    Close;
  if Assigned(FDatabase) then
    FDatabase.RemoveDataset(Self);
  inherited Destroy;
  FStoredProc.Free;
end;

{ Public method for executing the storedprocedure }
procedure TZStoredProc.DoProgress(Stage: TZProgressStage;
  Proc: TZProgressProc; Position: Integer);
var
  Cancel: Boolean;
begin
  if Assigned(OnProgress) then
  begin
    Cancel := False;
    OnProgress(Self, Stage, Proc, Position,
      Max(SqlBuffer.Count, StoredProc.RecordCount), Cancel);
  end;
end;

procedure TZStoredProc.ExecProc;
var
  WasPrepared: Boolean;
  HasResultSet: Boolean;
{$IFNDEF NO_GUI}
  OldCursor: TCursor;
{$ENDIF}
begin
{$IFNDEF NO_GUI}
  OldCursor := Screen.Cursor;
{$ENDIF}
  if (not Assigned(DatabaseObj) or not Assigned(TransactObj)) then
    Exit;

  AutoFillObjects;
  CreateConnections;

  if Active then
    Close;

  { prepare the storedprocedure }
  WasPrepared := FPrepared;
  Prepared := true;

  // Execute the storedprocedure
//  FDefaultFields := FieldCount = 0;
  FStoredProc.ExecProc;

  // if storedprocedure returned a dataset then fetch the records
  // before return parameters are queried
  CurRec := -1;

  HasResultSet := FStoredProc.FieldCount>0;
  if HasResultSet then
  begin
    GetAllRecords;
//    UpdateBufferCount;  //!! Doesn't compile under D4
    SetState(dsBrowse);
  end;

  // process output parameters
  QueryParams;
  // if no returned dataset the set prepared to value before execution
  if HasResultSet then
    Prepared := WasPrepared;

{$IFNDEF NO_GUI}
  Screen.Cursor := OldCursor;
{$ENDIF}
end;

{ Get all records }
function TZStoredProc.FindRecord(Restart, GoForward: Boolean): Boolean;
var
  Index: Integer;
  SaveFiltered: Boolean;
begin
  { Check state }
  CheckBrowseMode;
  DoBeforeScroll;
  Result := False;
  { Set position }
  if Restart then
  begin
    if GoForward then
      Index := 0
    else
    begin
      QueryRecords(True);
      Index := SqlBuffer.Count-1;
    end
  end
  else
  begin
    Index := CurRec;
    if GoForward then
      Inc(Index)
    else
      Dec(Index);
  end;
  { Find a record }
  SaveFiltered := FilterMark;
  try
    FilterMark := True;
    while (Index >= 0) and (Index < SqlBuffer.Count) do
    begin
      if CheckRecordByFilter(Index) then
      begin
        Result := True;
        Break;
      end;
      if not GoForward then
        Dec(Index)
      else begin
        Inc(Index);
        if (Index >= SqlBuffer.Count) and not StoredProc.EOF then
          QueryOneRecord;
      end;
    end
  finally
    FilterMark := SaveFiltered;
  end;

  SetFound(Result);
  if Result then
  begin
    RecNo := Index + 1;
    DoAfterScroll;
  end;
end;

procedure TZStoredProc.GetAllRecords;
begin
//  SetDefaultFields(FieldCount = 0); //!! Doesn't compile under D4
  InternalInitFieldDefs;
  if DefaultFields then
    CreateFields;
  BindFields(true);
  SqlBuffer.BindFields(SqlParser.SqlFields);
  CacheBuffer.SetCache(SqlBuffer);
  QueryRecords(true);
end;

function TZStoredProc.GetPrepared: Boolean;
begin
  Result := FPrepared;
end;

function TZStoredProc.GetRecord(Buffer: PChar; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  TempRec: LongInt;
  CanFetch: Boolean;
begin
  CanFetch := True;
  Result := grOK;
  case GetMode of
    gmNext:
      begin
        TempRec := CurRec;
        while Result <> grEOF do
        begin
          if TempRec < SqlBuffer.Count - 1 then
            Inc(TempRec)
          else
            if FStoredProc.EOF or (not CanFetch) then
              Result := grEOF
            else begin
              QueryOneRecord;
              if FStoredProc.EOF then
                Result := grEOF
              else if SqlBuffer.Count = 0 then
                Continue
              else
                Inc(TempRec);
            end;
          if Result = grEOF then
            Break;
          if CheckRecordByFilter(TempRec) then
            Break;
        end;
        if Result = grOk then
          CurRec := TempRec;
      end;
    gmPrior:
      begin
        TempRec := CurRec;
        while Result <> grBOF do
        begin
          if TempRec <= 0 then
            Result := grBOF
          else
            Dec(TempRec);
          if Result = grBOF then
            Break;
          if CheckRecordByFilter(TempRec) then
            Break;
        end;
        if Result = grOk then
          CurRec := TempRec;
      end;
    gmCurrent:
      begin
        TempRec := CurRec;
        while Result <> grError do
        begin
          if (TempRec < 0) or (TempRec >= SqlBuffer.Count) then
          begin
            if FStoredProc.EOF or (not CanFetch) then
              Result := grError
            else begin
              QueryOneRecord;
              if FStoredProc.EOF then
                Result := grError;
            end;
          end;
          if Result = grError then
            Break;
          if CheckRecordByFilter(TempRec) then
            Break;
          Inc(TempRec);
        end;
        if Result = grOk then
          CurRec := TempRec;
      end;
  end;

  if Result = grOK then
  begin
    SqlBuffer.CopyRecord(SqlBuffer[CurRec], PRecordData(Buffer), True);
    with PRecordData(Buffer)^ do
      BookmarkFlag := bfCurrent;
    GetCalcFields(Buffer);
  end
  else if (Result = grError) and DoCheck then
    DatabaseError(SNoMoreRec);
end;

function TZStoredProc.GetRecordCount: Integer;
var
  I: LongInt;
begin
  if Filtered then
  begin
    QueryRecords(True);
    Result := 0;
    for I := 0 to SqlBuffer.Count-1 do
      if CheckRecordByFilter(I) then
        Inc(Result);
  end
  else
  begin
    if not StoredProc.EOF then
      Result := StoredProc.RecordCount
    else
      Result := SqlBuffer.Count;
  end;
end;

procedure TZStoredProc.InternalClose;
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
    if doHourGlass in Options then
      Screen.Cursor := crSqlWait;
{$ENDIF}

    FStoredProc.Close;
    SqlBuffer.ClearBuffer(true);
    CacheBuffer.ClearBuffer(true);
    SqlParser.Clear;
    if DefaultFields then            //??????
      DestroyFields;
  finally
{$IFNDEF NO_GUI}
    if doHourGlass in Options then
      Screen.Cursor := OldCursor;
{$ENDIF}
  end;
end;

procedure TZStoredProc.InternalInitFieldDefs;
var
  SaveActive: Boolean;
  I: Integer;
  FieldName: string;
  FieldRequired: Boolean;
  FieldSize: Integer;
  FieldType: TFieldType;
  FieldNo: Integer;
  FieldDesc: PFieldDesc;
begin
  { Set start values }
  FieldDefs.Clear;
  FieldNo := 1;
  { Open connections for separate func call }
  SaveActive := FStoredProc.Active;
  if not FStoredProc.Active then
    ExecProc;

  { Create TField for every query field }
  for I := 0 to FStoredProc.FieldCount - 1 do
  begin
    FieldRequired := False;
    FieldDesc := SqlParser.SqlFields.FindByAlias(FStoredProc.FieldAlias(I));
    if Assigned(FieldDesc) then
    begin
      { Process table fields }
      FieldName := FieldDesc.Alias;
      FieldType := FieldDesc.FieldType;
      FieldSize := FieldDesc.Length;
      FieldRequired := not FieldDesc.IsNull and (FieldDesc.AutoType = atNone);
    end
    else
    begin
      { Process calc and unknown fields }
      FieldName := FStoredProc.FieldAlias(I);
      FieldSize := Max(FStoredProc.FieldSize(I), FStoredProc.FieldMaxSize(I));
      FieldType := FStoredProc.FieldDataType(I);
    end;
    { Correct field size }
    UpdateFieldDef(FieldDesc, FieldType, FieldSize);
    { Add new field def }
    TFieldDef.Create(FieldDefs, FieldName, FieldType, FieldSize,
      FieldRequired, FieldNo);
    Inc(FieldNo);
  end;
  { Restore dataset state }
  if not SaveActive then
    FStoredProc.Close;
end;

procedure TZStoredProc.InternalLast;
begin
  QueryRecords(True);
  CurRec := SqlBuffer.Count;
end;

procedure TZStoredProc.InternalOpen;
begin
  ExecProc;
end;

procedure TZStoredProc.InternalRefresh;
var
//  Error: string;
  KeyFields: string;
  KeyValues: Variant;
//  RecordCount: Integer;
{$IFNDEF NO_GUI}
  OldCursor: TCursor;
{$ENDIF}
begin
{$IFNDEF NO_GUI}
  OldCursor := Screen.Cursor;
{$ENDIF}
  try
{$IFNDEF NO_GUI}
    if doHourGlass in Options then
       Screen.Cursor := crSqlWait;
{$ENDIF}
    { Store record params }
//    RecordCount := Self.RecordCount;
    FormKeyValues(KeyFields, KeyValues);
    { Clear all collections }
    StoredProc.Close;
    SqlBuffer.ClearBuffer(False);
    CacheBuffer.ClearBuffer(False);

    { Exec the storedProc }
    ExecProc;

    { Set mail query params }
//    FRowsAffected := 0;
//    CurRec := -1;

    { Sort with old method }
    SqlBuffer.SortRestore;
    { Locate to old position }
    if KeyFields <> '' then
      Locate(KeyFields, KeyValues, []);
    { Resync records }
    if not (State in [dsInactive]) then Resync([]);
  finally
{$IFNDEF NO_GUI}
    if doHourGlass in Options then
      Screen.Cursor := OldCursor;
{$ENDIF}
  end;
end;

procedure TZStoredProc.InternalSort(Fields: string; SortType: TSortType);
var
  Index: Integer;
{$IFNDEF NO_GUI}
  OldCursor: TCursor;
{$ENDIF}
begin
  { Get all records and check buffer }
  QueryRecords(True);
  if SqlBuffer.Count = 0 then Exit;
{$IFNDEF NO_GUI}
  OldCursor := Screen.Cursor;
{$ENDIF}
  try
{$IFNDEF NO_GUI}
    if doHourGlass in Options then
      Screen.Cursor := crSqlWait;
{$ENDIF}
    { Save current position }
    if CurRec >= 0 then
      Index := SqlBuffer[CurRec].Index
    else
     Index := -1;
    { Sorting fields }
    if Fields <> '' then
      SqlBuffer.SetSort(Fields, SortType)
    else begin
      if SortType = stAscending then
        SqlBuffer.ClearSort
      else
        SqlBuffer.SortInverse;
    end;
    { Restore position }
    if Index >= 0 then
      CurRec := SqlBuffer.IndexOfIndex(Index);
    { Resync recordset }
    if not (State in [dsInactive]) then
      Resync([]);
  finally
{$IFNDEF NO_GUI}
    if doHourGlass in Options then
      Screen.Cursor := OldCursor;
{$ENDIF}
  end;
end;

function TZStoredProc.IsCursorOpen: Boolean;
begin
  Result := StoredProc.Active or Active or (SqlBuffer.Count > 0);
end;

procedure TZStoredProc.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
end;

{procedure TZStoredProc.ParamsRequery;
var
  I, N: Integer;
  MasterField: TField;
begin
  if FMasterLink.Active or not FDataLink.Active then Exit;
  if not MasterStateCheck(FDataLink.Dataset) then Exit;

{ Changing parameter values
  N := 0;
  for I := 0 to ParamCount-1 do
  begin
    MasterField := FDataLink.Dataset.FieldByName(Params[I].Name);
    if Assigned(MasterField) then
    begin
      Params[I].Value := MasterField.AsVariant;
      Inc(N);
    end;
  end;

  if (N = 0) then Exit;
end;
}
procedure TZStoredProc.Prepare;
begin
  Prepared := True;
end;

procedure TZStoredProc.QueryParams;
var
  I: Integer;
begin
  if FParamBindMode = zpbByName then
  begin
    for I := 0 to FStoredProc.ParamCount-1 do
      Params.ParamByName(FStoredProc.ParamName(I)).AsString := FStoredProc.Param(I);
  end
  else
  begin
    for I := 0 to FStoredProc.ParamCount-1 do
      Params[I].AsString := FStoredProc.Param(I);
  end;
  I := 0;
  while (I < Params.Count) and (Params[I].ParamType <> ptResult) do
    Inc(I);
  if I < Params.Count then
      Params[I].AsString := FStoredProc.GetReturnValue;
end;

procedure TZStoredProc.QueryRecords(Force: Boolean);
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
    if doHourGlass in Options then
      Screen.Cursor := crSqlWait;
{$ENDIF}
    { Check current fetch state }
    if not ((not FStoredProc.EOF) and ((doQueryAllRecords in Options)
      or (FStoredProc.RecNo < MIN_FETCH_ROWS) or Force
      or (DatabaseType = dtMsSql))) then Exit;
    { Invoke on progress event }
    DoProgress(psStarting, ppFetching, SqlBuffer.Count);
    { Query records }
    while (not FStoredProc.EOF) and ((doQueryAllRecords in Options)
      or (FStoredProc.RecNo < MIN_FETCH_ROWS) or Force) do
      QueryRecord;
    { Invoke on progress event }
    DoProgress(psEnding, ppFetching, SqlBuffer.Count);
  finally
{$IFNDEF NO_GUI}
    if doHourGlass in Options then
      Screen.Cursor := OldCursor;
{$ENDIF}
  end;
end;

procedure TZStoredProc.SetDatabase(Value: TZDatabase);
begin
  if Active then Close;
  try
    if Assigned(FDatabase) then
      FDatabase.RemoveDataset(Self);
    if Assigned(Value) then
    begin
      FStoredProc.Connect := Value.Handle;
      Value.AddDataset(Self);
      if not Assigned(FTransact) then
        SetTransact(TZTransact(Value.DefaultTransaction));
    end else
      FStoredProc.Connect := nil;
  finally
    FDatabase := Value;
  end;
end;

procedure TZStoredProc.SetPrepared(const Value: Boolean);
begin
  if Value <> FPrepared then
  begin
    if Value then
      FStoredProc.Prepare(Params)
    else FStoredProc.Unprepare;
    FPrepared := Value;
  end;
end;

procedure TZStoredProc.SetRecNo(Value: Integer);
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
    if doHourGlass in Options then
      Screen.Cursor := crSqlWait;
{$ENDIF}

    Value := Max(1, Value);

    { Invoke on progress event }
    DoProgress(psStarting, ppFetching, SqlBuffer.Count);
    { Fetch one record from server }
    while not StoredProc.EOF and (Value > SqlBuffer.Count) do
      QueryRecord;
    { Invoke on progress event }
    DoProgress(psEnding, ppFetching, SqlBuffer.Count);

    if Value <= SqlBuffer.Count then
      CurRec := Value - 1
    else
      CurRec := SqlBuffer.Count - 1;
    if not (State in [dsInactive]) then Resync([]);
  finally
{$IFNDEF NO_GUI}
    if doHourGlass in Options then
      Screen.Cursor := OldCursor;
{$ENDIF}
  end;
end;

procedure TZStoredProc.SetStoredProcName(const Value: string);
begin
  if FStoredProcName <> Value then
  begin
    if Active then
      Close;
    FStoredProcName := Value;
    if csDesigning	in ComponentState then
      GetAllParams(FStoredProcName);
    FStoredProc.StoredProcName := Value;
  end;
end;

procedure TZStoredProc.SetTransact(Value: TZTransact);
begin
  if Active then Close;
  FTransact := Value;
  if Assigned(FTransact) then
    FStoredProc.Transact := Value.Handle
  else
    FStoredProc.Transact := nil;
end;

{procedure TZStoredProc.ShortRefresh;
var
  OldCursor: TCursor;
begin
  { Change cursor
  OldCursor := Screen.Cursor;
  try
    if doHourGlass in FOptions then
      Screen.Cursor := crSqlWait;
    { Clear all collections
    StoredProc.Close;
    SqlBuffer.ClearBuffer(False);
    CacheBuffer.ClearBuffer(False);
    { Open the query
    StoredProc.ExecProc;
  finally
    { Recover cursor
    if doHourGlass in FOptions then
      Screen.Cursor := OldCursor;
  end;
end;
}
procedure TZStoredProc.UnPrepare;
begin
  Prepared := False;
end;

end.
