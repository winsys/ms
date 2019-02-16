unit ZIb_DB;

{$R *.dcr}

interface
uses
  Classes, SysUtils, DB, ZDirIbSql, ZIbSqlCon, ZIbSqlTr, ZIbSqlQuery;

type

  { Interbase database component BDE Like}
  TZIbDatabase = class(TZIbSqlDatabase)
  private
    function GetInTransaction: boolean;
    function GetTransIsolation: TZIbSqlTransIsolation;
    procedure SetTransIsolation(Value: TZIbSqlTransIsolation);
    function GetDefaultTransact: TComponent;
  protected
    FInternalTransact: TZIbSqlTransact;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Open;
    Procedure Close;
    procedure CheckInTransaction;
    procedure Commit;
    procedure Rollback;
    procedure StartTransaction;
    procedure ApplyUpdates(const DataSets: array of TZCustomIbSqlDataset);
    property InTransaction: Boolean read GetInTransaction;
    property DefaultTransaction read GetDefaultTransact;
  published
    property TransIsolation: TZIbSqlTransIsolation read GetTransIsolation write SetTransIsolation;
  end;

  {TZIbQuery}
  TZIbQuery = class (TZIbSqlQuery)
  private
    function GetDataBase: TZIbDatabase;
    procedure SetDataBase(Value: TZIbDatabase);
    procedure SetTransaction(Value: TZIbSqlTransact);
  published
    property Transaction write SetTransaction;
    property DataBase: TZIbDatabase read GetDataBase write SetDataBase;
  end;

  {TZIbTable}
  TZIbTable = class (TZIbSqlTable)
  private
    function GetDataBase: TZIbDatabase;
    procedure SetDataBase(Value: TZIbDatabase);
    procedure SetTransaction(Value: TZIbSqlTransact);
  published
    property Transaction write SetTransaction;
    property DataBase: TZIbDatabase read GetDataBase write SetDataBase;
  end;

  {TZIbStoredProc}
  TZIbStoredProc = class (TZIbSqlStoredProc)
  private
    function GetDataBase: TZIbDatabase;
    procedure SetDataBase(Value: TZIbDatabase);
    procedure SetTransaction(Value: TZIbSqlTransact);
  published
    property Transaction write SetTransaction;
    property DataBase: TZIbDatabase read GetDataBase write SetDataBase;
  end;



resourcestring
  SInvalidDataset = 'Cannot update, %s is not owned by %s';

implementation


{TZIbDatabase compatible BDE}
constructor TZIbDatabase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInternalTransact := TZIbSqlTransact.Create(nil);
  with FInternalTransact do
  begin
    Name := 'ZIBInternalTransact';
    Database := Self;
    AutoCommit := True;
    TransIsolation := itReadCommittedRec;
  end;
end;

destructor TZIbDatabase.Destroy;
begin
  FInternalTransact.Free;
  inherited;
end;

procedure TZIbDatabase.Open;
begin
  Connect;
end;

procedure TZIbDatabase.Close;
begin
  Disconnect;
end;

function TZIbDatabase.GetInTransaction: boolean;
begin
  Result := FInternalTransact.AutoCommit;
end;

function TZIbDatabase.GetTransIsolation: TZIbSqlTransIsolation;
begin
  Result := FInternalTransact.TransIsolation;
end;

procedure TZIbDatabase.SetTransIsolation(Value: TZIbSqlTransIsolation);
begin
  FInternalTransact.TransIsolation := Value;
end;

procedure TZIbDatabase.CheckInTransaction;
begin
  with FInternalTransact do
   if AutoCommit then
     DataBaseError('DataBase not in Transction');
end;


procedure TZIbDatabase.Commit;
begin
  CheckInTransaction;
  with FInternalTransact do
  begin
    Commit;
    AutoCommit := True;
  end;
end;

procedure TZIbDatabase.Rollback;
begin
  CheckInTransaction;
  with FInternalTransact do
  begin
    Rollback;
    AutoCommit := True;
  end;
end;

procedure TZIbDatabase.StartTransaction;
begin
  with FInternalTransact do
    AutoCommit := False;
end;


procedure TZIbDatabase.ApplyUpdates(const DataSets: array of TZCustomIbSqlDataset);
var
  I: Integer;
  DS: TZCustomIbSqlDataset;
begin
  StartTransaction;
  try
    for I := 0 to High(DataSets) do
    begin
      DS := DataSets[I];
      if DS.Database <> Self then
        DatabaseError(Format(SInvalidDataset, [DS.Name, Name]));
      DS.ApplyUpdates;
    end;
    Commit;
  except
    Rollback;
    raise;
  end;
  for I := 0 to High(DataSets) do
     DataSets[I].CommitUpdates;
end;


function TZIbDatabase.GetDefaultTransact: TComponent;
begin
  Result := FInternalTransact;
end;


{ TZIbQuery }
procedure TZIbQuery.SetTransaction(Value: TZIbSqlTransact);
begin
  if Assigned(DataBase) then
    inherited Transaction := DataBase.FInternalTransact;
end;

function TZIbQuery.GetDataBase: TZIbDatabase;
begin
  Result := inherited DataBase as TZIbDatabase;
end;

procedure TZIbQuery.SetDataBase(Value: TZIbDatabase);
begin
  inherited DataBase := Value;
end;


{TZIbTable}
procedure TZIbTable.SetTransaction(Value: TZIbSqlTransact);
begin
  if Assigned(DataBase) then
    inherited Transaction := DataBase.FInternalTransact;
end;

function TZIbTable.GetDataBase: TZIbDatabase;
begin
  Result := inherited DataBase as TZIbDatabase;
end;

procedure TZIbTable.SetDataBase(Value: TZIbDatabase);
begin
  inherited DataBase := Value;
end;

{TZIbStoredProc}
procedure TZIbStoredProc.SetTransaction(Value: TZIbSqlTransact);
begin
  if Assigned(DataBase) then
    inherited Transaction := DataBase.FInternalTransact;
end;

function TZIbStoredProc.GetDataBase: TZIbDatabase;
begin
  Result := inherited DataBase as TZIbDatabase;
end;

procedure TZIbStoredProc.SetDataBase(Value: TZIbDatabase);
begin
  inherited DataBase := Value;
end;


end.
