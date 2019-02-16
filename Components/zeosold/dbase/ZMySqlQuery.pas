{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{           MySql Query and Table components             }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZMySqlQuery;

interface

{$R *.dcr}

uses
  {$IFNDEF LINUX}Windows,{$ENDIF} SysUtils, DB, Classes, ZDirMySql, DBCommon,
  ZDirSql, ZMySqlCon, ZMySqlTr, ZToken, ZLibMySql, ZSqlExtra, ZQuery,
  ZSqlTypes, ZSqlItems, ZSqlParser, ZSqlBuffer;

{$IFNDEF LINUX}
{$INCLUDE ..\Zeos.inc}
{$ELSE}
{$INCLUDE ../Zeos.inc}
{$ENDIF}

type

  TZMySqlOption = (moStoreResult, moUseGen);
  TZMySqlOptions = set of TZMySqlOption;

  { Direct MySql query with descendant of TZDataSet }
  TZCustomMySqlDataset = class(TZDataSet)
  private
    FExtraOptions: TZMySqlOptions;
    FUseConnect: TDirMySqlConnect;
    FUseTransact: TDirMySqlTransact;

    { Internal methods }
    procedure SetDatabase(Value: TZMySqlDatabase);
    procedure SetTransact(Value: TZMySqlTransact);
    function GetDatabase: TZMySqlDatabase;
    function GetTransact: TZMySqlTransact;
    function GetAutoIncField(Table: string): Integer;
    {$IFDEF USE_GENERATORS}
    procedure ApplyGens(Buffer: PRecordData);
    {$ENDIF}
  protected
    { Overriding ZDataset methods }
    procedure QueryRecord; override;
    procedure ChangeAddBuffer(AddRecord: PRecordData); override;
    procedure CreateConnections; override;
    procedure UpdateAfterPost(OldData, NewData: PRecordData); override;
    { Overrided standart methods }
    procedure InternalClose; override;
    {$IFDEF WITH_IPROVIDER}
    { IProvider support }
    function PSInTransaction: Boolean; override;
    function PSExecuteStatement(const ASql: string; AParams: TParams;
      ResultSet: Pointer): Integer; override;
    procedure PSSetCommandText(const CommandText: string); override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); Override;
    destructor Destroy; override;

    procedure AddTableFields(Table: string; SqlFields: TSqlFields); override;
    procedure AddTableIndices(Table: string; SqlFields: TSqlFields;
      SqlIndices: TSqlIndices); override;
  published
    property Database: TZMySqlDatabase read GetDatabase write SetDatabase;
    property Transaction: TZMySqlTransact read GetTransact write SetTransact;
    property ExtraOptions: TZMySqlOptions read FExtraOptions write FExtraOptions;
  end;

  { Direct MySql query with descendant of TDataSet }
  TZMySqlQuery = class(TZCustomMySqlDataset)
  public
    property MacroCount;
    property ParamCount;
  published
    property MacroChar;
    property Macros;
    property MacroCheck;
    property Params;
    property ParamCheck;
    property DataSource;

    property Sql;
    property RequestLive;
    property Active;
  end;

  { Direct MySql query with descendant of TDataSet }
  TZMySqlTable = class(TZCustomMySqlDataset)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property TableName;
    property ReadOnly default False;
    property DefaultIndex default True;
    property Active;
  end;

implementation

uses ZExtra, ZDBaseConst, ZBlobStream, Math;

{***************** TZCustomMySqlDataset implemantation *******************}

{ Class constructor }
constructor TZCustomMySqlDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DatabaseType := dtMySql;
  Query := TDirMySqlQuery.Create(nil, nil);
  FExtraOptions := [moStoreResult];
  FUseConnect := TDirMySqlConnect.Create;
  FUseTransact := TDirMySqlTransact.Create(FUseConnect);
end;

{ Class destructor }
destructor TZCustomMySqlDataset.Destroy;
begin
  inherited Destroy;
  FUseTransact.Free;
  FUseConnect.Free;
end;

{ Set connect to database component }
procedure TZCustomMySqlDataset.SetDatabase(Value: TZMySqlDatabase);
begin
  inherited SetDatabase(Value);
end;

{ Set connect to transact-server component }
procedure TZCustomMySqlDataset.SetTransact(Value: TZMySqlTransact);
begin
  inherited SetTransact(Value);
end;

{ Get connect to database component }
function TZCustomMySqlDataset.GetDatabase: TZMySqlDatabase;
begin
  Result := TZMySqlDatabase(DatabaseObj);
end;

{ Get connect to transact-server component }
function TZCustomMySqlDataset.GetTransact: TZMySqlTransact;
begin
  Result := TZMySqlTransact(TransactObj);
end;

{ Read query from server to internal buffer }
procedure TZCustomMySqlDataset.QueryRecord;
var
  I, Count: Integer;
  RecordData: PRecordData;
  FieldDesc: PFieldDesc;
  BlobPtr: PRecordBlob;
  Cancel: Boolean;
  Temp: string;
begin
  { Start fetching fields }
  Count := SqlBuffer.Count;
  while not Query.EOF and (Count = SqlBuffer.Count) do
  begin
    { Go to the record }
    if SqlBuffer.FillCount > 0 then
      Query.Next;
    { Invoke OnProgress event }
    if Assigned(OnProgress) then
    begin
      Cancel := False;
      OnProgress(Self, psRunning, ppFetching, Query.RecNo+1,
        MaxIntValue([Query.RecNo+1, Query.RecordCount]), Cancel);
      if Cancel then Query.Close;
    end;
    if Query.EOF then Break;
    { Getting record }
    RecordData := SqlBuffer.Add;
    for I := 0 to SqlBuffer.SqlFields.Count - 1 do
    begin
      { Define field structure }
      FieldDesc := SqlBuffer.SqlFields[I];
      if FieldDesc.FieldNo < 0 then Continue;
      if Query.FieldIsNull(FieldDesc.FieldNo) and
        not (FieldDesc.FieldType in [ftBlob, ftMemo]) then
        Continue;
      { Converting field values }
      case FieldDesc.FieldType of
        ftDateTime:
          begin
            { Process datetime and timestamp field }
            if Query.FieldType(FieldDesc.FieldNo) = FIELD_TYPE_TIMESTAMP then
              SqlBuffer.SetField(FieldDesc, MyTimestampToSqlDate(
                Query.Field(FieldDesc.FieldNo)), RecordData)
            else
              SqlBuffer.SetField(FieldDesc, Query.Field(FieldDesc.FieldNo),
                RecordData);
          end;
        ftBlob, ftMemo:
          begin
            { Initialize blob and memo fields }
            BlobPtr := PRecordBlob(@RecordData.Bytes[FieldDesc.Offset+1]);
            BlobPtr.BlobType := btInternal;
            { Fill not null fields }
            if not Query.FieldIsNull(FieldDesc.FieldNo) then
            begin
              RecordData.Bytes[FieldDesc.Offset] := 0;
              BlobPtr.Size := Query.FieldSize(FieldDesc.FieldNo);
              BlobPtr.Data := AllocMem(BlobPtr.Size);
              if FieldDesc.FieldType = ftBlob then
                System.Move(Query.FieldBuffer(FieldDesc.FieldNo)^, BlobPtr.Data^,
                  BlobPtr.Size)
              else
                System.Move(PChar(ConvertFromSqlEnc(Query.Field(FieldDesc.FieldNo)))^,
                  BlobPtr.Data^, BlobPtr.Size)
            end
            { Fill null fields }
            else
            begin
              BlobPtr.Size := 0;
              BlobPtr.Data := nil;
            end;
          end;
        ftString:
          begin
            Temp := ConvertFromSqlEnc(Query.Field(FieldDesc.FieldNo));
            SqlBuffer.SetFieldDataLen(FieldDesc, PChar(Temp),
              RecordData, Length(Temp));
          end;
        ftSmallInt, ftInteger, ftFloat, ftBoolean, ftDate, ftTime, ftAutoInc
        {$IFNDEF VER100}, ftLargeInt{$ENDIF}:
          SqlBuffer.SetField(FieldDesc, Query.Field(FieldDesc.FieldNo), RecordData);
        else
          DatabaseError(SUnknownType + FieldDesc.Alias);
      end;
    end;
    { Filter received record }
    SqlBuffer.FilterItem(SqlBuffer.Count-1);
  end;
end;

{ Internal close query }
procedure TZCustomMySqlDataset.InternalClose;
begin
  inherited InternalClose;
  { Close lowerlevel connect to database }
  FUseTransact.Close;
  FUseConnect.Disconnect;
end;

{ Internal procedure for change inserting data }
procedure TZCustomMySqlDataset.ChangeAddBuffer(AddRecord: PRecordData);
begin
{$IFDEF USE_GENERATORS}
  if moUseGen in FExtraOptions then
    ApplyGens(AddRecord.Data);
{$ENDIF}
end;

{************** Sql-queries processing ******************}

{ Define all fields in query }
procedure TZCustomMySqlDataset.AddTableFields(Table: string;
  SqlFields: TSqlFields);
var
  Size: Integer;
  Decimals: Integer;
  FieldType: TFieldType;
  Query: TDirMySqlQuery;
  Default: string;
  AutoType: TAutoType;
begin
  { Set start values }
  Query := TDirMySqlQuery(Transaction.QueryHandle);
  Query.ShowColumns(Table, '');
  { Fetch columns description }
  while not Query.EOF do
  begin
    { Evalute field parameters }
    Size := 0;
    Decimals := 0;
    FieldType := MySqlToDelphiTypeDesc(Query.Field(1), Size, Decimals);
    if (Query.Field(4) <> '') and (Query.Field(4) <> 'NULL') then
      Default := ''''+Query.Field(4)+''''
    else Default := '';
    if StrCaseCmp(Query.Field(5), 'auto_increment') then AutoType := atAutoInc
    else AutoType := atNone;
    { Put new field description }
    SqlFields.Add(Table, Query.Field(0), '', Query.Field(1), FieldType, Size,
      Decimals, AutoType, Query.Field(2) <> '', False, Default, btInternal);
    Query.Next;
  end;
  Query.Close;
end;

{ Define all indices in query }
procedure TZCustomMySqlDataset.AddTableIndices(Table: string;
  SqlFields: TSqlFields; SqlIndices: TSqlIndices);
var
  KeyType: TKeyType;
  SortType: TSortType;
  Query: TDirMySqlQuery;
begin
  { Set start values }
  Query := TDirMySqlQuery(TransactObj.QueryHandle);
  Query.ShowIndexes(Table);
  { Fetch index descriptions }
  while not Query.EOF do
  begin
    { Define a key type }
    if Query.Field(1) = '0' then
    begin
      if StrCmpBegin(Query.Field(2),'PRIMARY') then KeyType := ktPrimary
      else KeyType := ktUnique;
    end else KeyType := ktIndex;
    { Define sorting type }
    if Query.Field(5) = 'A' then SortType := stAscending
    else SortType := stDescending;
    { Put new index description }
    SqlIndices.AddIndex(Query.Field(2), Table, Query.Field(4),
      KeyType, SortType);
    Query.Next;
  end;
  Query.Close;
end;

{ Get auto_increment field of table }
function TZCustomMySqlDataset.GetAutoIncField(Table: string): Integer;
var
  I: Integer;
  FieldDesc: PFieldDesc;
begin
  Result := -1;
  for I := 0 to SqlBuffer.SqlFields.Count-1 do
  begin
    FieldDesc := SqlBuffer.SqlFields[I];
    if (FieldDesc.AutoType = atAutoInc) and StrCaseCmp(FieldDesc.Table, Table) then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

{$IFDEF USE_GENERATORS}
{ Auto set zeos generators to primary keys }
procedure TZCustomMySqlDataset.ApplyGens(Buffer: PRecordData);
var
  KeyField, KeyAlias: string;
  KeyNo, GenValue: LongInt;
begin
  KeyField := GetPrimaryKey(SqlParse.Tables[0]);
  KeyAlias := GetPrimaryKeyAlias(SqlParser.Tables[0]);

  KeyNo := GetRealFieldNo(KeyAlias);
  if KeyNo < 0 then Continue;
  GenValue := Database.GetGen(ExtractField(FTables[0]));
  SetRealFieldValue(KeyNo, IntToStr(GenValue), Buffer);
end;
{$ENDIF}

{ Update record after post updates }
procedure TZCustomMySqlDataset.UpdateAfterPost(OldData, NewData: PRecordData);
var
  Index: Integer;
  FieldDesc: PFieldDesc;
begin
  { Apply auto_increment fields }
  Index := GetAutoIncField(SqlParser.Tables[0]);
  if (OldData.RecordType = ztInserted) and (Index >= 0) then
  begin
    FieldDesc := SqlBuffer.SqlFields[Index];
    if SqlBuffer.GetFieldNull(FieldDesc, NewData) then
      SqlBuffer.SetField(FieldDesc, EvaluteDef('LAST_INSERT_ID()'), NewData);
  end;

  inherited UpdateAfterPost(OldData, NewData);
end;

{ Create demanded connections }
procedure TZCustomMySqlDataset.CreateConnections;
begin
{ Check database and transaction object }
  if not Assigned(DatabaseObj) then
    DatabaseError(SConnectNotDefined);
  if not Assigned(TransactObj) then
    DatabaseError(STransactNotDefined);
{ Connect to transact-server }
  TransactObj.Connect;
  if not TransactObj.Connected then
    DatabaseError(SConnectTransactError);

{ Define database connect by open mode }
  if moStoreResult in FExtraOptions then
  begin
    Query.Connect := DatabaseObj.Handle;
    Query.Transact := TransactObj.Handle;
    TDirMySqlQuery(Query).StoreResult := True;
  end
  else
  begin
    { Attach to database }
    FUseConnect.HostName := DatabaseObj.Handle.HostName;
    FUseConnect.Port := DatabaseObj.Handle.Port;
    FUseConnect.Database := DatabaseObj.Handle.Database;
    FUseConnect.Login := DatabaseObj.Handle.Login;
    FUseConnect.Passwd := DatabaseObj.Handle.Passwd;

    FUseConnect.Connect;
    if not FUseConnect.Active then
      DatabaseError(SConnectError);
    { Attach to database }
//!!    FUseTransact.TransIsolation := TransactObj.Handle.TransIsolation;
    FUseTransact.TransactSafe := TransactObj.Handle.TransactSafe;

    FUseTransact.Open;
    if not FUseTransact.Active then
      DatabaseError(SConnectError);
    { Assign new connect }
    Query.Connect := FUseConnect;
    Query.Transact := FUseTransact;
    TDirMySqlQuery(Query).StoreResult := False;
  end;
end;

{$IFDEF WITH_IPROVIDER}
{ IProvider support }

{ Is in transaction }
function TZCustomMySqlDataset.PSInTransaction: Boolean;
begin
  Result := False;
end;

{ Execute an sql statement }
function TZCustomMySqlDataset.PSExecuteStatement(const ASql: string; AParams: TParams;
  ResultSet: Pointer): Integer;
begin
  if Assigned(ResultSet) then
  begin
    TDataSet(ResultSet^) := TZMySqlQuery.Create(nil);
    with TZMySqlQuery(ResultSet^) do
    begin
      Sql.Text := ASql;
      Params.Assign(AParams);
      Open;
      Result := RowsAffected;
    end;
  end
  else
    Result := FTransact.ExecSql(ASql);
end;

{ Set command query }
procedure TZCustomMySqlDataset.PSSetCommandText(const CommandText: string);
begin
  Close;
  if Self is TZMySqlQuery then
    TZMySqlQuery(Self).Sql.Text := CommandText
  else
  if Self is TZMySqlTable then
    TZMySqlQuery(Self).TableName := CommandText;
end;

{$ENDIF}

{ TZMySqlTable }

constructor TZMySqlTable.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 DefaultIndex := True;
 ReadOnly := False;
end;

end.
