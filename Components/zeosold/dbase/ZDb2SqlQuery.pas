{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{             DB2 Query and Table components             }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZDb2SqlQuery;

interface

{$R *.dcr}

uses
  SysUtils, Windows, Db, Classes, ZDirSql, ZDirDb2Sql, DbCommon,
  ZDb2SqlCon, ZDb2SqlTr, ZToken, ZLibDb2Sql, ZSqlExtra, ZQuery,
  ZSqlTypes, ZSqlItems, ZSqlBuffer;

{$INCLUDE ..\Zeos.inc}

type

  TZDb2SqlOption = (doStoreResult);
  TZDb2SqlOptions = set of TZDb2SqlOption;

  { Direct Oracle8 dataset with descendant of TZDataSet }
  TZCustomDb2SqlDataset = class(TZDataSet)
  private
    FExtraOptions: TZDb2SqlOptions;
    FUseConnect: TDirDb2SqlConnect;
    FUseTransact: TDirDb2SqlTransact;

    procedure SetDatabase(Value: TZDb2SqlDatabase);
    procedure SetTransact(Value: TZDb2SqlTransact);
    function  GetDatabase: TZDb2SqlDatabase;
    function  GetTransact: TZDb2SqlTransact;
  protected
    { Overriding ZDataset methods }
    procedure QueryRecord; override;
    function GetIdentityField(Table: string): Integer;
    procedure UpdateAfterPost(OldData, NewData: PRecordData); override;
    procedure UpdateAfterInit(RecordData: PRecordData); override;
    {$IFDEF WITH_IPROVIDER}
    { IProvider support }
    function  PSInTransaction: Boolean; override;
    function  PSExecuteStatement(const ASql: string; AParams: TParams;
              ResultSet: Pointer): Integer; override;
    procedure PSSetCommandText(const CommandText: string); override;
    {$ENDIF}

    procedure InternalClose; override;
    procedure CreateConnections; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure AddTableFields(Table: string; SqlFields: TSqlFields); override;
    procedure AddTableIndices(Table: string; SqlFields: TSqlFields;
      SqlIndices: TSqlIndices); override;

    function FieldValueToSql(Value: string; FieldDesc: PFieldDesc): string; override;
  published
//    property ExtraOptions: TZDb2SqlOptions read FExtraOptions write FExtraOptions;
    property Database: TZDb2SqlDatabase read GetDatabase write SetDatabase;
    property Transaction: TZDb2SqlTransact read GetTransact write SetTransact;
  end;

{ Direct Db2Sql query with descendant of TDataSet }
  TZDb2SqlQuery = class(TZCustomDb2SqlDataset)
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
    property Database;
    property Transaction;
    property Active;
  end;

{ Direct Db2Sql query with descendant of TDataSet }
  TZDb2SqlTable = class(TZCustomDb2SqlDataset)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property TableName;
    property ReadOnly default False;
    property DefaultIndex default True;
    property Database;
    property Transaction;
    property Active;
  end;

implementation

uses ZExtra, ZDBaseConst, ZBlobStream, Math;

{********** TZCustomDb2SqlDataset implementation **********}

{ Class constructor }
constructor TZCustomDb2SqlDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Query := TDirDb2SqlQuery.Create(nil, nil);
  DatabaseType := dtDb2;
  FExtraOptions := [{doStoreResult}];
  FUseConnect := TDirDb2SqlConnect.Create;
  FUseTransact := TDirDb2SqlTransact.Create(FUseConnect);
end;

{ Set connect to database component }
procedure TZCustomDb2SqlDataset.SetDatabase(Value: TZDb2SqlDatabase);
begin
  inherited SetDatabase(Value);
end;

{ Set connect to transact-server component }
procedure TZCustomDb2SqlDataset.SetTransact(Value: TZDb2SqlTransact);
begin
  inherited SetTransact(Value);
end;

{ Get connect to database component }
function TZCustomDb2SqlDataset.GetDatabase: TZDb2SqlDatabase;
begin
  Result := TZDb2SqlDatabase(DatabaseObj);
end;

{ Get connect to transact-server component }
function TZCustomDb2SqlDataset.GetTransact: TZDb2SqlTransact;
begin
  Result := TZDb2SqlTransact(TransactObj);
end;

{ Read query from server to internal buffer }
procedure TZCustomDb2SqlDataset.QueryRecord;
var
  I, Count: Integer;
  RecordData: PRecordData;
  FieldDesc: PFieldDesc;

  TempLong: LongInt;
  TempDate: PSQL_DATE_STRUCT;
  TempTime: PSQL_TIME_STRUCT;
  TempDateTime: PSQL_TIMESTAMP_STRUCT;
  TempTime1: TDateTime;
  TimeStamp: TTimeStamp;

  BlobPtr: PRecordBlob;
//  Status: Integer;
  Cancel: Boolean;
begin
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
      FieldDesc := SqlBuffer.SqlFields[I];
      if FieldDesc.FieldNo < 0 then Continue;
      if Query.FieldIsNull(FieldDesc.FieldNo) and
        not (FieldDesc.FieldType in [ftBlob, ftMemo]) then
        Continue;

      case FieldDesc.FieldType of
        ftString:
          begin
            SqlBuffer.SetFieldDataLen(FieldDesc,
              Query.FieldBuffer(FieldDesc.FieldNo), RecordData,
              Query.FieldSize(FieldDesc.FieldNo));
          end;
        ftInteger, ftFloat {$IFNDEF VER100}, ftLargeInt {$ENDIF}:
          SqlBuffer.SetFieldData(FieldDesc,
             Query.FieldBuffer(FieldDesc.FieldNo), RecordData);
        ftDateTime:
          begin
            TempDateTime := PSQL_TIMESTAMP_STRUCT(Query.FieldBuffer(FieldDesc.FieldNo));
            TimeStamp := DateTimeToTimeStamp(EncodeDate(TempDateTime.year,
              TempDateTime.month, TempDateTime.day) + EncodeTime(
              TempDateTime.hour, TempDateTime.minute, TempDateTime.second, 0));
            TempTime1 := TimeStampToMSecs(TimeStamp);
            SqlBuffer.SetFieldData(FieldDesc, @TempTime1, RecordData);
          end;
        ftDate:
          begin
            TempDate := PSQL_DATE_STRUCT(Query.FieldBuffer(FieldDesc.FieldNo));
            TempLong := DateTimeToTimeStamp(EncodeDate(TempDate.year,
              TempDate.month, TempDate.day)).Date;
            SqlBuffer.SetFieldData(FieldDesc, @TempLong, RecordData);
          end;
        ftTime:
          begin
            TempTime := PSQL_TIME_STRUCT(Query.FieldBuffer(FieldDesc.FieldNo));
            TempLong := DateTimeToTimeStamp(EncodeTime(TempTime.hour,
              TempTime.minute, TempTime.second, 0)).Time;
            SqlBuffer.SetFieldData(FieldDesc, @TempLong, RecordData);
          end;
        ftMemo, ftBlob:
          begin
            { Process blob and memo fields }
            BlobPtr := PRecordBlob(@RecordData.Bytes[FieldDesc.Offset+1]);
            BlobPtr.Handle.Ptr := 0;
            BlobPtr.Handle.PtrEx := TDirDb2SqlQuery(Query).FieldTypeCode(
              FieldDesc.FieldNo) + 1000;
            BlobPtr.Size := 0;
            BlobPtr.Data := nil;
            BlobPtr.BlobType := btExternal;

            if not Query.FieldIsNull(FieldDesc.FieldNo) then
            begin
              RecordData.Bytes[FieldDesc.Offset] := 0;
              BlobPtr.Handle.Ptr := PInteger(Query.FieldBuffer(FieldDesc.FieldNo))^;
            end;
          end;
        else
          DatabaseError(SUnknownType + FieldDesc.Alias);
      end;
    end;
    { Filter received record }
    SqlBuffer.FilterItem(SqlBuffer.Count-1);
  end;
end;

{ Internal close query }
procedure TZCustomDb2SqlDataset.InternalClose;
begin
  inherited InternalClose;
  { Close lowerlevel connect to database }
  FUseTransact.Close;
  FUseConnect.Disconnect;
end;

{************** Sql-queries processing ******************}

{ Fill collection with fields }
procedure TZCustomDb2SqlDataset.AddTableFields(Table: string;
  SqlFields: TSqlFields);
var
  Size: Integer;
  Decimals: Integer;
  FieldType: TFieldType;
  Query: TDirDb2SqlQuery;
  Default: string;
  BlobType: TBlobType;
  AutoType: TAutoType;
begin
  Query := TDirDb2SqlQuery(Transaction.QueryHandle);
  Query.ShowColumns(Table, '');
  while not Query.EOF do
  begin
    { Evalute field parameters }
    Size := StrToIntDef(Query.Field(3), 0);
    Decimals := StrToIntDef(Query.Field(6), 0);
    FieldType := Db2SqlToDelphiType(Query.Field(2), Size, Decimals, BlobType);
    if FieldType <> ftString then Size := 0;
    Default := Query.Field(5);

    if Query.Field(7) = 'Y' then
      AutoType := atIdentity
    else if Trim(Query.Field(8)) <> '' then
      AutoType := atGenerated
    else AutoType := atNone;

    { Put new field description }
    SqlFields.Add(Table, Query.Field(1), '', Query.Field(2), FieldType,
      Size, Decimals, AutoType, Query.Field(4) = 'Y', False, Default, BlobType);
    Query.Next;
  end;
  Query.Close;
end;

{ Fill collection with indices }
procedure TZCustomDb2SqlDataset.AddTableIndices(Table: string;
  SqlFields: TSqlFields; SqlIndices: TSqlIndices);
var
  KeyType: TKeyType;
  SortType: TSortType;
  Query: TDirDb2SqlQuery;
begin
  Query := TDirDb2SqlQuery(TransactObj.QueryHandle);
  Query.ShowIndexes(Table);
  while not Query.EOF do
  begin
    { Define a key type }
    if Query.Field(2) = 'P' then
      KeyType := ktPrimary
    else if Query.Field(2) = 'U' then
      KeyType := ktUnique
    else KeyType := ktIndex;
    { Define a sorting mode }
    if Query.Field(3) = 'D' then
      SortType := stDescending
    else SortType := stAscending;

    { Put new index description }
    SqlIndices.AddIndex(Query.Field(0), Table, Query.Field(4),
      KeyType, SortType);
    Query.Next;
  end;
  Query.Close;
end;

{ Convert field value to sql value }
function TZCustomDb2SqlDataset.FieldValueToSql(Value: string;
  FieldDesc: PFieldDesc): string;
begin
  Result := inherited FieldValueToSql(Value, FieldDesc);

  if FieldDesc.FieldType = ftDateTime then
    Result := 'TIMESTAMP(' + Result + ')'
  else if FieldDesc.FieldType = ftDate then
    Result := 'DATE(' + Result + ')'
  else if FieldDesc.FieldType = ftTime then
    Result := 'TIME(' + Result + ')';
end;

{ Get identity field of table }
function TZCustomDb2SqlDataset.GetIdentityField(Table: string): Integer;
var
  I: Integer;
  FieldDesc: PFieldDesc;
begin
  Result := -1;
  for I := 0 to SqlBuffer.SqlFields.Count-1 do
  begin
    FieldDesc := SqlBuffer.SqlFields[I];
    if (FieldDesc.AutoType = atIdentity) and StrCaseCmp(FieldDesc.Table, Table) then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

{ Update record after post updates }
procedure TZCustomDb2SqlDataset.UpdateAfterPost(OldData, NewData: PRecordData);
var
  Index: Integer;
  FieldDesc: PFieldDesc;
begin
  { Apply identity fields }
  Index := GetIdentityField(SqlParser.Tables[0]);
  if (OldData.RecordType = ztInserted) and (Index >= 0) then
  begin
    FieldDesc := SqlBuffer.SqlFields[Index];
    if SqlBuffer.GetFieldNull(FieldDesc, NewData) then
      SqlBuffer.SetField(FieldDesc, EvaluteDef('IDENTITY_VAL_LOCAL()'), NewData);
  end;

  inherited UpdateAfterPost(OldData, NewData);
end;

{ Update record after initialization }
procedure TZCustomDb2SqlDataset.UpdateAfterInit(RecordData: PRecordData);
var
  I: Integer;
  FieldDesc: PFieldDesc;
  RecordBlob: PRecordBlob;
begin
  inherited UpdateAfterInit(RecordData);
  { Correct blobs description }
  for I := 0 to SqlBuffer.SqlFields.Count-1 do
  begin
    FieldDesc := SqlBuffer.SqlFields[I];
    if FieldDesc.FieldType in [ftBlob, ftMemo, ftGraphic, ftFmtMemo] then
    begin
      RecordBlob := PRecordBlob(@RecordData.Bytes[FieldDesc.Offset+1]);
      RecordBlob.BlobType := btExternal;
      RecordBlob.Handle.PtrEx := TDirDb2SqlQuery(Query).FieldTypeCode(
        FieldDesc.FieldNo) + 1000;
    end;
  end;
end;

{ Create demanded connections }
procedure TZCustomDb2SqlDataset.CreateConnections;
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
  if doStoreResult in FExtraOptions then
  begin
    Query.Connect := DatabaseObj.Handle;
    Query.Transact := TransactObj.Handle;
//    FetchAll := True;
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
    FUseTransact.TransIsolation := TDirDb2SqlTransact(TransactObj.Handle).TransIsolation;
    FUseTransact.TransactSafe := TransactObj.Handle.TransactSafe;

    FUseTransact.Open;
    if not FUseTransact.Active then
      DatabaseError(SConnectError);
    { Assign new connect }
    Query.Connect := FUseConnect;
    Query.Transact := FUseTransact;
//    FetchAll := False;
  end;
end;

{$IFDEF WITH_IPROVIDER}
{ IProvider support }

{ Is in transaction }
function TZCustomDb2SqlDataset.PSInTransaction: Boolean;
begin
  Result := True;
end;

{ Execute an sql statement }
function TZCustomDb2SqlDataset.PSExecuteStatement(const ASql: string; AParams: TParams;
  ResultSet: Pointer): Integer;
begin
  if Assigned(ResultSet) then
  begin
    TDataSet(ResultSet^) := TZDb2SqlQuery.Create(nil);
    with TZDb2SqlQuery(ResultSet^) do
    begin
      Sql.Text := ASql;
      Params.Assign(AParams);
      Open;
      Result := RowsAffected;
    end;
  end else
    Result := TransactObj.ExecSql(ASql);
end;

{ Set command query }
procedure TZCustomDb2SqlDataset.PSSetCommandText(const CommandText: string);
begin
  Close;
  if Self is TZDb2SqlQuery then
    TZDb2SqlQuery(Self).Sql.Text := CommandText
  else
  if Self is TZDb2SqlTable then
    TZDb2SqlQuery(Self).TableName := CommandText;
end;

{$ENDIF}

{ TZDb2SqlTable }

constructor TZDb2SqlTable.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 DefaultIndex := True;
 ReadOnly := False;
end;

end.
