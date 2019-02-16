{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{            MS SQL Query and Table components           }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZMsSqlQuery;

interface

{$R *.dcr}

uses
  SysUtils, Windows, Db, Classes, ZDirSql, ZDirMsSql, DbCommon,
  ZMsSqlCon, ZMsSqlTr, ZToken, ZLibMsSql, ZSqlExtra, ZQuery,
  ZSqlTypes, ZSqlItems;

{$INCLUDE ..\Zeos.inc}

type

  TZMsSqlOption = (soStoreResult);
  TZMsSqlOptions = set of TZMsSqlOption;

  { Direct MS SQL dataset with descendant of TZDataSet }
  TZCustomMsSqlDataset = class(TZDataSet)
  private
    FExtraOptions: TZMsSqlOptions;
    FUseConnect: TDirMsSqlConnect;
    FUseTransact: TDirMsSqlTransact;

    procedure SetDatabase(Value: TZMsSqlDatabase);
    procedure SetTransact(Value: TZMsSqlTransact);
    function GetDatabase: TZMsSqlDatabase;
    function GetTransact: TZMsSqlTransact;
    function GetIdentField(Table: string): Integer;
  protected
    { Overriding ZDataset methods }
    procedure QueryRecord; override;
    procedure UpdateAfterPost(OldData, NewData: PRecordData); override;
    procedure CreateConnections; override;
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
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddTableFields(Table: string; SqlFields: TSqlFields); override;
    procedure AddTableIndices(Table: string; SqlFields: TSqlFields;
      SqlIndices: TSqlIndices); override;
  published
    property Database: TZMsSqlDatabase read GetDatabase write SetDatabase;
    property Transaction: TZMsSqlTransact read GetTransact write SetTransact;
    property ExtraOptions: TZMsSqlOptions read FExtraOptions write FExtraOptions;
  end;

{ Direct MsSql query with descendant of TDataSet }
  TZMsSqlQuery = class(TZCustomMsSqlDataset)
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

{ Direct MsSql query with descendant of TDataSet }
  TZMsSqlTable = class(TZCustomMsSqlDataset)
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

{********** TZCustomMsSqlDataset implementation **********}

{ Class constructor }
constructor TZCustomMsSqlDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Query := TDirMsSqlQuery.Create(nil, nil);
  DatabaseType := dtMsSql;
  FExtraOptions := [soStoreResult];
  FUseConnect := TDirMsSqlConnect.Create;
  FUseTransact := TDirMsSqlTransact.Create(FUseConnect);
end;

{ Class destructor }
destructor TZCustomMsSqlDataset.Destroy;
begin
  inherited Destroy;
  FUseTransact.Free;
  FUseConnect.Free;
end;

{ Set connect to database component }
procedure TZCustomMsSqlDataset.SetDatabase(Value: TZMsSqlDatabase);
begin
  inherited SetDatabase(Value);
end;

{ Set connect to transact-server component }
procedure TZCustomMsSqlDataset.SetTransact(Value: TZMsSqlTransact);
begin
  inherited SetTransact(Value);
end;

{ Get connect to database component }
function TZCustomMsSqlDataset.GetDatabase: TZMsSqlDatabase;
begin
  Result := TZMsSqlDatabase(DatabaseObj);
end;

{ Get connect to transact-server component }
function TZCustomMsSqlDataset.GetTransact: TZMsSqlTransact;
begin
  Result := TZMsSqlTransact(TransactObj);
end;

{ Read query from server to internal buffer }
procedure TZCustomMsSqlDataset.QueryRecord;
var
  I, Count: Integer;
  RecordData: PRecordData;
  FieldDesc: PFieldDesc;
  TempSmall: SmallInt;
  TempDouble: Double;
  TempBool: WordBool;
  TempDate: DBDATETIME;
  DateRec: DBDATEREC;
  TempTime: TDateTime;
  TimeStamp: TTimeStamp;
  BlobPtr: PRecordBlob;
  Cancel: Boolean;
begin
  Count := SqlBuffer.Count;
  while not Query.Eof and (Count = SqlBuffer.Count) do
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
        ftString, ftBytes:
          begin
            SqlBuffer.SetFieldDataLen(FieldDesc, Query.FieldBuffer(FieldDesc.FieldNo),
              RecordData, Query.FieldSize(FieldDesc.FieldNo));
          end;
        ftSmallInt:
          begin
            case Query.FieldType(FieldDesc.FieldNo) of
              SQLINT1:
                TempSmall := PByte(Query.FieldBuffer(FieldDesc.FieldNo))^;
              SQLINT2:
                TempSmall := PSmallInt(Query.FieldBuffer(FieldDesc.FieldNo))^;
            end;
            SqlBuffer.SetFieldData(FieldDesc, @TempSmall, RecordData);
          end;
        ftBoolean:
          begin
            TempBool := (PByte(Query.FieldBuffer(FieldDesc.FieldNo))^ <> 0);
            SqlBuffer.SetFieldData(FieldDesc, @TempBool, RecordData);
          end;
        ftInteger, ftAutoInc:
          begin
            SqlBuffer.SetFieldData(FieldDesc, Query.FieldBuffer(FieldDesc.FieldNo),
              RecordData);
          end;
        ftFloat, ftCurrency:
          begin
            dbconvert(TDirMsSqlTransact(Query.Transact).Handle,
              Query.FieldType(FieldDesc.FieldNo),
              PByte(Query.FieldBuffer(FieldDesc.FieldNo)),
              Query.FieldSize(FieldDesc.FieldNo),
              SQLFLT8, @TempDouble, SizeOf(TempDouble));
            SqlBuffer.SetFieldData(FieldDesc, @TempDouble, RecordData);
          end;
        ftDateTime:
          begin
            dbconvert(TDirMsSqlTransact(Query.Transact).Handle,
              Query.FieldType(FieldDesc.FieldNo),
              PByte(Query.FieldBuffer(FieldDesc.FieldNo)),
              Query.FieldSize(FieldDesc.FieldNo),
              SQLDATETIME, @TempDate, SizeOf(TempDate));
            dbdatecrack(TDirMsSqlTransact(Query.Transact).Handle,
              @DateRec, @TempDate);

            TimeStamp := DateTimeToTimeStamp(EncodeDate(DateRec.year,
              DateRec.month, DateRec.day) + EncodeTime(DateRec.hour,
              DateRec.minute, DateRec.second, DateRec.millisecond));
            TempTime := TimeStampToMSecs(TimeStamp);
            SqlBuffer.SetFieldData(FieldDesc, @TempTime, RecordData);
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
              System.Move(Query.FieldBuffer(FieldDesc.FieldNo)^,
                BlobPtr.Data^, BlobPtr.Size);
            end
            { Fill null fields }
            else
            begin
              BlobPtr.Size := 0;
              BlobPtr.Data := nil;
            end;
          end;
        else
          DatabaseError(SUnknownType + FieldDesc.Alias);
      end;
    end;
    { Filter received record }
    SqlBuffer.FilterItem(SqlBuffer.Count-1);
  end;
  if Query.Eof then
    Query.Close;
end;

{ Internal close query }
procedure TZCustomMsSqlDataset.InternalClose;
begin
  inherited InternalClose;
  { Close lowerlevel connect to database }
  FUseTransact.Close;
  FUseConnect.Disconnect;
end;

{************** Sql-queries processing ******************}

{ Fill collection with fields }
procedure TZCustomMsSqlDataset.AddTableFields(Table: string;
  SqlFields: TSqlFields);
var
  Size: Integer;
  Decimals: Integer;
  FieldType: TFieldType;
  Query: TDirMsSqlQuery;
  Default: string;
  AutoType: TAutoType;
begin
  Query := TDirMsSqlQuery(Transaction.QueryHandle);
  Query.ShowColumns(Table, '');
  while not Query.Eof do
  begin
    { Evalute field parameters }
    Size := StrToIntDef(Query.Field(3), 0);
    Decimals := StrToIntDef(Query.Field(4), 0);
    FieldType := MsSqlToDelphiTypeDesc(Query.Field(6));
    if FieldType <> ftString then Size := 0;
    Default := Query.Field(7);
    AutoType := atNone;
    if Query.Field(10) = '1' then
      AutoType := atIdentity;
    if Query.Field(6) = 'timestamp' then
      AutoType := atTimestamp;

    { Put new field description }
    SqlFields.Add(Table, Query.Field(0), '', Query.Field(6), FieldType, Size,
      Decimals, AutoType, Query.Field(9) = '1', False, Default, btInternal);
    Query.Next;
  end;
  Query.Close;
end;

{ Fill collection with indices }
procedure TZCustomMsSqlDataset.AddTableIndices(Table: string;
  SqlFields: TSqlFields; SqlIndices: TSqlIndices);
var
  Buffer, Token: string;
  KeyType: TKeyType;
  SortType: TSortType;
  Query: TDirMsSqlQuery;
begin
  Query := TDirMsSqlQuery(TransactObj.QueryHandle);
  Query.ShowIndexes(Table);
  while not Query.Eof do
  begin
    { Define a key type }
    KeyType := ktIndex;
    Buffer := Query.Field(1);
    Token := '';
    while (Buffer <> '') and not StrCaseCmp(Token, 'located') do
    begin
      Token := StrTok(Buffer, ' ,');
      if StrCmpBegin(Token, 'primary') then
        KeyType := ktPrimary
      else if (KeyType <> ktPrimary) and StrCaseCmp(Token, 'unique') then
        KeyType := ktUnique;
    end;
    { Define a sorting mode }
    SortType := stAscending;

    { Put new index description }
    SqlIndices.AddIndex(Query.Field(0), Table, Query.Field(2), KeyType, SortType);
    Query.Next;
  end;
  Query.Close;
end;

{ Get auto_increment field of table }
function TZCustomMsSqlDataset.GetIdentField(Table: string): Integer;
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
procedure TZCustomMsSqlDataset.UpdateAfterPost(OldData, NewData: PRecordData);
var
  Index: Integer;
  FieldDesc: PFieldDesc;
begin
  { Apply identity fields }
  Index := GetIdentField(SqlParser.Tables[0]);
  if (OldData.RecordType = ztInserted) and (Index >= 0) then
  begin
    FieldDesc := SqlBuffer.SqlFields[Index];
    SqlBuffer.SetField(FieldDesc, EvaluteDef('@@IDENTITY'), NewData);
  end;
end;

{ Create demanded connections }
procedure TZCustomMsSqlDataset.CreateConnections;
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
  if soStoreResult in FExtraOptions then
  begin
    Query.Connect := DatabaseObj.Handle;
    Query.Transact := TransactObj.Handle;
    FetchAll := True;
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
    FetchAll := False;
  end;
end;

{$IFDEF WITH_IPROVIDER}
{ IProvider support }

{ Is in transaction }
function TZCustomMsSqlDataset.PSInTransaction: Boolean;
begin
  Result := True;
end;

{ Execute an sql statement }
function TZCustomMsSqlDataset.PSExecuteStatement(const ASql: string; AParams: TParams;
  ResultSet: Pointer): Integer;
begin
  if Assigned(ResultSet) then
  begin
    TDataSet(ResultSet^) := TZMsSqlQuery.Create(nil);
    with TZMsSqlQuery(ResultSet^) do
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
procedure TZCustomMsSqlDataset.PSSetCommandText(const CommandText: string);
begin
  Close;
  if Self is TZMsSqlQuery then
    TZMsSqlQuery(Self).Sql.Text := CommandText
  else
  if Self is TZMsSqlTable then
    TZMsSqlQuery(Self).TableName := CommandText;
end;

{$ENDIF}

{ TZMsSqlTable }

constructor TZMsSqlTable.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 DefaultIndex := True;
 ReadOnly := False;
end;

end.
