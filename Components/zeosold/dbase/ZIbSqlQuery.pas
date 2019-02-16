{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{            Interbase Query and Table components        }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZIbSqlQuery;

interface

{$R *.dcr}

{$IFNDEF LINUX}
{$INCLUDE ..\ZeosDef.inc}
{$ELSE}
{$INCLUDE ../ZeosDef.inc}
{$ENDIF}

uses
  SysUtils, {$IFNDEF LINUX} Windows,{$ENDIF} DB, Classes, ZDirSql, ZDirIbSql,
  DbCommon, ZIbSqlCon, ZIbSqlTr, ZToken, ZLibIbSql, ZSqlExtra, ZQuery,
  ZSqlTypes, ZSqlItems {$IFDEF VER100}, DbTables{$ENDIF}
  {$IFDEF VERCLX}, Variants{$ENDIF};

{$IFNDEF LINUX}
{$INCLUDE ..\Zeos.inc}
{$ELSE}
{$INCLUDE ../Zeos.inc}
{$ENDIF}

type

  TZIbSqlOption = (ioAutoIncKey);
  TZIbSqlOptions = set of TZIbSqlOption;

  { Direct Interbase dataset with descendant of TZDataSet }
  TZCustomIbSqlDataset = class(TZDataSet)
  private
    FieldDescKey: PFieldDesc;
    FExtraOptions: TZIbSqlOptions;
    procedure SetDatabase(Value: TZIbSqlDatabase);
    procedure SetTransact(Value: TZIbSqlTransact);
    function  GetDatabase: TZIbSqlDatabase;
    function  GetTransact: TZIbSqlTransact;
  protected
    { Overriding ZDataset methods }
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure QueryRecord; override;
    procedure UpdateAfterInit(RecordData: PRecordData); override;
    {$IFDEF WITH_IPROVIDER}
    { IProvider support }
    function  PSInTransaction: Boolean; override;
    function  PSExecuteStatement(const ASql: string; AParams: TParams;
              ResultSet: Pointer): Integer; override;
    procedure PSSetCommandText(const CommandText: string); override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;

    procedure AddTableFields(Table: string; SqlFields: TSqlFields); override;
    procedure AddTableIndices(Table: string; SqlFields: TSqlFields;
      SqlIndices: TSqlIndices); override;
  published
    property ExtraOptions: TZIbSqlOptions read FExtraOptions write FExtraOptions;
    property Database: TZIbSqlDatabase read GetDatabase write SetDatabase;
    property Transaction: TZIbSqlTransact read GetTransact write SetTransact;
  end;

  { Direct IbSql query with descendant of TDataSet }
  TZIbSqlQuery = class(TZCustomIbSqlDataset)
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

  { Direct IbSql query with descendant of TDataSet }
  TZIbSqlTable = class(TZCustomIbSqlDataset)
  private
    procedure InternalExecute(Sql: string);
  public
    constructor Create(AOwner: TComponent); override;
    { Extra methods }
    procedure AddIndex(const Name, Fields: string; Options: TIndexOptions;
      const DescFields: string);
    procedure CreateTable(CreateIndexes: Boolean);
    procedure DeleteTable;
    procedure EmptyTable;
  published
    property TableName;
    property DefaultIndex default True;
    property ReadOnly default False;
    property Database;
    property Transaction;
    property Active;
  end;

  { Direct IbSql query with descendant of TDataSet }
  TZIbSqlStoredProc = class(TZCustomIbSqlDataset)
  private
    FPrepared: Boolean;
    FIsSelectProc: Boolean;
    FProcName: string;

    procedure SetProcName(Value: string);
    procedure Prepare;
    procedure FetchDataIntoOutputParams;
    function GetIsSelectProc: Boolean;
    function GetParams: TParams;
  protected
    procedure InternalOpen; override;

    {$IFDEF WITH_IPROVIDER}
    { IProvider support }
    function  PSGetTableName: string;
    procedure PSExecute; override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecProc;

    property ParamCount;
  published
    property Params read GetParams;
    property DataSource;

    property Database;
    property Transaction;
    property StoredProcName: string read FProcName write SetProcName;
    property Active;
  end;

implementation

uses ZExtra, ZDBaseConst, ZBlobStream, Math;

{********** TZCustomIbSqlDataset implementation **********}

{ Class constructor }
constructor TZCustomIbSqlDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Query := TDirIbSqlQuery.Create(nil, nil);
  DatabaseType := dtInterbase;
  FExtraOptions := [];
end;

{ Set connect to database component }
procedure TZCustomIbSqlDataset.SetDatabase(Value: TZIbSqlDatabase);
begin
  inherited SetDatabase(Value);
end;

{ Set connect to transact-server component }
procedure TZCustomIbSqlDataset.SetTransact(Value: TZIbSqlTransact);
begin
  inherited SetTransact(Value);
end;

{ Get connect to database component }
function TZCustomIbSqlDataset.GetDatabase: TZIbSqlDatabase;
begin
  Result := TZIbSqlDatabase(DatabaseObj);
end;

{ Get connect to transact-server component }
function TZCustomIbSqlDataset.GetTransact: TZIbSqlTransact;
begin
  Result := TZIbSqlTransact(TransactObj);
end;

procedure TZCustomIbSqlDataset.InternalOpen;

  function FindPrimaryKey: PFieldDesc;
  var
    I: Integer;
    IndexDesc: PIndexDesc;
  begin
    Result := nil;
    if SqlParser.Tables.Count = 0 then Exit;
    { Find primary key }
    IndexDesc := nil;
    for I := 0 to SqlBuffer.SqlIndices.Count-1 do
      if StrCaseCmp(SqlBuffer.SqlIndices[I].Table, SqlParser.Tables[0])
        and (SqlBuffer.SqlIndices[I].KeyType = ktPrimary) then
      begin
        IndexDesc := SqlBuffer.SqlIndices[I];
        Break;
      end;
    { Check primary key }
    if (IndexDesc = nil) or (IndexDesc.FieldCount <> 1) then Exit;
    Result := SqlBuffer.SqlFields.FindByName(SqlParser.Tables[0],
      IndexDesc.Fields[0]);
    if Result = nil then Exit;
    //if Result.FieldType <> ftInteger then
    if not (Result.FieldType in [ftSmallint, ftInteger, ftFloat, ftBCD
      {$IFNDEF VER100}, ftLargeInt{$ENDIF}])  then
       Result := nil;
  end;

begin
 inherited;
 FieldDescKey := FindPrimaryKey;
end;

procedure TZCustomIbSqlDataset.InternalClose;
begin
 inherited;
 FieldDescKey := nil;
end;

{ Read query from server to internal buffer }
procedure TZCustomIbSqlDataset.QueryRecord;
var
  I, Count: Integer;
  RecordData: PRecordData;
  FieldDesc: PFieldDesc;
  TempStr: string;
  TempLong: LongInt;
  TempDouble: Double;
  TempTime: TDateTime;
  TimeStamp: TTimeStamp;
  TempDate: TCTimeStructure;
  TempPtr: PISC_QUAD;
  BlobPtr: PRecordBlob;
  Cancel: Boolean;
  TempCurrency: System.Currency;
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
      OnProgress(Self, psRunning, ppFetching, Query.RecNo + 1,
        MaxIntValue([Query.RecNo + 1, Query.RecordCount]), Cancel);
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
            case Query.FieldType(FieldDesc.FieldNo) of
              SQL_VARYING:
                begin
                  SqlBuffer.SetFieldDataLen(FieldDesc,
                    Pointer(LongInt(Query.FieldBuffer(FieldDesc.FieldNo)) + 2),
                    RecordData, Query.FieldSize(FieldDesc.FieldNo));
                  RecordData.Bytes[FieldDesc.Offset
                    + PSmallInt(Query.FieldBuffer(FieldDesc.FieldNo))^ + 1] := 0;
                end;
              else
                begin
                  TempStr := Query.Field(FieldDesc.FieldNo);
                  SqlBuffer.SetFieldDataLen(FieldDesc,
                    PChar(TempStr), RecordData, Length(TempStr));
                end;
            end;
          end;
        ftInteger, ftSmallInt:
          begin
            case Query.FieldType(FieldDesc.FieldNo) of
              SQL_SHORT:
                TempLong := PSmallint(Query.FieldBuffer(FieldDesc.FieldNo))^;
              SQL_LONG:
                TempLong := PLongInt(Query.FieldBuffer(FieldDesc.FieldNo))^;
            end;
            SqlBuffer.SetFieldData(FieldDesc, @TempLong, RecordData);
          end;
        ftFloat:
          begin
            case Query.FieldType(FieldDesc.FieldNo) of
{$IFNDEF VER100}
              SQL_INT64:
                TempDouble := PInt64(Query.FieldBuffer(FieldDesc.FieldNo))^
                  / IntPower(10, Query.FieldDecimals(FieldDesc.FieldNo));
{$ENDIF}
              SQL_LONG {$IFDEF VER100}, SQL_INT64{$ENDIF}:
                TempDouble := PLongInt(Query.FieldBuffer(FieldDesc.FieldNo))^
                  / IntPower(10, Query.FieldDecimals(FieldDesc.FieldNo));
              SQL_DOUBLE:
                TempDouble := PDouble(Query.FieldBuffer(FieldDesc.FieldNo))^;
              else
                TempDouble := PSingle(Query.FieldBuffer(FieldDesc.FieldNo))^;
            end;
            SqlBuffer.SetFieldData(FieldDesc, @TempDouble, RecordData);
          end;
        ftDateTime:
          begin
            isc_decode_date(PISC_QUAD(Query.FieldBuffer(FieldDesc.FieldNo)), @TempDate);
            TimeStamp := DateTimeToTimeStamp(EncodeDate(TempDate.tm_year + 1900,
              TempDate.tm_mon + 1, TempDate.tm_mday) + EncodeTime(TempDate.tm_hour,
              TempDate.tm_min, TempDate.tm_sec, 0));
            TempTime := TimeStampToMSecs(TimeStamp);
            SqlBuffer.SetFieldData(FieldDesc, @TempTime, RecordData);
          end;
        ftDate:
          begin
            isc_decode_sql_date(PISC_DATE(Query.FieldBuffer(FieldDesc.FieldNo)), @TempDate);
            TempLong := DateTimeToTimeStamp(EncodeDate(TempDate.tm_year + 1900,
              TempDate.tm_mon + 1, TempDate.tm_mday)).Date;
            SqlBuffer.SetFieldData(FieldDesc, @TempLong, RecordData);
          end;
        ftTime:
          begin
            isc_decode_sql_time(PISC_TIME(Query.FieldBuffer(FieldDesc.FieldNo)), @TempDate);
            TempLong := DateTimeToTimeStamp(EncodeTime(TempDate.tm_hour,
            TempDate.tm_min, TempDate.tm_sec, 0)).Time;
            SqlBuffer.SetFieldData(FieldDesc, @TempLong, RecordData);
          end;
        ftBCD:
          begin
            case Query.FieldType(FieldDesc.FieldNo) of
              SQL_DOUBLE:
                TempCurrency := PDouble(Query.FieldBuffer(FieldDesc.FieldNo))^;
              SQL_SHORT:
                TempCurrency := PSmallint(Query.FieldBuffer(FieldDesc.FieldNo))^
                  / IntPower(10, Query.FieldDecimals(FieldDesc.FieldNo));
              SQL_LONG {$IFDEF VER100}, SQL_INT64{$ENDIF}:
                TempCurrency := PLongInt(Query.FieldBuffer(FieldDesc.FieldNo))^
                  / IntPower(10, Query.FieldDecimals(FieldDesc.FieldNo));
{$IFNDEF VER100}
              SQL_INT64:
                TempCurrency := PInt64(Query.FieldBuffer(FieldDesc.FieldNo))^
                  / IntPower(10, Query.FieldDecimals(FieldDesc.FieldNo));
{$ENDIF}
            end;
            SqlBuffer.SetFieldData(FieldDesc, @TempCurrency, RecordData);
          end;
{$IFNDEF VER100}
        ftLargeInt:
          begin
            SqlBuffer.SetFieldData(FieldDesc, Query.FieldBuffer(FieldDesc.FieldNo),
              RecordData);
          end;
{$ENDIF}
        ftBlob, ftMemo:
          begin
            { Initialize blob field }
            BlobPtr := PRecordBlob(@RecordData.Bytes[FieldDesc.Offset + 1]);
            BlobPtr.BlobType := btExternal;
            BlobPtr.Data := nil;
            BlobPtr.Size := 0;
            TempPtr := PISC_QUAD(Query.FieldBuffer(FieldDesc.FieldNo));
            { Fill not null fields }
            if Assigned(TempPtr) then
            begin
              RecordData.Bytes[FieldDesc.Offset] := 0;
              BlobPtr.Handle.Ptr := TempPtr.gds_quad_high;
              BlobPtr.Handle.PtrEx := TempPtr.gds_quad_Low;
            end
            { Fill null fields }
            else
            begin
              BlobPtr.Handle.Ptr := 0;
              BlobPtr.Handle.PtrEx := 0;
            end;
          end;
      //ftArray:  todo
        else
          DatabaseError(SUnknownType + FieldDesc.Alias);
      end;
    end;
    { Filter received record }
    SqlBuffer.FilterItem(SqlBuffer.Count - 1);
  end;
end;

{************** Sql-queries processing ******************}

{ Fill collection with fields }
procedure TZCustomIbSqlDataset.AddTableFields(Table: string;
  SqlFields: TSqlFields);
var
  Size: Integer;
  Decimals: Integer;
  FieldType: TFieldType;
  Query: TDirIbSqlQuery;
  Default: string;
  BlobType: TBlobType;
  SubType: Integer;
  FReadOnly: Boolean;
  FTypeName, FAlias: string;
begin
  Query := TDirIbSqlQuery(Transaction.QueryHandle);
  Query.ShowColumns(Table, '');
  while not Query.EOF do
  begin
    { Evalute field parameters }
    Size := StrToIntDef(Query.Field(3), 0);
    Decimals := StrToIntDef(Query.Field(6), 0);
    SubType := StrToIntDef(Query.Field(7), 0);
    FieldType := IbSqlToDelphiType(StrToIntDef(Query.Field(2), 0), SubType, Decimals);
    if FieldType in [ftBlob, ftMemo] then BlobType := btExternal
    else BlobType := btInternal;

    if FieldType = ftBCD then
      Size := Decimals
    else if FieldType <> ftString then
      Size := 0;

    Default := Query.Field(5);
    StrTok(Default, ' '#9#10#13);

    FTypeName := Query.Field(2);
    FReadOnly := not Query.FieldIsNull(8);
    FAlias := Query.Field(9);

    { Put new field description }
    SqlFields.Add(Table, Query.Field(1), FAlias, FTypeName, FieldType, Size, Decimals,
      atNone, Query.Field(4) <> '1', FReadOnly, Default, BlobType);
    Query.Next;
  end;
  Query.Close;
end;

{ Fill collection with indices }
procedure TZCustomIbSqlDataset.AddTableIndices(Table: string;
  SqlFields: TSqlFields; SqlIndices: TSqlIndices);
var
  KeyType: TKeyType;
  SortType: TSortType;
  Query: TDirIbSqlQuery;
begin
  Query := TDirIbSqlQuery(TransactObj.QueryHandle);
  Query.ShowIndexes(Table);
  while not Query.EOF do
  begin
    { Define a key type }
    if Query.Field(3) = '1' then
    begin
      if StrCmpBegin(Query.Field(1),'RDB$PRIMARY') then
        KeyType := ktPrimary
      else KeyType := ktUnique;
    end else KeyType := ktIndex;
    { Define a sorting mode }
    if Query.Field(4) <> '1' then SortType := stAscending
    else SortType := stDescending;

    { Put new index description }
    SqlIndices.AddIndex(Query.Field(1), Table, Query.Field(5), KeyType, SortType);
    Query.Next;
  end;
  Query.Close;
end;

{ Update record after initialization }
procedure TZCustomIbSqlDataset.UpdateAfterInit(RecordData: PRecordData);
begin
  inherited UpdateAfterInit(RecordData);

  if ioAutoIncKey in FExtraOptions then
  begin
    if FieldDescKey <> nil then
      SqlBuffer.SetField(FieldDescKey, EvaluteDef(Format('GEN_ID(%s_%s_gen, 1)',
        [SqlParser.Tables[0], FieldDescKey.Field])), RecordData);
  end;
end;

{$IFDEF WITH_IPROVIDER}
{ IProvider support }

{ Is in transaction }
function TZCustomIbSqlDataset.PSInTransaction: Boolean;
begin
  Result := True;
end;

{ Execute an sql statement }
function TZCustomIbSqlDataset.PSExecuteStatement(const ASql: string; AParams: TParams;
  ResultSet: Pointer): Integer;
begin
  if Assigned(ResultSet) then
  begin
    TDataSet(ResultSet^) := TZIbSqlQuery.Create(nil);
    with TZIbSqlQuery(ResultSet^) do
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
procedure TZCustomIbSqlDataset.PSSetCommandText(const CommandText: string);
begin
  Close;
  if Self is TZIbSqlQuery then
    TZIbSqlQuery(Self).Sql.Text := CommandText
  else if Self is TZIbSqlTable then
    TZIbSqlQuery(Self).TableName := CommandText
  else if Self is TZIbSqlStoredProc then
    TZIbSqlStoredProc(Self).StoredProcName := CommandText;
end;

{$ENDIF}

{ TZIbSqlTable }

constructor TZIbSqlTable.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 DefaultIndex := True;
 ReadOnly := False;
end;

procedure TZIbSqlTable.InternalExecute(Sql: string);
begin
  if Assigned(TransactObj) then
  begin
    TransactObj.Connected := True;
    TransactObj.ExecSql(ConvertToSqlEnc(Sql));
  end else
    DatabaseError(STransactNotDefined);
end;

procedure TZIbSqlTable.AddIndex(const Name, Fields: string;
  Options: TIndexOptions; const DescFields: string);
var
  FieldList: string;
  Temp: string;
begin
  FieldDefs.Update;
  if Active then
  begin
    CheckBrowseMode;
    CursorPosChanged;
  end;

  FieldList := FormatFieldsList(Fields);
  if (ixPrimary in Options) then
    Temp := 'ALTER TABLE ' + ProcessIdent(TableName) + ' ADD CONSTRAINT ' +
      ProcessIdent(Name) + ' PRIMARY KEY (' + FormatFieldsList(Fields) + ')'
  else if ([ixUnique, ixDescending] * Options = [ixUnique, ixDescending]) then
    Temp := 'CREATE UNIQUE DESCENDING INDEX ' + ProcessIdent(Name) + ' ON ' +
      ProcessIdent(TableName) + ' (' + FieldList + ')'
  else if (ixUnique in Options) then
    Temp := 'CREATE UNIQUE INDEX ' + ProcessIdent(Name) + ' ON ' +
      ProcessIdent(TableName) + ' (' + FieldList + ')'
  else if (ixDescending in Options) then
    Temp := 'CREATE DESCENDING INDEX ' + ProcessIdent(Name) + ' ON ' +
      ProcessIdent(TableName) + ' (' + FieldList + ')'
  else
    Temp := 'CREATE INDEX ' + ProcessIdent(Name) + ' ON ' +
      ProcessIdent(TableName) + ' (' + FieldList + ')';
  InternalExecute(Temp);
  IndexDefs.Updated := False;
end;

procedure TZIbSqlTable.CreateTable(CreateIndexes: Boolean);
var
  FieldList: string;

  procedure InitFieldsList;
  var
    I: Integer;
  begin
    {$IFNDEF VER100}
    InitFieldDefsFromFields;
    {$ENDIF}
    for I := 0 to FieldDefs.Count - 1 do
    begin
      if I > 0 then
        FieldList := FieldList + ', ';
      with FieldDefs[I] do
      begin
        case DataType of
          ftString:
            FieldList := FieldList + ProcessIdent(Name) +
              ' VARCHAR(' + IntToStr(Size) + ')';
          {$IFNDEF VER100}
          ftFixedChar:
            FieldList := FieldList + ProcessIdent(Name) +
              ' CHAR(' + IntToStr(Size) + ')';
          {$ENDIF}
          ftBoolean, ftSmallint, ftWord:
            FieldList := FieldList + ProcessIdent(Name) + ' SMALLINT';
          ftInteger:
            FieldList := FieldList + ProcessIdent(Name) + ' INTEGER';
          ftFloat, ftCurrency:
            FieldList := FieldList + ProcessIdent(Name) + ' DOUBLE PRECISION';
          ftBCD: begin
            {$IFNDEF ENABLE_BCD}
            if (Database.SQLDialect = 1) then
            begin
              if (Precision > 9) then
                DatabaseError('Unsupported field type');
              if (Precision <= 4) then
                Precision := 9;
            end
            else
           {$ENDIF}
           if (Precision <= 4) then
               Precision := 4;
          {$IFNDEF VER100}
            if Size > 4  then
               Size := 4;
          {$ENDIF}
            {
            if (Precision <= 4 ) then
              FieldList := FieldList + ProcessIdent(Name) + ' NUMERIC(18, IntToStr(Size))'
            else
            }
            FieldList := FieldList + ProcessIdent(Name) +
              ' NUMERIC(' + IntToStr(Precision) + ', IntToStr(Size))';
          end;
          ftDate:
            FieldList := FieldList + ProcessIdent(Name) + ' DATE';
          ftTime:
            FieldList := FieldList + ProcessIdent(Name) + ' TIME';
          ftDateTime:
            if (Database.SQLDialect = 1) then
              FieldList := FieldList + ProcessIdent(Name) + ' DATE'
            else
              FieldList := FieldList + ProcessIdent(Name) + ' TIMESTAMP';
          {$IFNDEF VER100}
          ftLargeInt:
            if (Database.SQLDialect = 1) then
              DatabaseError('Unsupported field type')
            else
              FieldList := FieldList + ProcessIdent(Name) + ' NUMERIC(18, 0)';
          {$ENDIF}
          ftBlob, ftMemo:
            FieldList := FieldList + ProcessIdent(Name) + ' BLOB SUB_TYPE 1';
          ftBytes, ftVarBytes, ftGraphic..ftTypedBinary:
            FieldList := FieldList + ProcessIdent(Name) + ' BLOB SUB_TYPE 0';
          else
            DatabaseError('Unsupported field type');
        end;
        {$IFNDEF VER100}
        if faRequired in Attributes then
          FieldList := FieldList + ' NOT NULL';
        {$ENDIF}
      end;
    end;
  end;

  procedure InternalCreateTable;
  var
    I: Integer;
    Temp: string;
  begin
    if FieldList = '' then
      DatabaseError('Unsupported field type');

    Temp := 'CREATE TABLE ' + ProcessIdent(TableName) + ' (' + FieldList;
    for I := 0 to IndexDefs.Count - 1 do
    with IndexDefs[I] do
    begin
      if ixPrimary in Options then
        Temp := Temp + ', CONSTRAINT ' + ProcessIdent(Name)
          + ' PRIMARY KEY (' + FormatFieldsList(Fields) + ')';
    end;
    Temp := Temp + ')';
    InternalExecute(Temp);
  end;

  procedure InternalCreateIndex;
  var
    I: Integer;
  begin
    for I := 0 to IndexDefs.Count - 1 do
    with IndexDefs[I] do
      if not (ixPrimary in Options) then
        AddIndex(Name, Fields, Options, '');
  end;

begin
  CheckInactive;
  InitFieldsList;
  InternalCreateTable;
  if CreateIndexes then
    InternalCreateIndex;
end;

procedure TZIbSqlTable.DeleteTable;
begin
  InternalExecute('DROP TABLE ' + ProcessIdent(TableName));
  if Active then Close;
end;

procedure TZIbSqlTable.EmptyTable;
begin
  InternalExecute('DELETE FROM ' + ProcessIdent(TableName));
  if Active then Refresh;
end;

{************* TZIbSqlStoredProc implementation ***************}

constructor TZIbSqlStoredProc.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FPrepared := False;
end;


procedure TZIbSqlStoredProc.SetProcName(Value: string);
begin
  if not (csReading in ComponentState) then
  begin
    CheckInactive;
    if FProcName <> Value then
    begin
      FProcName := UpperCase(Value);
      FPrepared := False;
      //SQL.Text := '';
      if (Value <> '') and (Database <> nil) then
        Prepare;
    end;
  end
  else
    begin
      FProcName := UpperCase(Value);
      FPrepared := False;
      //SQL.Text := '';
      if (Value <> '') and (Database <> nil) then
        Prepare;
    end;
end;


function TZIbSqlStoredProc.GetisSelectProc: boolean;
const
 Suspend = 'SUSPEND';
var
 AQuery: TDirIbSqlQuery;
 Ps: string;
 d, f: integer;
begin
 Result := False;

 AQuery := TDirIbSqlQuery(Transaction.QueryHandle);
 if AQuery.Active then AQuery.Close;
 AQuery.Sql := 'Select RDB$PROCEDURE_SOURCE from RDB$PROCEDURES where ' +
  'RDB$PROCEDURE_NAME=' + QuotedStr(UpperCase(FProcName));
 try
  AQuery.Open;
  Ps := UpperCase(AQuery.Field(0));

  D := Pos(Suspend, Ps);
  F := D + length(Suspend);
  if (D > 5) and (Ps[D - 1] in [' ', ';', #10, #13]) and
   (F < length(PS) - 3) and (Ps[F + 1] in [' ', ';', #10, #13]) then
   Result := True;
 finally
  AQuery.Close;
 end;
end;

function TZIbSqlStoredProc.GetParams:TParams;
begin
  Prepare;
  Result := inherited Params;
end;

procedure TZIbSqlStoredProc.Prepare;
var
 AQuery: TDirIbSqlQuery;
 ASQL: string;
 Input: string;
 ParamName: string;
 PrmType: integer;
begin
  if FPrepared or
    not Assigned(Database) or
    not Assigned(Transaction) then exit;

  if Active then Close;

  Input := '';

  AQuery := TDirIbSqlQuery(Transaction.QueryHandle);
  if AQuery.Active then AQuery.Close;

  CreateConnections;

  AQuery.ShowProcsParams(FProcName);
 while not AQuery.EOF do
  begin
   ParamName := AQuery.Field(1);
   PrmType := StrToIntDef(AQuery.Field(2), 0);

   if PrmType = 0 then
    begin
     if input <> '' then
      input := input + ',';
     input := input + ':' + ParamName;
    end;

   AQuery.Next;
  end;

 AQuery.Close;

 FisSelectProc := GetisSelectProc;

 if FisSelectProc then
  ASQL := 'Select * From ' + ProcessIdent(FProcName)
 else
  ASQL := 'EXECUTE PROCEDURE ' + ProcessIdent(FProcName);

 if input <> '' then
  ASQL := ASQL + '(' + input + ')';

  SQL.Text := ASQL;

 FPrepared := ASQL <> '';
end;


{ Fill collection with fields }
procedure TZIbSqlStoredProc.FetchDataIntoOutputParams;
var
 I: Integer;
 ParamName: string;
 FieldType: TFieldType;
 FieldValue: Variant;
 Parami: TParam;
begin
  if not FPrepared then Exit;

 if Query.FieldCount > 0 then
   for I := 0 to Query.FieldCount - 1 do
   begin
    ParamName := Query.FieldName(I);
    FieldType := Query.FieldDataType(I);
    FieldValue := TDirIbSqlQuery(Query).FieldValue(I);

{$IFNDEF VER100}
    Parami := Params.FindParam(Paramname);
{$ELSE}
    try
      Parami := Params.ParamByName(Paramname);
    except
      Parami := nil;
    end;
{$ENDIF}
    if Parami = nil then
    begin
      Parami := Params.CreateParam(FieldType, ParamName, ptOutput);
      if VarIsNull(FieldValue)then
        Parami.AsString := ''
      else Parami.AsString := FieldValue;
    end
    else
    begin
      Parami.DataType := FieldType;
      Parami.ParamType := ptOutput;
      Parami.Value := FieldValue;
    end;
  end;
end;

procedure TZIbSqlStoredProc.InternalOpen;
begin
 Prepare;

 if not FisSelectProc then
  DataBaseError('is not selected proc');

 inherited InternalOpen;
end;


procedure TZIbSqlStoredProc.ExecProc;
begin
 Prepare;

 if FisSelectProc then
  DataBaseError('is selectted proc use open');

 Open;
 try
  FetchDataIntoOutputParams;
 finally
  Close;
 end;
end;

{$IFDEF WITH_IPROVIDER}

function TZIbSqlStoredProc.PSGetTableName: string;
begin
  { ? }
end;

procedure TZIbSqlStoredProc.PSExecute;
begin
  ExecProc;
end;

{$ENDIF}

end.
