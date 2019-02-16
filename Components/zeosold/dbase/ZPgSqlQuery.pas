{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{          PostgreSql Query and Table components         }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZPgSqlQuery;

interface

{$R *.dcr}

uses
  SysUtils, {$IFNDEF LINUX}Windows,{$ENDIF} DB, Classes, ZDirSql, ZDirPgSql,
  DBCommon, ZPgSqlCon, ZPgSqlTr, ZToken, ZLibPgSql, ZSqlExtra, ZQuery,
  ZSqlTypes, ZSqlItems, ZSqlParser, ZSqlBuffer, ZBlobStream;

{$IFNDEF LINUX}
{$INCLUDE ..\Zeos.inc}
{$ELSE}
{$INCLUDE ../Zeos.inc}
{$ENDIF}

type

  TZPgSqlOption = (poTextAsMemo, poOidAsBlob);
  TZPgSqlOptions = set of TZPgSqlOption;

  { Direct PgSql dataset with descendant of TZDataSet }
  TZCustomPgSqlDataset = class(TZDataSet)
  private
    FExtraOptions: TZPgSqlOptions;
    procedure SetDatabase(Value: TZPgSqlDatabase);
    procedure SetTransact(Value: TZPgSqlTransact);
    function GetDatabase: TZPgSqlDatabase;
    function GetTransact: TZPgSqlTransact;
  protected
    { Overriding ZDataset methods }
    procedure FormSqlQuery(OldData, NewData: PRecordData); override;
    procedure QueryRecord; override;
    procedure UpdateAfterInit(RecordData: PRecordData); override;
    procedure UpdateAfterPost(OldData, NewData: PRecordData); override;
    procedure UpdateFieldDef(FieldDesc: PFieldDesc; var FieldType: TFieldType;
      var FieldSize: Integer); override;
    function ValueToRowId(Value: string): TRowId; override;
    function RowIdToValue(Value: TRowId): string; override;
    {$IFDEF WITH_IPROVIDER}
    { IProvider support }
    function PSInTransaction: Boolean; override;
    function PSExecuteStatement(const ASql: string; AParams: TParams;
      ResultSet: Pointer): Integer; override;
    procedure PSSetCommandText(const CommandText: string); override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;

    procedure AddTableFields(Table: string; SqlFields: TSqlFields); override;
    procedure AddTableIndices(Table: string; SqlFields: TSqlFields;
      SqlIndices: TSqlIndices); override;
    function CheckTableExistence(Table: string): Boolean; override;
  published
    property ExtraOptions: TZPgSqlOptions read FExtraOptions write FExtraOptions;
    property Database: TZPgSqlDatabase read GetDatabase write SetDatabase;
    property Transaction: TZPgSqlTransact read GetTransact write SetTransact;
  end;

  { Direct PgSql query with descendant of TDataSet }
  TZPgSqlQuery = class(TZCustomPgSqlDataset)
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

  { Direct PgSql query with descendant of TDataSet }
  TZPgSqlTable = class(TZCustomPgSqlDataset)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property TableName;
    property ReadOnly default False;
    property DefaultIndex default True;
    property Active;
  end;

implementation

uses ZExtra, ZDBaseConst, Math;

{***************** TZCustomPgSqlDataset implemantation *******************}

{ Class constructor }
constructor TZCustomPgSqlDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DatabaseType := dtPostgreSql;
  Query := TDirPgSqlQuery.Create(nil, nil);
  FExtraOptions := [poTextAsMemo, poOidAsBlob];
end;

{ Set connect to database component }
procedure TZCustomPgSqlDataset.SetDatabase(Value: TZPgSqlDatabase);
begin
  inherited SetDatabase(Value);
end;

{ Set connect to transaction component }
procedure TZCustomPgSqlDataset.SetTransact(Value: TZPgSqlTransact);
begin
  inherited SetTransact(Value);
end;

{ Get connect to database component }
function TZCustomPgSqlDataset.GetDatabase: TZPgSqlDatabase;
begin
  Result := TZPgSqlDatabase(DatabaseObj);
end;

{ Get connect to transact-server component }
function TZCustomPgSqlDataset.GetTransact: TZPgSqlTransact;
begin
  Result := TZPgSqlTransact(TransactObj);
end;

{ Read query from server to internal buffer }
procedure TZCustomPgSqlDataset.QueryRecord;
var
  I, Count: Integer;
  FieldNo, FieldNoOffs: Integer;
  RecordData: PRecordData;
  FieldDesc: PFieldDesc;
  BlobPtr: PRecordBlob;
  Temp: string;
  Cancel: Boolean;
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

    if SqlParser.UsedRowId then
    begin
      RecordData^.RowId := ValueToRowId(Query.Field(0));
      FieldNoOffs := 1;
    end else
      FieldNoOffs := 0;

    for I := 0 to SqlBuffer.SqlFields.Count - 1 do
    begin
      { Define field structure }
      FieldDesc := SqlBuffer.SqlFields[I];
      if FieldDesc.FieldNo < 0 then Continue;
      FieldNo := FieldDesc.FieldNo + FieldNoOffs;

      if Query.FieldIsNull(FieldNo) and
        not (FieldDesc.FieldType in [ftBlob, ftMemo
        {$IFNDEF VER100}, ftArray{$ENDIF}]) then
        Continue;
      { Converting field values }
      case FieldDesc.FieldType of
        ftFloat, ftCurrency:
          SqlBuffer.SetFieldValue(FieldDesc,
            MoneyToFloat(Query.Field(FieldNo)), RecordData);
{$IFNDEF VER100}
        ftArray:
        begin
          Temp := ConvertFromSqlEnc(Query.Field(FieldNo));
        end;
{$ENDIF}
        ftMemo, ftBlob:
          begin
            { Process blob and memo fields }
            BlobPtr := PRecordBlob(@RecordData.Bytes[FieldDesc.Offset+1]);
            BlobPtr.Handle.Ptr := 0;
            BlobPtr.Handle.PtrEx := 0;
            BlobPtr.Size := 0;
            BlobPtr.Data := nil;
            if FieldDesc.FieldType = ftMemo then
              BlobPtr.BlobType := btInternal
            else
              BlobPtr.BlobType := btExternal;
            if not Query.FieldIsNull(FieldNo) then
            begin
              RecordData.Bytes[FieldDesc.Offset] := 0;
              { Fill internal blobs }
              if FieldDesc.FieldType = ftMemo then
              begin
                BlobPtr.Size := Query.FieldSize(FieldNo);
                BlobPtr.Data := AllocMem(BlobPtr.Size);
                System.Move(PChar(ConvertFromSqlEnc(Query.Field(FieldNo)))^,
                  BlobPtr.Data^,  BlobPtr.Size);
              end
              { Fill external blobs }
              else
                BlobPtr.Handle.Ptr := StrToIntDef(Query.Field(FieldNo),0);
            end;
          end;
        ftString:
          begin
            Temp := ConvertFromSqlEnc(Query.Field(FieldNo));
            SqlBuffer.SetFieldDataLen(FieldDesc, PChar(Temp),
              RecordData, Length(Temp));
          end;
        ftSmallInt, ftInteger, ftBoolean, ftDate, ftTime, ftDateTime, ftAutoInc
        {$IFNDEF VER100}, ftLargeInt{$ENDIF}:
          SqlBuffer.SetField(FieldDesc, Query.Field(FieldNo), RecordData);
        else
          DatabaseError(SUnknownType + FieldDesc.Field);
      end;
    end;
    { Filter received record }
    SqlBuffer.FilterItem(SqlBuffer.Count-1);
  end;
end;

{ Update record after initialization }
procedure TZCustomPgSqlDataset.UpdateAfterInit(RecordData: PRecordData);
var
  I: Integer;
  FieldDesc: PFieldDesc;
  RecordBlob: PRecordBlob;
begin
  { Correct blobs description }
  for I := 0 to SqlBuffer.SqlFields.Count-1 do
  begin
    FieldDesc := SqlBuffer.SqlFields[I];
    if FieldDesc.FieldType in [ftBlob, ftMemo, ftGraphic, ftFmtMemo] then
    begin
      RecordBlob := PRecordBlob(@RecordData.Bytes[FieldDesc.Offset+1]);
      if FieldDesc.FieldType = ftMemo then
        RecordBlob.BlobType := btInternal
      else
        RecordBlob.BlobType := btExternal;
      RecordBlob.Data := nil;
      RecordBlob.Size := 0;
      RecordBlob.Handle.Ptr := 0;
      RecordBlob.Handle.PtrEx := 0;
    end;
  end;
end;

{ Update field parameters }
procedure TZCustomPgSqlDataset.UpdateFieldDef(FieldDesc: PFieldDesc;
  var FieldType: TFieldType; var FieldSize: Integer);
begin
  inherited UpdateFieldDef(FieldDesc, FieldType, FieldSize);
  { Fix oid fields according options }
  if (FieldType = ftBlob) and not (poOidAsBlob in ExtraOptions) then
    FieldType := ftInteger;
  { Fix text fields according options }
  if (FieldType = ftMemo) and not (poTextAsMemo in ExtraOptions) then
  begin
    FieldType := ftString;
    FieldSize := 255;
  end;
  { Fix autoinc fields }
  if (doEnableAutoInc in Options) and (FieldDesc <> nil) and (FieldType = ftInteger)
    and StrCmpBegin(FieldDesc.Default, 'nextval(') then
    FieldType := ftAutoInc;
end;

{ Update record after post updates }
procedure TZCustomPgSqlDataset.UpdateAfterPost(OldData,
  NewData: PRecordData);
begin
  if OldData.RecordType = ztInserted then
    PInteger(@NewData.RowId)^ := TZPgSqlTransact(TransactObj).LastInsertOid;
end;

{ Convert RowId value to string }
function TZCustomPgSqlDataset.RowIdToValue(Value: TRowId): string;
begin
  Result := IntToStr(PInteger(@Value)^);
end;

{ Convert string value to RowId }
function TZCustomPgSqlDataset.ValueToRowId(Value: string): TRowId;
begin
  FillChar(Result, SizeOf(TRowId), 0);
  PInteger(@Result)^ := StrToIntDef(Value, 0);
end;

{************** Sql-queries processing ******************}

{ Define all fields in query }
procedure TZCustomPgSqlDataset.AddTableFields(Table: string; SqlFields: TSqlFields);
var
  Size: Integer;
  Decimals: Integer;
  FieldType: TFieldType;
  Query: TDirPgSqlQuery;
  Default: string;
  BlobType: TBlobType;
  ArraySubType: TFieldType;
begin
  { Set start values }
  Query := TDirPgSqlQuery(Transaction.QueryHandle);
  Query.ShowColumns(Table, '');
  { Fetch columns description }
  while not Query.EOF do
  begin
    { Evalute field parameters }
    Size := Max(0, StrToIntDef(Query.Field(3),0));
    if (Query.Field(2) = 'numeric') and (Size > 0) then
      Decimals := StrToIntDef(Query.Field(3),0) and $ffff
    else if Query.Field(2) = 'money' then Decimals := 2
    else Decimals := 4;
    FieldType := PgSqlToDelphiType(Query.Field(2), Size, ArraySubType, BlobType);
    if (FieldType = ftBlob) and StrCmpBegin(LowerCase(Table),'pg_') then
      FieldType := ftInteger;
    Default := Query.Field(5);
    { Put new field description }
    with SqlFields.Add(Table, Query.Field(1), '', Query.Field(2), FieldType,
      Size, Decimals,  atNone, StrCmpBegin(Query.Field(4),'t'), False, Default,
      BlobType)^ do
    begin
      Index := StrToIntDef(Query.Field(0), -1);
    end;
    Query.Next;
  end;
  Query.Close;
end;

{ Define all indices in query }
procedure TZCustomPgSqlDataset.AddTableIndices(Table: string;
  SqlFields: TSqlFields; SqlIndices: TSqlIndices);
  { Find field by index }
  function FindField(Index: Integer): PFieldDesc;
  var
    I: Integer;
  begin
    for I := 0 to SqlFields.Count-1 do
    begin
      Result := SqlFields[I];
      if StrCaseCmp(Result.Table, Table) and (Result.Index = Index) then
        Exit;
    end;
    Result := nil;
  end;
var
  Buffer: string;
  KeyType: TKeyType;
  SortType: TSortType;
  Query: TDirPgSqlQuery;
  FieldDesc: PFieldDesc;
begin
  { Set start values }
  Query := TDirPgSqlQuery(TransactObj.QueryHandle);
  Query.ShowIndexes(Table);
  { Fetch index descriptions }
  while not Query.EOF do
  begin
    { Define a key type }
    if Query.Field(3) = 't' then
    begin
      if StrCmpBegin(LowerCase(Query.Field(1)),LowerCase(Table)+'_pkey') then
        KeyType := ktPrimary
      else KeyType := ktUnique;
    end else KeyType := ktIndex;
    { Define sorting type }
    SortType := stAscending;
    { Define field names }
    Buffer := Query.Field(4);
    while Buffer <> '' do
    begin
      FieldDesc := FindField(StrToIntDef(StrTok(Buffer, ' '), -1));
      { Put new index description }
      if FieldDesc <> nil then
        SqlIndices.AddIndex(Query.Field(1), Table, FieldDesc.Field,
          KeyType, SortType);
    end;
    Query.Next;
  end;
  Query.Close;
end;

{ Check is table exist }
function TZCustomPgSqlDataset.CheckTableExistence(Table: string): Boolean;
var
  Query: TDirPgSqlQuery;
begin
  Query := TDirPgSqlQuery(TransactObj.QueryHandle);
  Query.Sql := Format('select tablename from pg_tables where tablename=''%s''',
    [LowerCase(Table)]);
  Query.Open;
  Result := (Query.Field(0) <> '');
  Query.Close;
end;

{ Auto form update sql query }
procedure TZCustomPgSqlDataset.FormSqlQuery(OldData, NewData: PRecordData);
var
  I: Integer;
  FieldDesc: PFieldDesc;
  RecordBlob: PRecordBlob;
  BlobObj: TDirBlob;
begin
  { Process large objects }
  if OldData.RecordType = ztDeleted then
    for I := 0 to SqlBuffer.SqlFields.Count-1 do
    begin
      FieldDesc := SqlBuffer.SqlFields[I];
      if not (FieldDesc.FieldType in [ftBlob, ftMemo, ftGraphic, ftFmtMemo]) then
        Continue;
      RecordBlob := PRecordBlob(@NewData.Bytes[FieldDesc.Offset+1]);
      if RecordBlob.Handle.Ptr <> 0 then
      begin
        BlobObj := Query.CreateBlobObject;
        try
          BlobObj.Handle := RecordBlob.Handle;
          DeleteBlob(BlobObj);
        finally
          BlobObj.Free;
        end;
      end;
    end;
  { Call inherited method }
  inherited FormSqlQuery(OldData, NewData);
end;

{ TZPgSqlQuery definition }

{$IFDEF WITH_IPROVIDER}
{ IProvider support }

{ Is in transaction }
function TZCustomPgSqlDataset.PSInTransaction: Boolean;
begin
  Result := True;
end;

{ Execute an sql statement }
function TZCustomPgSqlDataset.PSExecuteStatement(const ASql: string; AParams: TParams;
  ResultSet: Pointer): Integer;
begin
  if Assigned(ResultSet) then
  begin
    TDataSet(ResultSet^) := TZPgSqlQuery.Create(nil);
    with TZPgSqlQuery(ResultSet^) do
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
procedure TZCustomPgSqlDataset.PSSetCommandText(const CommandText: string);
begin
  Close;
  if Self is TZPgSqlQuery then
    TZPgSqlQuery(Self).Sql.Text := CommandText
  else
  if Self is TZPgSqlTable then
    TZPgSqlQuery(Self).TableName := CommandText;
end;

{$ENDIF}

{ TZPgSqlTable }

constructor TZPgSqlTable.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 DefaultIndex := True;
 ReadOnly := False;
end;

end.
