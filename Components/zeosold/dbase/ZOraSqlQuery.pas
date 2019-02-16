{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{             Oracle8 Query and Table components         }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZOraSqlQuery;

interface

{$R *.dcr}

uses
  SysUtils, Windows, Db, Classes, ZDirSql, ZDirOraSql, DbCommon,
  ZOraSqlCon, ZOraSqlTr, ZToken, ZLibOraSql, ZSqlExtra, ZQuery,
  ZSqlTypes, ZSqlItems, ZSqlBuffer;

{$INCLUDE ..\Zeos.inc}

type

  TZOraSqlOption = (ooAutoIncKey);
  TZOraSqlOptions = set of TZOraSqlOption;

  { Direct Oracle8 dataset with descendant of TZDataSet }
  TZCustomOraSqlDataset = class(TZDataSet)
  private
    FExtraOptions: TZOraSqlOptions;
    procedure SetDatabase(Value: TZOraSqlDatabase);
    procedure SetTransact(Value: TZOraSqlTransact);
    function  GetDatabase: TZOraSqlDatabase;
    function  GetTransact: TZOraSqlTransact;
  protected
    { Overriding ZDataset methods }
    procedure QueryRecord; override;
    procedure UpdateAfterInit(RecordData: PRecordData); override;
    procedure UpdateAfterPost(OldData, NewData: PRecordData); override;
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

    function FieldValueToSql(Value: string; FieldDesc: PFieldDesc): string; override;

    { Buffer support methods }
    procedure CopyRecord(SqlBuffer: TSqlBuffer; Source, Dest: PRecordData); override;
    procedure FreeRecord(SqlBuffer: TSqlBuffer; Value: PRecordData); override;
  published
    property ExtraOptions: TZOraSqlOptions read FExtraOptions write FExtraOptions;
    property Database: TZOraSqlDatabase read GetDatabase write SetDatabase;
    property Transaction: TZOraSqlTransact read GetTransact write SetTransact;
  end;

{ Direct OraSql query with descendant of TDataSet }
  TZOraSqlQuery = class(TZCustomOraSqlDataset)
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

{ Direct OraSql query with descendant of TDataSet }
  TZOraSqlTable = class(TZCustomOraSqlDataset)
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

{********** TZCustomOraSqlDataset implementation **********}

{ Class constructor }
constructor TZCustomOraSqlDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Query := TDirOraSqlQuery.Create(nil, nil);
  DatabaseType := dtOracle;
  FExtraOptions := [];
end;

{ Set connect to database component }
procedure TZCustomOraSqlDataset.SetDatabase(Value: TZOraSqlDatabase);
begin
  inherited SetDatabase(Value);
end;

{ Set connect to transact-server component }
procedure TZCustomOraSqlDataset.SetTransact(Value: TZOraSqlTransact);
begin
  inherited SetTransact(Value);
end;

{ Get connect to database component }
function TZCustomOraSqlDataset.GetDatabase: TZOraSqlDatabase;
begin
  Result := TZOraSqlDatabase(DatabaseObj);
end;

{ Get connect to transact-server component }
function TZCustomOraSqlDataset.GetTransact: TZOraSqlTransact;
begin
  Result := TZOraSqlTransact(TransactObj);
end;

{ Read query from server to internal buffer }
procedure TZCustomOraSqlDataset.QueryRecord;
var
  I, Count: Integer;
  RecordData: PRecordData;
  FieldDesc: PFieldDesc;
  Temp: string;
  TempTime: TDateTime;
  TimeStamp: TTimeStamp;
  BlobPtr: PRecordBlob;
  Status: Integer;
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
            Temp := ConvertFromSqlEnc(Query.Field(FieldDesc.FieldNo));
            SqlBuffer.SetFieldDataLen(FieldDesc,
              PChar(Temp), RecordData, Length(Temp));
          end;
        ftInteger, ftFloat:
          SqlBuffer.SetFieldData(FieldDesc,
             Query.FieldBuffer(FieldDesc.FieldNo), RecordData);
        ftDateTime:
          begin
            TimeStamp := DateTimeToTimeStamp(
              OraDateToDateTime(Query.FieldBuffer(FieldDesc.FieldNo)));
            TempTime := TimeStampToMSecs(TimeStamp);
            SqlBuffer.SetFieldData(FieldDesc, @TempTime, RecordData);
          end;
        ftMemo, ftBlob:
          begin
            { Process blob and memo fields }
            BlobPtr := PRecordBlob(@RecordData.Bytes[FieldDesc.Offset+1]);
            BlobPtr.Handle.Ptr := 0;
            BlobPtr.Handle.PtrEx := 0;
            BlobPtr.Size := 0;
            BlobPtr.Data := nil;
            BlobPtr.BlobType := FieldDesc.BlobType;

            if not Query.FieldIsNull(FieldDesc.FieldNo) then
            begin
              RecordData.Bytes[FieldDesc.Offset] := 0;
              { Fill internal blobs }
              if FieldDesc.BlobType = btInternal then
              begin
                if FieldDesc.FieldType = ftMemo then
                begin
                  Temp := ConvertFromSqlEnc(Query.Field(FieldDesc.FieldNo));
                  BlobPtr.Size := Length(Temp);
                  BlobPtr.Data := AllocMem(BlobPtr.Size);
                  System.Move(PChar(Temp)^, BlobPtr.Data^,  BlobPtr.Size);
                end
                else
                begin
                  BlobPtr.Size := PInteger(Query.FieldBuffer(FieldDesc.FieldNo))^;
                  BlobPtr.Data := AllocMem(BlobPtr.Size);
                  System.Move((Query.FieldBuffer(FieldDesc.FieldNo)+SizeOf(Integer))^,
                    BlobPtr.Data^,  BlobPtr.Size);
                end;
              end
              { Fill external blobs }
              else if not Query.FieldIsNull(FieldDesc.FieldNo) then begin
                Status := OCIDescriptorAlloc(TDirOraSqlConnect(Query.Connect).Handle,
                  POCIDescriptor(BlobPtr.Handle.Ptr), OCI_DTYPE_LOB, 0, nil);
                if Status <> OCI_SUCCESS then
                  DatabaseError('Lob allocation error in field "' + FieldDesc.Alias + '"');
                Status := OCILobAssign(TDirOraSqlConnect(Query.Connect).Handle,
                  TDirOraSqlTransact(Query.Transact).ErrorHandle,
                  PPOCIDescriptor(Query.FieldBuffer(FieldDesc.FieldNo))^,
                  POCIDescriptor(BlobPtr.Handle.Ptr));
                if Status <> OCI_SUCCESS then
                  DatabaseError('Lob assign error in field "' + FieldDesc.Alias + '"');
              end;
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

{************** Sql-queries processing ******************}

{ Fill collection with fields }
procedure TZCustomOraSqlDataset.AddTableFields(Table: string;
  SqlFields: TSqlFields);
var
  Size: Integer;
  Decimals: Integer;
  FieldType: TFieldType;
  Query: TDirOraSqlQuery;
  Default: string;
  BlobType: TBlobType;
begin
  Query := TDirOraSqlQuery(Transaction.QueryHandle);
  Query.ShowColumns(Table, '');
  while not Query.EOF do
  begin
    { Evalute field parameters }
    Size := StrToIntDef(Query.Field(3), 0);
    Decimals := StrToIntDef(Query.Field(6), 0);
    FieldType := OraSqlToDelphiType(Query.Field(2), Size, Decimals, BlobType);
    if FieldType <> ftString then Size := 0;
    Default := Query.Field(5);

    { Put new field description }
    SqlFields.Add(Table, Query.Field(1), '', Query.Field(2), FieldType,
      Size, Decimals, atNone, Query.Field(4) = 'Y', False, Default, BlobType);
    Query.Next;
  end;
  Query.Close;
end;

{ Fill collection with indices }
procedure TZCustomOraSqlDataset.AddTableIndices(Table: string;
  SqlFields: TSqlFields; SqlIndices: TSqlIndices);
var
  KeyType: TKeyType;
  SortType: TSortType;
  Query: TDirOraSqlQuery;
begin
  Query := TDirOraSqlQuery(TransactObj.QueryHandle);
  Query.ShowIndexes(Table);
  while not Query.EOF do
  begin
    { Define a key type }
    if Query.Field(2) = 'UNIQUE' then
    begin
      if Query.Field(3) = 'Y' then
        KeyType := ktPrimary
      else KeyType := ktUnique;
    end else KeyType := ktIndex;
    { Define a sorting mode }
    SortType := stAscending;

    { Put new index description }
    SqlIndices.AddIndex(Query.Field(0), Table, Query.Field(4),
      KeyType, SortType);
    Query.Next;
  end;
  Query.Close;
end;

{ Convert field value to sql value }
function TZCustomOraSqlDataset.FieldValueToSql(Value: string;
  FieldDesc: PFieldDesc): string;

  function BytesToSql(Value: string): string;
  var
    I: Integer;
  begin
    if Value = '' then
    begin
      Result := 'NULL';
      Exit;
    end
    else
    begin
      Result := '';
      for I := 1 to Length(Value) do
        Result := Result + IntToHex(Ord(Value[I]),2);
      Result := '''' + Result + '''';
    end;
  end;

begin
  if FieldDesc.FieldType = ftBlob then
    Result := BytesToSql(Value)
  else begin
    Result := inherited FieldValueToSql(Value, FieldDesc);

    if FieldDesc.FieldType = ftDateTime then
    begin
//      Result := 'TO_DATE(' + Result + ',''YYYY-MM-DD HH24-MI-SS'')'
      if Pos(' ', Result) > 0 then
        Result := 'TO_DATE(' + Result + ',''' + UpperCase(ShortDateFormat) + ' HH24' + TimeSeparator + 'MI' + TimeSeparator + 'SS'')'
      else
        Result := 'TO_DATE(' + Result + ',''' + UpperCase(ShortDateFormat) + ''')';
    end
    else
    if (FieldDesc.FieldType = ftString)
      and (StrCaseCmp(FieldDesc.TypeName, 'NCHAR')
      or StrCaseCmp(FieldDesc.TypeName, 'NVARCHAR2')) then
      Result := 'N' + Result;
  end;
end;

{ Update record after initialization }
procedure TZCustomOraSqlDataset.UpdateAfterInit(RecordData: PRecordData);

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
    if Result.FieldType <> ftInteger then
      Result := nil;
  end;

var
  FieldDesc: PFieldDesc;
begin
  inherited UpdateAfterInit(RecordData);

  if ooAutoIncKey in FExtraOptions then
  begin
    FieldDesc := FindPrimaryKey;
    if FieldDesc <> nil then
      SqlBuffer.SetField(FieldDesc, EvaluteDef(Format('%s_%s_seq.NextVal',
        [SqlParser.Tables[0], FieldDesc.Field])), RecordData);
  end;
end;

{ Update Lobs after update or insert }
procedure TZCustomOraSqlDataset.UpdateAfterPost(OldData,
  NewData: PRecordData);
var
  I: Integer;
  Sql: string;
  BlobDescs: array[0..MAX_FIELD_COUNT-1] of PFieldDesc;
  BlobCount: Integer;
  FieldDesc: PFieldDesc;
  FieldValue: string;
  OraConnect: TDirOraSqlConnect;
  OraTransact: TDirOraSqlTransact;
  UpdateQuery: TDirOraSqlQuery;
  LobHandle: POCILobLocator;
  Affected: ub4;
begin
  inherited UpdateAfterPost(OldData, NewData);

  if SqlParser.Tables.Count = 0 then Exit;
  if NewData.RecordType in [ztUnmodified, ztDeleted] then Exit;

  Sql := '';
  BlobCount := 0;
  for I := 0 to SqlBuffer.SqlFields.Count-1 do
  begin
    FieldDesc := SqlBuffer.SqlFields[I];
    if not StrCaseCmp(FieldDesc.Table, SqlParser.Tables[0]) then
      Continue;
    if not (FieldDesc.FieldType in [ftBlob, ftMemo])
      or (FieldDesc.BlobType <> btExternal) then Continue;
    if SqlBuffer.GetFieldNull(FieldDesc, NewData) then Continue;
    FieldValue := SqlBuffer.GetField(FieldDesc, NewData);
    if (NewData.RecordType = ztModified)
      and (FieldValue = SqlBuffer.GetField(FieldDesc, OldData)) then
      Continue;
    if Sql <> '' then Sql := Sql + ',';
    Sql := Sql + FieldDesc.Field;
    BlobDescs[BlobCount] := FieldDesc;
    Inc(BlobCount);
  end;
  if Sql = '' then Exit;

  Sql := 'SELECT ' + Sql + ' FROM ' + SqlParser.Tables[0]
    + FormSqlWhere(SqlParser.Tables[0], OldData) + ' FOR UPDATE';
  OraConnect := TDirOraSqlConnect(Query.Connect);
  OraTransact := TDirOraSqlTransact(Query.Transact);
  UpdateQuery := TDirOraSqlQuery.Create(OraConnect, OraTransact);
  UpdateQuery.Sql := Sql;
  UpdateQuery.Open;
  for I := 0 to MinIntValue([BlobCount, UpdateQuery.FieldCount]) do
  begin
    if not (UpdateQuery.FieldType(I) in [SQLT_BLOB, SQLT_CLOB]) or
      UpdateQuery.FieldIsNull(I) then
      Continue;
    FieldValue := SqlBuffer.GetField(BlobDescs[I], NewData);
    LobHandle := PPOCIDescriptor(UpdateQuery.FieldBuffer(I))^;

    Affected := Length(FieldValue);
    OCILobWrite(OraTransact.Handle, OraTransact.ErrorHandle,
      LobHandle, Affected, 1, PChar(FieldValue), Affected,
      OCI_ONE_PIECE, nil, nil, 0, SQLCS_IMPLICIT);
  end;
  UpdateQuery.Close;
end;

{ Assign Lob handlers }
procedure TZCustomOraSqlDataset.CopyRecord(SqlBuffer: TSqlBuffer; Source,
  Dest: PRecordData);
var
  I, Status: Integer;
  NewPtr: POCILobLocator;
  DestBlob: PRecordBlob;
begin
  for I := 0 to SqlBuffer.SqlFields.Count-1 do
    if (SqlBuffer.SqlFields[I].FieldType in [ftBlob, ftMemo])
      and (Dest.Bytes[SqlBuffer.SqlFields[I].Offset] = 0) then
    begin
      DestBlob := PRecordBlob(@Dest.Bytes[SqlBuffer.SqlFields[I].Offset+1]);
      if (DestBlob.BlobType = btExternal) and (DestBlob.Handle.Ptr <> 0) then
      begin
        Status := OCIDescriptorAlloc(TDirOraSqlConnect(Query.Connect).Handle,
           POCIDescriptor(NewPtr), OCI_DTYPE_LOB, 0, nil);
        if Status = OCI_SUCCESS then
          Status := OCILobAssign(TDirOraSqlConnect(Query.Connect).Handle,
            TDirOraSqlTransact(Query.Transact).ErrorHandle,
            POCIDescriptor(DestBlob.Handle.Ptr),
            POCIDescriptor(NewPtr));
        if Status = OCI_SUCCESS then
          DestBlob.Handle.Ptr := Integer(NewPtr)
        else
          DestBlob.Handle.Ptr := 0;
      end;
   end;
end;

{ Free Lob handlers }
procedure TZCustomOraSqlDataset.FreeRecord(SqlBuffer: TSqlBuffer;
  Value: PRecordData);
var
  I: Integer;
  BlobPtr: PRecordBlob;
begin
  for I := 0 to SqlBuffer.SqlFields.Count-1 do
    if (SqlBuffer.SqlFields[I].FieldType in [ftBlob, ftMemo])
      and (Value.Bytes[SqlBuffer.SqlFields[I].Offset] = 0) then
    begin
      BlobPtr := PRecordBlob(@Value.Bytes[SqlBuffer.SqlFields[I].Offset+1]);
      if (BlobPtr.BlobType = btExternal) and (BlobPtr.Handle.Ptr <> 0) then
      begin          
        OCIDescriptorFree(POCIDescriptor(BlobPtr.Handle.Ptr), OCI_DTYPE_LOB);
        BlobPtr.Handle.Ptr := 0;
      end;
   end;
end;

{$IFDEF WITH_IPROVIDER}
{ IProvider support }

{ Is in transaction }
function TZCustomOraSqlDataset.PSInTransaction: Boolean;
begin
  Result := True;
end;

{ Execute an sql statement }
function TZCustomOraSqlDataset.PSExecuteStatement(const ASql: string; AParams: TParams;
  ResultSet: Pointer): Integer;
begin
  if Assigned(ResultSet) then
  begin
    TDataSet(ResultSet^) := TZOraSqlQuery.Create(nil);
    with TZOraSqlQuery(ResultSet^) do
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
procedure TZCustomOraSqlDataset.PSSetCommandText(const CommandText: string);
begin
  Close;
  if Self is TZOraSqlQuery then
    TZOraSqlQuery(Self).Sql.Text := CommandText
  else
  if Self is TZOraSqlTable then
    TZOraSqlQuery(Self).TableName := CommandText;
end;

{$ENDIF}

{ TZOraSqlTable }

constructor TZOraSqlTable.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 DefaultIndex := True;
 ReadOnly := False;
end;

end.
